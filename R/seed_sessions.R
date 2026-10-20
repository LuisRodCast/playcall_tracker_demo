# =========================================================================
# R/seed_sessions.R
#
# Builds the preloaded sessions_store for a fresh demo visitor. Called
# once per Shiny session from app.R so every browser tab lands on an
# app that already has data in History, Summary, Coach, and Trends --
# rather than a blank state that requires clicking around to populate.
#
# Returns a NAMED list of session records matching the shape of
# db_demo.R's app_state$sessions_store. Each record contains:
#   session_id, session_name, session_date, team, session_type,
#   session_created_at, updated_at, possessions (data.frame)
#
# All content is synthetic. Play names come from the generic playbook
# in data/plays.yaml. No real NMSU scouting information is used.
# =========================================================================

# ---- Sampling helpers --------------------------------------------------

# Weighted result sampler reflecting realistic D1 women's basketball
# proportions. Weights are illustrative; adjust to taste in the file.
.RESULT_WEIGHTS <- c(
  "+3"       = 0.05,
  "+2"       = 0.15,
  "-2"       = 0.33,
  "-3"       = 0.14,
  "+1F"      = 0.05,
  "+2F"      = 0.05,
  "-1F"      = 0.02,
  "-2F"      = 0.02,
  "+3F"      = 0.005,
  "-3F"      = 0.005,
  "And1-3p"  = 0.01,
  "And1-4p"  = 0.005,
  "0"        = 0.03,
  "FOUL"     = 0.05,
  "TO"       = 0.08
)
# Normalize in case weights don't sum to 1
.RESULT_WEIGHTS <- .RESULT_WEIGHTS / sum(.RESULT_WEIGHTS)

# Look up a RESULT_BUTTONS spec by label
.spec_for <- function(label) {
  for (s in RESULT_BUTTONS) if (identical(s$label, label)) return(s)
  stop("Unknown result label in seed sampler: ", label)
}

# Build one possession row from a sampled result + play + quarter + category
.seed_possession <- function(id, ts, quarter, play, result, category = NA_character_,
                              reversals, paint, clock = NA_character_) {
  spec <- .spec_for(result)
  data.frame(
    possession_id    = as.integer(id),
    ts               = ts,
    quarter          = quarter,
    play             = play,
    result           = spec$label,
    pts              = as.numeric(spec$pts),
    is_fga           = isTRUE(spec$is_fga),
    is_fgm           = isTRUE(spec$is_fgm),
    is_to            = isTRUE(spec$is_to),
    is_foul          = isTRUE(spec$is_foul),
    ball_reversals   = as.integer(reversals),
    paint_touches    = as.integer(paint),
    clock            = if (is.na(clock) || !nzchar(clock)) NA_character_ else clock,
    category         = if (is.na(category) || !nzchar(category)) NA_character_ else category,
    stringsAsFactors = FALSE
  )
}

# Reversals/paint touches sampled from loose plausible ranges
.sample_rev   <- function() sample(0:3, 1, prob = c(0.45, 0.30, 0.18, 0.07))
.sample_paint <- function() sample(0:3, 1, prob = c(0.35, 0.35, 0.20, 0.10))

# ---- Practice session builder ------------------------------------------

.build_practice_session <- function() {
  plays <- c("ISO", "HORNS", "FLEX", "STAGGER", "PNR-L", "PNR-R",
             "FIST", "DRAG", "PINCH-POST")
  n <- 22L
  set.seed(2027)  # stable seed = same preload for every visitor
  results <- sample(names(.RESULT_WEIGHTS), n, replace = TRUE,
                     prob = unname(.RESULT_WEIGHTS))
  picked_plays <- sample(plays, n, replace = TRUE)
  base_time <- as.POSIXct("2026-10-13 15:00:00", tz = "UTC")

  rows <- lapply(seq_len(n), function(i) {
    .seed_possession(
      id        = i,
      ts        = base_time + (i * 90),
      quarter   = "Practice",
      play      = picked_plays[i],
      result    = results[i],
      category  = NA_character_,  # practice sessions don't tag categories
      reversals = .sample_rev(),
      paint     = .sample_paint(),
      clock     = NA_character_
    )
  })
  poss <- do.call(rbind, rows)

  list(
    session_id         = "demo_practice_20261013",
    session_name       = "Demo Practice -- Film Review Day",
    session_date       = as.Date("2026-10-13"),
    team               = "Demo Team",
    session_type       = "Practice",
    session_created_at = "2026-10-13T15:00:00Z",
    updated_at         = "2026-10-13T17:15:00Z",
    possessions        = poss
  )
}

# ---- Game session builder ----------------------------------------------

.build_game_session <- function() {
  plays <- c("ISO", "HORNS", "FLEX", "STAGGER", "PNR-L", "PNR-R",
             "FIST", "DRAG", "SPREAD", "PITCH-AHEAD",
             "PUT-BACK", "ZONE-OVER", "ZONE-SKIP")
  n <- 34L
  set.seed(2029)  # stable seed
  results <- sample(names(.RESULT_WEIGHTS), n, replace = TRUE,
                     prob = unname(.RESULT_WEIGHTS))
  picked_plays <- sample(plays, n, replace = TRUE)
  # Assign categories with weights roughly matching real coaching mix
  cats_pool <- CATEGORIES   # Transition / Secondary / 2nd Shots / HC Sets
  cat_weights <- c(0.18, 0.14, 0.08, 0.60)  # HC Sets dominates
  cats <- sample(cats_pool, n, replace = TRUE, prob = cat_weights)
  # Distribute possessions evenly across 4 quarters
  qtr <- rep(c("Q1", "Q2", "Q3", "Q4"), length.out = n)
  # Fake clock stamps counting down from 10:00 each quarter
  mk_clock <- function(i) {
    secs_in <- (i %% 9) * 60
    mins_left <- 10 - (i %% 9)
    sprintf("%d:%02d", pmax(0, mins_left), 60 - (secs_in %% 60))
  }

  base_time <- as.POSIXct("2026-10-16 19:05:00", tz = "UTC")

  rows <- lapply(seq_len(n), function(i) {
    .seed_possession(
      id        = i,
      ts        = base_time + (i * 45),
      quarter   = qtr[i],
      play      = picked_plays[i],
      result    = results[i],
      category  = cats[i],
      reversals = .sample_rev(),
      paint     = .sample_paint(),
      clock     = mk_clock(i)
    )
  })
  poss <- do.call(rbind, rows)

  list(
    session_id         = "demo_game_20261016",
    session_name       = "Demo Game -- vs North Rivals",
    session_date       = as.Date("2026-10-16"),
    team               = "Demo Team",
    session_type       = "Game",
    session_created_at = "2026-10-16T19:00:00Z",
    updated_at         = "2026-10-16T21:45:00Z",
    possessions        = poss
  )
}

# ---- Public entry point ------------------------------------------------

#' Returns the preloaded sessions_store for a fresh visitor.
#' Called once from app.R's server() right after app_state is created.
seed_demo_sessions <- function() {
  list(
    "demo_practice_20261013" = .build_practice_session(),
    "demo_game_20261016"     = .build_game_session()
  )
}
