# =========================================================================
# R/data_io.R
#
# Session persistence + shared stat helpers.
#
# Phase 3 originally wrote one JSON file per session to data/sessions/.
# Phase 5 (Turso migration) moved persistence to a cloud SQLite database.
# The session CRUD functions below (save_session, load_session,
# list_sessions, delete_session) are now THIN WRAPPERS around the
# db_* functions in R/db.R, kept here so the rest of the app doesn't
# have to care which backend is in use.
# =========================================================================

suppressPackageStartupMessages({
  library(jsonlite)
})

# ---------- derive one possession-record from a button spec --------------
make_possession <- function(spec, play, quarter, reversals, paint_touches, clock,
                             category = NA_character_) {
  data.frame(
    possession_id    = NA_integer_,
    ts               = Sys.time(),
    quarter          = quarter,
    play             = play,
    result           = spec$label,
    pts              = as.numeric(spec$pts),
    is_fga           = isTRUE(spec$is_fga),
    is_fgm           = isTRUE(spec$is_fgm),
    is_to            = isTRUE(spec$is_to),
    is_foul          = isTRUE(spec$is_foul),
    ball_reversals   = as.integer(reversals),
    paint_touches    = as.integer(paint_touches),
    clock            = ifelse(is.null(clock) || !nzchar(clock), NA_character_, clock),
    category         = if (is.null(category) || !nzchar(as.character(category))) NA_character_
                       else as.character(category),
    stringsAsFactors = FALSE
  )
}

# ---------- stats helpers ----------------------------------------------
summarise_possessions <- function(df) {
  if (is.null(df) || nrow(df) == 0) {
    return(list(poss = 0L, ppp = NA_real_, fg_pct = NA_real_,
                avg_rev = NA_real_, avg_paint = NA_real_,
                pts = 0, fga = 0, fgm = 0, tos = 0, fouls = 0))
  }
  poss <- nrow(df)
  pts  <- sum(df$pts, na.rm = TRUE)
  fga  <- sum(df$is_fga, na.rm = TRUE)
  fgm  <- sum(df$is_fgm, na.rm = TRUE)
  list(
    poss      = poss,
    ppp       = pts / poss,
    fg_pct    = if (fga > 0) fgm / fga else NA_real_,
    avg_rev   = mean(df$ball_reversals, na.rm = TRUE),
    avg_paint = mean(df$paint_touches,  na.rm = TRUE),
    pts       = pts,
    fga       = fga,
    fgm       = fgm,
    tos       = sum(df$is_to, na.rm = TRUE),
    fouls     = sum(df$is_foul, na.rm = TRUE)
  )
}

filter_quarter <- function(df, q) {
  if (is.null(df) || nrow(df) == 0 || q == "TOTAL") return(df)
  dplyr::filter(df, quarter == q)
}
filter_play <- function(df, p) {
  if (is.null(df) || nrow(df) == 0 || is.null(p) || is.na(p) || !nzchar(p))
    return(df)
  dplyr::filter(df, play == p)
}

fmt_int <- function(x) if (is.na(x)) "-" else as.character(as.integer(x))
fmt_dec <- function(x, d = 2) if (is.na(x)) "-" else formatC(x, format = "f", digits = d)
fmt_pct <- function(x, d = 1) if (is.na(x)) "-" else paste0(formatC(100*x, format = "f", digits = d), "%")

# ---------- Session id --------------------------------------------------
make_session_id <- function(ts = Sys.time()) {
  format(ts, "%Y%m%d_%H%M%S")
}

# =========================================================================
# Session persistence -- DEMO: in-memory per-visitor store (R/db_demo.R).
# All functions take app_state so each browser tab stays isolated.
# =========================================================================

#' Save the current app_state possessions to the per-visitor store.
#' No-op if zero possessions logged.
save_session <- function(app_state) {
  if (nrow(app_state$possessions) == 0) return(invisible(NULL))
  db_save_session(app_state)
}

#' Load a saved session by its ID.
load_session <- function(session_id, app_state) {
  db_load_session(session_id, app_state)
}

#' Delete a saved session by ID.
delete_session <- function(session_id, app_state) {
  db_delete_session(session_id, app_state)
}

#' Session roster for the History tab + Trends refresh trigger.
list_sessions <- function(app_state) {
  df <- db_list_sessions(app_state)
  if (is.null(df) || nrow(df) == 0) return(data.frame())
  df
}

# ---------- misc --------------------------------------------------------
empty_possessions_df <- function() {
  data.frame(
    possession_id    = integer(0),
    ts               = as.POSIXct(character(0)),
    quarter          = character(0),
    play             = character(0),
    result           = character(0),
    pts              = numeric(0),
    is_fga           = logical(0),
    is_fgm           = logical(0),
    is_to            = logical(0),
    is_foul          = logical(0),
    ball_reversals   = integer(0),
    paint_touches    = integer(0),
    clock            = character(0),
    category         = character(0),
    stringsAsFactors = FALSE
  )
}
