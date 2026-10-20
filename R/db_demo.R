# =========================================================================
# R/db_demo.R  --  DEMO replacement for R/db.R
#
# The production app persists sessions to Turso (cloud SQLite) over HTTP.
# For the public demo we DON'T want:
#   - credentials embedded in the deploy
#   - visitors to see each other's clicks (per-visitor fresh state)
#   - a persistent DB that drifts away from the seeded state
#
# So this file keeps the SAME function signatures as db.R but writes to
# an in-memory per-Shiny-session store that lives on app_state. Each
# browser tab gets its own `app_state$sessions_store` (a named list of
# session records); when the tab closes the store is garbage-collected
# and the next visitor starts fresh from the preloaded seed data.
#
# `seed_demo_sessions()` lives in R/seed_sessions.R and is called once
# per Shiny session from app.R to populate the initial store.
# =========================================================================

# ---- Schema (no-op for in-memory) --------------------------------------
db_ensure_schema <- function() invisible(TRUE)
db_safe_ensure_schema <- function() invisible(TRUE)

# ---- Session CRUD ------------------------------------------------------

#' Upsert the current app_state possessions as a saved session in the
#' per-visitor in-memory store.
db_save_session <- function(app_state) {
  sid  <- isolate(app_state$session_id)
  poss <- isolate(app_state$possessions)
  if (nrow(poss) == 0) return(invisible(NULL))

  current <- isolate(app_state$sessions_store)
  if (is.null(current)) current <- list()

  current[[sid]] <- list(
    session_id         = sid,
    session_name       = isolate(app_state$session_name),
    session_date       = isolate(app_state$session_date),
    session_type       = isolate(app_state$session_type) %||na% "Practice",
    team               = isolate(app_state$team),
    session_created_at = isolate(app_state$session_created_at),
    updated_at         = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    possessions        = poss
  )
  app_state$sessions_store <- current
  invisible(sid)
}

#' Load one session by ID from the per-visitor store.
db_load_session <- function(session_id, app_state) {
  store <- isolate(app_state$sessions_store)
  if (is.null(store) || !session_id %in% names(store)) return(NULL)
  rec  <- store[[session_id]]
  list(
    session_id         = rec$session_id,
    session_name       = rec$session_name,
    session_date       = as.Date(rec$session_date),
    team               = rec$team,
    session_type       = rec$session_type %||na% "Practice",
    session_created_at = rec$session_created_at,
    possessions        = rec$possessions
  )
}

#' Delete a session from the per-visitor store.
db_delete_session <- function(session_id, app_state) {
  current <- isolate(app_state$sessions_store)
  if (is.null(current)) return(invisible(TRUE))
  current[[session_id]] <- NULL
  app_state$sessions_store <- current
  invisible(TRUE)
}

#' Session roster for the History tab + Trends refresh key.
#' Returns a data.frame with one row per session plus roll-up stats --
#' identical columns to the production db_list_sessions() so downstream
#' code doesn't notice the backend swap.
db_list_sessions <- function(app_state) {
  store <- isolate(app_state$sessions_store)
  if (is.null(store) || length(store) == 0) return(data.frame())

  rows <- lapply(store, function(rec) {
    poss <- rec$possessions
    n_poss  <- nrow(poss)
    pts_tot <- if (n_poss > 0) sum(poss$pts, na.rm = TRUE) else 0L
    fga_tot <- if (n_poss > 0) sum(poss$is_fga, na.rm = TRUE) else 0L
    fgm_tot <- if (n_poss > 0) sum(poss$is_fgm, na.rm = TRUE) else 0L
    most_used <- if (n_poss > 0) {
      tt <- sort(table(poss$play), decreasing = TRUE)
      names(tt)[1]
    } else NA_character_

    data.frame(
      session_id   = rec$session_id,
      session_name = rec$session_name,
      session_date = as.character(rec$session_date),
      team         = rec$team,
      session_type = rec$session_type %||na% "Practice",
      updated_at   = rec$updated_at %||na% rec$session_created_at,
      n_poss       = as.integer(n_poss),
      total_points = as.integer(pts_tot),
      ppp          = if (n_poss > 0) pts_tot / n_poss else NA_real_,
      fg_pct       = if (fga_tot > 0) fgm_tot / fga_tot else NA_real_,
      most_used    = most_used,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  df <- df[order(df$session_date, decreasing = TRUE), ]
  df
}

#' Concatenate every saved session's possessions into one data.frame
#' with session metadata attached. Used by the Trends tab.
#' Returns NULL (not an empty frame) when the store is empty, matching
#' the production db_load_all_possessions() contract.
db_load_all_possessions <- function(app_state) {
  store <- isolate(app_state$sessions_store)
  if (is.null(store) || length(store) == 0) return(NULL)

  rows <- lapply(store, function(rec) {
    poss <- rec$possessions
    if (nrow(poss) == 0) return(NULL)
    poss$session_id   <- rec$session_id
    poss$session_name <- rec$session_name
    poss$session_date <- as.Date(rec$session_date)
    poss$session_type <- rec$session_type %||na% "Practice"
    poss
  })
  rows <- Filter(Negate(is.null), rows)
  if (length(rows) == 0) return(NULL)

  # Ensure column union across sessions (older sessions may lack newer
  # columns like `category` from the 2026-10 schema expansion).
  all_cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    for (c in setdiff(all_cols, names(r))) r[[c]] <- NA
    r[all_cols]
  })
  df <- do.call(rbind, rows)
  rownames(df) <- NULL
  df
}
