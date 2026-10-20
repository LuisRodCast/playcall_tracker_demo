# =========================================================================
# R/mod_halftime.R  --  Halftime popup content
# Returns the body of the Halftime report. The popup container itself
# is declared in mod_tracker_ui() so it opens via updateF7Popup().
# =========================================================================

halftime_body_ui <- function(app_state) {

  poss_all <- isolate(app_state$possessions)
  session_name <- isolate(app_state$session_name)
  session_date <- isolate(app_state$session_date)

  if (nrow(poss_all) == 0) {
    return(tagList(
      tags$p("Nothing to report yet."),
      tags$p("Log at least one possession first, then try again.")
    ))
  }

  first_half <- dplyr::filter(poss_all, quarter %in% c("Q1","Q2"))

  if (nrow(first_half) == 0) {
    return(tagList(
      tags$p(tags$b(session_name), " · ", session_date),
      tags$p("No possessions logged in Q1 or Q2 yet."),
      tags$p("Total possessions across all quarters: ",
             tags$b(nrow(poss_all)))
    ))
  }

  s <- summarise_possessions(first_half)
  scoring <- sum(first_half$pts > 0, na.rm = TRUE)
  to_pct  <- if (s$poss > 0) s$tos / s$poss else NA_real_

  per_play <- first_half |>
    dplyr::group_by(play) |>
    dplyr::summarise(
      poss   = dplyr::n(),
      pts    = sum(pts, na.rm = TRUE),
      ppp    = sum(pts, na.rm = TRUE) / dplyr::n(),
      fga    = sum(is_fga, na.rm = TRUE),
      fgm    = sum(is_fgm, na.rm = TRUE),
      fg_pct = ifelse(sum(is_fga, na.rm = TRUE) > 0,
                      sum(is_fgm, na.rm = TRUE) / sum(is_fga, na.rm = TRUE),
                      NA_real_),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(poss))

  best  <- if (any(per_play$poss >= 2)) {
    sub <- dplyr::filter(per_play, poss >= 2); sub[which.max(sub$ppp), ]
  } else NULL
  worst <- if (any(per_play$poss >= 2)) {
    sub <- dplyr::filter(per_play, poss >= 2); sub[which.min(sub$ppp), ]
  } else NULL
  most <- per_play[1, ]

  tagList(
    tags$div(class = "detail-title",
             paste0("Halftime report — ", session_name)),
    tags$div(class = "detail-sub",
             session_date, " · First half (Q1+Q2)"),

    div(class = "detail-stats",
      history_stat("POSS",         fmt_int(s$poss)),
      history_stat("PTS",          fmt_int(s$pts)),
      history_stat("PPP",          fmt_dec(s$ppp, 2)),
      history_stat("FG%",          fmt_pct(s$fg_pct, 1)),
      history_stat("SCORING POSS", fmt_int(scoring)),
      history_stat("TO%",          fmt_pct(to_pct, 1))
    ),

    if (!is.null(most))
      tags$p(tags$b("Most used: "), most$play,
             " — ", most$poss, " possessions"),
    if (!is.null(best))
      tags$p(tags$b("Best play (≥2 POSS): "),
             best$play, " — PPP ", fmt_dec(best$ppp, 2)),
    if (!is.null(worst))
      tags$p(tags$b("Worst play (≥2 POSS): "),
             worst$play, " — PPP ", fmt_dec(worst$ppp, 2)),

    tags$h4("Play breakdown"),
    reactable::reactable(
      per_play |> dplyr::transmute(play, poss, pts, ppp, fg_pct),
      compact = TRUE, pagination = FALSE,
      theme = tracker_table_theme(),
      columns = list(
        play   = reactable::colDef(name = "PLAY", minWidth = 150),
        poss   = reactable::colDef(name = "POSS",  width = 70, align = "right"),
        pts    = reactable::colDef(name = "PTS",   width = 70, align = "right"),
        ppp    = reactable::colDef(name = "PPP",   width = 80, align = "right",
                   cell = function(v) fmt_dec(v, 2)),
        fg_pct = reactable::colDef(name = "FG%",   width = 90, align = "right",
                   cell = function(v) fmt_pct(v, 1))
      )
    )
  )
}
