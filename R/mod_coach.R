# =========================================================================
# R/mod_coach.R  --  Tab 3  --  Coach dashboard
#
# Matches the "Coach" screen in the Live Play Call Tracker reference:
#   - LAST 5 POSSESSIONS strip of colored chips
#   - 8 top-line stat tiles (POSS, PPP, TOTAL POINTS, SCORING POSS,
#     FG%, TURNOVERS, TO%, LAST 5 PPP)
#   - BEST PLAY / WORST PLAY / MOST USED hero cards
#   - PLAY COMPARISON table (PLAY, POSS, PTS, PPP, SCORING POSS, FG%, TO)
# =========================================================================

mod_coach_ui <- function(id) {
  ns <- NS(id)
  div(class = "coach-page",

    tab_header("Coach"),

    div(class = "quarter-row",
      tags$label("QUARTER", class = "control-label"),
      quarter_chips(ns, "quarter", selected = "Q1")
    ),

    # ---- Category filter (Game sessions only) ---------------------------
    # Appears only when the current session is a Game. "All" leaves the
    # whole dashboard aggregated across categories; picking a category
    # filters every stat tile, hero card and comparison row to that
    # category only.
    uiOutput(ns("category_filter_row")),

    # ---- Last 5 strip --------------------------------------------------
    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title", "LAST 5 POSSESSIONS"),
        tags$div(class = "legend-row",
          legend_dot(NMSU$make_2,    "Make"),
          legend_dot(NMSU$ft_make_1, "FT"),
          legend_dot(NMSU$miss_2,    "Miss"),
          legend_dot(NMSU$turnover,  "TO")
        )
      ),
      uiOutput(ns("last5_strip"))
    ),

    # ---- 8 top stat tiles ---------------------------------------------
    div(class = "coach-stats-grid",
      coach_stat_tile(ns, "possessions",    "POSSESSIONS"),
      coach_stat_tile(ns, "ppp",            "PPP",             highlight = TRUE),
      coach_stat_tile(ns, "total_points",   "TOTAL POINTS"),
      coach_stat_tile(ns, "scoring_poss",   "SCORING POSS"),
      coach_stat_tile(ns, "fg_pct",         "FG%"),
      coach_stat_tile(ns, "turnovers",      "TURNOVERS"),
      coach_stat_tile(ns, "to_pct",         "TO%"),
      coach_stat_tile(ns, "last5_ppp",      "LAST 5 PPP",      highlight = TRUE)
    ),

    # ---- Best / Worst / Most Used hero cards --------------------------
    div(class = "hero-grid",
      hero_card(ns, "best_play",   "BEST PLAY (PPP)",  color = NMSU$make_2),
      hero_card(ns, "worst_play",  "WORST PLAY (PPP)", color = NMSU$miss_2),
      hero_card(ns, "most_used",   "MOST USED",        color = NMSU$ft_make_1)
    ),

    # ---- Play comparison table ----------------------------------------
    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title",
                HTML("&#9889; PLAY COMPARISON"))
      ),
      reactable::reactableOutput(ns("comparison_tbl"))
    )
  )
}

mod_coach_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    # Category filter chip row — only built during Game sessions
    output$category_filter_row <- renderUI({
      is_game <- identical(app_state$session_type, "Game")
      if (!is_game) return(NULL)
      div(class = "quarter-row",
        tags$label("CATEGORY", class = "control-label"),
        quarter_chips(ns, "cat_filter",
                      quarters = c("All", CATEGORIES),
                      selected = "All")
      )
    })

    # Quarter + category combined filter.
    q_df <- reactive({
      base <- filter_quarter(app_state$possessions, input$quarter)
      cat  <- input$cat_filter
      if (is.null(cat) || !nzchar(cat) || identical(cat, "All")) return(base)
      dplyr::filter(base, !is.na(category), category == cat)
    })

    # ---- Last 5 strip -------------------------------------------------
    output$last5_strip <- renderUI({
      df <- q_df()
      if (nrow(df) == 0) {
        return(tags$div(class = "muted-note",
                        "No possessions yet this quarter."))
      }
      last5 <- utils::tail(df, 5)
      chips <- lapply(seq_len(nrow(last5)), function(i) {
        r <- last5[i, ]
        tags$div(class = "poss-chip",
                 style = sprintf("background:%s;", possession_chip_color(r)),
                 title = sprintf("%s  %s", r$play, r$result))
      })
      do.call(tagList, chips)
    })

    # ---- 8 top-line stat tiles ---------------------------------------
    stats <- reactive({
      s <- summarise_possessions(q_df())
      s$scoring_poss <- sum(q_df()$pts > 0, na.rm = TRUE)
      s$to_pct       <- if (s$poss > 0) s$tos / s$poss else NA_real_
      s$last5_ppp    <- {
        tail5 <- utils::tail(q_df(), 5)
        if (nrow(tail5) == 0) NA_real_
        else sum(tail5$pts, na.rm = TRUE) / nrow(tail5)
      }
      s
    })

    output$possessions  <- renderText(fmt_int(stats()$poss))
    output$ppp          <- renderText(fmt_dec(stats()$ppp, 2))
    output$total_points <- renderText(fmt_int(stats()$pts))
    output$scoring_poss <- renderText(fmt_int(stats()$scoring_poss))
    output$fg_pct       <- renderText(fmt_pct(stats()$fg_pct, 1))
    output$turnovers    <- renderText(fmt_int(stats()$tos))
    output$to_pct       <- renderText(fmt_pct(stats()$to_pct, 1))
    output$last5_ppp    <- renderText(fmt_dec(stats()$last5_ppp, 2))

    # ---- Best / Worst / Most-Used calculations -----------------------
    # Returns a properly-typed empty tibble when no possessions exist
    # so downstream filter / which.max / which.min calls can safely run.
    empty_rollup <- function() {
      tibble::tibble(
        play         = character(0),
        poss         = integer(0),
        pts          = numeric(0),
        ppp          = numeric(0),
        scoring_poss = integer(0),
        fga          = integer(0),
        fgm          = integer(0),
        fg_pct       = numeric(0),
        tos          = integer(0)
      )
    }

    play_rollup <- reactive({
      df <- q_df()
      if (nrow(df) == 0) return(empty_rollup())
      df |>
        dplyr::group_by(play) |>
        dplyr::summarise(
          poss   = dplyr::n(),
          pts    = sum(pts, na.rm = TRUE),
          ppp    = sum(pts, na.rm = TRUE) / dplyr::n(),
          scoring_poss = sum(pts > 0, na.rm = TRUE),
          fga    = sum(is_fga, na.rm = TRUE),
          fgm    = sum(is_fgm, na.rm = TRUE),
          fg_pct = ifelse(sum(is_fga, na.rm = TRUE) > 0,
                          sum(is_fgm, na.rm = TRUE) / sum(is_fga, na.rm = TRUE),
                          NA_real_),
          tos    = sum(is_to, na.rm = TRUE),
          .groups = "drop"
        )
    })

    # Game-session rollup split by (play, category) — used by the
    # comparison table when a Game session is active AND the category
    # filter is "All". When a specific category is selected, q_df() has
    # already filtered the data, so play_rollup() is enough.
    play_cat_rollup <- reactive({
      df <- q_df()
      if (nrow(df) == 0) return(NULL)
      df |>
        dplyr::mutate(category = ifelse(is.na(category) | !nzchar(category),
                                         "-", category)) |>
        dplyr::group_by(play, category) |>
        dplyr::summarise(
          poss   = dplyr::n(),
          pts    = sum(pts, na.rm = TRUE),
          ppp    = sum(pts, na.rm = TRUE) / dplyr::n(),
          scoring_poss = sum(pts > 0, na.rm = TRUE),
          fga    = sum(is_fga, na.rm = TRUE),
          fgm    = sum(is_fgm, na.rm = TRUE),
          fg_pct = ifelse(sum(is_fga, na.rm = TRUE) > 0,
                          sum(is_fgm, na.rm = TRUE) / sum(is_fga, na.rm = TRUE),
                          NA_real_),
          tos    = sum(is_to, na.rm = TRUE),
          .groups = "drop"
        )
    })

    output$best_play <- renderUI({
      pr <- play_rollup()
      if (nrow(pr) == 0) return(hero_body("Not enough data", NA_character_))
      pr <- dplyr::filter(pr, poss >= 2)
      if (nrow(pr) == 0) return(hero_body("Not enough data", NA_character_))
      best <- pr[which.max(pr$ppp), ]
      hero_body(best$play, fmt_dec(best$ppp, 2))
    })

    output$worst_play <- renderUI({
      pr <- play_rollup()
      if (nrow(pr) == 0) return(hero_body("Not enough data", NA_character_))
      pr <- dplyr::filter(pr, poss >= 2)
      if (nrow(pr) == 0) return(hero_body("Not enough data", NA_character_))
      worst <- pr[which.min(pr$ppp), ]
      hero_body(worst$play, fmt_dec(worst$ppp, 2))
    })

    output$most_used <- renderUI({
      pr <- play_rollup()
      if (nrow(pr) == 0) return(hero_body("No plays yet", NA_character_))
      m <- pr[which.max(pr$poss), ]
      hero_body(m$play, sprintf("%d poss", m$poss))
    })

    # ---- Comparison table --------------------------------------------
    output$comparison_tbl <- reactable::renderReactable({
      is_game <- identical(app_state$session_type, "Game")
      cat_sel <- input$cat_filter
      split_by_cat <- is_game &&
        (is.null(cat_sel) || !nzchar(cat_sel) || identical(cat_sel, "All"))

      pr <- if (split_by_cat) play_cat_rollup() else play_rollup()

      if (is.null(pr) || nrow(pr) == 0) {
        return(reactable::reactable(
          data.frame(` ` = "No plays called this quarter yet.",
                     check.names = FALSE),
          sortable = FALSE, borderless = TRUE,
          theme = tracker_table_theme()
        ))
      }
      pr <- pr |> dplyr::arrange(dplyr::desc(poss))

      ppp_cell <- function(v) {
        if (is.na(v)) return("-")
        bg <- if (v >= 1.0) NMSU$make_2 else NMSU$miss_2
        tags$span(class = "ppp-pill",
                  style = sprintf("background:%s;", bg),
                  fmt_dec(v, 2))
      }

      show <- if (split_by_cat) {
        pr |> dplyr::transmute(play, category, poss, pts, ppp,
                                scoring_poss, fg_pct, tos)
      } else {
        pr |> dplyr::transmute(play, poss, pts, ppp,
                                scoring_poss, fg_pct, tos)
      }

      cols <- list(
        play         = reactable::colDef(name = "PLAY",         minWidth = 150),
        poss         = reactable::colDef(name = "POSS",         width = 70, align = "right"),
        pts          = reactable::colDef(name = "PTS",          width = 70, align = "right"),
        ppp          = reactable::colDef(name = "PPP",          width = 90, align = "right", cell = ppp_cell),
        scoring_poss = reactable::colDef(name = "SCORING POSS", width = 120, align = "right"),
        fg_pct       = reactable::colDef(name = "FG%",          width = 90, align = "right",
                         cell = function(v) fmt_pct(v, 1)),
        tos          = reactable::colDef(name = "TO",           width = 60, align = "right")
      )
      if ("category" %in% names(show)) {
        cols$category <- reactable::colDef(name = "CAT", width = 110)
      }

      reactable::reactable(
        show,
        compact = TRUE, highlight = TRUE, pagination = FALSE, sortable = TRUE,
        theme = tracker_table_theme(),
        columns = cols
      )
    })
  })
}

# ---- small helpers used only on the Coach tab -------------------------
coach_stat_tile <- function(ns, id, label, highlight = FALSE) {
  div(class = sprintf("coach-stat-tile%s",
                      if (highlight) " coach-stat-tile--hero" else ""),
      div(class = "coach-stat-label", label),
      div(class = "coach-stat-value", textOutput(ns(id), inline = TRUE))
  )
}

hero_card <- function(ns, id, title, color) {
  div(class = "hero-card",
      style = sprintf("--accent:%s;", color),
      div(class = "hero-eyebrow", title),
      uiOutput(ns(id))
  )
}
hero_body <- function(main_text, sub_text = NA) {
  tags$div(
    tags$div(class = "hero-main", main_text),
    if (!is.na(sub_text) && nzchar(sub_text))
      tags$div(class = "hero-sub", sub_text)
  )
}

legend_dot <- function(color, label) {
  tags$span(class = "legend-dot-wrap",
    tags$span(class = "legend-dot", style = sprintf("background:%s;", color)),
    tags$span(class = "legend-label", label)
  )
}

#' Picks a chip color for the last-5 strip based on possession result.
possession_chip_color <- function(r) {
  if (isTRUE(r$is_to))                        return(NMSU$turnover)
  if (identical(r$result, "+1F"))             return(NMSU$ft_make_1)
  if (identical(r$result, "+2F"))             return(NMSU$ft_make_2)
  if (identical(r$result, "-1F") ||
      identical(r$result, "-2F"))             return(NMSU$ft_miss_1)
  if (isTRUE(r$is_fgm))                       return(NMSU$make_2)
  if (isTRUE(r$is_fga))                       return(NMSU$miss_2)
  if (identical(r$result, "FOUL"))            return(NMSU$foul)
  NMSU$empty_0
}
