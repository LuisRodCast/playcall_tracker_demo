# =========================================================================
# R/mod_summary.R  --  Tab 2  --  Session Summary
#
# Rolls up every play that has been called this session into one table:
# PLAY GROUP / POSS / PPP / FG% / AVG REV / AVG PAINT, filterable by quarter.
#
# Plays with 0 possessions are still shown (so the coach can see which
# plays in the master list haven't been called yet).
# =========================================================================

mod_summary_ui <- function(id) {
  ns <- NS(id)
  div(class = "summary-page",

    tab_header("Summary"),

    div(class = "quarter-row",
      tags$label("QUARTER", class = "control-label"),
      quarter_chips(ns, "quarter", selected = "Q1")
    ),

    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title", "SESSION SUMMARY"),
        tags$div(class = "section-meta",
                 textOutput(ns("total_poss"), inline = TRUE))
      ),
      reactable::reactableOutput(ns("summary_tbl"))
    )
  )
}

mod_summary_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {

    # The quarter filter lives on each tab independently so the coach can
    # view Q1 Summary while still tracking in Q2.
    q_df <- reactive({
      filter_quarter(app_state$possessions, input$quarter)
    })

    output$total_poss <- renderText({
      sprintf("TOTAL POSS: %d", nrow(q_df()))
    })

    output$summary_tbl <- reactable::renderReactable({
      poss      <- q_df()
      all_plays <- load_play_choices_flat()
      is_game   <- identical(app_state$session_type, "Game")

      if (is_game) {
        # ---- Game sessions: one row per (play, category) tuple ----------
        # Called combinations get real stats; every play that hasn't been
        # called this quarter gets a single "-" row so the whole menu is
        # still visible to the coach.
        called_combos <- poss |>
          dplyr::filter(!is.na(category), nzchar(category)) |>
          dplyr::distinct(play, category) |>
          dplyr::arrange(play, category)

        combo_rows <- if (nrow(called_combos) > 0) {
          lapply(seq_len(nrow(called_combos)), function(i) {
            p  <- called_combos$play[i]
            ct <- called_combos$category[i]
            sub <- dplyr::filter(poss, play == p, category == ct)
            s   <- summarise_possessions(sub)
            data.frame(
              play      = p,
              category  = ct,
              poss      = s$poss,
              ppp       = s$ppp,
              fg_pct    = s$fg_pct,
              avg_rev   = s$avg_rev,
              avg_paint = s$avg_paint,
              stringsAsFactors = FALSE
            )
          })
        } else list()

        called_plays <- unique(called_combos$play)
        uncalled     <- setdiff(c(all_plays, unique(poss$play)), called_plays)
        uncalled_rows <- lapply(uncalled, function(p) {
          data.frame(
            play      = p,
            category  = "-",
            poss      = 0L,
            ppp       = NA_real_,
            fg_pct    = NA_real_,
            avg_rev   = NA_real_,
            avg_paint = NA_real_,
            stringsAsFactors = FALSE
          )
        })

        df <- do.call(rbind, c(combo_rows, uncalled_rows))
        df <- df |>
          dplyr::arrange(dplyr::desc(poss > 0),
                         dplyr::desc(poss),
                         play, category)
      } else {
        # ---- Practice sessions: original one-row-per-play layout --------
        called     <- unique(poss$play)
        every_play <- unique(c(all_plays, called))
        rows <- lapply(every_play, function(p) {
          sub <- dplyr::filter(poss, play == p)
          s   <- summarise_possessions(sub)
          data.frame(
            play      = p,
            poss      = s$poss,
            ppp       = s$ppp,
            fg_pct    = s$fg_pct,
            avg_rev   = s$avg_rev,
            avg_paint = s$avg_paint,
            stringsAsFactors = FALSE
          )
        })
        df <- do.call(rbind, rows)
        df <- df |>
          dplyr::arrange(dplyr::desc(poss > 0),
                         dplyr::desc(poss),
                         play)
      }

      # Column spec — CAT only present in Game mode
      cols <- list(
        play      = reactable::colDef(name = "PLAY GROUP", minWidth = 160),
        poss      = reactable::colDef(name = "POSS",      width = 80,
                      align = "right"),
        ppp       = reactable::colDef(name = "PPP",       width = 90,
                      align = "right",
                      cell  = function(v) fmt_dec(v, 2)),
        fg_pct    = reactable::colDef(name = "FG%",       width = 90,
                      align = "right",
                      cell  = function(v) fmt_pct(v, 1)),
        avg_rev   = reactable::colDef(name = "AVG REV",   width = 95,
                      align = "right",
                      cell  = function(v) fmt_dec(v, 1)),
        avg_paint = reactable::colDef(name = "AVG PAINT", width = 100,
                      align = "right",
                      cell  = function(v) fmt_dec(v, 1))
      )
      if ("category" %in% names(df)) {
        cols$category <- reactable::colDef(name = "CAT", width = 110)
      }

      reactable::reactable(
        df,
        compact = TRUE, highlight = TRUE, pagination = FALSE,
        sortable = TRUE, defaultSorted = NULL,
        theme    = tracker_table_theme(),
        columns  = cols
      )
    })
  })
}


# Shared table theme used by Summary / Coach / Sheet / History ---------
tracker_table_theme <- function() {
  reactable::reactableTheme(
    backgroundColor = NMSU$card,
    color           = NMSU$text_primary,
    borderColor     = NMSU$card_border,
    highlightColor  = "#232329",
    headerStyle     = list(color = NMSU$text_muted,
                           fontWeight = 600,
                           letterSpacing = "0.05em",
                           fontSize = "0.75rem"),
    style           = list(fontSize = "0.95rem"),
    rowStyle        = list(borderColor = NMSU$card_border)
  )
}
