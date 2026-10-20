# =========================================================================
# R/mod_tracker.R
# Tab 1 -- Live tracker.
#
# Popups are declared in app.R (top-level). This module only emits
# "open popup X" signals by incrementing tick counters in app_state.
# =========================================================================

mod_tracker_ui <- function(id) {
  ns <- NS(id)

  div(class = "tracker-page",

    tab_header("Tracker"),

    # ---------- Session header + action buttons -------------------------
    # Session name | session type (Practice/Game) | date
    div(class = "session-header",
      div(class = "session-left",
        textInput(ns("session_name"), label = NULL,
                  placeholder = "Session name"),
        div(class = "session-type-wrap",
          selectInput(ns("session_type"), label = NULL,
                      choices  = SESSION_TYPES,
                      selected = "Practice",
                      selectize = FALSE)
        ),
        dateInput(ns("session_date"), label = NULL,
                  format = "yyyy-mm-dd")
      ),
      div(class = "session-right",
        actionButton(ns("open_guide"),    "Guide",
                     class = "nav-action-btn"),
        actionButton(ns("open_halftime"), "Halftime",
                     class = "nav-action-btn nav-action-crimson"),
        actionButton(ns("new_session"),   "+ New Session",
                     class = "new-session-btn")
      )
    ),

    # ---------- Row 1: play dropdown (+ category when Game) + quarter chips
    div(class = "control-row",
      div(class = "control-col play-col",
        div(class = "play-category-row",
          div(class = "play-sub",
            tags$label("PLAY", class = "control-label"),
            shinyMobile::f7Select(
              inputId  = ns("play"),
              label    = NULL,
              choices  = load_play_choices(),
              selected = load_play_choices_flat()[1]
            )
          ),
          # Category dropdown — only visible when session_type == "Game".
          # Uses f7Select so it visually matches the PLAY dropdown.
          conditionalPanel(
            condition = sprintf("input['%s'] == 'Game'", ns("session_type")),
            div(class = "category-sub",
              tags$label("CATEGORY", class = "control-label"),
              shinyMobile::f7Select(
                inputId  = ns("category"),
                label    = NULL,
                choices  = CATEGORIES,
                selected = CATEGORIES[1]
              )
            )
          )
        )
      ),
      div(class = "control-col quarter-col",
        tags$label("QUARTER", class = "control-label"),
        # Dynamic chip row -- includes "Practice" chip only during Practice sessions
        uiOutput(ns("quarter_chip_row"))
      )
    ),

    # ---------- Row 2: five running-stat tiles --------------------------
    div(class = "stats-grid",
      stat_tile(ns, "poss",      "POSS"),
      stat_tile(ns, "ppp",       "PPP"),
      stat_tile(ns, "fg_pct",    "FG%"),
      stat_tile(ns, "avg_rev",   "AVG REV"),
      stat_tile(ns, "avg_paint", "AVG PAINT")
    ),

    # ---------- Row 3: clock input --------------------------------------
    div(class = "clock-row",
      tags$label("CLOCK", class = "control-label clock-label"),
      shinyMobile::f7Text(inputId = ns("clock"), label = NULL,
                          placeholder = "e.g. 7:34  (optional)")
    ),

    # ---------- Row 4: counters -----------------------------------------
    counter_row(ns, "reversal", "BALL REVERSALS",
                "Resets after each possession", NMSU$ft_make_1),
    counter_row(ns, "paint", "PAINT TOUCHES",
                "Resets after each possession", NMSU$turnover),

    # ---------- Row 5: 5x3 result grid ----------------------------------
    div(class = "result-grid",
      lapply(RESULT_BUTTONS, function(spec) {
        action_tile(ns, spec$id, spec$label, spec$color)
      })
    ),

    # ---------- Row 6: play history table -------------------------------
    div(class = "history-section",
      div(class = "history-header",
        tags$h3(class = "history-title",
                textOutput(ns("history_title"), inline = TRUE)),
        actionButton(ns("undo"),
                     HTML('<span class="undo-icon">&#8630;</span> UNDO LAST'),
                     class = "undo-btn")
      ),
      reactable::reactableOutput(ns("history_tbl"), height = "auto")
    )
  )
}


mod_tracker_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {

    ns <- session$ns

    toast <- function(msg, type = "default") {
      shiny::showNotification(msg, duration = 2,
                              type = switch(type,
                                            warning = "warning",
                                            error   = "error",
                                            message = "message",
                                            "default"))
    }

    observeEvent(TRUE, {
      updateTextInput(session, "session_name",
                      value = isolate(app_state$session_name))
      updateDateInput(session, "session_date",
                      value = isolate(app_state$session_date))
    }, once = TRUE, ignoreInit = FALSE)

    observeEvent(input$session_name, {
      if (!is.null(input$session_name) && nzchar(input$session_name))
        app_state$session_name <- input$session_name
    })
    observeEvent(input$session_date, {
      if (!is.null(input$session_date))
        app_state$session_date <- input$session_date
    })

    # ======================================================================
    # Session-header buttons:  just raise a tick in app_state.
    # app.R watches these ticks and opens the appropriate popup.
    # ======================================================================
    observeEvent(input$open_guide, {
      message("[DEBUG] tracker-open_guide fired at ", Sys.time())
      app_state$open_guide_tick <- isolate(app_state$open_guide_tick) + 1L
    }, ignoreInit = TRUE)

    observeEvent(input$open_halftime, {
      message("[DEBUG] tracker-open_halftime fired at ", Sys.time())
      app_state$open_halftime_tick <- isolate(app_state$open_halftime_tick) + 1L
    }, ignoreInit = TRUE)

    observeEvent(input$new_session, {
      message("[DEBUG] tracker-new_session fired at ", Sys.time())
      app_state$open_new_session_tick <-
        isolate(app_state$open_new_session_tick) + 1L
    }, ignoreInit = TRUE)

    # When app.R has reset the session, mirror into module-level inputs
    observeEvent(app_state$reset_inputs_tick, {
      updateTextInput(session, "session_name",
                      value = app_state$session_name)
      updateDateInput(session, "session_date",
                      value = app_state$session_date)
      updateTextInput(session, "clock", value = "")
      # Reset session_type to Practice (its default) and clear category
      updateSelectInput(session, "session_type",
                        selected = app_state$session_type)
      shinyMobile::updateF7Select(
        inputId  = "category",
        selected = CATEGORIES[1],
        session  = session
      )
    }, ignoreInit = TRUE)

    # ---- play / quarter / clock / session type / category -----------
    observeEvent(input$play,    { app_state$current_play    <- input$play })
    observeEvent(input$quarter, { app_state$current_quarter <- input$quarter })
    observeEvent(input$clock,   { app_state$clock           <- input$clock },
                 ignoreInit = TRUE, ignoreNULL = FALSE)

    # Initialize session_type dropdown from app_state once at startup
    observeEvent(TRUE, {
      updateSelectInput(session, "session_type",
                        selected = isolate(app_state$session_type))
    }, once = TRUE, ignoreInit = FALSE)

    # When session_type changes:  update app_state AND make sure the
    # current_quarter is still valid for the new chip set.
    observeEvent(input$session_type, {
      new_type <- input$session_type
      if (is.null(new_type) || !nzchar(new_type)) return()
      app_state$session_type <- new_type
      cq <- isolate(app_state$current_quarter)
      if (new_type == "Practice") {
        # Practice has the extra "Practice" chip; default selection to it
        if (is.null(cq) || !cq %in% c(QUARTERS, "Practice")) {
          app_state$current_quarter <- "Practice"
        }
      } else {
        # Game drops the Practice chip -- fall back to Q1 if currently "Practice"
        if (identical(cq, "Practice") || is.null(cq) || !cq %in% QUARTERS) {
          app_state$current_quarter <- "Q1"
        }
        # Also seed a sensible category default
        if (is.null(isolate(app_state$current_category)) ||
            is.na(isolate(app_state$current_category))) {
          app_state$current_category <- CATEGORIES[1]
        }
      }
    }, ignoreInit = TRUE)

    observeEvent(input$category, {
      app_state$current_category <- input$category
    }, ignoreInit = TRUE, ignoreNULL = FALSE)

    # ---- Dynamic quarter chip row (switches when session_type flips) ----
    output$quarter_chip_row <- renderUI({
      st <- app_state$session_type
      quarters <- if (identical(st, "Practice")) c(QUARTERS, "Practice") else QUARTERS
      cq <- isolate(app_state$current_quarter)
      if (is.null(cq) || !cq %in% quarters) cq <- quarters[1]
      quarter_chips(ns, "quarter", quarters = quarters, selected = cq)
    })

    # ---- counters ---------------------------------------------------
    observeEvent(input$reversal_plus,  {
      app_state$ball_reversals <- isolate(app_state$ball_reversals) + 1L
    })
    observeEvent(input$reversal_minus, {
      app_state$ball_reversals <- max(0L, isolate(app_state$ball_reversals) - 1L)
    })
    observeEvent(input$paint_plus,     {
      app_state$paint_touches  <- isolate(app_state$paint_touches)  + 1L
    })
    observeEvent(input$paint_minus,    {
      app_state$paint_touches  <- max(0L, isolate(app_state$paint_touches)  - 1L)
    })

    output$reversal_value <- renderUI(tags$span(as.character(app_state$ball_reversals)))
    output$paint_value    <- renderUI(tags$span(as.character(app_state$paint_touches)))

    # ---- result buttons ---------------------------------------------
    for (spec in RESULT_BUTTONS) {
      local({
        s <- spec
        observeEvent(input[[s$id]], {
          play <- app_state$current_play
          if (is.null(play) || is.na(play) || !nzchar(play)) {
            toast("Pick a play first.", type = "warning")
            return()
          }
          # Only tag a category when the session is a Game. During
          # Practice we leave it NA so Practice sessions aren't filtered
          # by category downstream.
          this_cat <- if (identical(isolate(app_state$session_type), "Game"))
                        isolate(app_state$current_category)
                      else NA_character_
          row <- make_possession(
            spec           = s,
            play           = play,
            quarter        = isolate(app_state$current_quarter),
            reversals      = isolate(app_state$ball_reversals),
            paint_touches  = isolate(app_state$paint_touches),
            clock          = isolate(app_state$clock),
            category       = this_cat
          )
          row$possession_id <- nrow(isolate(app_state$possessions)) + 1L
          app_state$possessions <- rbind(isolate(app_state$possessions), row)

          app_state$ball_reversals <- 0L
          app_state$paint_touches  <- 0L
        })
      })
    }

    # ---- undo last --------------------------------------------------
    observeEvent(input$undo, {
      n <- nrow(isolate(app_state$possessions))
      if (n == 0) return()
      app_state$possessions <- isolate(app_state$possessions)[-n, , drop = FALSE]
      toast("Last possession removed.", type = "message")
    })

    # ---- filters / tiles / history table ---------------------------
    play_q_df <- reactive({
      app_state$possessions |>
        filter_play(app_state$current_play) |>
        filter_quarter(app_state$current_quarter)
    })

    stats <- reactive(summarise_possessions(play_q_df()))

    output$poss      <- renderText(fmt_int(stats()$poss))
    output$ppp       <- renderText(fmt_dec(stats()$ppp, 2))
    output$fg_pct    <- renderText(fmt_pct(stats()$fg_pct, 1))
    output$avg_rev   <- renderText(fmt_dec(stats()$avg_rev,  1))
    output$avg_paint <- renderText(fmt_dec(stats()$avg_paint, 1))

    output$history_title <- renderText({
      p <- app_state$current_play
      n <- nrow(play_q_df())
      if (is.null(p) || !nzchar(p))
        "PLAY HISTORY"
      else
        sprintf("%s — PLAY HISTORY  (%d %s)",
                toupper(p), n, if (n == 1) "play" else "plays")
    })

    output$history_tbl <- reactable::renderReactable({
      df <- play_q_df()
      if (nrow(df) == 0) {
        return(reactable::reactable(
          data.frame(` ` = "No plays logged for this selection yet.",
                     check.names = FALSE),
          sortable = FALSE, borderless = TRUE,
          theme = tracker_table_theme()
        ))
      }

      is_game <- identical(app_state$session_type, "Game")

      df_show <- df |>
        dplyr::arrange(dplyr::desc(possession_id)) |>
        dplyr::transmute(
          `#`     = possession_id,
          QTR     = quarter,
          CAT     = ifelse(is.na(category) | !nzchar(category), "-", category),
          RESULT  = result,
          REV     = ball_reversals,
          PT      = paint_touches,
          PTS     = pts,
          CLOCK   = ifelse(is.na(clock), "-", clock)
        )

      # CAT column is only meaningful during Game sessions; drop it during
      # Practice so the column strip doesn't fill with "-".
      if (!is_game) {
        df_show <- df_show |> dplyr::select(-CAT)
      }

      cols <- list(
        `#`     = reactable::colDef(width = 50),
        QTR     = reactable::colDef(width = 60),
        RESULT  = reactable::colDef(cell = function(v) {
          tags$span(class = "result-chip",
                    style = sprintf("background:%s;",
                                    result_color(v)), v)
        }),
        REV     = reactable::colDef(width = 60),
        PT      = reactable::colDef(width = 60),
        PTS     = reactable::colDef(width = 60),
        CLOCK   = reactable::colDef(width = 90)
      )
      if ("CAT" %in% names(df_show)) {
        cols$CAT <- reactable::colDef(width = 95)
      }

      reactable::reactable(
        df_show,
        compact = TRUE, highlight = TRUE, sortable = FALSE, pagination = FALSE,
        theme = tracker_table_theme(),
        columns = cols
      )
    })
  })
}

result_color <- function(label) {
  for (s in RESULT_BUTTONS) if (identical(s$label, label)) return(s$color)
  NMSU$card_border
}
