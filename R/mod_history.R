# =========================================================================
# R/mod_history.R  --  Tab 5  --  Saved-session browser
#
# View and Delete dialogs use our hand-built Framework7 popups
# (R/popups.R), because shiny::showModal() does not render inside f7Page.
# One View popup + one Delete popup, body re-rendered each time based on
# which session's row button was clicked.
# =========================================================================

mod_history_ui <- function(id) {
  ns <- NS(id)
  div(class = "history-page",
    tab_header("History"),
    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title", "SAVED SESSIONS"),
        tags$div(class = "section-meta",
                 textOutput(ns("count_blurb"), inline = TRUE))
      ),
      uiOutput(ns("sessions_list"))
    ),

    # ---- Framework7 popups (see R/popups.R) ----------------------------
    f7_popup_manual(
      id    = ns("view_popup"),
      title = "Session details",
      uiOutput(ns("view_body"))
    ),
    f7_popup_manual(
      id    = ns("delete_popup"),
      title = "Delete this session?",
      uiOutput(ns("delete_body"))
    )
  )
}


mod_history_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    sessions_df <- reactive({
      app_state$history_bump
      tryCatch(list_sessions(app_state),
               error = function(e) {
                 message("[history] DB load failed: ", conditionMessage(e))
                 data.frame()
               })
    })

    output$count_blurb <- renderText({
      n <- nrow(sessions_df())
      if (n == 0) "No saved sessions yet"
      else sprintf("%d saved %s", n, if (n == 1) "session" else "sessions")
    })

    # ---- Render list of saved-session cards ----------------------------
    output$sessions_list <- renderUI({
      df <- sessions_df()
      if (nrow(df) == 0) {
        return(tags$div(class = "muted-note",
          paste0("Log at least one possession on the Tracker tab -- your ",
                 "current session will auto-save and appear here.")))
      }
      cards <- lapply(seq_len(nrow(df)), function(i) {
        row <- df[i, ]
        div(class = "history-card",
          div(class = "history-card-top",
            div(class = "history-meta",
              tags$div(class = "history-date", row$session_date),
              tags$div(class = "history-name", row$session_name)
            ),
            div(class = "history-actions",
              actionButton(ns(paste0("view_", i)),
                           "View",
                           class = "history-btn history-btn-view"),
              actionButton(ns(paste0("delete_", i)),
                           "Delete",
                           class = "history-btn history-btn-del")
            )
          ),
          div(class = "history-stats",
            history_stat("POSS",        fmt_int(row$n_poss)),
            history_stat("PTS",         fmt_int(row$total_points)),
            history_stat("PPP",         fmt_dec(row$ppp, 2)),
            history_stat("FG%",         fmt_pct(row$fg_pct, 1)),
            history_stat("MOST USED",   row$most_used %||na% "-")
          )
        )
      })
      do.call(tagList, cards)
    })

    # ======================================================================
    # View popup
    # ======================================================================
    view_target_id <- reactiveVal(NULL)

    output$view_body <- renderUI({
      sid <- view_target_id()
      if (is.null(sid)) {
        return(tags$p("No session selected."))
      }
      build_session_detail_content(sid, app_state)
    })

    # ======================================================================
    # Delete popup
    # ======================================================================
    delete_target <- reactiveVal(NULL)  # list(id, name, date, n_poss)

    output$delete_body <- renderUI({
      t <- delete_target()
      if (is.null(t)) {
        return(tags$p("Nothing to delete."))
      }
      tagList(
        tags$p(tags$b(t$name), " on ", t$date,
               " (", t$n_poss, " possessions)"),
        tags$p("This cannot be undone."),
        tags$div(class = "popup-btn-row",
          actionButton(ns("cancel_delete"), "Cancel",
                       class = "popup-cancel-btn"),
          actionButton(ns("confirm_delete"), "Delete",
                       class = "history-btn-del-confirm")
        )
      )
    })

    # ======================================================================
    # Wire each row's View / Delete button to the shared popups
    # ======================================================================
    observe({
      df <- sessions_df()
      lapply(seq_len(nrow(df)), function(i) {
        local({
          idx   <- i
          sid   <- df$session_id[idx]
          name  <- df$session_name[idx]
          sdate <- df$session_date[idx]
          nposs <- df$n_poss[idx]

          observeEvent(input[[paste0("view_", idx)]], {
            message("[DEBUG] history-view_", idx, " fired (sid=", sid, ")")
            view_target_id(sid)
            open_popup(ns("view_popup"), session = session)
          }, ignoreInit = TRUE)

          observeEvent(input[[paste0("delete_", idx)]], {
            message("[DEBUG] history-delete_", idx, " fired (sid=", sid, ")")
            delete_target(list(
              id     = sid,
              name   = name,
              date   = sdate,
              n_poss = nposs
            ))
            open_popup(ns("delete_popup"), session = session)
          }, ignoreInit = TRUE)
        })
      })
    })

    # Cancel delete
    observeEvent(input$cancel_delete, {
      message("[DEBUG] history cancel_delete fired")
      close_popup(ns("delete_popup"), session = session)
    }, ignoreInit = TRUE)

    # Confirm delete
    observeEvent(input$confirm_delete, {
      t <- delete_target()
      if (is.null(t)) return()
      message("[DEBUG] history confirm_delete fired -- deleting ", t$id)
      try(delete_session(t$id, app_state), silent = TRUE)
      close_popup(ns("delete_popup"), session = session)
      app_state$history_bump <- isolate(app_state$history_bump) + 1L
      shiny::showNotification("Session deleted.", type = "message",
                              duration = 2)
    }, ignoreInit = TRUE)
  })
}


history_stat <- function(label, value) {
  div(class = "history-stat",
    div(class = "history-stat-label", label),
    div(class = "history-stat-value", value)
  )
}

`%||na%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || is.na(a) || !nzchar(as.character(a))) b
  else a
}


# =========================================================================
# Build the "session details" popup body (previously a showModal body)
# =========================================================================
build_session_detail_content <- function(session_id, app_state) {
  dat <- tryCatch(load_session(session_id, app_state), error = function(e) NULL)
  if (is.null(dat)) {
    return(tagList(
      tags$p("Could not open session."),
      tags$p("The session file may be corrupt or missing.")
    ))
  }

  poss <- dat$possessions
  s    <- summarise_possessions(poss)
  scoring <- sum(poss$pts > 0, na.rm = TRUE)

  per_play <- if (nrow(poss) > 0) {
    poss |>
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
  } else data.frame()

  best  <- if (nrow(per_play) > 0 && any(per_play$poss >= 2)) {
    sub <- dplyr::filter(per_play, poss >= 2); sub[which.max(sub$ppp), ]
  } else NULL
  worst <- if (nrow(per_play) > 0 && any(per_play$poss >= 2)) {
    sub <- dplyr::filter(per_play, poss >= 2); sub[which.min(sub$ppp), ]
  } else NULL

  tagList(
    tags$div(class = "detail-title", dat$session_name),
    tags$div(class = "detail-sub",   dat$session_date, " · ",
             dat$team, " · ", s$poss, " possessions"),

    div(class = "detail-stats",
      history_stat("POSS",         fmt_int(s$poss)),
      history_stat("PTS",          fmt_int(s$pts)),
      history_stat("PPP",          fmt_dec(s$ppp, 2)),
      history_stat("FG%",          fmt_pct(s$fg_pct, 1)),
      history_stat("SCORING POSS", fmt_int(scoring)),
      history_stat("TURNOVERS",    fmt_int(s$tos))
    ),

    if (!is.null(best))
      tags$p(tags$b("Best play (≥2 POSS): "),
             best$play, "  —  PPP ", fmt_dec(best$ppp, 2)),
    if (!is.null(worst))
      tags$p(tags$b("Worst play (≥2 POSS): "),
             worst$play, "  —  PPP ", fmt_dec(worst$ppp, 2)),

    tags$h4("Play comparison"),
    if (nrow(per_play) == 0) tags$p("No possessions logged.")
    else reactable::reactable(
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
