# =========================================================================
# apps/playcall_tracker/app.R
#
# Popups live at the TOP-LEVEL UI (outside the tracker module) so their
# cancel/confirm button IDs are root-level and their observers live here.
# The tracker module only tells us "open popup X" via tick counters in
# app_state.
# =========================================================================

for (f in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(f)

format_concise_date <- function(d) {
  m  <- as.integer(format(d, "%m"))
  dd <- as.integer(format(d, "%d"))
  yy <- format(d, "%y")
  sprintf("%d/%d/%s", m, dd, yy)
}

ui <- shinyMobile::f7Page(
  title = "Play Call Tracker -- Demo",
  options = list(
    theme    = "md",
    dark     = TRUE,
    filled   = TRUE,
    color    = "#8B0029"
  ),
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$link(rel = "icon", type = "image/png", href = "nmsu_zia.png"),
    tags$meta(name = "viewport",
              content = "width=device-width, initial-scale=1, maximum-scale=1"),
    popup_message_handler_tag()
  ),
  shinyMobile::f7TabLayout(
    navbar = shinyMobile::f7Navbar(
      title    = tags$div(
        class = "aggie-navbar",
        tags$img(src = "nmsu_zia.png", class = "aggie-logo"),
        tags$span("Play Call Tracker", class = "aggie-title"),
        tags$span("DEMO", class = "demo-badge")
      ),
      hairline = FALSE
    ),
    shinyMobile::f7Tabs(
      animated = TRUE, swipeable = FALSE, id = "main_tabs",
      shinyMobile::f7Tab(
        tabName = "Tracker", icon = shinyMobile::f7Icon("square_grid_2x2"),
        active  = TRUE, mod_tracker_ui("tracker")
      ),
      shinyMobile::f7Tab(
        tabName = "Summary", icon = shinyMobile::f7Icon("square_list"),
        mod_summary_ui("summary")
      ),
      shinyMobile::f7Tab(
        tabName = "Coach", icon = shinyMobile::f7Icon("square_favorites_alt"),
        mod_coach_ui("coach")
      ),
      shinyMobile::f7Tab(
        tabName = "Sheet", icon = shinyMobile::f7Icon("doc_text"),
        mod_sheet_ui("sheet")
      ),
      shinyMobile::f7Tab(
        tabName = "History", icon = shinyMobile::f7Icon("archivebox"),
        mod_history_ui("history")
      ),
      shinyMobile::f7Tab(
        tabName = "Trends", icon = shinyMobile::f7Icon("chart_bar_square"),
        mod_trends_ui("trends")
      )
    )
  ),

  # ---- Top-level popups (hand-built, see R/popups.R) --------------------
  f7_popup_manual(
    id    = "guide_popup",
    title = "Quick Start Guide",
    guide_body_ui()
  ),
  f7_popup_manual(
    id    = "halftime_popup",
    title = "Halftime Report",
    uiOutput("halftime_body")
  ),
  f7_popup_manual(
    id    = "new_session_popup",
    title = "Start a new session?",
    uiOutput("new_session_body")
  )
)


server <- function(input, output, session) {

  now_iso <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

  app_state <- reactiveValues(
    session_id             = make_session_id(),
    session_created_at     = now_iso,
    session_name           = paste("Practice", format_concise_date(Sys.Date())),
    session_date           = Sys.Date(),
    team                   = "Demo Team",
    session_type           = "Practice",      # "Practice" or "Game"
    current_play           = NA_character_,
    current_quarter        = "Practice",      # defaults to Practice chip (matches session_type)
    current_category       = NA_character_,   # Transition / Secondary / 2nd Shots / HC Sets, Game only
    ball_reversals         = 0L,
    paint_touches          = 0L,
    clock                  = NA_character_,
    possessions            = empty_possessions_df(),
    history_bump           = 0L,

    # DEMO: per-visitor in-memory sessions store (see R/db_demo.R).
    # Pre-populated with two fake sessions so the app lands with
    # content in History/Summary/Coach/Trends on first paint.
    sessions_store         = seed_demo_sessions(),

    # Popup open signals (incremented by the tracker module)
    open_guide_tick        = 0L,
    open_halftime_tick     = 0L,
    open_new_session_tick  = 0L,

    # Signal back to tracker module to refresh its visible inputs
    reset_inputs_tick      = 0L
  )

  # ----- schema init (no-op for demo, kept for signature parity) ----
  observeEvent(TRUE, {
    db_safe_ensure_schema()
  }, once = TRUE, ignoreInit = FALSE)

  # ----- auto-save on possession change (debounced to 3s) -------------
  # Each possession click triggers one write; without debounce every
  # click would be a Turso HTTP round-trip. Debouncing by 3 seconds
  # batches rapid logging into one DB round-trip after the user pauses.
  save_trigger <- debounce(reactive(app_state$possessions), 3000)

  observeEvent(save_trigger(), {
    try(save_session(app_state), silent = TRUE)
    app_state$history_bump <- isolate(app_state$history_bump) + 1L
  }, ignoreInit = TRUE)

  # ======================================================================
  # Guide popup
  # ======================================================================
  observeEvent(app_state$open_guide_tick, {
    message("[DEBUG] opening guide popup")
    open_popup("guide_popup", session = session)
  }, ignoreInit = TRUE)

  # ======================================================================
  # Halftime popup (rebuilt each time user clicks the Halftime button)
  # ======================================================================
  halftime_body_rv <- reactiveVal(
    tags$p("Click 'Halftime' to generate a Q1+Q2 report.")
  )
  output$halftime_body <- renderUI({ halftime_body_rv() })

  observeEvent(app_state$open_halftime_tick, {
    message("[DEBUG] opening halftime popup")
    halftime_body_rv(halftime_body_ui(app_state))
    open_popup("halftime_popup", session = session)
  }, ignoreInit = TRUE)

  # ======================================================================
  # +New Session popup
  # ======================================================================
  output$new_session_body <- renderUI({
    tagList(
      tags$p(paste0("The current session will be saved to history. ",
                    "A fresh session will start with no possessions.")),
      tags$p("Session will be saved as: ",
             tags$b(app_state$session_name)),
      tags$p("Current possessions logged: ",
             tags$b(nrow(app_state$possessions))),
      tags$div(class = "popup-btn-row",
        actionButton("cancel_new_session", "Cancel",
                     class = "popup-cancel-btn"),
        actionButton("confirm_new_session", "Save & Start New",
                     class = "new-session-btn")
      )
    )
  })

  observeEvent(app_state$open_new_session_tick, {
    message("[DEBUG] opening new-session popup")
    open_popup("new_session_popup", session = session)
  }, ignoreInit = TRUE)

  observeEvent(input$cancel_new_session, {
    message("[DEBUG] cancel_new_session clicked")
    close_popup("new_session_popup", session = session)
  }, ignoreInit = TRUE)

  observeEvent(input$confirm_new_session, {
    message("[DEBUG] confirm_new_session clicked -- saving & resetting")

    # 1) Save current session to history
    try(save_session(app_state), silent = TRUE)

    # 2) Close the popup immediately
    close_popup("new_session_popup", session = session)

    # 3) CLEAN SLATE RESET  (keeps the preloaded demo sessions in store)
    app_state$session_id         <- make_session_id()
    app_state$session_created_at <- format(Sys.time(),
                                           "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    app_state$session_name       <- paste("Practice",
                                          format_concise_date(Sys.Date()))
    app_state$session_date       <- Sys.Date()
    app_state$session_type       <- "Practice"
    app_state$possessions        <- empty_possessions_df()
    app_state$ball_reversals     <- 0L
    app_state$paint_touches      <- 0L
    app_state$clock              <- NA_character_
    app_state$current_quarter    <- "Practice"
    app_state$current_category   <- NA_character_
    app_state$history_bump       <- isolate(app_state$history_bump) + 1L

    # 4) Tell the tracker module to refresh its visible inputs
    app_state$reset_inputs_tick  <- isolate(app_state$reset_inputs_tick) + 1L

    shiny::showNotification("New session started.", type = "message",
                            duration = 2)
  }, ignoreInit = TRUE)

  # ----- modules ------------------------------------------------------
  mod_tracker_server("tracker", app_state)
  mod_summary_server("summary", app_state)
  mod_coach_server("coach",     app_state)
  mod_sheet_server("sheet",     app_state)
  mod_history_server("history", app_state)
  mod_trends_server("trends",   app_state)
}

shinyApp(ui, server)
