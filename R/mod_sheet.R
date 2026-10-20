# =========================================================================
# R/mod_sheet.R  --  Tab 4  --  Play Call Sheet + CSV export
# =========================================================================

mod_sheet_ui <- function(id) {
  ns <- NS(id)
  div(class = "sheet-page",

    tab_header("Play Call Sheet"),

    div(class = "quarter-row",
      tags$label("QUARTER", class = "control-label"),
      quarter_chips(ns, "quarter", selected = "Q1")
    ),

    div(class = "sheet-toolbar",
      div(class = "sheet-title-block",
        tags$h2(class = "sheet-title", "Play Call Sheet"),
        tags$div(class = "sheet-sub",
                 textOutput(ns("sub"), inline = TRUE))
      ),
      # Hand-built download link so Framework7 doesn't intercept the click.
      # The "external" class + target="_blank" tell F7's router to let the
      # browser handle it natively. The "shiny-download-link" class is what
      # Shiny's JS binding attaches to -- the server-side downloadHandler
      # keyed on ns("export_csv") works unchanged.
      tags$a(
        id       = ns("export_csv"),
        class    = "shiny-download-link sheet-export-btn external",
        href     = "",
        target   = "_blank",
        download = NA,
        `data-ignore-cache` = "true",
        HTML('<span class="dl-ico">&#8681;</span> Export CSV')
      )
    ),

    reactable::reactableOutput(ns("sheet_tbl"))
  )
}

mod_sheet_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {

    q_df <- reactive({
      filter_quarter(app_state$possessions, input$quarter) |>
        dplyr::arrange(possession_id)
    })

    output$sub <- renderText({
      n <- nrow(q_df())
      sprintf("%d possessions · chronological order", n)
    })

    output$sheet_tbl <- reactable::renderReactable({
      df <- q_df()
      if (nrow(df) == 0) {
        return(reactable::reactable(
          data.frame(` ` = "No plays logged this quarter yet.",
                     check.names = FALSE),
          sortable = FALSE, borderless = TRUE,
          theme = tracker_table_theme()
        ))
      }

      is_game <- identical(app_state$session_type, "Game")

      df_show <- df |>
        dplyr::transmute(
          `#`         = possession_id,
          `PLAY NAME` = play,
          CAT         = ifelse(is.na(category) | !nzchar(category), "-", category),
          Q           = quarter,
          RESULT      = result,
          CLOCK       = ifelse(is.na(clock) | !nzchar(clock), "-", clock)
        )

      # Hide CAT column during Practice sessions
      if (!is_game) {
        df_show <- df_show |> dplyr::select(-CAT)
      }

      cols <- list(
        `#`         = reactable::colDef(width = 60,  align = "right"),
        `PLAY NAME` = reactable::colDef(minWidth = 180,
                        style = list(fontWeight = 700)),
        Q           = reactable::colDef(width = 70),
        RESULT      = reactable::colDef(width = 100,
                        cell = function(v) {
                          tags$span(class = "result-chip",
                                    style = sprintf("background:%s;",
                                                    result_color(v)), v)
                        }),
        CLOCK       = reactable::colDef(width = 90)
      )
      if ("CAT" %in% names(df_show)) {
        cols$CAT <- reactable::colDef(width = 110)
      }

      reactable::reactable(
        df_show,
        compact = TRUE, highlight = TRUE, pagination = FALSE, sortable = FALSE,
        theme = tracker_table_theme(),
        columns = cols
      )
    })

    # ---- CSV export --------------------------------------------------
    output$export_csv <- downloadHandler(
      filename = function() {
        q <- input$quarter %||% "ALL"
        sprintf("play_call_sheet_%s_%s.csv",
                format(Sys.Date(), "%Y%m%d"),
                q)
      },
      content = function(file) {
        df <- q_df()
        if (nrow(df) == 0) {
          utils::write.csv(data.frame(note = "no data"),
                           file, row.names = FALSE)
          return()
        }
        out <- df |>
          dplyr::transmute(
            possession_id,
            timestamp = format(ts, "%Y-%m-%d %H:%M:%S"),
            quarter,
            play,
            category      = ifelse(is.na(category) | !nzchar(category),
                                    "", category),
            result,
            points        = pts,
            is_fga,
            is_fgm,
            is_turnover   = is_to,
            is_foul,
            ball_reversals,
            paint_touches,
            clock         = ifelse(is.na(clock), "", clock)
          )
        utils::write.csv(out, file, row.names = FALSE)
      }
    )
  })
}

# Null-coalesce helper used by filename
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || !nzchar(a)) b else a
