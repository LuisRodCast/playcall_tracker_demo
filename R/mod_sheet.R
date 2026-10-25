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
      # Plain actionButton -- Framework7 handles native button clicks
      # cleanly. The download itself is triggered client-side via Blob
      # (see tags$script below + the observer in the server function);
      # this avoids Shiny's downloadLink + F7-router conflict that made
      # the previous target=_blank/external hack unreliable (CSV would
      # open as text in a new tab on Safari/iPad instead of saving).
      actionButton(
        inputId = ns("export_csv_click"),
        label   = HTML('<span class="dl-ico">&#8681;</span> Export CSV'),
        class   = "sheet-export-btn"
      )
    ),

    reactable::reactableOutput(ns("sheet_tbl")),

    # Client-side download handler. Receives {content, filename} from
    # the server, builds a Blob, synthesizes a hidden <a download>, and
    # clicks it programmatically. Works in every browser including
    # iPad Safari. The __registered guard prevents duplicate handler
    # registration if the module re-renders.
    tags$script(HTML(
      "if (!window.__playcall_csv_dl_registered) {
         Shiny.addCustomMessageHandler('playcall_csv_download', function(msg) {
           try {
             var blob = new Blob([msg.content],
                                 {type: 'text/csv;charset=utf-8;'});
             var url  = URL.createObjectURL(blob);
             var a    = document.createElement('a');
             a.href       = url;
             a.download   = msg.filename;
             a.style.cssText = 'display:none;';
             document.body.appendChild(a);
             a.click();
             document.body.removeChild(a);
             // Give the browser a moment to start the download before
             // revoking the object URL (Safari in particular needs this).
             setTimeout(function() { URL.revokeObjectURL(url); }, 1500);
           } catch (err) {
             console.error('[CSV download] failed:', err);
             alert('CSV download failed: ' + err.message);
           }
         });
         window.__playcall_csv_dl_registered = true;
       }"
    ))
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

    # ---- CSV export via Shiny custom-message handler -----------------
    # Fired when the user clicks the Export CSV button. We build the
    # CSV string server-side (same column shape as before) and ship it
    # to the client, where the registered handler turns it into a Blob
    # and triggers a native browser download. No downloadHandler, no
    # Shiny download binding, no Framework7 router conflict.
    observeEvent(input$export_csv_click, {
      df    <- q_df()
      q_lbl <- input$quarter %||% "ALL"
      filename <- sprintf("play_call_sheet_%s_%s.csv",
                          format(Sys.Date(), "%Y%m%d"), q_lbl)

      if (nrow(df) == 0) {
        shiny::showNotification("No plays logged this quarter yet.",
                                type = "warning", duration = 3)
        return()
      }

      out <- df |>
        dplyr::transmute(
          possession_id,
          timestamp = format(ts, "%Y-%m-%d %H:%M:%S"),
          quarter,
          play,
          category    = ifelse(is.na(category) | !nzchar(category),
                                "", category),
          result,
          points      = pts,
          is_fga,
          is_fgm,
          is_turnover = is_to,
          is_foul,
          ball_reversals,
          paint_touches,
          clock       = ifelse(is.na(clock), "", clock)
        )

      # Capture write.csv's output into a character vector via a
      # textConnection, then collapse to one string we can ship.
      csv_lines <- character(0)
      tc <- textConnection("csv_lines", open = "w", local = TRUE)
      utils::write.csv(out, tc, row.names = FALSE)
      close(tc)
      csv_text <- paste(csv_lines, collapse = "\n")

      session$sendCustomMessage("playcall_csv_download",
        list(content = csv_text, filename = filename))
    }, ignoreInit = TRUE)
  })
}

# Null-coalesce helper used by filename
`%||%` <- function(a, b) if (is.null(a) || is.na(a) || !nzchar(a)) b else a
