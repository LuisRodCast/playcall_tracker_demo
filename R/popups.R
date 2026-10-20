# =========================================================================
# R/popups.R
# Lightweight Framework7 popup helpers.
#
# Why custom? Some shinyMobile 2.x releases ship an f7Popup() that calls
# session$ns(id) at UI build time, which crashes because there is no
# active reactive domain yet. These helpers bypass f7Popup entirely by
# building the Framework7 popup HTML directly and toggling it with a tiny
# custom-message JS handler. They work regardless of shinyMobile version.
# =========================================================================

# ---- UI: build a Framework7 popup div -----------------------------------
# Usage: f7_popup_manual("my_popup", title = "Title", content_tag_list)
f7_popup_manual <- function(id, title = NULL, ...,
                             closeByBackdropClick = TRUE,
                             swipeToClose = TRUE) {

  props <- list(
    backdrop             = TRUE,
    closeByBackdropClick = closeByBackdropClick,
    closeOnEscape        = FALSE,
    animate              = TRUE,
    swipeToClose         = swipeToClose,
    push                 = FALSE
  )

  tags$div(
    class        = "popup",
    id           = id,
    `data-props` = as.character(jsonlite::toJSON(props, auto_unbox = TRUE)),
    tags$div(class = "view",
      tags$div(class = "page",
        tags$div(class = "navbar",
          tags$div(class = "navbar-bg"),
          tags$div(class = "navbar-inner",
            if (!is.null(title)) tags$div(class = "title", title),
            tags$div(class = "right",
              tags$a(href = "#", class = "link popup-close",
                     "Close")
            )
          )
        ),
        tags$div(class = "page-content popup-body-padded", ...)
      )
    )
  )
}

# ---- Server: toggle a popup via custom message --------------------------
open_popup <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("f7_popup_toggle",
                            list(id = id, opened = TRUE))
}

close_popup <- function(id, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage("f7_popup_toggle",
                            list(id = id, opened = FALSE))
}

# ---- Client-side JS handler ---------------------------------------------
# Include ONCE in tags$head (e.g. inside f7Page's head block).
popup_message_handler_tag <- function() {
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      Shiny.addCustomMessageHandler('f7_popup_toggle', function(msg) {
        var id = msg.id;
        var selector = '#' + id;
        var app = window.app;
        try {
          if (app && app.popup) {
            if (msg.opened) {
              app.popup.open(selector);
            } else {
              app.popup.close(selector);
            }
          } else {
            console.warn('[popup] Framework7 app not ready, using fallback');
            var el = document.getElementById(id);
            if (!el) return;
            if (msg.opened) {
              el.classList.add('modal-in');
              el.style.display = 'block';
              el.style.zIndex = '13500';
            } else {
              el.classList.remove('modal-in');
              el.style.display = 'none';
            }
          }
        } catch(e) {
          console.error('[popup] toggle failed', e);
        }
      });
    });
  "))
}
