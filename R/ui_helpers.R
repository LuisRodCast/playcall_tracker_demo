# =========================================================================
# R/ui_helpers.R
# Small UI builders for the live tracker.  Keep the modules readable.
# =========================================================================

#' A colored action tile ("+3", "FOUL", "TO", etc.)
#' Clicks go to session$input$<ns>-<id>.
action_tile <- function(ns, id, label, color) {
  tags$button(
    id    = ns(id),
    class = "action-tile action-button",
    style = sprintf("background:%s;", color),
    type  = "button",
    label
  )
}

#' A small label-above-value stat card (POSS / PPP / FG% / AVG REV / AVG PAINT)
stat_tile <- function(ns, id, label) {
  div(class = "stat-tile",
      div(class = "stat-label", label),
      div(class = "stat-value", id = ns(id), textOutput(ns(id), inline = TRUE))
  )
}

#' A counter row (ball reversals, paint touches) with +/- buttons.
#' Counter display uses uiOutput so resets re-render reliably across
#' shinyMobile versions.
counter_row <- function(ns, id_prefix, title, subtitle, accent) {
  div(class = "counter-row",
      div(class = "counter-meta",
          div(class = "counter-title", title),
          div(class = "counter-sub", subtitle)),
      div(class = "counter-controls",
          tags$button(id = ns(paste0(id_prefix, "_minus")),
                      class = "counter-btn counter-minus action-button",
                      type = "button", "-"),
          div(class = "counter-value",
              uiOutput(ns(paste0(id_prefix, "_value")), inline = TRUE)),
          tags$button(id = ns(paste0(id_prefix, "_plus")),
                      class = "counter-btn counter-plus action-button",
                      type = "button",
                      style = sprintf("background:%s;color:#fff;", accent),
                      HTML("+ ", toupper(sub("_", " ", id_prefix))))
      )
  )
}

#' Quarter chip row (segmented control for Q1/Q2/Q3/Q4/OT/TOTAL).
quarter_chips <- function(ns, id, quarters = QUARTERS, selected = "Q1") {
  radioButtons(
    inputId  = ns(id),
    label    = NULL,
    choices  = setNames(quarters, quarters),
    selected = selected,
    inline   = TRUE
  ) |>
    tagAppendAttributes(class = "quarter-chips")
}

#' Branded header strip across the top of each tab.
tab_header <- function(title_text, show_logo = TRUE) {
  div(class = "tab-header",
      if (show_logo) tags$img(src = "nmsu_zia.png", class = "tab-header-logo"),
      div(class = "tab-header-text",
          div(class = "tab-header-eyebrow", "LIVE PLAY CALL TRACKER"),
          div(class = "tab-header-title", title_text))
  )
}
