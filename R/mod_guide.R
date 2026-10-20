# =========================================================================
# R/mod_guide.R  --  Guide popup content
# Returns the body of the Quick Start Guide. The popup container itself
# is declared in mod_tracker_ui() so it opens via updateF7Popup().
# =========================================================================

guide_body_ui <- function() {
  tagList(
    div(class = "guide-eyebrow", "QUICK START GUIDE"),
    tags$h2(class = "guide-title", "Call it. Track it. Coach it."),

    tags$p(class = "guide-intro",
           paste0("Live Play Call Tracker turns every possession into clear, ",
                  "usable sideline insight -- without slowing down your bench.")),

    guide_step(
      "1",
      "CHOOSE A PLAY",
      "Select the play you called.",
      paste0("Tap a play from the dropdown before the possession starts. ",
             "Add, rename, or remove plays any time by editing ",
             "data/plays.yaml.")
    ),

    guide_step(
      "2",
      "TRACK MOVEMENT",
      "Count reversals and paint touches.",
      paste0("Use the counters while the possession develops. They reset ",
             "automatically when you record the result. Paint touches ",
             "capture purposeful entries into the lane.")
    ),

    guide_step(
      "3",
      "LOG THE RESULT",
      "One tap ends the possession.",
      paste0("Record makes, misses, fouls, turnovers, or an empty ",
             "possession. Your live log and stats update immediately.")
    ),

    guide_step(
      "4",
      "MAKE THE ADJUSTMENT",
      "Read the numbers that matter.",
      paste0("Watch POSS, PPP, FG%, average reversals, and paint touches ",
             "live. Use the Summary and Coach tabs to spot what is ",
             "working."))
  )
}

guide_step <- function(num, eyebrow, title, text) {
  div(class = "guide-step",
    div(class = "guide-step-num", num),
    div(class = "guide-step-body",
      div(class = "guide-step-eyebrow", eyebrow),
      div(class = "guide-step-title",   title),
      div(class = "guide-step-text",    text)
    )
  )
}
