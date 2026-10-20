# =========================================================================
# R/global.R
# Loads packages and app-wide constants.  Sourced first by app.R.
# =========================================================================

suppressPackageStartupMessages({
  library(shiny)
  library(shinyMobile)
  library(dplyr)
  library(tidyr)
  library(yaml)
  library(DT)
  library(ggplot2)
  library(reactable)
  library(httr2)   # Turso HTTP client (R/db.R)
})

# ---------- Crisper plots -----------------------------------------------
# ragg produces anti-aliased, high-DPI PNGs. Shiny falls back to the
# default (fuzzy) device if ragg is not installed.
if (requireNamespace("ragg", quietly = TRUE)) {
  options(shiny.useragg = TRUE)
}

# ---------- NMSU brand colors -------------------------------------------
NMSU <- list(
  crimson        = "#8B0029",
  crimson_light  = "#B22E52",
  crimson_dark   = "#5E001B",
  white          = "#FFFFFF",
  black          = "#000000",
  bg             = "#0D0D0F",
  card           = "#1A1A1D",
  card_border    = "#2A2A2E",
  text_primary   = "#F5F5F5",
  text_muted     = "#9EA3AE",

  # Semantic colors for action buttons (kept distinct from brand crimson so
  # miss-buttons don't look like crimson UI chrome)
  make_3        = "#1DD1A1",   # +3 teal
  make_2        = "#2ECC71",   # +2 green
  miss_2        = "#E74C3C",   # -2 red
  miss_3        = "#C0392B",   # -3 deep red
  ft_make_1     = "#3498DB",   # +1F
  ft_make_2     = "#2980B9",   # +2F
  ft_make_3     = "#1F5F8E",   # +3F  (deeper blue, continues +1F -> +2F -> +3F)
  ft_miss_1     = "#1ABC9C",   # -1F
  ft_miss_2     = "#5B4CBE",   # -2F
  ft_miss_3     = "#4A3BA0",   # -3F  (deeper purple, continues -1F -> -2F -> -3F)
  and1_3        = "#F39C12",   # And1-3p  (2pt FG + 1 FT = 3)
  and1_4        = "#D35400",   # And1-4p  (3pt FG + 1 FT = 4)
  empty_0       = "#7F8C8D",   # 0
  foul          = "#8E44AD",   # FOUL
  turnover      = "#E67E22"    # TO
)

# ---------- Result button spec ------------------------------------------
# Order matters -- this is the on-screen grid order (5 cols x 3 rows).
#
# Row 1: FG attempts (makes & misses) + +1F
# Row 2: 2-shot FT outcomes + 0 + FOUL
# Row 3: 3-shot FT outcomes + And1 conversions + TO
#
# `is_foul` policy (normalized 2026-10):
# A possession has is_foul = TRUE whenever a foul was drawn on it --
# that is, any time free throws were awarded (regardless of whether
# they went in), any And1 situation, and any explicit non-shooting
# FOUL call. Field-goal makes/misses and turnovers set is_foul = FALSE.
# This is a change from earlier builds that only tagged +2F and FOUL
# as fouls; the Coach dashboard's FOUL count will retroactively climb
# for sessions saved before this release when they're reopened.
RESULT_BUTTONS <- list(
  list(id = "r_plus3",   label = "+3",       color = NMSU$make_3,     pts = 3, is_fga = TRUE,  is_fgm = TRUE,  is_to = FALSE, is_foul = FALSE),
  list(id = "r_plus2",   label = "+2",       color = NMSU$make_2,     pts = 2, is_fga = TRUE,  is_fgm = TRUE,  is_to = FALSE, is_foul = FALSE),
  list(id = "r_minus2",  label = "-2",       color = NMSU$miss_2,     pts = 0, is_fga = TRUE,  is_fgm = FALSE, is_to = FALSE, is_foul = FALSE),
  list(id = "r_minus3",  label = "-3",       color = NMSU$miss_3,     pts = 0, is_fga = TRUE,  is_fgm = FALSE, is_to = FALSE, is_foul = FALSE),
  list(id = "r_plus1f",  label = "+1F",      color = NMSU$ft_make_1,  pts = 1, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),

  list(id = "r_plus2f",  label = "+2F",      color = NMSU$ft_make_2,  pts = 2, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),
  list(id = "r_minus1f", label = "-1F",      color = NMSU$ft_miss_1,  pts = 0, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),
  list(id = "r_minus2f", label = "-2F",      color = NMSU$ft_miss_2,  pts = 0, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),
  list(id = "r_zero",    label = "0",        color = NMSU$empty_0,    pts = 0, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = FALSE),
  list(id = "r_foul",    label = "FOUL",     color = NMSU$foul,       pts = 0, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),

  # Row 3 -- 3-shot FTs, And1 conversions, TO.
  # +3F / -3F represent 3 FTs attempted after a shooting foul on a
  # 3-point attempt (all made or all missed, respectively).
  # And1-3p / And1-4p represent a made FG + 1 FT converted off the
  # And1 opportunity (2pt + 1 FT = 3, 3pt + 1 FT = 4).
  list(id = "r_plus3f",  label = "+3F",      color = NMSU$ft_make_3,  pts = 3, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),
  list(id = "r_minus3f", label = "-3F",      color = NMSU$ft_miss_3,  pts = 0, is_fga = FALSE, is_fgm = FALSE, is_to = FALSE, is_foul = TRUE),
  list(id = "r_and1_3",  label = "And1-3p",  color = NMSU$and1_3,     pts = 3, is_fga = TRUE,  is_fgm = TRUE,  is_to = FALSE, is_foul = TRUE),
  list(id = "r_and1_4",  label = "And1-4p",  color = NMSU$and1_4,     pts = 4, is_fga = TRUE,  is_fgm = TRUE,  is_to = FALSE, is_foul = TRUE),
  list(id = "r_to",      label = "TO",       color = NMSU$turnover,   pts = 0, is_fga = FALSE, is_fgm = FALSE, is_to = TRUE,  is_foul = FALSE)
)

# ---------- Quarters ----------------------------------------------------
QUARTERS <- c("Q1","Q2","Q3","Q4","OT","TOTAL")

# ---------- Session type + play category --------------------------------
# Practice sessions tag possessions with quarter = "Practice".
# Game sessions use Q1/Q2/Q3/Q4/OT (TOTAL is a view filter).
# Category is only used during Game sessions.
SESSION_TYPES <- c("Practice", "Game")
CATEGORIES    <- c("Transition", "Secondary", "2nd Shots", "HC Sets")

# ---------- Paths -------------------------------------------------------
APP_DIR     <- getwd()                                # app.R's dir at runtime
DATA_DIR    <- file.path(APP_DIR, "data")
SESSION_DIR <- file.path(DATA_DIR, "sessions")
PLAYS_FILE  <- file.path(DATA_DIR, "plays.yaml")
FAVES_FILE  <- file.path(DATA_DIR, "favorites.yaml")

if (!dir.exists(SESSION_DIR)) dir.create(SESSION_DIR, recursive = TRUE)
