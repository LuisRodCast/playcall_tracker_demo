# =========================================================================
# R/play_list.R
# Reads plays.yaml + favorites.yaml and produces the dropdown choices.
# Alpha-numeric sort = numbers first (as strings), then letters.
# Favorites get a leading section in insertion order.
# =========================================================================

#' Numeric-first sort: strings starting with a digit come before strings
#' starting with a letter.  Within each group, case-insensitive ascending.
sort_alnum <- function(x) {
  starts_with_digit <- grepl("^[0-9]", x)
  ord <- order(!starts_with_digit, tolower(x))
  x[ord]
}

#' Load plays.yaml + favorites.yaml and build the dropdown choice vector.
#' Returns a NAMED character vector where names are the display labels and
#' the values are the play IDs (same string here -- we might diverge later
#' if plays get aliases).
load_play_choices <- function(plays_file = PLAYS_FILE,
                              faves_file = FAVES_FILE,
                              max_favorites = 15L) {

  if (!file.exists(plays_file)) {
    warning("plays.yaml not found at ", plays_file, " -- using empty list.")
    return(c(` ` = ""))
  }
  all_plays <- yaml::read_yaml(plays_file)$plays
  all_plays <- unique(as.character(all_plays))
  all_plays <- all_plays[nzchar(all_plays)]

  favorites <- character(0)
  if (file.exists(faves_file)) {
    favorites <- yaml::read_yaml(faves_file)$favorites
    favorites <- as.character(favorites)
    favorites <- favorites[favorites %in% all_plays]
    favorites <- head(favorites, max_favorites)
  }

  rest   <- setdiff(all_plays, favorites)
  rest   <- sort_alnum(rest)

  # shinyMobile's f7Select accepts a plain character vector; selectInput
  # supports grouped lists via a nested named list.  We feed grouped-form
  # which f7Select renders as <optgroup> too.
  if (length(favorites) > 0) {
    list(
      "★ Favorites" = favorites,
      "All plays"       = rest
    )
  } else {
    list("All plays" = rest)
  }
}

#' Flat version (handy where we need just the character vector of every
#' play name without grouping).
load_play_choices_flat <- function(plays_file = PLAYS_FILE) {
  if (!file.exists(plays_file)) return(character(0))
  all_plays <- as.character(yaml::read_yaml(plays_file)$plays)
  sort_alnum(unique(all_plays[nzchar(all_plays)]))
}
