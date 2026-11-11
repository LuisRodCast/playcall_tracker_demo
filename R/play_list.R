# =========================================================================
# R/play_list.R
# Reads plays.yaml + favorites.yaml and produces the dropdown choices.
# Alpha-numeric sort = numbers first (as strings), then letters.
# Favorites get a leading section in insertion order.
#
# Favorites matching rule (updated 2026-10):
#   -- Matching between favorites.yaml and plays.yaml is CASE-INSENSITIVE,
#      so "FLOW" in favorites.yaml and "Flow" in plays.yaml are treated as
#      the same play. The plays.yaml spelling wins (canonical form) so
#      downstream analytics group them.
#   -- Favorites that have NO match in plays.yaml (e.g. "Isolation") are
#      still shown in the Favorites section AND added to the master play
#      list so the app accepts them as valid play IDs when logged. This
#      keeps favorites.yaml authoritative for what the coach sees pinned.
#
# Favorites cap (updated 2026-11):
#   -- The dropdown's "Favorites" section now shows up to 30 pinned plays
#      (was 15). Anything past the 30th entry in favorites.yaml still
#      works as a valid play, it just isn't highlighted in the pinned
#      section.
# =========================================================================

#' Numeric-first sort: strings starting with a digit come before strings
#' starting with a letter.  Within each group, case-insensitive ascending.
sort_alnum <- function(x) {
  starts_with_digit <- grepl("^[0-9]", x)
  ord <- order(!starts_with_digit, tolower(x))
  x[ord]
}

#' Canonicalize a vector of favorite names against the master plays list.
#' Case-insensitive: a favorite that matches a play ignoring case is
#' rewritten to the plays.yaml spelling. A favorite with no match is
#' returned verbatim (the user explicitly pinned it, show it).
canonicalize_favorites <- function(favs, all_plays) {
  if (length(favs) == 0) return(character(0))
  play_lc <- tolower(all_plays)
  vapply(favs, function(f) {
    idx <- match(tolower(f), play_lc)
    if (!is.na(idx)) all_plays[idx] else f
  }, character(1), USE.NAMES = FALSE)
}

#' Load plays.yaml + favorites.yaml and build the dropdown choice vector.
#' Returns a NAMED character vector where names are the display labels and
#' the values are the play IDs (same string here -- we might diverge later
#' if plays get aliases).
load_play_choices <- function(plays_file = PLAYS_FILE,
                              faves_file = FAVES_FILE,
                              max_favorites = 30L) {

  if (!file.exists(plays_file)) {
    warning("plays.yaml not found at ", plays_file, " -- using empty list.")
    return(c(` ` = ""))
  }
  all_plays <- yaml::read_yaml(plays_file)$plays
  all_plays <- unique(as.character(all_plays))
  all_plays <- all_plays[nzchar(all_plays)]

  favorites <- character(0)
  if (file.exists(faves_file)) {
    raw_favs  <- as.character(yaml::read_yaml(faves_file)$favorites)
    raw_favs  <- raw_favs[nzchar(raw_favs)]
    favorites <- canonicalize_favorites(raw_favs, all_plays)
    # Two differently-cased favorites could canonicalize to the same
    # play; drop later duplicates, preserve the user's listed order.
    favorites <- favorites[!duplicated(favorites)]
    favorites <- head(favorites, max_favorites)
  }

  # Favorites that aren't in plays.yaml at all are added to the master
  # list so the app treats them as valid play IDs when they're logged.
  missing_from_plays <- setdiff(favorites, all_plays)
  if (length(missing_from_plays) > 0) {
    all_plays <- c(all_plays, missing_from_plays)
  }

  rest   <- setdiff(all_plays, favorites)
  rest   <- sort_alnum(rest)

  # shinyMobile's f7Select accepts a plain character vector; selectInput
  # supports grouped lists via a nested named list.  We feed grouped-form
  # which f7Select renders as <optgroup> too.
  if (length(favorites) > 0) {
    list(
      "★ Favorites" = favorites,
      "All plays"        = rest
    )
  } else {
    list("All plays" = rest)
  }
}

#' Flat version (handy where we need just the character vector of every
#' play name without grouping). Also folds in any favorites that aren't
#' in plays.yaml so the flat list treats them as valid plays too.
load_play_choices_flat <- function(plays_file = PLAYS_FILE,
                                   faves_file = FAVES_FILE) {
  if (!file.exists(plays_file)) return(character(0))
  all_plays <- as.character(yaml::read_yaml(plays_file)$plays)
  all_plays <- unique(all_plays[nzchar(all_plays)])

  if (!is.null(faves_file) && file.exists(faves_file)) {
    raw_favs <- as.character(yaml::read_yaml(faves_file)$favorites)
    raw_favs <- raw_favs[nzchar(raw_favs)]
    play_lc  <- tolower(all_plays)
    extra    <- raw_favs[!tolower(raw_favs) %in% play_lc]
    if (length(extra) > 0) all_plays <- c(all_plays, extra)
  }

  sort_alnum(unique(all_plays))
}
