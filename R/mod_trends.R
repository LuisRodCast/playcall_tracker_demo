# =========================================================================
# R/mod_trends.R  --  Tab 6  --  Cross-session trends  (Phase 4)
#
# Six charts aggregated across EVERY saved session in data/sessions/:
#   1. PPP by play over sessions (line)
#   2. Play usage frequency (horizontal bar)
#   3. Make / miss / turnover distribution per play (100% stacked bar)
#   4. Avg ball reversals vs PPP (bubble scatter)
#   5. Avg paint touches vs FG% (bubble scatter)
#   6. Quarter PPP heatmap (play x quarter)
#
# All charts share the NMSU dark theme. ggplot2 only -- no plotly.
# =========================================================================

mod_trends_ui <- function(id) {
  ns <- NS(id)
  div(class = "trends-page",
    tab_header("Trends"),
    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title", "CROSS-SESSION TRENDS"),
        tags$div(class = "section-meta",
                 textOutput(ns("trends_blurb"), inline = TRUE))
      ),
      uiOutput(ns("trends_body"))
    ),
    # Category analyses -- populated only once Game sessions with
    # tagged possessions exist in the saved pool.
    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title", "CATEGORY ANALYSES"),
        tags$div(class = "section-meta",
                 textOutput(ns("category_blurb"), inline = TRUE))
      ),
      uiOutput(ns("category_body"))
    ),
    # Practice vs Game -- requires at least one saved session of each type.
    div(class = "section-card",
      div(class = "section-header",
        tags$h3(class = "section-title", "PRACTICE vs GAME"),
        tags$div(class = "section-meta",
                 textOutput(ns("pvg_blurb"), inline = TRUE))
      ),
      uiOutput(ns("pvg_body"))
    )
  )
}

mod_trends_server <- function(id, app_state) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Reload the full pool whenever the visitor saves/deletes a session.
    # Backed by the in-memory per-visitor store -- see R/db_demo.R.
    all_poss <- reactive({
      app_state$history_bump
      tryCatch(db_load_all_possessions(app_state),
               error = function(e) {
                 message("[trends] store load failed: ", conditionMessage(e))
                 NULL
               })
    })

    output$trends_blurb <- renderText({
      df <- all_poss()
      if (is.null(df) || nrow(df) == 0) return("No saved data yet")
      n_sessions <- length(unique(df$session_id))
      sprintf("%d possessions across %d %s",
              nrow(df), n_sessions,
              if (n_sessions == 1) "session" else "sessions")
    })

    output$trends_body <- renderUI({
      df <- all_poss()
      if (is.null(df) || nrow(df) == 0) {
        return(tags$div(class = "muted-note",
          paste0("Save at least one session on the Tracker tab (via ",
                 "'+ New Session') -- cross-session trends will populate ",
                 "automatically.")))
      }
      tagList(
        div(class = "trends-grid",
          chart_card(ns, "ppp_trend",   "PPP BY PLAY OVER SESSIONS"),
          chart_card(ns, "usage",       "PLAY USAGE FREQUENCY"),
          chart_card(ns, "outcomes",    "MAKE / MISS / TURNOVER MIX"),
          chart_card(ns, "rev_ppp",     "BALL REVERSALS  vs  PPP"),
          chart_card(ns, "paint_fg",    "PAINT TOUCHES  vs  FG%"),
          chart_card(ns, "quarter_hm",  "QUARTER PPP HEATMAP")
        )
      )
    })

    # res = 120 renders text/lines at ~1.7x the Shiny default (72 DPI).
    # Combined with ragg (set in global.R), this is close to retina crisp.
    output$chart_ppp_trend <- renderPlot({
      req(nrow(all_poss()) > 0)
      make_chart_ppp_trend(all_poss())
    }, bg = "transparent", res = 120)

    output$chart_usage <- renderPlot({
      req(nrow(all_poss()) > 0)
      make_chart_usage(all_poss())
    }, bg = "transparent", res = 120)

    output$chart_outcomes <- renderPlot({
      req(nrow(all_poss()) > 0)
      make_chart_outcomes(all_poss())
    }, bg = "transparent", res = 120)

    output$chart_rev_ppp <- renderPlot({
      req(nrow(all_poss()) > 0)
      make_chart_rev_vs_ppp(all_poss())
    }, bg = "transparent", res = 120)

    output$chart_paint_fg <- renderPlot({
      req(nrow(all_poss()) > 0)
      make_chart_paint_vs_fg(all_poss())
    }, bg = "transparent", res = 120)

    output$chart_quarter_hm <- renderPlot({
      req(nrow(all_poss()) > 0)
      make_chart_quarter_heatmap(all_poss())
    }, bg = "transparent", res = 120)

    # ======================================================================
    # Category analyses — populated only once Game sessions have tagged
    # possessions in the saved pool. Everything below filters to rows with
    # a non-NA, non-empty `category` so Practice-session rows are excluded.
    # ======================================================================

    cat_poss <- reactive({
      df <- all_poss()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      d <- dplyr::filter(df, !is.na(category), nzchar(category))
      if (nrow(d) == 0) return(NULL)
      d
    })

    output$category_blurb <- renderText({
      d <- cat_poss()
      if (is.null(d)) return("No categorized possessions yet")
      n_sessions <- length(unique(d$session_id))
      sprintf("%d categorized possessions across %d Game %s",
              nrow(d), n_sessions,
              if (n_sessions == 1) "session" else "sessions")
    })

    output$category_body <- renderUI({
      d <- cat_poss()
      if (is.null(d)) {
        return(tags$div(class = "muted-note",
          paste0("No category data yet. Start a session with type = 'Game' ",
                 "and tag each possession with a category — the analyses ",
                 "below will populate automatically.")))
      }
      tagList(
        div(class = "trends-grid",
          chart_card(ns, "cat_usage",       "CATEGORY USAGE SHARE"),
          chart_card(ns, "cat_ppp",         "CATEGORY EFFICIENCY (PPP)"),
          chart_card(ns, "cat_play_hm",     "PLAY × CATEGORY PPP"),
          chart_card(ns, "cat_quarter_hm",  "CATEGORY × QUARTER PPP"),
          chart_card(ns, "cat_trend",       "CATEGORY PPP OVER SESSIONS"),
          chart_card(ns, "cat_reversals",   "CATEGORY × BALL REVERSALS → PPP"),
          chart_card(ns, "cat_paint",       "CATEGORY × PAINT TOUCHES → PPP")
        ),
        # Plain-English reading guide for coaches
        tags$div(class = "muted-note",
                 style = "margin-top:16px; line-height:1.5;",
          tags$strong("How to read these charts."),
          HTML(paste0(
            " PPP = points per possession, our main scoring-efficiency ",
            "number. The dashed line at <b>1.00</b> is the sport-science ",
            "break-even for a half-court basketball possession — above it ",
            "is good, below it is a chance to work on something. The ",
            "<b style='color:#5A9E6A;'>green</b>/",
            "<b style='color:#F2B544;'>yellow</b>/",
            "<b style='color:#D4455E;'>red</b> ",
            "coloring on bars, cells, and dots all follow that same ",
            "scale. Bubble/cell size = possessions backing the number — ",
            "bigger = more trustworthy. In the Play × Category and ",
            "Ball Reversals / Paint Touches charts, each dot is one of ",
            "our plays called in one category; a play that lives in the ",
            "upper-right corner is scoring with lots of ball movement ",
            "(reversals chart) or lots of paint attacks (paint chart) — ",
            "those are the money plays. Dots in the lower-left are the ",
            "ones to re-rep, re-call, or drop."
          ))
        )
      )
    })

    output$chart_cat_usage <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_usage(cat_poss())
    }, bg = "transparent", res = 120)

    output$chart_cat_ppp <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_ppp(cat_poss())
    }, bg = "transparent", res = 120)

    output$chart_cat_play_hm <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_play_heatmap(cat_poss())
    }, bg = "transparent", res = 120)

    output$chart_cat_quarter_hm <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_quarter_heatmap(cat_poss())
    }, bg = "transparent", res = 120)

    output$chart_cat_trend <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_trend(cat_poss())
    }, bg = "transparent", res = 120)

    output$chart_cat_reversals <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_reversals_scatter(cat_poss())
    }, bg = "transparent", res = 120)

    output$chart_cat_paint <- renderPlot({
      req(!is.null(cat_poss()))
      make_chart_cat_paint_scatter(cat_poss())
    }, bg = "transparent", res = 120)

    # ======================================================================
    # Practice vs Game comparison — not category-specific, pulls from the
    # full saved pool and splits by session_type.
    # ======================================================================

    pvg_df <- reactive({
      df <- all_poss()
      if (is.null(df) || nrow(df) == 0) return(NULL)
      # Only plays called in at least one of the two contexts
      d <- df |>
        dplyr::filter(session_type %in% c("Practice", "Game"))
      if (nrow(d) == 0) return(NULL)
      d
    })

    output$pvg_blurb <- renderText({
      d <- pvg_df()
      if (is.null(d)) return("No practice/game data yet")
      n_prac <- length(unique(d$session_id[d$session_type == "Practice"]))
      n_game <- length(unique(d$session_id[d$session_type == "Game"]))
      sprintf("%d practice %s · %d game %s",
              n_prac, if (n_prac == 1) "session" else "sessions",
              n_game, if (n_game == 1) "session" else "sessions")
    })

    output$pvg_body <- renderUI({
      d <- pvg_df()
      if (is.null(d)) {
        return(tags$div(class = "muted-note",
          "Save at least one Practice and one Game session to populate this view."))
      }
      has_prac <- any(d$session_type == "Practice")
      has_game <- any(d$session_type == "Game")
      if (!has_prac || !has_game) {
        return(tags$div(class = "muted-note",
          paste0("Need at least one saved session of EACH type (Practice and ",
                 "Game) for a side-by-side comparison. Currently have only ",
                 if (has_prac) "Practice" else "Game",
                 " sessions.")))
      }
      tagList(
        reactable::reactableOutput(ns("pvg_table")),
        # Plain-English stats explainer for coaches
        tags$div(class = "muted-note",
                 style = "margin-top:16px; line-height:1.5;",
          tags$strong("How to read this table."),
          HTML(paste0(
            " <b>&#916; PPP</b> = how much better (+) or worse (&minus;) ",
            "each play scored in games compared to practice. ",
            "The <b>95% CI</b> is the plausible range for that true ",
            "practice-vs-game gap, computed from how spread-out the ",
            "individual possession outcomes were. ",
            "<b style='color:#5A9E6A;'>Green &#916; pill</b> = we are ",
            "confident the play scored MORE in games than practice ",
            "(it travels under pressure). ",
            "<b style='color:#D4455E;'>Red &#916; pill</b> = we are ",
            "confident the play scored LESS in games than practice ",
            "(worth a coaching conversation — scouting, physical ",
            "pressure, personnel fit). ",
            "<b style='color:#6B6B6B;'>Gray &#916; pill</b> = too close ",
            "to call &mdash; the practice and game numbers overlap enough ",
            "that chance could explain the gap. ",
            "<i>n too small</i> appears when a play has fewer than ",
            "2 possessions in one of the two contexts, which isn't ",
            "enough to run the comparison &mdash; log more reps and ",
            "check back. Underlying statistical test: ",
            "<b>Welch's t-test</b> (handles unequal practice/game ",
            "sample sizes and unequal variability, which is almost ",
            "always the case in team-sport data)."
          ))
        )
      )
    })

    output$pvg_table <- reactable::renderReactable({
      d <- pvg_df()
      if (is.null(d)) return(NULL)

      # Per-play Welch's t-test (two-sided, 95% CI) on the raw possession
      # points vectors for Practice vs Game. Falls back to NA on groups
      # smaller than 2 possessions or constant-variance edge cases.
      compute_welch <- function(prac_pts, game_pts) {
        n_p <- length(prac_pts)
        n_g <- length(game_pts)
        mean_p <- if (n_p > 0) mean(prac_pts) else NA_real_
        mean_g <- if (n_g > 0) mean(game_pts) else NA_real_
        delta  <- mean_g - mean_p

        if (n_p < 2 || n_g < 2) {
          return(list(delta = delta, ci_lo = NA_real_, ci_hi = NA_real_,
                      p = NA_real_, sig_direction = "insufficient"))
        }
        tt <- tryCatch(
          stats::t.test(game_pts, prac_pts, var.equal = FALSE, conf.level = 0.95),
          error = function(e) NULL
        )
        if (is.null(tt)) {
          return(list(delta = delta, ci_lo = NA_real_, ci_hi = NA_real_,
                      p = NA_real_, sig_direction = "insufficient"))
        }
        ci <- as.numeric(tt$conf.int)
        dir <- if (!is.na(ci[1]) && ci[1] > 0) "positive"
               else if (!is.na(ci[2]) && ci[2] < 0) "negative"
               else "ns"
        list(delta = delta, ci_lo = ci[1], ci_hi = ci[2],
             p = tt$p.value, sig_direction = dir)
      }

      plays <- sort(unique(d$play))
      rows <- lapply(plays, function(p) {
        prac <- d$pts[d$play == p & d$session_type == "Practice"]
        game <- d$pts[d$play == p & d$session_type == "Game"]
        s <- compute_welch(prac, game)
        data.frame(
          play          = p,
          prac_n        = length(prac),
          prac_ppp      = if (length(prac) > 0) mean(prac) else NA_real_,
          game_n        = length(game),
          game_ppp      = if (length(game) > 0) mean(game) else NA_real_,
          delta         = s$delta,
          ci_lo         = s$ci_lo,
          ci_hi         = s$ci_hi,
          p_value       = s$p,
          sig_direction = s$sig_direction,
          stringsAsFactors = FALSE
        )
      })

      wide <- do.call(rbind, rows) |>
        dplyr::filter(prac_n + game_n > 0) |>
        dplyr::mutate(
          both = prac_n > 0 & game_n > 0,
          ci_text = ifelse(is.na(ci_lo) | is.na(ci_hi),
                           NA_character_,
                           sprintf("[%+.2f, %+.2f]", ci_lo, ci_hi))
        ) |>
        # Both-context plays first (highest-info rows), then by |delta| so
        # the biggest practice-vs-game gaps top the list.
        dplyr::arrange(dplyr::desc(both),
                       dplyr::desc(abs(delta)),
                       dplyr::desc(prac_n + game_n))

      ppp_cell <- function(v) {
        if (is.na(v)) return("-")
        formatC(v, format = "f", digits = 2)
      }

      # Delta pill color is driven by the CI from the Welch test, NOT by
      # the raw point estimate — so a +0.5 delta with a CI straddling 0
      # correctly shows as gray (we're not confident the gap is real).
      delta_cell <- function(value, index) {
        if (is.na(value)) return(tags$span(style = "color:#808080;", "-"))
        dir <- wide$sig_direction[index]
        bg <- switch(dir,
                     "positive"     = "#5A9E6A",
                     "negative"     = "#D4455E",
                     "ns"           = "#6B6B6B",
                     "insufficient" = "#4A4A4A",
                     "#6B6B6B")
        tags$span(class = "ppp-pill",
                  style = sprintf("background:%s;", bg),
                  sprintf("%+.2f", value))
      }

      ci_cell <- function(v) {
        if (is.na(v)) {
          return(tags$span(style = "color:#808080; font-style:italic;",
                           "n too small"))
        }
        v
      }

      reactable::reactable(
        wide |> dplyr::transmute(
          play,
          prac_n   = as.integer(prac_n),
          prac_ppp = prac_ppp,
          game_n   = as.integer(game_n),
          game_ppp = game_ppp,
          delta    = delta,
          ci_text  = ci_text
        ),
        compact = TRUE, highlight = TRUE, pagination = FALSE, sortable = TRUE,
        theme   = tracker_table_theme(),
        columns = list(
          play     = reactable::colDef(name = "PLAY",         minWidth = 150),
          prac_n   = reactable::colDef(name = "PRAC N",       width = 75,  align = "right"),
          prac_ppp = reactable::colDef(name = "PRAC PPP",     width = 95,  align = "right",
                       cell = ppp_cell),
          game_n   = reactable::colDef(name = "GAME N",       width = 75,  align = "right"),
          game_ppp = reactable::colDef(name = "GAME PPP",     width = 95,  align = "right",
                       cell = ppp_cell),
          delta    = reactable::colDef(name = "Δ PPP",   width = 100, align = "right",
                       cell = delta_cell),
          ci_text  = reactable::colDef(name = "95% CI",       width = 140, align = "right",
                       cell = ci_cell)
        )
      )
    })
  })
}

# ---------- UI helper ----------------------------------------------------
chart_card <- function(ns, key, title) {
  div(class = "chart-card",
    tags$h4(class = "chart-title", title),
    plotOutput(ns(paste0("chart_", key)), height = "340px")
  )
}

# The old file-based load_all_sessions_possessions() was removed in the
# Turso migration (Phase 5). The Trends tab now calls
# db_load_all_possessions() directly -- see R/db.R.

# =========================================================================
# Shared ggplot theme + palette
# =========================================================================
nmsu_theme <- function() {
  ggplot2::theme_minimal(base_size = 11) +
  ggplot2::theme(
    plot.background   = ggplot2::element_rect(fill = "transparent", color = NA),
    panel.background  = ggplot2::element_rect(fill = "transparent", color = NA),
    panel.grid.major  = ggplot2::element_line(color = "#2A2A2E", linewidth = 0.3),
    panel.grid.minor  = ggplot2::element_blank(),
    text              = ggplot2::element_text(color = "#D0D0D0"),
    axis.text         = ggplot2::element_text(color = "#A0A0A0", size = 9),
    axis.title        = ggplot2::element_text(color = "#C0C0C0", size = 10),
    legend.background = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.key        = ggplot2::element_rect(fill = "transparent", color = NA),
    legend.text       = ggplot2::element_text(color = "#C0C0C0", size = 8),
    legend.title      = ggplot2::element_text(color = "#C0C0C0", size = 9),
    legend.position   = "bottom",
    plot.title        = ggplot2::element_blank(),
    strip.background  = ggplot2::element_rect(fill = "#8B0029", color = NA),
    strip.text        = ggplot2::element_text(color = "#FFFFFF", face = "bold")
  )
}

NMSU_PALETTE <- c(
  "#8B0029", "#B8003A", "#D4455E", "#E07A8C",
  "#4A6FA5", "#6B8DBE", "#8CAAD7",
  "#F2B544", "#E8914C",
  "#5A9E6A", "#7FB58D"
)

empty_plot <- function(msg) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = msg,
                      color = "#808080", size = 4.5) +
    ggplot2::theme_void() +
    ggplot2::theme(plot.background =
                     ggplot2::element_rect(fill = "transparent", color = NA))
}

# =========================================================================
# Chart 1 -- PPP by play over sessions
# =========================================================================
make_chart_ppp_trend <- function(df) {
  s <- df |>
    dplyr::group_by(session_date, play) |>
    dplyr::summarise(
      ppp  = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss = dplyr::n(),
      .groups = "drop"
    )

  keep_plays <- s |>
    dplyr::group_by(play) |>
    dplyr::summarise(tot = sum(poss), .groups = "drop") |>
    dplyr::filter(tot >= 3) |>
    dplyr::pull(play)

  s <- dplyr::filter(s, play %in% keep_plays)

  if (nrow(s) == 0 || length(unique(s$session_date)) < 2) {
    return(empty_plot("Need possessions across at least 2 sessions"))
  }

  ggplot2::ggplot(s, ggplot2::aes(x = session_date, y = ppp,
                                   color = play, group = play)) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::geom_point(ggplot2::aes(size = poss), alpha = 0.9) +
    ggplot2::scale_size(range = c(1.8, 5), guide = "none") +
    ggplot2::scale_color_manual(values = rep(NMSU_PALETTE, length.out = 50)) +
    ggplot2::labs(x = NULL, y = "PPP", color = NULL) +
    nmsu_theme()
}

# =========================================================================
# Chart 2 -- Play usage frequency
# =========================================================================
make_chart_usage <- function(df) {
  s <- df |>
    dplyr::count(play, name = "poss") |>
    dplyr::arrange(dplyr::desc(poss)) |>
    utils::head(15)

  if (nrow(s) == 0) return(empty_plot("No possessions logged"))

  s$play <- factor(s$play, levels = rev(s$play))

  ggplot2::ggplot(s, ggplot2::aes(x = poss, y = play)) +
    ggplot2::geom_col(fill = "#8B0029", width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = poss),
                       hjust = -0.3, size = 3.3, color = "#E0E0E0") +
    ggplot2::expand_limits(x = max(s$poss) * 1.12) +
    ggplot2::labs(x = "Possessions", y = NULL) +
    nmsu_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# =========================================================================
# Chart 3 -- Make / miss / turnover mix per play
# =========================================================================
make_chart_outcomes <- function(df) {
  df$outcome_cat <- dplyr::case_when(
    df$is_fgm %in% TRUE                                             ~ "Make",
    df$is_fga %in% TRUE & !(df$is_fgm %in% TRUE)                    ~ "Miss",
    grepl("TO|turnover", df$result, ignore.case = TRUE)             ~ "Turnover",
    df$pts > 0                                                      ~ "FT scoring",
    TRUE                                                            ~ "Empty"
  )

  top_plays <- df |> dplyr::count(play) |>
    dplyr::arrange(dplyr::desc(n)) |> utils::head(10) |> dplyr::pull(play)
  d <- df |> dplyr::filter(play %in% top_plays)

  s <- d |> dplyr::count(play, outcome_cat)
  if (nrow(s) == 0) return(empty_plot("Not enough data"))

  s$play <- factor(s$play, levels = rev(top_plays))
  s$outcome_cat <- factor(s$outcome_cat,
                           levels = c("Make", "FT scoring", "Miss",
                                      "Turnover", "Empty"))

  ggplot2::ggplot(s, ggplot2::aes(x = play, y = n, fill = outcome_cat)) +
    ggplot2::geom_col(position = "fill", width = 0.72) +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::scale_fill_manual(values = c(
      "Make"       = "#5A9E6A",
      "FT scoring" = "#F2B544",
      "Miss"       = "#D4455E",
      "Turnover"   = "#E8914C",
      "Empty"      = "#6B6B6B"
    )) +
    ggplot2::coord_flip() +
    ggplot2::labs(x = NULL, y = "Share", fill = NULL) +
    nmsu_theme()
}

# =========================================================================
# Chart 4 -- Ball reversals vs PPP
# =========================================================================
make_chart_rev_vs_ppp <- function(df) {
  s <- df |>
    dplyr::group_by(play) |>
    dplyr::summarise(
      avg_rev = mean(ball_reversals, na.rm = TRUE),
      ppp     = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss    = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(poss >= 2)

  if (nrow(s) == 0) return(empty_plot("Need >= 2 possessions per play"))

  ggplot2::ggplot(s, ggplot2::aes(x = avg_rev, y = ppp)) +
    ggplot2::geom_point(ggplot2::aes(size = poss, color = ppp), alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = play),
                       color = "#E0E0E0", size = 3.1,
                       nudge_y = 0.08, check_overlap = TRUE) +
    ggplot2::scale_color_gradient(low = "#D4455E", high = "#5A9E6A",
                                   name = "PPP") +
    ggplot2::scale_size(range = c(3, 11), guide = "none") +
    ggplot2::labs(x = "Avg ball reversals", y = "PPP") +
    nmsu_theme()
}

# =========================================================================
# Chart 5 -- Paint touches vs FG%
# =========================================================================
make_chart_paint_vs_fg <- function(df) {
  s <- df |>
    dplyr::group_by(play) |>
    dplyr::summarise(
      avg_paint = mean(paint_touches, na.rm = TRUE),
      fg_pct    = ifelse(sum(is_fga, na.rm = TRUE) > 0,
                         sum(is_fgm, na.rm = TRUE) /
                           sum(is_fga, na.rm = TRUE),
                         NA_real_),
      poss      = dplyr::n(),
      .groups   = "drop"
    ) |>
    dplyr::filter(poss >= 2, !is.na(fg_pct))

  if (nrow(s) == 0) return(empty_plot("Need FG attempts across plays"))

  ggplot2::ggplot(s, ggplot2::aes(x = avg_paint, y = fg_pct)) +
    ggplot2::geom_point(ggplot2::aes(size = poss, color = fg_pct), alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = play),
                       color = "#E0E0E0", size = 3.1,
                       nudge_y = 0.025, check_overlap = TRUE) +
    ggplot2::scale_color_gradient(low = "#D4455E", high = "#5A9E6A",
                                   labels = scales::percent_format(),
                                   name = "FG%") +
    ggplot2::scale_size(range = c(3, 11), guide = "none") +
    ggplot2::scale_y_continuous(labels = scales::percent_format()) +
    ggplot2::labs(x = "Avg paint touches", y = "FG%") +
    nmsu_theme()
}

# =========================================================================
# Chart 6 -- Quarter PPP heatmap
# =========================================================================
make_chart_quarter_heatmap <- function(df) {
  d <- df |> dplyr::filter(quarter %in% c("Q1","Q2","Q3","Q4"))

  if (nrow(d) == 0) return(empty_plot("No quarter data"))

  top_plays <- d |> dplyr::count(play) |>
    dplyr::arrange(dplyr::desc(n)) |> utils::head(10) |> dplyr::pull(play)

  s <- d |>
    dplyr::filter(play %in% top_plays) |>
    dplyr::group_by(play, quarter) |>
    dplyr::summarise(
      ppp  = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(poss >= 1)

  if (nrow(s) == 0) return(empty_plot("Not enough quarter data"))

  s$play    <- factor(s$play,    levels = rev(top_plays))
  s$quarter <- factor(s$quarter, levels = c("Q1","Q2","Q3","Q4"))

  ggplot2::ggplot(s, ggplot2::aes(x = quarter, y = play, fill = ppp)) +
    ggplot2::geom_tile(color = "#1A1A1C", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", ppp)),
                       color = "#FFFFFF", size = 3.4,
                       fontface = "bold") +
    ggplot2::scale_fill_gradient2(low = "#D4455E", mid = "#F2B544",
                                   high = "#5A9E6A", midpoint = 1.0,
                                   name = "PPP") +
    ggplot2::labs(x = NULL, y = NULL) +
    nmsu_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# =========================================================================
# Category chart 1 -- Category usage share
#
# How much of our offensive possession mix is each category? Flags
# whether we're over- or under-indexing on any one style vs where our
# actual production (next chart) is coming from.
# =========================================================================
make_chart_cat_usage <- function(df) {
  s <- df |>
    dplyr::count(category, name = "poss") |>
    dplyr::mutate(share = poss / sum(poss)) |>
    dplyr::arrange(dplyr::desc(poss))

  if (nrow(s) == 0) return(empty_plot("No category data"))

  s$category <- factor(s$category, levels = rev(s$category))

  ggplot2::ggplot(s, ggplot2::aes(x = poss, y = category)) +
    ggplot2::geom_col(fill = "#8B0029", width = 0.72) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%d  (%.0f%%)",
                                                     poss, 100 * share)),
                       hjust = -0.1, size = 3.3, color = "#E0E0E0") +
    ggplot2::expand_limits(x = max(s$poss) * 1.28) +
    ggplot2::labs(x = "Possessions", y = NULL) +
    nmsu_theme() +
    ggplot2::theme(panel.grid.major.y = ggplot2::element_blank())
}

# =========================================================================
# Category chart 2 -- Efficiency (PPP) per category
#
# Bars colored by distance from 1.0 PPP (sport-science break-even for a
# half-court basketball possession). Labels include n and FG% so the
# coach can gauge sample size and shot quality at the same time.
# =========================================================================
make_chart_cat_ppp <- function(df) {
  s <- df |>
    dplyr::group_by(category) |>
    dplyr::summarise(
      poss   = dplyr::n(),
      ppp    = sum(pts, na.rm = TRUE) / dplyr::n(),
      fg_pct = ifelse(sum(is_fga, na.rm = TRUE) > 0,
                      sum(is_fgm, na.rm = TRUE) / sum(is_fga, na.rm = TRUE),
                      NA_real_),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(ppp))

  if (nrow(s) == 0) return(empty_plot("No category data"))

  s$category <- factor(s$category, levels = s$category)

  ggplot2::ggplot(s, ggplot2::aes(x = category, y = ppp)) +
    ggplot2::geom_col(ggplot2::aes(fill = ppp), width = 0.65) +
    ggplot2::geom_hline(yintercept = 1.0, color = "#808080",
                         linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf(
                         "%.2f\n(n=%d · %s)",
                         ppp, poss,
                         ifelse(is.na(fg_pct), "--",
                                sprintf("%.0f%%", 100 * fg_pct)))),
                       vjust = -0.3, size = 3.1, color = "#E0E0E0",
                       lineheight = 0.9) +
    ggplot2::scale_fill_gradient2(low = "#D4455E", mid = "#F2B544",
                                   high = "#5A9E6A", midpoint = 1.0,
                                   guide = "none") +
    ggplot2::expand_limits(y = max(s$ppp, na.rm = TRUE) * 1.28) +
    ggplot2::labs(x = NULL, y = "PPP") +
    nmsu_theme()
}

# =========================================================================
# Category chart 3 -- Play x Category PPP heatmap
#
# Top-10 most-called plays across all Game sessions, broken down by
# category. Tells the coach WHICH play works in WHICH context -- "ISO
# plays better in HC Sets than in Secondary", for example.
# =========================================================================
make_chart_cat_play_heatmap <- function(df) {
  top_plays <- df |> dplyr::count(play) |>
    dplyr::arrange(dplyr::desc(n)) |> utils::head(10) |> dplyr::pull(play)

  s <- df |>
    dplyr::filter(play %in% top_plays) |>
    dplyr::group_by(play, category) |>
    dplyr::summarise(
      ppp  = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(poss >= 1)

  if (nrow(s) == 0) return(empty_plot("Not enough data"))

  s$play     <- factor(s$play, levels = rev(top_plays))
  s$category <- factor(s$category, levels = CATEGORIES)

  ggplot2::ggplot(s, ggplot2::aes(x = category, y = play, fill = ppp)) +
    ggplot2::geom_tile(color = "#1A1A1C", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f\n(n=%d)",
                                                     ppp, poss)),
                       color = "#FFFFFF", size = 2.9, fontface = "bold",
                       lineheight = 0.9) +
    ggplot2::scale_fill_gradient2(low = "#D4455E", mid = "#F2B544",
                                   high = "#5A9E6A", midpoint = 1.0,
                                   name = "PPP") +
    ggplot2::labs(x = NULL, y = NULL) +
    nmsu_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# =========================================================================
# Category chart 4 -- Category x Quarter PPP heatmap
#
# Does Transition efficiency drop in Q4 when legs are gone? Do we run
# more HC Sets late and get rewarded? Game-flow / clock-management view.
# =========================================================================
make_chart_cat_quarter_heatmap <- function(df) {
  d <- df |> dplyr::filter(quarter %in% c("Q1", "Q2", "Q3", "Q4"))

  if (nrow(d) == 0) return(empty_plot("No quarter data"))

  s <- d |>
    dplyr::group_by(category, quarter) |>
    dplyr::summarise(
      ppp  = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(s) == 0) return(empty_plot("Not enough data"))

  s$quarter  <- factor(s$quarter, levels = c("Q1", "Q2", "Q3", "Q4"))
  s$category <- factor(s$category, levels = CATEGORIES)

  ggplot2::ggplot(s, ggplot2::aes(x = quarter, y = category, fill = ppp)) +
    ggplot2::geom_tile(color = "#1A1A1C", linewidth = 0.6) +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f\n(n=%d)",
                                                     ppp, poss)),
                       color = "#FFFFFF", size = 3.2, fontface = "bold",
                       lineheight = 0.9) +
    ggplot2::scale_fill_gradient2(low = "#D4455E", mid = "#F2B544",
                                   high = "#5A9E6A", midpoint = 1.0,
                                   name = "PPP") +
    ggplot2::labs(x = NULL, y = NULL) +
    nmsu_theme() +
    ggplot2::theme(panel.grid = ggplot2::element_blank())
}

# =========================================================================
# Category chart 5 -- Category PPP trend over sessions
#
# One line per category across session dates. Shows season development:
# is Secondary efficiency climbing as the new reads install? Is HC Sets
# plateauing? Bubble size = possessions for that category that night.
# =========================================================================
make_chart_cat_trend <- function(df) {
  s <- df |>
    dplyr::group_by(session_date, category) |>
    dplyr::summarise(
      ppp  = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss = dplyr::n(),
      .groups = "drop"
    )

  if (nrow(s) == 0 || length(unique(s$session_date)) < 2) {
    return(empty_plot("Need Game-session category data across ≥ 2 sessions"))
  }

  s$category <- factor(s$category, levels = CATEGORIES)

  # Pick four visually distinct colors from the brand palette, one per
  # category. Falls back to recycled palette if CATEGORIES grows later.
  cat_colors <- stats::setNames(
    rep(NMSU_PALETTE[c(1, 5, 8, 10)], length.out = length(CATEGORIES)),
    CATEGORIES
  )

  ggplot2::ggplot(s, ggplot2::aes(x = session_date, y = ppp,
                                   color = category, group = category)) +
    ggplot2::geom_hline(yintercept = 1.0, color = "#808080",
                         linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_line(linewidth = 0.9, alpha = 0.9) +
    ggplot2::geom_point(ggplot2::aes(size = poss), alpha = 0.9) +
    ggplot2::scale_size(range = c(2, 6), guide = "none") +
    ggplot2::scale_color_manual(values = cat_colors, drop = FALSE) +
    ggplot2::labs(x = NULL, y = "PPP", color = NULL) +
    nmsu_theme()
}

# =========================================================================
# Category chart 6 -- Category x Ball Reversals -> PPP
#
# How are the points being generated? Within each category, do the
# high-PPP plays rely on more ball movement, or do they get there with
# fewer reversals? Tells the staff the TACTICAL SIGNATURE of each
# category -- "HC Sets scores off movement, Transition scores off zero
# reversals" is a very different coaching read than "both score the
# same amount."
# =========================================================================
make_chart_cat_reversals_scatter <- function(df) {
  s <- df |>
    dplyr::group_by(play, category) |>
    dplyr::summarise(
      avg_rev = mean(ball_reversals, na.rm = TRUE),
      ppp     = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss    = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::filter(poss >= 3)

  if (nrow(s) == 0) {
    return(empty_plot("Need ≥ 3 possessions per (play, category) pair"))
  }

  s$category <- factor(s$category, levels = CATEGORIES)
  cat_colors <- stats::setNames(
    rep(NMSU_PALETTE[c(1, 5, 8, 10)], length.out = length(CATEGORIES)),
    CATEGORIES
  )

  ggplot2::ggplot(s, ggplot2::aes(x = avg_rev, y = ppp)) +
    ggplot2::geom_hline(yintercept = 1.0, color = "#808080",
                         linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = category, size = poss),
                         alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = play),
                       color = "#E0E0E0", size = 2.9,
                       nudge_y = 0.07, check_overlap = TRUE) +
    ggplot2::scale_color_manual(values = cat_colors, drop = FALSE,
                                  name = NULL) +
    ggplot2::scale_size(range = c(3, 10), guide = "none") +
    ggplot2::labs(x = "Avg ball reversals", y = "PPP") +
    nmsu_theme()
}

# =========================================================================
# Category chart 7 -- Category x Paint Touches -> PPP
#
# Same question but for paint attacks instead of ball movement. A play
# that lives in Transition and scores off paint touches is a very
# different animal than one that scores from HC Sets with zero paint
# touches. Pair this with the reversals view to classify each
# (play, category) pair as "movement scorer", "paint scorer", "both",
# or "neither" (and the latter is the one to re-rep or drop).
# =========================================================================
make_chart_cat_paint_scatter <- function(df) {
  s <- df |>
    dplyr::group_by(play, category) |>
    dplyr::summarise(
      avg_paint = mean(paint_touches, na.rm = TRUE),
      ppp       = sum(pts, na.rm = TRUE) / dplyr::n(),
      poss      = dplyr::n(),
      .groups   = "drop"
    ) |>
    dplyr::filter(poss >= 3)

  if (nrow(s) == 0) {
    return(empty_plot("Need ≥ 3 possessions per (play, category) pair"))
  }

  s$category <- factor(s$category, levels = CATEGORIES)
  cat_colors <- stats::setNames(
    rep(NMSU_PALETTE[c(1, 5, 8, 10)], length.out = length(CATEGORIES)),
    CATEGORIES
  )

  ggplot2::ggplot(s, ggplot2::aes(x = avg_paint, y = ppp)) +
    ggplot2::geom_hline(yintercept = 1.0, color = "#808080",
                         linetype = "dashed", linewidth = 0.4) +
    ggplot2::geom_point(ggplot2::aes(color = category, size = poss),
                         alpha = 0.9) +
    ggplot2::geom_text(ggplot2::aes(label = play),
                       color = "#E0E0E0", size = 2.9,
                       nudge_y = 0.07, check_overlap = TRUE) +
    ggplot2::scale_color_manual(values = cat_colors, drop = FALSE,
                                  name = NULL) +
    ggplot2::scale_size(range = c(3, 10), guide = "none") +
    ggplot2::labs(x = "Avg paint touches", y = "PPP") +
    nmsu_theme()
}
