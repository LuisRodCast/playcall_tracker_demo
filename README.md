# Play Call Tracker — Demo

A public, interactive demo of the Live Play Call Tracker — a mobile-first
R Shiny application that lets a basketball coaching staff log every
possession in real time (play, result, ball reversals, paint touches,
clock, quarter, and category), then renders cross-session trends,
practice-vs-game comparisons with statistical rigor, and category
tactical signatures.

**Live demo:** [https://01a157db-9f7e-82d4-a78d-6e2114dda3d9.share.connect.posit.cloud/](https://01a157db-9f7e-82d4-a78d-6e2114dda3d9.share.connect.posit.cloud/)

**Production repo:** [LuisRodCast/aggies-play-call-tracker](https://github.com/LuisRodCast/aggies-play-call-tracker)
(built for New Mexico State women's basketball)

---

## What this demo is

- Every visitor gets a fresh app on load — **nothing persists** between
  browser tabs or between visitors.
- The app opens pre-populated with **two synthetic sessions** (one
  Practice, one Game) so Trends, Summary, Coach and History tabs have
  data to show immediately. Click through them without logging a thing.
- The playbook is **generic** (ISO, HORNS, FLEX, PNR-L, PNR-R, DRAG,
  SPREAD, …) — no real team's scouting information is included.
- Session storage is a per-visitor in-memory R list (see
  `R/db_demo.R`), so there are no credentials, no cloud DB, no
  outbound HTTP calls. The production app uses a Turso cloud SQLite
  backend; the two share identical function signatures so swapping the
  backend is one `source()` difference.

## What's inside

Six tabs, built with `shinyMobile` on a Framework7 dark theme:

| Tab | Purpose |
|---|---|
| **Tracker** | Live possession logger. Pick a play, (optionally) a category, tap a result tile. Logs a row to the current session. |
| **Summary** | Per-play rollup for the current session, split by category when a Game session is active. |
| **Coach** | Last-5 strip, 8 top-line stat tiles, Best/Worst/Most-Used, play comparison table, category filter. |
| **Sheet** | Chronological possession log for the current session, CSV export. |
| **History** | Saved sessions (two preloaded + anything the visitor logs this session). View/Delete. |
| **Trends** | Cross-session analytics across every saved session: ACWR-style rollups, category analyses, play × ball-reversals and play × paint-touches scatters, and a **Practice vs Game comparison table with Welch's t-test and 95% CI** on the Δ PPP per play. |

## How to run locally

```r
# From the repo root in R:
shiny::runApp(".")
```

Requires R ≥ 4.3 and the packages listed in the production manifest
(`shinyMobile`, `shiny`, `dplyr`, `tidyr`, `ggplot2`, `reactable`,
`DT`, `yaml`, `jsonlite`, `httr2`, `scales`, `lubridate`). The demo
build does NOT need `TURSO_URL` or `TURSO_TOKEN` env vars — they are
only used by the production backend in `R/db.R`, which has been
replaced here with `R/db_demo.R`.

## How to deploy to Posit Connect Cloud

See `DEPLOYMENT.md`.

## File differences from the production repo

```
+ R/db_demo.R           NEW — in-memory per-visitor store
+ R/seed_sessions.R     NEW — generates the two preloaded fake sessions
- R/db.R                REMOVED — not needed
~ R/data_io.R           Modified — wrappers now pass app_state through
~ R/mod_history.R       Modified — list/load/delete calls now take app_state
~ R/mod_trends.R        Modified — db_load_all_possessions takes app_state
~ app.R                 Modified — initializes sessions_store, DEMO badge
~ www/custom.css        Modified — demo-badge pill style added
~ data/plays.yaml       Replaced — generic playbook
~ data/favorites.yaml   Replaced — matches generic playbook
```

Everything else is identical to the production repo.

## About

Built by [Luis Rodriguez-Castellano, PhD](https://luisrodcast.github.io/)
— applied sport scientist / sports performance coach.
