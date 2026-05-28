# CLAUDE.md — panna R Package

Player rating system for football using RAPM + SPM methodology. This is the primary development workspace in the pannaverse ecosystem.

**Active data source: Opta only.** FBref/Understat modules and pipelines are deprecated and slated for archival — do not extend them.

## Development Commands

```r
# Load package for interactive dev
devtools::load_all()

# Run tests
devtools::test()                          # All tests
devtools::test(filter = "rapm")           # Single test file

# Full package check (run before considering feature complete)
devtools::check()

# Rebuild docs (after editing roxygen comments)
devtools::document()

# Build pkgdown site
pkgdown::build_site()
```

**Working directory**: Always `cd panna` before running R commands, or use `devtools::load_all("panna")` from pannaverse root.

## Architecture

### R Source Files (R/)

| Module | Key Files | Purpose |
|--------|-----------|---------|
| **RAPM** | `rapm_matrix.R`, `rapm_model.R`, `splint_creation.R` | Regularized Adjusted Plus-Minus via glmnet |
| **SPM** | `spm_model.R`, `spm_opta.R`, `feature_engineering.R` | Statistical Plus-Minus (XGBoost-based prior for RAPM) |
| **Panna Rating** | `panna_rating.R`, `offensive_defensive.R` | Final combined rating = xRAPM with SPM prior |
| **Skills** | `estimated_skills.R`, `skill_optimization.R` | Per-stat skill estimation with Bayesian shrinkage |
| **xMetrics** | `xg_model.R`, `xpass_model.R`, `epv_model.R`, `epv_features.R` | xG/xA/xPass/EPV from SPADL events |
| **SPADL** | `spadl_conversion.R` | Opta events -> SPADL action format |
| **Data Loaders** | `data_loaders.R`, `opta_loaders.R` | Load from local parquet or GitHub Releases |
| **Player Stats** | `player_stats_opta.R` (active); `player_stats_fbref.R`, `player_stats_understat.R` (deprecated) | Aggregate player-level statistics |
| **Match Prediction** | `match_prediction.R` | Team-level features + XGBoost match outcome model |
| **Scraping** (deprecated) | `scrape_fbref_*.R`, `scrape_understat*.R` | Web scraping utilities — archival candidates |
| **Data Processing** | `data_processing.R`, `possession_chains.R` | Transformations, possession chain analysis |
| **Utilities** | `utils.R`, `constants.R`, `globals.R`, `piggyback.R` | Helpers, NSE declarations, GitHub Releases I/O |
| **PSR/PSV** | `psr.R` | Player Skill Rating (smoothed) + Player Stat Value (per-game) with O/D decomposition |
| **WPA** | `wp_model.R`, `wp_credit.R` | Win probability model (3-class: home/draw/away) and WPA credit assignment |
| **EPR** | `player_ratings_epv.R` | Expected Points Rating. Legacy `calculate_epr()` = decay-weighted Bayesian mean; modern `calculate_epr_regression()` = weighted ridge with league-season FE + opp_def_rating control (2026-05-19, the production version used in `debug/keep/build_epr_weekly.R`) |
| **Skill Config** | `skill_config.R` | Soccer stat rating definitions, position map, hyperparameters |
| **Game Ratings** | `player_game_ratings.R` | Unified per-game output: EPV + WPA + PSV → panna_value |
| **Centrality** | `centrality.R` | Network centrality metrics for player influence |
| **Simulation** | `simulate.R` | Match simulation engine |
| **Pitch Plot** | `pitch_plot.R` | Football pitch visualization |
| **Attribution** | `player_attribution.R` | Player contribution attribution |
| **Weather** | `weather.R` | Weather data integration |
| **Comparison** | `compare_players.R` | Player comparison utilities |
| **Competitions** (deprecated) | `fbref_competitions.R`, `understat_competitions.R` | League/competition metadata lookups |
| **Package** | `panna-package.R` | Package-level roxygen docs |

### Pipelines (data-raw/)

Run order matters — later pipelines depend on earlier ones.

| Pipeline | Directory | Entry Point | Depends On |
|----------|-----------|-------------|------------|
| **EPV/xMetrics** | `epv/` | `03_calculate_player_xmetrics.R` | Pre-trained models via `pannamodels::load_panna_model()` |
| **Opta RAPM/SPM** ⭐ | `player-ratings-opta/` | `run_pipeline_opta.R` | xMetrics output |
| **FBref RAPM/SPM** (deprecated) | `player-ratings-fbref/` | `run_pipeline.R` | pannadata FBref data — archival candidate |
| **Skills** | `estimated-skills/` | `run_skills_pipeline.R` | Opta RAPM output (cache-opta/) |
| **Predictions** | `match-predictions-opta/` | `run_predictions_opta.R` | Opta RAPM + Skills output |
| **Blog Export** | `match-predictions-opta/` | Steps 10b + 10c (opt-in) | EPV/WPA/PSV models + match events |
| **WC 2026 Sim** | `match-predictions-opta/` | Step 11 (opt-in) `11_simulate_wc2026.R` | 07_predictions + hand-curated `wc2026_groups.rds`. Outputs BT ratings, champion probs, group expectations |

**Pipeline scripts are numbered** (01, 02, ...) and run sequentially within each pipeline. The `run_*.R` entry points source them in order. Shared pipeline infrastructure lives in `data-raw/pipeline_utils.R` (`run_step()`, `check_critical_step()`, `print_pipeline_summary()`, `clear_cache_files()`).

### Cache Directories (all gitignored)

| Directory | Contents | Shared With |
|-----------|----------|-------------|
| `data-raw/cache/` | SPADL conversions, EPV intermediate | EPV pipeline |
| `data-raw/cache-opta/` | Opta RAPM steps 01-09 output | Skills pipeline reads `07_seasonal_ratings.rds` |
| `data-raw/cache-skills/` | Skills pipeline steps 01-08 | Predictions reads `06_seasonal_ratings.rds` |
| `data-raw/cache-predictions-opta/` | Prediction model intermediates | Blog data export |
| `data-raw/debug/` | Temporary debug scripts | `debug/keep/` for saved scripts |

### Value Metrics Architecture

Two-path rating system (mirrors torpverse TORP pattern):

```
SPADL actions → EPV credits → per-game EPV → EPR (decay-weighted) ──┐
                     └→ WP model → WPA → per-game WPA               ├→ panna_value (50/50 blend)
Box-score stats → PSV/OSV/DSV (per-game via glmnet coefficients)     │
Stat ratings → PSR/OSR/DSR (smoothed skills via glmnet) ────────────┘
     └→ Multi-target RAPM (xG, EPV, WPA, PSV as response variables)
          └→ Match predictions (team-aggregated value metrics)
```

**Key functions:**
- `aggregate_player_game_epv()` — Per-game EPV with offensive/defensive decomposition
- `calculate_psv()` / `calculate_psv_components()` — Per-game stat value, `osv + dsv = psv`
- `create_wp_features()` → `train_wp_model()` → `add_wp_vars()` — WPA pipeline (draw = 0.5)
- `assign_wpa_credit()` → `aggregate_player_game_wpa()` — WPA credit split and per-game aggregation
- `calculate_epr()` / `calculate_epr_batch()` — Legacy: Bayesian-shrunk EPV ratings with decay
- `calculate_epr_regression()` / `optimize_epr_decay()` — Production: weighted ridge with player + league-season FE + opp_def_rating control. Inputs require `opp_def_rating` column joined from `cache-opta/team_season_strength.parquet`. β_player IS the EPR. Decay parameter near-irrelevant for prediction (chosen 900 days via held-out MSE)
- `build_player_game_ratings()` — Merges EPV + WPA + PSV → `panna_value`
- `add_value_metrics_to_splints()` — Joins per-game values to RAPM splints for multi-target
- `fit_spm_opta_target()` — SPM with custom target column (for multi-target RAPM)

**Constants** (`constants.R`): `PANNA_EPR_WEIGHT = 0.5`, `PANNA_PSR_WEIGHT = 0.5`
**EPR defaults** (`player_ratings_epv.R`): `EPR_DECAY_OFFENSIVE = 400`, `EPR_PRIOR_GAMES = 10.2`
**WPA defaults** (`wp_credit.R`): `WPA_ACTOR_SHARE = 0.5`, `WP_DRAW_VALUE = 0.5`

### Tests (tests/testthat/)

36 test files covering loaders, models, pipelines, scraping, value metrics. Shared fixtures in `helper-fixtures.R`. Uses testthat edition 3.

## Key Conventions

- **`source = "remote"` default**: All `load_opta_*()` and `player_opta_*()` functions default to downloading from GitHub Releases. Pass `source = "local"` for local data.
- **Player IDs**: Opta pipeline uses real alphanumeric Opta IDs (e.g., `5ilkkfbsss0bxd6ttdlqg0uz9`) throughout. FBref uses FBref player IDs.
- **Config override pattern**: `run_pipeline_opta.R` uses `if (!exists(...))` so test scripts can set variables before sourcing.
- **Tournament seasons**: Use "YYYY Country" format (e.g., "2018 Russia"), not "YYYY-YYYY".
- **Position taxonomy**: Prefer the 16-role classification (GK/CB/LB/RB/LWB/RWB/DM/CM/LM/RM/CAM/LW/RW/CF/LF/RF) over the legacy 4-bucket GK/DEF/MID/FWD when adding features, aggregations, or display labels. Canonical mapper: `classify_role(position, position_side)` in `R/minutes_model.R`. Empirical 10+ min spread between roles inside the same broad bucket (e.g. CB averages 87 min, LB 86, AM 80, ST-Right 77). Refactor old broad-bucket usage opportunistically.

## Gotchas

- **Splint creation uses second precision** — `extract_period_end_times()` reads Opta `type_id == 30` markers from raw match events to set exact period boundaries; `create_splint_boundaries_fast()` uses `events$minute + events$added_time` (where `added_time = second/60`) for sub/goal/red-card boundaries. The historical `+ 0.5` buffer is gone. Sub boundaries from lineups (`extract_sub_events`) are minute-precision and only used as a fallback when events have no subs (otherwise we'd duplicate near-boundaries).
- **Published `defense` is sign-flipped to positive=good** in `10_export_blog_data.R` for blog consumption; internal model retains negative=good convention. `panna_ratings.parquet` shows `defense` as "defensive value added" (xG suppression per 90).
- **Replacement Level filter at export** — `10_export_blog_data.R` drops `player_id == "replacement"` rows before publishing. The synthetic row is a model artifact (game-state confound, picks up uncontrolled variance from league-season fixed effects), not a coherent player rating.
- **`exists("sample_n")` collides with dplyr** — `dplyr::sample_n()` is exported, so `if (!exists("sample_n")) sample_n <- 500` skips the default and `sample_n` resolves to the dplyr function. Use `exists(x, inherits = FALSE)` (and optionally `is.function(get(x))`) for config guards in pipeline scripts.
- **data.table NSE bare-symbol subsetting fails** — `psr_data[, psr_cols]` throws `j is a single symbol but column name 'psr_cols' is not found` when `psr_data` is a data.table and `psr_cols` is a character vector. Coerce with `as.data.frame()` before subsetting (or use `..psr_cols` / `with = FALSE`).
- **SPADL has no `is_penalty` column** — detect from raw Opta qualifier 9, match to SPADL via composite key (match_id + player_id + minute)
- **SPADL preserves `original_event_id`** — maps SPADL actions back to Opta event IDs (used by equity export for blog chain join)
- **Inline `Rscript -e` segfaults** — always write to `debug/` and run with `Rscript debug/script.R`
- **`setwd()` in Rscript segfaults** — `cd panna` in bash, then `Rscript debug/script.R`
- **data.table NSE shadowing** — function params named after columns (e.g., `player_name`) shadow the column inside `[...]`. Rename param to `target_player` or capture early.
- **Opta lineups have no score columns** — derive match scores by counting goal events
- **`filter_bad_xg_data` threshold** — use 30% for Opta (25% zero-xG splints is normal with SPADL)
- **`library()` not `requireNamespace()`** in parallel workers — PSOCK clusters need `library()` to attach packages to the search path
- **`.get_col()` warns on missing columns** — memoized warnings via `.get_col_warned` env in `utils.R`
- **Own goal xG override** — own goals use model EPV (not xG) because xG at `start_x=3` (near own goal) gives 0.97, which is nonsensical for a deflection
- **Data location debugging — call `data_location_report()` first** — if `load_opta_*()` returns nothing for data you believe is on disk, run `data_location_report()` to see (a) where `opta_data_dir()` resolved to, (b) which consolidated `opta_*.parquet` files exist with row counts per league, (c) any consolidated-vs-per-season inconsistencies. The package has a fall-through so reads usually succeed even when sources disagree, but the report shows you what's actually being read. Three resolution chains can each fail silently: `pannadata_dir()` cwd-walks for `pannadata/data`; `list_opta_seasons('local')` unions per-season dirs + consolidated parquets; `load_opta_table()` prefers consolidated but falls through to per-season on 0-row return.
- **`compute_match_elos()` time decay is opt-in** — pass `time_decay_halflife = N` (days) to scale K by `0.5 ^ ((max_date - match_date) / N)`. Default `NULL` = no decay (legacy behaviour). The v5 Elo optimization treated this as a tunable param and converged near "off" (~6500-day halflife), so it's not the default — but callers wanting recency weighting should set it (~720 days ≈ 0.7 weight at 1 year, matches the FIFA / SPI intuition).

## GitHub Actions

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `R-CMD-check.yaml` | Push to `dev`, PRs to `main` | Package checks |
| `opta-pipeline.yml` | Manual dispatch | Opta RAPM/SPM on GHA, auto-uploads caches |
| `pkgdown.yaml` | Push | Documentation site |
| `predictions-pipeline.yml` | Wed 8 AM UTC / manual / dispatch | Weekly match predictions |
| `psr-weekly-snapshot.yml` | Weekly snapshot / manual | PSR weekly snapshot generation |
| `epv-pipeline.yml` | Manual dispatch | EPV model training pipeline |
| `ratings-pipeline.yml.disabled` | (disabled) | FBref RAPM/SPM — superseded by Opta pipeline |

## Dependencies

**Core** (Imports): cli, data.table, DBI, duckdb, glmnet, httr2, janitor, jsonlite, Matrix, rlang, stringi, tools

**Optional** (Suggests): arrow, xgboost, parallel, piggyback, ggplot2, ggrepel, testthat, and others
