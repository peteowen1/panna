# CLAUDE.md — panna R Package

Player rating system for football using RAPM + SPM methodology. This is
the primary development workspace in the pannaverse ecosystem.

**Active data source: Opta only.** The FBref/Understat archival sweep is
done (2026-06): their R sources, loaders, and the
`player-ratings-fbref/` pipeline were removed from this package. Only
`pannadata` retains disabled scraper code for reference.

## Development Commands

``` r
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

**Working directory**: Always `cd panna` before running R commands, or
use `devtools::load_all("panna")` from pannaverse root.

## Architecture

### R Source Files (R/)

| Module                  | Key Files                                                                                                           | Purpose                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
|-------------------------|---------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| **RAPM**                | `rapm_matrix.R`, `rapm_model.R`, `splint_creation.R`                                                                | Regularized Adjusted Plus-Minus via glmnet                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| **SPM**                 | `spm_model.R`, `spm_opta.R`, `feature_engineering.R`                                                                | Statistical Plus-Minus (XGBoost-based prior for RAPM)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| **Panna Rating**        | `panna_rating.R`, `offensive_defensive.R`                                                                           | Final combined rating = xRAPM with SPM prior                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| **Skills**              | `estimated_skills.R`, `skill_optimization.R`                                                                        | Per-stat skill estimation with Bayesian shrinkage                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| **xMetrics**            | `xg_model.R`, `xgot_model.R`, `xpass_model.R`, `epv_model.R`, `epv_features.R`                                      | xG/xGOT/xA/xPass/EPV from SPADL events                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| **SPADL**               | `spadl_conversion.R`                                                                                                | Opta events -\> SPADL action format                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **Data Loaders**        | `opta_loaders.R`, `dirs.R`, `data_location_report.R`                                                                | Load from local parquet or GitHub Releases; data-source-agnostic dir resolution; load diagnostics                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| **Player Stats**        | `player_stats_opta.R`                                                                                               | Aggregate player-level statistics                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           |
| **Match Prediction**    | `match_prediction.R`, `match_mirror.R`                                                                              | Team-level features + XGBoost match outcome model; orientation-symmetric match rows                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
| **Expected Minutes**    | `expected_minutes.R` (production); `minutes_model.R`, `minutes_model_train.R`, `minutes_query.R` (XGBoost, benched) | National-team minutes projection: decay-weighted heuristic with tournament boost + p_start Beta prior                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
| **Data Processing**     | `data_processing.R`, `possession_chains.R`                                                                          | Transformations, possession chain analysis                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| **Utilities**           | `utils.R`, `constants.R`, `globals.R`, `piggyback.R`                                                                | Helpers, NSE declarations, GitHub Releases I/O                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| **PSR/PSV**             | `psr.R`                                                                                                             | Player Skill Rating (smoothed) + Player Stat Value (per-game) with O/D decomposition                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
| **WPA**                 | `wp_model.R`, `wp_credit.R`                                                                                         | Win probability model (3-class: home/draw/away) and WPA credit assignment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   |
| **EPR**                 | `player_ratings_epv.R`                                                                                              | Expected Points Rating. Legacy [`calculate_epr()`](https://peteowen1.github.io/panna/reference/calculate_epr.md) = decay-weighted Bayesian mean; modern [`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md) = weighted ridge with league-season FE + opp_def_rating control (2026-05-19, the production version used in `data-raw/match-predictions-opta/build_epr_weekly.R` — incremental, run weekly by `epr-weekly-snapshot.yml`; moved out of gitignored `debug/keep/` 2026-06-23 so CI can run it) |
| **Skill Config**        | `skill_config.R`                                                                                                    | Soccer stat rating definitions, position map, hyperparameters                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               |
| **Game Ratings**        | `player_game_ratings.R`                                                                                             | Unified per-game output: EPV + WPA + PSV → panna_value                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| **Career RAPM**         | `career_rapm.R`                                                                                                     | Career-trait Panna: decay-weighted multi-season xRAPM (365d half-life)                                                                                                                                                                                                                                                                                                                                                                                                                                                                                      |
| **WC Simulation**       | `simulate_world_cup.R`, `knockout_model.R`, `shootout.R`                                                            | 48-team WC 2026 tournament simulator; full-model knockout match probabilities; penalty-shootout win probability                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| **Team Ratings**        | `team_rating.R`, `elo_calibration.R`, `league_offsets.R`                                                            | Bradley-Terry team rating; Elo calibration (per-match-type K + cross-confederation multiplier); league quality offsets vs UCL group stage                                                                                                                                                                                                                                                                                                                                                                                                                   |
| **EPV Adjustments**     | `epv_adjustments.R`                                                                                                 | EPV position centering and opponent adjustment                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              |
| **Player IDs**          | `player_id_canonical.R`                                                                                             | Player-ID canonicalization                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
| **Pipeline Validation** | `pipeline_validation.R`                                                                                             | Domain-truth assertions on pipeline outputs                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| **Centrality**          | `centrality.R`                                                                                                      | Network centrality metrics for player influence                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| **Simulation**          | `simulate.R`                                                                                                        | Match simulation engine                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                     |
| **Pitch Plot**          | `pitch_plot.R`                                                                                                      | Football pitch visualization                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                |
| **Attribution**         | `player_attribution.R`                                                                                              | Player contribution attribution                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                             |
| **Weather**             | `weather.R`                                                                                                         | Weather data integration                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    |
| **Comparison**          | `compare_players.R`                                                                                                 | Player comparison utilities                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
| **Package**             | `panna-package.R`                                                                                                   | Package-level roxygen docs                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |

### Pipelines (data-raw/)

Run order matters — later pipelines depend on earlier ones.

| Pipeline             | Directory                 | Entry Point                             | Depends On                                                                                                |
|----------------------|---------------------------|-----------------------------------------|-----------------------------------------------------------------------------------------------------------|
| **EPV/xMetrics**     | `epv/`                    | `03_calculate_player_xmetrics.R`        | Pre-trained models via `pannamodels::load_panna_model()`                                                  |
| **Opta RAPM/SPM** ⭐ | `player-ratings-opta/`    | `run_pipeline_opta.R`                   | xMetrics output                                                                                           |
| **Skills**           | `estimated-skills/`       | `run_skills_pipeline.R`                 | Opta RAPM output (cache-opta/)                                                                            |
| **Predictions**      | `match-predictions-opta/` | `run_predictions_opta.R`                | Opta RAPM + Skills output                                                                                 |
| **Blog Export**      | `match-predictions-opta/` | Steps 10b + 10c (opt-in)                | EPV/WPA/PSV models + match events                                                                         |
| **WC 2026 Sim**      | `match-predictions-opta/` | Step 11 (opt-in) `11_simulate_wc2026.R` | 07_predictions + hand-curated `wc2026_groups.rds`. Outputs BT ratings, champion probs, group expectations |

**Pipeline scripts are numbered** (01, 02, …) and run sequentially
within each pipeline. The `run_*.R` entry points source them in order.
Shared pipeline infrastructure lives in `data-raw/pipeline_utils.R`
(`run_step()`, `check_critical_step()`, `print_pipeline_summary()`,
`clear_cache_files()`).

### Cache Directories (all gitignored)

| Directory                          | Contents                            | Shared With                                     |
|------------------------------------|-------------------------------------|-------------------------------------------------|
| `data-raw/cache/`                  | SPADL conversions, EPV intermediate | EPV pipeline                                    |
| `data-raw/cache-opta/`             | Opta RAPM steps 01-09 output        | Skills pipeline reads `07_seasonal_ratings.rds` |
| `data-raw/cache-skills/`           | Skills pipeline steps 01-08         | Predictions reads `06_seasonal_ratings.rds`     |
| `data-raw/cache-predictions-opta/` | Prediction model intermediates      | Blog data export                                |
| `data-raw/debug/`                  | Temporary debug scripts             | `debug/keep/` for saved scripts                 |

### Value Metrics Architecture

Two-path rating system (mirrors torpverse TORP pattern):

    SPADL actions → EPV credits → per-game EPV → EPR (decay-weighted) ──┐
                         └→ WP model → WPA → per-game WPA               ├→ panna_value (50/50 blend)
    Box-score stats → PSV/OSV/DSV (per-game via glmnet coefficients)     │
    Stat ratings → PSR/OSR/DSR (smoothed skills via glmnet) ────────────┘
         └→ Multi-target RAPM (xG, EPV, WPA, PSV as response variables)
              └→ Match predictions (team-aggregated value metrics)

**Key functions:** -
[`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md)
— Per-game EPV with offensive/defensive decomposition -
[`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md)
/
[`calculate_psv_components()`](https://peteowen1.github.io/panna/reference/calculate_psv_components.md)
— Per-game stat value, `osv + dsv = psv` -
[`create_wp_features()`](https://peteowen1.github.io/panna/reference/create_wp_features.md)
→
[`train_wp_model()`](https://peteowen1.github.io/panna/reference/train_wp_model.md)
→
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md)
— WPA pipeline (draw = 0.5) -
[`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md)
→
[`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md)
— WPA credit split and per-game aggregation -
[`calculate_epr()`](https://peteowen1.github.io/panna/reference/calculate_epr.md)
/
[`calculate_epr_batch()`](https://peteowen1.github.io/panna/reference/calculate_epr_batch.md)
— Legacy: Bayesian-shrunk EPV ratings with decay -
[`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md)
/
[`optimize_epr_decay()`](https://peteowen1.github.io/panna/reference/optimize_epr_decay.md)
— Production: weighted ridge with player + league-season FE +
opp_def_rating control. Inputs require `opp_def_rating` column joined
from `cache-opta/team_season_strength.parquet`. β_player IS the EPR.
Decay parameter near-irrelevant for prediction (chosen 900 days via
held-out MSE) -
[`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md)
— Merges EPV + WPA + PSV → `panna_value` -
[`add_value_metrics_to_splints()`](https://peteowen1.github.io/panna/reference/add_value_metrics_to_splints.md)
— Joins per-game values to RAPM splints for multi-target -
[`fit_spm_opta_target()`](https://peteowen1.github.io/panna/reference/fit_spm_opta_target.md)
— SPM with custom target column (for multi-target RAPM)

**Constants** (`constants.R`): `PANNA_EPR_WEIGHT = 0.5`,
`PANNA_PSR_WEIGHT = 0.5` **EPR defaults** (`player_ratings_epv.R`):
`EPR_DECAY_OFFENSIVE = 400`, `EPR_PRIOR_GAMES = 10.2` **WPA defaults**
(`wp_credit.R`): `WPA_ACTOR_SHARE = 0.5`, `WP_DRAW_VALUE = 0.5`

### Tests (tests/testthat/)

42 test files covering loaders, models, pipelines, scraping, value
metrics. Shared fixtures in `helper-fixtures.R`. Uses testthat edition
3.

## Key Conventions

- **`source = "remote"` default**: All `load_opta_*()` and
  `player_opta_*()` functions default to downloading from GitHub
  Releases. Pass `source = "local"` for local data.
- **Player IDs**: Opta pipeline uses real alphanumeric Opta IDs (e.g.,
  `5ilkkfbsss0bxd6ttdlqg0uz9`) throughout. FBref uses FBref player IDs.
- **Config override pattern**: `run_pipeline_opta.R` uses
  `if (!exists(...))` so test scripts can set variables before sourcing.
- **Tournament seasons**: Use “YYYY Country” format (e.g., “2018
  Russia”), not “YYYY-YYYY”.
- **Season subsetting: ALWAYS select by
  [`extract_season_end_year()`](https://peteowen1.github.io/panna/reference/extract_season_end_year.md),
  never by exact `"YYYY-YYYY"` string match.** Three label formats share
  one end year: “2025-2026” (European), “2026” (calendar-year leagues —
  MLS/Argentina/Brazil), “2026 Canada-Mexico-USA” (tournaments). The
  exact-match-then-fallback-if-empty pattern is a trap: the European
  labels always match, so the fallback never fires and calendar-league
  rows are silently dropped. This exact bug excluded MLS/ARG/BRA from
  every season’s SPM build until 2026-06-12 (`07_seasonal_ratings.R`,
  the blog’s 25% missing-SPM gap).
- **Position taxonomy**: Prefer the 16-role classification
  (GK/CB/LB/RB/LWB/RWB/DM/CM/LM/RM/CAM/LW/RW/CF/LF/RF) over the legacy
  4-bucket GK/DEF/MID/FWD when adding features, aggregations, or display
  labels. Canonical mapper: `classify_role(position, position_side)` in
  `R/minutes_model.R`. Empirical 10+ min spread between roles inside the
  same broad bucket (e.g. CB averages 87 min, LB 86, AM 80, ST-Right
  77). Refactor old broad-bucket usage opportunistically.

## Gotchas

- **Skill-estimator stat-column detection must catch `_per90`, not just
  `_p90`** —
  [`.estimate_prematch_skills_batch()`](https://peteowen1.github.io/panna/reference/dot-estimate_prematch_skills_batch.md)
  (psr.R) auto-detects which columns to estimate via
  `grep("_p90$|_per90$", names(dt))` **plus** a union with
  [`.get_psr_skill_cols()`](https://peteowen1.github.io/panna/reference/dot-get_psr_skill_cols.md)/[`.get_gk_skill_cols()`](https://peteowen1.github.io/panna/reference/dot-get_gk_skill_cols.md).
  The original `_p90$`-only pattern silently skipped every xMetrics
  column (`xg_per90`, `npg_minus_npxg_per90`, `gsaa_per90`, …) — so they
  were listed in the feature set but **never estimated, never got a
  coefficient**. This is why `xg_per90` had been in
  [`.get_psr_skill_cols()`](https://peteowen1.github.io/panna/reference/dot-get_psr_skill_cols.md)
  for ages yet had a 0-row coefficient: xG had never actually trained
  into the value model. When adding a feature, confirm its name matches
  the grep OR is in the registered skill-col lists, and that it appears
  in the trained coefficient CSV after a retrain.
- **PSV/PSR coefficient training (step 7) reads box-only
  `01_match_stats` — it needs the xG join too, on BOTH the outfield and
  GK paths.** `07_train_psr_model.R` reads the box-score
  `cache-skills/01_match_stats.rds` and estimates skills itself; the
  per-match xG join that lives in step 2 does NOT carry over. Without an
  explicit join the coefficients omit
  `xg_per90`/over-performance/`gsaa`. Use the shared
  [`enrich_match_stats_with_xmetrics()`](https://peteowen1.github.io/panna/reference/enrich_match_stats_with_xmetrics.md)
  helper (R/opta_loaders.R) — step 2, step 7 outfield, AND step 7’s
  separate GK extraction (`ms_dt_gk <- readRDS(...)`) each call it, so
  training and skill-ratings see the identical feature set. The helper
  takes `fail_if_missing_frac` (pipeline callers pass 0.6) so a
  total/\>60% bymatch gap STOPS instead of silently training an xG-blind
  model. Source artifact: `xmetrics_bymatch/` from
  `03_calculate_player_xmetrics.R`
  (`aggregate_player_xmetrics(by_match = TRUE)`); load via
  `load_opta_xmetrics(by_match = TRUE)`.
- **[`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md)
  routes keepers through the GK sub-model — like
  [`compute_player_psr()`](https://peteowen1.github.io/panna/reference/compute_player_psr.md).**
  It splits `is_gk` (grepl GK/Goalkeeper on
  `primary_position`/`position`), scores keepers with the `gk_`
  coefficients (which carry `gsaa_per90`) and outfield with the target
  model, then `rbindlist`s. Without the split, keepers are scored as bad
  outfielders (all-negative DSV, no shot-stopping credit). NB the split
  also centers GKs-vs-GKs and outfield-vs-outfield, and **does not
  preserve input row order** (outfield rows then GK rows) — fine for 10b
  which merges by `player_id`, but don’t assume order is kept.
- **PSV `target = "blend"` is the DISPLAYED value model; the RAPM target
  stays xG.** `07` trains a third `blend_*` coefficient set on
  `α·xg_diff + (1−α)·goal_diff` (`psv_blend_alpha`, default 0.6) so the
  blog PSV credits finishing without pure-goal noise; the unprefixed
  (xG) and `gd_` sets are unchanged for the RAPM `psvf90` target / other
  consumers. `load_psr_coefficients(target="blend")` falls back to the
  xG set with a warning if the blend CSVs aren’t present. The export
  (10b) uses
  `target="blend", exclude_efficiency=FALSE, scale_to_minutes=TRUE`.
- **Finishing is represented as xG OVER-PERFORMANCE counts, not
  ratios.** The scale-free finishing ratios (`ibox_goal_rate`,
  `goals_per_shot`, `big_chance_conversion`, `headed_goal_rate`,
  `penalty_conversion`) were REMOVED from
  `efficiency_cols`/`skill_config` (1/1 == 10/10 discards volume) and
  replaced by additive over-performance per-90 features:
  `npg_minus_npxg_per90`, `ibox_g_minus_xg_per90`,
  `obox_g_minus_xg_per90` (zonal: bucket existing per-shot xG by the
  in-box rule `start_x>83 & start_y∈(21,79)` — NO xG-model retrain
  needed) + `placement_added_per90` (xGOT−xG skill). Keeper GSAA
  (`gsaa_per90` = xGOT-faced − goals-conceded, cross-team attribution in
  [`.compute_keeper_gsaa()`](https://peteowen1.github.io/panna/reference/dot-compute_keeper_gsaa.md))
  replaced `save_percentage`. The duel/aerial/tackle accuracy ratios
  (`duel_success`, `aerial_success`, `tackle_success`) were ALSO removed
  from PSR/PSV (panna#116) and replaced by the 5 xDuel above-expected
  counts (`aerial_woe`, `aerial_poss_woe`, `takeon_woe`,
  `tackle_poss_woe`, `containment_woe`, all `_per90`); pass accuracy by
  `xpass_overperformance_per90`. `efficiency_cols` is gone from `psr.R`
  — the PSR/PSV skill set is
  [`.get_psr_skill_cols()`](https://peteowen1.github.io/panna/reference/dot-get_psr_skill_cols.md)/[`.get_gk_skill_cols()`](https://peteowen1.github.io/panna/reference/dot-get_gk_skill_cols.md).
  NB `spm_opta.R`’s `success_cols` STILL uses the ratios (SPM
  modernization deliberately deferred), so check it separately when
  changing features.
- **Box-minute override: `coalesce(na_if(box_minutes, 0), splint)`, not
  `coalesce(box_minutes, splint)`.** In `06_seasonal_skill_ratings.R`,
  some players have anomalous 0-minute box rows in old seasons (Alaba
  2014: 55 rows summing to 0 min) while their splints have real minutes.
  Plain `coalesce` only falls back on NA, so a 0 silently overrides a
  valid splint value and zeroes out a top-50 player. The `total_minutes`
  sanity hard-stop is scoped to RECENT seasons (`>= max−3`) — old-season
  minutes are chronically incomplete (both box and splint capture only
  1–3 matches), a known data limitation, not a regression.
- **[`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)
  is the incremental Opta data sync** (R/piggyback.R) — there’s no
  tarball on `opta-latest` anymore (just individual consolidated
  parquets), so `pb_download_source("opta")` can’t refresh a stale local
  copy.
  [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)
  lists release assets and downloads only those missing or size-changed
  (timestamp check opt-in), verifying each landed at the expected size
  (piggyback’s `pb_list` can include phantom assets `gh` doesn’t have —
  those now report failed, not silently “synced”).
  `pb_download_opta(dry_run=TRUE)` previews.
- **WC expected-minutes tuning lives in `announced_squads.R`** — the
  `WC2026_EM_*` constants (tournament_boost = 5, prob_prior_k = 1,
  tournament_start) feed both the announced and derived squad resolvers.
  Values come from a WC2022 backtest (96 team-matches, game-2+; harness
  in `debug/wc_minutes_test/backtest_wc2022.R`): boost 5 cuts minutes
  MAE 21.0→19.2, XI-hit peaks at boost 3–5 and degrades by 8 (chases
  dead-rubber rotations). Don’t raise `prob_prior_k`: the Beta prior
  monotonically worsens aggregate MAE — k=1 exists only to damp
  single-cap `p_start = 1.00` pathologies.
- **The XGBoost minutes model is benched, deliberately** —
  `minutes_model*.R`/`minutes_query.R` are feature-complete
  (incl. `p_start_decay`, within-tournament accumulators,
  prev-team-match involvement, training/query parity tests) but LOSE to
  the tuned heuristic on the WC2022 holdout (22.4 vs 19.6 MAE, XI-hit
  tie). Don’t wire it into the pipeline without re-running
  `debug/wc_minutes_test/train_eval_xgb.R` and beating the heuristic;
  its top features are the heuristic’s own signals, so it mostly adds
  variance.
- **[`is.na()`](https://rdrr.io/r/base/NA.html) on a data.table
  list-column is FALSE for NULL elements** — after a keyed join with
  `nomatch = NA`, unmatched rows fill list-columns with NULL, so
  `!is.na(dt$list_col)` does NOT detect them (crashed
  [`query_minutes_features()`](https://peteowen1.github.io/panna/reference/query_minutes_features.md)
  for unknown player_ids). Test emptiness:
  `vapply(col, function(v) !is.null(v) && length(v) > 0, logical(1))`.
- **Splint creation uses second precision** —
  [`extract_period_end_times()`](https://peteowen1.github.io/panna/reference/extract_period_end_times.md)
  reads Opta `type_id == 30` markers from raw match events to set exact
  period boundaries;
  [`create_splint_boundaries_fast()`](https://peteowen1.github.io/panna/reference/create_splint_boundaries_fast.md)
  uses `events$minute + events$added_time` (where
  `added_time = second/60`) for sub/goal/red-card boundaries. The
  historical `+ 0.5` buffer is gone. Sub boundaries from lineups
  (`extract_sub_events`) are minute-precision and only used as a
  fallback when events have no subs (otherwise we’d duplicate
  near-boundaries).
- **Published `defense` is sign-flipped to positive=good** in
  `10_export_blog_data.R` for blog consumption; internal model retains
  negative=good convention. `panna_ratings.parquet` shows `defense` as
  “defensive value added” (xG suppression per 90).
- **Replacement Level filter at export** — `10_export_blog_data.R` drops
  `player_id == "replacement"` rows before publishing. The synthetic row
  is a model artifact (game-state confound, picks up uncontrolled
  variance from league-season fixed effects), not a coherent player
  rating.
- **`exists("sample_n")` collides with dplyr** —
  [`dplyr::sample_n()`](https://dplyr.tidyverse.org/reference/sample_n.html)
  is exported, so `if (!exists("sample_n")) sample_n <- 500` skips the
  default and `sample_n` resolves to the dplyr function. Use
  `exists(x, inherits = FALSE)` (and optionally `is.function(get(x))`)
  for config guards in pipeline scripts.
- **data.table NSE bare-symbol subsetting fails** —
  `psr_data[, psr_cols]` throws
  `j is a single symbol but column name 'psr_cols' is not found` when
  `psr_data` is a data.table and `psr_cols` is a character vector.
  Coerce with
  [`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) before
  subsetting (or use `..psr_cols` / `with = FALSE`).
- **SPADL has no `is_penalty` column** — detect from raw Opta qualifier
  9, match to SPADL via composite key (match_id + player_id + minute)
- **Opta shot type_ids: 13=Miss, 14=Post, 15=Attempt Saved, 16=Goal**
  (per `OPTA_TYPE_NAMES` / DATA_DICTIONARY, confirmed vs goal-mouth
  data). On-target = `c(15L, 16L)`. These were **swapped (13↔︎15)** in
  several places until 2026-06 — the swap mislabelled `shots_on_target`
  (counted misses) and mis-gated GK save credit in
  [`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md)
  (credited misses, not saves). xGOT/placement (`xgot_model.R`) and
  [`aggregate_player_xmetrics()`](https://peteowen1.github.io/panna/reference/aggregate_player_xmetrics.md)’s
  decomposition depend on this being correct.
- **SPADL preserves `original_event_id`** — maps SPADL actions back to
  Opta event IDs (used by equity export for blog chain join)
- **Inline `Rscript -e` segfaults** — always write to `debug/` and run
  with `Rscript debug/script.R`
- **[`setwd()`](https://rdrr.io/r/base/getwd.html) in Rscript
  segfaults** — `cd panna` in bash, then `Rscript debug/script.R`
- **data.table NSE shadowing** — function params named after columns
  (e.g., `player_name`) shadow the column inside `[...]`. Rename param
  to `target_player` or capture early.
- **Opta lineups have no score columns** — derive match scores by
  counting goal events
- **`filter_bad_xg_data` threshold** — use 30% for Opta (25% zero-xG
  splints is normal with SPADL)
- **[`library()`](https://rdrr.io/r/base/library.html) not
  [`requireNamespace()`](https://rdrr.io/r/base/ns-load.html)** in
  parallel workers — PSOCK clusters need
  [`library()`](https://rdrr.io/r/base/library.html) to attach packages
  to the search path
- **[`.get_col()`](https://peteowen1.github.io/panna/reference/dot-get_col.md)
  warns on missing columns** — memoized warnings via `.get_col_warned`
  env in `utils.R`
- **Own goal xG override** — own goals use model EPV (not xG) because xG
  at `start_x=3` (near own goal) gives 0.97, which is nonsensical for a
  deflection
- **Data location debugging — call
  [`data_location_report()`](https://peteowen1.github.io/panna/reference/data_location_report.md)
  first** — if `load_opta_*()` returns nothing for data you believe is
  on disk, run
  [`data_location_report()`](https://peteowen1.github.io/panna/reference/data_location_report.md)
  to see (a) where
  [`opta_data_dir()`](https://peteowen1.github.io/panna/reference/opta_data_dir.md)
  resolved to, (b) which consolidated `opta_*.parquet` files exist with
  row counts per league, (c) any consolidated-vs-per-season
  inconsistencies. The package has a fall-through so reads usually
  succeed even when sources disagree, but the report shows you what’s
  actually being read. Three resolution chains can each fail silently:
  [`pannadata_dir()`](https://peteowen1.github.io/panna/reference/pannadata_dir.md)
  cwd-walks for `pannadata/data`; `list_opta_seasons('local')` unions
  per-season dirs + consolidated parquets;
  [`load_opta_table()`](https://peteowen1.github.io/panna/reference/load_opta_table.md)
  prefers consolidated but falls through to per-season on 0-row return.
- **[`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md)
  time decay is opt-in** — pass `time_decay_halflife = N` (days) to
  scale K by `0.5 ^ ((max_date - match_date) / N)`. Default `NULL` = no
  decay (legacy behaviour). The v5 Elo optimization treated this as a
  tunable param and converged near “off” (~6500-day halflife), so it’s
  not the default — but callers wanting recency weighting should set it
  (~720 days ≈ 0.7 weight at 1 year, matches the FIFA / SPI intuition).
- **WP model is possession-team POV, not home POV** — since retraining
  2026-05-19 the WP model predicts `P(team_in_possession wins)`.
  [`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md)
  is torp-style:
  `wpa = fcase(team_id_next == team_id, wp_next - wp, default = (1 - wp_next) - wp)`.
  **Never** consume `wp` as a fixed-POV (home) probability and subtract
  neighbouring rows — possession changes flip POV and the delta gets
  ~30× inflated. The retro of this exact bug is at
  `panna/CLAUDE_TODO_WPA_SCALE_REGRESSION.md`. Sanity bounds: per-event
  \|WPA\| ≤ 0.05 typically, per-match max ~0.5-1.0 (goal-causing events
  in close games), per-season top players ±5-10.
- **Pipeline-script skip signal is a typed condition, not a magic
  string** — 10b/10c export scripts use `skip_league_cond("reason")`
  defined inline; outer
  `tryCatch(..., panna_skip_league = handler, error = ...)` dispatches
  on class. If you add a new league-iterating step, mirror that pattern
  (and the `.required_*_cols` + `validate_*_schema()` helper) rather
  than `stop("__skip_xxx__")` + `if (identical(e$message, ...))`. Class
  dispatch is robust to message drift.
- **Step 10b auto-refreshes stale LOCAL events before its coverage
  guard** — the pre-flight
  [`assert_events_coverage()`](https://peteowen1.github.io/panna/reference/assert_events_coverage.md)
  runs with `source = "local"`, but the step’s actual event loads
  default to `source = "remote"` (fresh from `opta-latest`). On a dev
  box the local `events_consolidated/` copy lags the daily cloud scrape,
  which used to abort the whole step. 10b now probes coverage warn-only
  (`abort_threshold = Inf`), pulls fresh per-league events (+ the
  `opta_fixtures.parquet` / `opta_player_stats.parquet` /
  `event_less_match_ids.parquet` singles) for any `partial_gap` league
  via an inline `.refresh_local_events()`, then re-runs the hard guard —
  so it aborts ONLY when the **cloud itself** is short (a real upstream
  gap → fix with `pannadata`’s `rebuild-events.yml`, not retried
  locally). Default on; set `auto_refresh_stale_events <- FALSE` before
  sourcing for a deliberately offline run. `source_missing` leagues (0
  local events, lazy-loaded remotely on a fresh GHA runner) are NOT
  refreshed — only genuine partial gaps.
- **[`check_events_coverage()`](https://peteowen1.github.io/panna/reference/check_events_coverage.md)
  gap is measured against EXPECTED events, not played fixtures** —
  `gap = |player_stats_match_ids − event_less_registry − event_ids|`.
  Two classes of match that can never have events are excluded from the
  denominator: (a) played fixtures Opta has no data for at all (absent
  from `opta_player_stats.parquet`), and (b) matches Opta has stats but
  no event feed for — the **event-less registry**
  (`event_less_match_ids.parquet` on `opta-latest`, written by
  pannadata’s `rebuild_events.py`, loaded via internal
  [`load_opta_eventless_ids()`](https://peteowen1.github.io/panna/reference/load_opta_eventless_ids.md)).
  This was added 2026-06-03 because UEL/UECL played fixtures include
  qualifier rounds Opta only box-scores: a naive played-fixtures gate
  flagged UEL 217/239 (gap 22) and UECL 163/261 (gap 98) as
  unsatisfiable failures. With the registry, both → gap 0. The gate
  still catches a genuine domestic shortfall (the Championship 265/557
  case) because those matches DO have events and aren’t event-less.
  Registry absent (pre-first-rebuild) ⇒ falls back to the stricter
  all-player_stats denominator.
- **When patching a hardcoded path in one pipeline script, grep for the
  same path across sister scripts** — pipeline steps that share inputs
  often duplicate the path expression instead of factoring it.
  2026-05-29 incident: fixed
  `readRDS(file.path(cache_dir, "wc2026_groups.rds"))` in
  `11_simulate_wc2026.R` with a
  [`system.file()`](https://rdrr.io/r/base/system.file.html) fallback
  for the GHA runner, but `12_export_wc2026_blog.R` had the exact same
  line and was missed. Step 11 passed, step 12 failed with the identical
  error one CI cycle later. Before pushing a fix that touches a shared
  input path, `grep -rn "wc2026_groups.rds" data-raw/` (or whatever the
  path is) and patch all callers in the same commit.
- **`panna` = the career TRAIT, not season xRAPM.** Since 2026-06-09
  `panna` means the career-trait rating (decay-weighted multi-season
  xRAPM, point-in-time “best guess of next game”) from
  `career_panna.parquet` (`estimated-skills/09_career_panna.R` →
  `fit_career_rapm`). Single-season `xrapm` is a season aggregate
  (stat-ish), a DIFFERENT quantity. Legacy code still relabels season
  xRAPM as `panna` in places — `08_panna_ratings.R:36` (`panna = xrapm`)
  and the model’s feature pipeline (`02_player_ratings_to_team.R:77`, so
  the model feature `home_sum_panna` is really aggregated season xRAPM /
  live SPM, not the career trait). When something says “panna,” check
  whether it’s the trait (`career_panna.parquet`) or a season-xRAPM
  relabel.
- **Two different rating estimators: played-side vs upcoming-fixture
  (model) vs display.** `02_player_ratings_to_team.R` builds the model’s
  team features via *different* sources by row: PLAYED matches use the
  seasonal `ratings` table (season xRAPM relabeled `panna`,
  league-centered seasonal PSR); UPCOMING fixtures use a date-specific
  path (~line 560): `panna = offense_spm − defense_spm` (live **SPM**)
  and `compute_player_psr(live_skills, center=TRUE)`
  (WC-population-centered). So the model’s `panna` feature is neither
  the career trait nor consistent train-vs-serve. Fine for the XGBoost
  match model (consumes home−away diffs, so per-batch shifts cancel),
  but do NOT reuse the match-dataset `home_sum_*` features as a
  *display* rating. The WC2026 team-strength export
  (`12_export_wc2026_blog.R` §5) instead aggregates the displayed squad
  **traits** — career-trait `panna` + league-centered seasonal PSR +
  weekly EPR — minutes-weighted, so team == Σ(players shown). See
  METRICS.md §14.
- **`fit_career_rapm(reference_date=D)` does NOT filter splints by date
  — it only decays.** It uses D for recency weighting only
  (`age = D − match_date`, `decay = 0.5^(age/halflife)`). A match AFTER
  D gets negative age → `decay = 0.5^(negative) > 1` → **up-weighted,
  not excluded**. For a leak-free as-of-date fit you MUST filter
  `splint_data$splints` (and `$match_info`) to `match_date <= D` BEFORE
  calling it. Verified: as-of-2020 (filtered) returns 2020-era stars (De
  Bruyne/Salah/Müller); unfiltered would leak 2021–2026 results.
  (Residual: the `skill_spm` shrinkage prior is still as-of-now — a
  second-order leak via the prior only.) Bonus tuning result: across
  as-of-date fits, `cv.glmnet` lambda.min ≈ **16.67·n_obs^−0.58 (≈
  1/√n_obs, R²=0.96)** — so an as-of-date pipeline can skip CV and
  compute λ from the observation count, adapting to each window’s sample
  size (`fit_rapm`/`fit_rapm_with_prior` take a `fixed_lambda`;
  `fit_career_rapm` takes a `lambda_formula(n_obs)` callback). NB
  validated against *pruned* (\>8yr dropped) fits this formula
  over-predicts λ ~30% in the recent high-`n` regime (2024–26) while
  staying good for unpruned — kept it anyway: over-reg is conservative
  (shrinks toward the skill prior) and immaterial (flat ridge optimum +
  wash gate). The monthly as-of build
  (`estimated-skills/09b_career_panna_asof.R`) logs `n_obs`+λ per
  snapshot to track.
- **`calculate_psr(center=TRUE)` centers over the INPUT population, not
  the league.** `psr.R:604` subtracts `mean(psr_raw)` of whatever rows
  it’s handed — the variable is *named* `league_mean` but it’s just
  `mean(input)`. The canonical exports (seasonal `06`, weekly `08b`,
  game-logs `10b`) pass the full league, so they’re genuinely
  league-centered (this is what ITG/the blog displays). But step 02’s
  upcoming-fixture path pre-filters `match_stats` to only the upcoming
  players (~line 486), so its PSR is centered over that subset — for
  WC2026, “above the average World Cup player,” which collapses team PSR
  toward 0 for all but the deepest squads. Model-safe (diffs cancel the
  constant, no retrain needed); flagged `TODO(psr-centering)`. If you
  ever consume fixture-side PSR directly (not as a diff), re-center over
  the league first.
- **Cross-league league offsets use the same-season co-occurrence
  NETWORK, per metric, via
  [`build_league_network()`](https://peteowen1.github.io/panna/reference/build_league_network.md)
  (league_offsets.R).** PSR is a dot-product of box-score skill *rates*
  × glmnet betas; those rates barely vary by league, so a strong player
  in a weak league posts an inflated PSR and tops the sortable column
  (the 2026-06 “unknown WC players \#1 in PSR” report). The fix is **one
  metric-agnostic estimator**: regress per-(player, season, competition)
  per-90 value on league dummies **+ a player-season fixed effect**
  (implemented as within-player-season demeaning), Big-5 mean anchored
  to 0, small-N shrunk (`offset = -strength · n/(n+shrink_k)`). The FE
  uses every same-season pairing a player straddles (domestic +
  continental UCL/UEL + international WC/Euro/Copa) — **all bridges at
  once**, not just-vs-UCL like
  [`compute_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_league_offsets.md).
  **Each metric league-adjusts with its OWN signal so there’s no
  cross-scale rescaling:** PSR ←
  `build_league_network(game_logs, "psv")` (wrapped by
  [`compute_psr_league_offsets()`](https://peteowen1.github.io/panna/reference/compute_psr_league_offsets.md),
  which maps game-log codes→display names via `to_opta_league`); EPR ←
  the EPV network; **Panna needs none** (RAPM controls opponents
  in-regression). Applied at **full strength** (PSV is PSR’s own
  per-game analogue → same units → no 0.4/0.52 scaling, no re-centering,
  no cap), **display-only**, in step `06` (seasonal — loads
  `cache-predictions-opta/game_logs_*.parquet`, saves
  `cache-skills/psr_league_offsets.parquet`+csv) and step `08b` (weekly
  — *reads* that parquet; 06 must run first; 08b attaches each player’s
  as-of-date primary league).
  [`apply_psr_league_offsets()`](https://peteowen1.github.io/panna/reference/apply_psr_league_offsets.md)
  adds the offset and splits across osr/dsr to keep `osr+dsr=psr`. The
  RAPM `psvf90` target and match-prediction `home_sum_psr` features are
  UNTOUCHED. **Why the network (vs the earlier single-anchor /
  transfer-graph attempts):** (1) Elo/opponent-adjustment is dead — the
  prediction Elo has 0% coverage for the isolated leagues
  (A_League/Brazilian/Belgian/CAF), so `opp_factor=1`; (2) UCL-only
  `compute_league_offsets` gives A-League a noisy 7-player chained
  estimate AND mis-signs flat leagues (PSV had Turkey at +0.006,
  wrong); (3) the network connects A-League through the World-Cup web
  (A-League players who played the WC), lifting its bridge count and
  **fixing Turkey’s sign on its own** (network PSV TUR −0.019, correct).
  cor(network PSV, network EPV) = 0.91. Result: A-League ≈ −0.107 (PSV
  scale, n=20, alone at the bottom), Big-5 ≈ 0, Mbappé \#1, Osimhen not
  boosted. NB the **game-log/EPV pipeline must COVER the same leagues as
  skills/PSR** — A_League/Brazilian/Belgian/CAF were added to `10b`’s
  `blog_leagues` 2026-06-22 (events existed, just unprocessed) so they
  get EPR + PSV-network coverage; `10b` has a `merge_subset_leagues`
  mode for that backfill (CAFCL needs `events_coverage_abort_threshold`
  raised — its qualifiers are event-less).
- **An offset-ONLY change must NOT trigger a full weekly-PSR rebuild —
  use a strip-and-re-add MIGRATION.** PSR (and EPR) end-add a
  decay-weighted league offset; the raw skills are unchanged when only
  the offset moves. A full 08b rebuild re-estimates ~28k players’ skills
  at ~234 dates (~89s/date ≈ 4–6h) — pure waste. Because the offset is
  ADDITIVE, migrate the EXISTING released `opta_psr_weekly.parquet`
  instead: **strip the currently-applied offset, add the new one** (both
  recomputed from the same `match_stats` + `psr_offsets`, so the strip
  is exact — seconds not hours). `_run_psr_blend_migration.R`
  (2026-06-23) is the worked example: it strips the old primary-league
  offset and adds the decay-blend, and **self-checks that stripping
  re-inflates a known weak-league name (J. Randall/A-League → rank ~8)
  before re-applying** — aborting if the live parquet isn’t in the
  offset state you assumed (this verification is the whole ballgame for
  strip-add; don’t trust memory). After a migration, 08b’s weekly
  incremental stays consistent. (The earlier primary-league
  `_run_psr_offset_fastpath.R` was removed 2026-06-23 — superseded by
  the decay-blend + this migration.) NB 08b’s incremental download from
  `opta-latest` once did a FULL rebuild even without
  `PSR_FORCE_FULL_REBUILD` (didn’t collapse to recent dates) — worth
  investigating if a weekly run is slow.
- **Cross-league offsets are END-ADD + DECAY-WEIGHTED BLEND for BOTH PSR
  and EPR** (2026-06-23). Each game contributes its league’s network
  offset, weighted by the metric’s OWN decay (EPR 900d; PSR
  `decay_params$rate` ≈231d), end-added OUTSIDE the regression so the
  ridge can’t wash it out, blended across leagues so mid-season movers
  converge correctly (single-league players unchanged). A flat
  (non-quality-dependent) offset is mover-validated correct — see
  `[[reference_league_offset_methodology]]`. **Future polish (low
  priority, tracked):** (a) rescale offsets ~1.1–1.2× — the mover gap
  coefficient was \>1 (1.10 EPV / 1.20 PSV), so the network mildly
  under-estimates the true gap; (b) low-minutes leaderboard noise
  (e.g. a 2.4-nineties player in the top-5) is a SEPARATE minutes-filter
  issue at display/export, NOT a league-offset problem.
- **[`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md)
  (no `path`) prefers the PUBLISHED model, not the same-run cache.** Its
  resolution order is explicit `path` →
  `pannamodels::load_panna_model("epv_model")` → local pannadata
  fallback — none of which is `01_train_epv_models.R`’s output at
  `data-raw/cache/epv/epv_model.rds`. So `05_train_wp_model.R` (which
  derives the WP `epv` feature via
  [`calculate_action_epv()`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md))
  was training against the *previously published* EPV model, not the one
  just trained in the same pipeline run — a silent EPV-version skew
  between the WP `epv` feature and the shipped EPV model. Fixed
  2026-06-18 by passing `load_epv_model(path = "data-raw/cache/epv")`
  (it falls through to pannamodels if the cache file is absent, so
  non-pipeline callers are unaffected). When wiring any new step that
  consumes the EPV model inside the EPV/predictions pipeline, pass the
  cache path explicitly.
- **Game-logs rebuilds MUST set `epv_model_override` +
  `wp_model_override` — see `MODELS.md`.** The bare default loaders
  ([`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md)/[`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md)
  with no path) return the OLD pre-overhaul models from
  `pannadata/data/opta/models/*.rds`, NOT the post-overhaul clean
  models. A standalone 11-season game-logs rebuild that omitted the
  overrides shipped inflated EPV (Messi 3.65 vs correct 2.49;
  match-total EPV ~20 vs ~3-4; every player positive, no negatives) —
  the clean models are
  `data-raw/cache/epv/epv_model_xg_clean_full.rds` +
  `data-raw/cache/epv/wp_final_d2repl_reg/wp_model.rds` (the exact set
  `_run_gamelogs_gt.R` uses — that script is the canonical recipe).
  Loaders now print the resolved file + **modification date** and warn
  if \>14 days old (`.report_model_provenance`) so a stale fallback is
  visible in the log — but the real fix is to pin via overrides.
  `MODELS.md` (panna root) is the source-of-truth for every model
  loader, its silent fallback chain, and the canonical version.
- **EPV offensive/defensive split is presentational —
  `offensive + defensive == epv_total` ALWAYS.** Re-bucketing an action
  type between OFF/DEF never changes a player’s headline
  `epv_total`/`epv_total_adj` or ranking. The split (in
  `aggregate_player_game_epv`): OFF = passing + shooting + dribbling +
  attacking-third aerials (`start_x>67`) + receiver; DEF = defending
  (tackle/interception/clearance/recovery/save/foul/dispossessed) +
  **keeper handling** (pickup/claim/punch) + mid/defensive-third
  aerials + duel_blame. Keeper handling and defensive headers are
  DEFENSIVE (they end opponent attacks); only the per-action-type
  display components (EPV PASS/SHOT/…/GK) are raw totals.
  `epv_total_adj` (EPV ADJ) = position-centered total + opponent adj;
  OFF/DEF adj are position-centered only (opp adj folded into total
  only, exposed as `opp_adj`).
- **EPV feature-contract changes require a model+code lockstep** —
  `EPV_SIMPLE_FEATURE_COLS` (the EPV model’s input contract) is shared
  between the *published* model (trained on exactly those columns) and
  the package code that *emits* them (`create_epv_features_simple`, plus
  `create_wp_features` for the WP `epv`-interacted features). If they
  drift, the pipeline scores with mismatched features and silently
  produces garbage. So when you change the contract, the published model
  (pannamodels `epv` release + the worker’s R2 JSON) and the merged
  package code must move **together** — publish the retrained model and
  merge the code back-to-back. 2026-06-19 clean-EPV/d2-repl-WP swap is
  the worked example: publish-first breaks the WP path (new WP needs
  interacted features only new code emits); merge-first breaks the EPV
  path (old published model wants 17 features new code stops emitting).
  The window is safe only because the pipeline runs Wed/manual. Local
  pipeline runs that must NOT wait for the published model can pass
  `epv_model_override` / `wp_model_override` (10b/10c) to score with a
  candidate `.rds` directly — see the
  [`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md)
  published-vs-cache gotcha above and the pannamodels cache-staleness
  gotcha.

## GitHub Actions

| Workflow                   | Trigger                                                 | Purpose                                                                                                                                                                                                                                                                                                  |
|----------------------------|---------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `R-CMD-check.yaml`         | Push to `dev`, PRs to `main`                            | Package checks                                                                                                                                                                                                                                                                                           |
| `opta-pipeline.yml`        | Manual dispatch                                         | Opta RAPM/SPM on GHA, auto-uploads caches. ⚠ OOMs the 16GB hosted runner since the 2026-06 4-league expansion (panna#87) — run the pipeline locally (needs ~25GB+ RAM) until fixed; the scrape-complete auto-trigger was removed for this reason                                                         |
| `pkgdown.yaml`             | Push                                                    | Documentation site                                                                                                                                                                                                                                                                                       |
| `predictions-pipeline.yml` | Wed 8 AM UTC / manual / `opta-scrape-complete` dispatch | Weekly match predictions. Runs steps 1-10c + 11 (WC2026 sim) + 12 (WC2026 blog export). Triggers `predictions-complete` repository_dispatch on `pannadata` to refresh blog data. Note: WC2026 sim defaults to FALSE in `run_predictions_opta.R` but the workflow enables it in its `run_steps` override. |
| `psr-weekly-snapshot.yml`  | Weekly snapshot / manual                                | PSR weekly snapshot generation                                                                                                                                                                                                                                                                           |
| `epv-pipeline.yml`         | Manual dispatch                                         | EPV model training pipeline                                                                                                                                                                                                                                                                              |

## Dependencies

**Core** (Imports): cli, data.table, DBI, duckdb, glmnet, httr2,
janitor, jsonlite, Matrix, rlang, stringi, tools

**Optional** (Suggests): arrow, xgboost, parallel, piggyback, ggplot2,
ggrepel, testthat, and others
