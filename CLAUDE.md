# CLAUDE.md — panna R Package

Player rating system for football using RAPM + SPM methodology. This is the primary development workspace in the pannaverse ecosystem.

**Active data source: Opta only.** The FBref/Understat archival sweep is complete (2026-06 code removal; 2026-07-14 scraper scripts deleted). Their R sources, loaders, and the `player-ratings-fbref/` pipeline were removed from this package, and the disabled scraper code in `pannadata/scripts/{fbref,understat}/` was also removed — git history is the archive.

## Development Commands

Standard `devtools::load_all()` / `check()` / `document()` workflow, plus `devtools::test(filter = "rapm")` to target a single test file and `pkgdown::build_site()` for the docs site.

**Working directory**: Always `cd panna` before running R commands, or use `devtools::load_all("panna")` from pannaverse root.

## Architecture

### Pipelines (data-raw/)

Run order matters — later pipelines depend on earlier ones.

| Pipeline | Directory | Entry Point | Depends On |
|----------|-----------|-------------|------------|
| **EPV/xMetrics** | `epv/` | `03_calculate_player_xmetrics.R` | Pre-trained models via `pannamodels::load_panna_model()` |
| **Opta RAPM/SPM** ⭐ | `player-ratings-opta/` | `run_pipeline_opta.R` | xMetrics output |
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
                     └→ WP model → WPA → per-game WPA               ├→ piero_value (50/50 blend)
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
- `build_player_game_ratings()` — Merges EPV + WPA + PSV → `piero_value`
- `add_value_metrics_to_splints()` — Joins per-game values to RAPM splints for multi-target
- `fit_spm_opta_target()` — SPM with custom target column (for multi-target RAPM)

**Constants** (`constants.R`): `PANNA_EPR_WEIGHT = 0.5`, `PANNA_PSR_WEIGHT = 0.5`
**EPR defaults** (`player_ratings_epv.R`): `EPR_DECAY_OFFENSIVE = 400`, `EPR_PRIOR_GAMES = 10.2`
**WPA defaults** (`wp_credit.R`): `WPA_ACTOR_SHARE = 0.5`, `WP_DRAW_VALUE = 0.5`

## Key Conventions

- **`source = "remote"` default**: All `load_opta_*()` and `player_opta_*()` functions default to downloading from GitHub Releases. Pass `source = "local"` for local data.
- **Player IDs**: Opta pipeline uses real alphanumeric Opta IDs (e.g., `5ilkkfbsss0bxd6ttdlqg0uz9`) throughout. FBref uses FBref player IDs.
- **Config override pattern**: `run_pipeline_opta.R` uses `if (!exists(...))` so test scripts can set variables before sourcing.
- **Tournament seasons**: Use "YYYY Country" format (e.g., "2018 Russia"), not "YYYY-YYYY".
- **Season subsetting: ALWAYS select by `extract_season_end_year()`, never by exact `"YYYY-YYYY"` string match.** Three label formats share one end year: "2025-2026" (European), "2026" (calendar-year leagues — MLS/Argentina/Brazil), "2026 Canada-Mexico-USA" (tournaments). The exact-match-then-fallback-if-empty pattern is a trap: the European labels always match, so the fallback never fires and calendar-league rows are silently dropped. This exact bug excluded MLS/ARG/BRA from every season's SPM build until 2026-06-12 (`07_seasonal_ratings.R`, the blog's 25% missing-SPM gap).
- **Position taxonomy**: Prefer the 16-role classification (GK/CB/LB/RB/LWB/RWB/DM/CM/LM/RM/CAM/LW/RW/CF/LF/RF) over the legacy 4-bucket GK/DEF/MID/FWD when adding features, aggregations, or display labels. Canonical mapper: `classify_role(position, position_side)` in `R/minutes_model.R`. Empirical 10+ min spread between roles inside the same broad bucket (e.g. CB averages 87 min, LB 86, AM 80, ST-Right 77). Refactor old broad-bucket usage opportunistically.

## Gotchas

- **WC expected-minutes tuning lives in `announced_squads.R`** — the `WC2026_EM_*` constants (tournament_boost = 5, prob_prior_k = 1, tournament_start) feed both the announced and derived squad resolvers. Values come from a WC2022 backtest (96 team-matches, game-2+; harness in `debug/wc_minutes_test/backtest_wc2022.R`): boost 5 cuts minutes MAE 21.0→19.2, XI-hit peaks at boost 3–5 and degrades by 8 (chases dead-rubber rotations). Don't raise `prob_prior_k`: the Beta prior monotonically worsens aggregate MAE — k=1 exists only to damp single-cap `p_start = 1.00` pathologies.
- **The XGBoost minutes model is benched, deliberately** — `minutes_model*.R`/`minutes_query.R` are feature-complete (incl. `p_start_decay`, within-tournament accumulators, prev-team-match involvement, training/query parity tests) but LOSE to the tuned heuristic on the WC2022 holdout (22.4 vs 19.6 MAE, XI-hit tie). Don't wire it into the pipeline without re-running `debug/wc_minutes_test/train_eval_xgb.R` and beating the heuristic; its top features are the heuristic's own signals, so it mostly adds variance.
- **NAMING: `panna` IS decayed xRAPM — nothing else may use the name** (Pete, 2026-09-03). The career-trait rating from `fit_career_rapm()` (halflife 365d, skill-SPM prior, `career_panna.parquet`). **`piero` is the weighted average of EPR + PSR + Panna** — the three decayed ratings. Four places currently relabel a non-decayed xRAPM as `panna` and must be renamed: `08_panna_ratings.R:36` (career/pooled), `estimated-skills/05_skill_panna_ratings.R:37` (skill), `10_export_blog_data.R:187` (**season xRAPM, published to the blog as "panna"** — the urgent one), and `02_player_ratings_to_team.R:77` (model feature `home_sum_panna`; note that file already half-migrates it at lines 191-223). Full audit + suggested names: `pannaverse/docs/reference/RATING-TIME-AGGREGATIONS.md`.
- **CONVENTION: positive = good, everywhere** (Pete, 2026-09-03). Not yet true of the code — RAPM/xRAPM `defense` and `team_season_strength.parquet`'s `def_rating` are still **negative=good** internally and flipped at 5 export sites (`09_export_ratings.R:98`, `10_export_blog_data.R:97` and `:197`, `12d_export_domestic_team_strength.R:578`, `12_export_wc2026_blog.R:338`). EPV/PSV/WPA are already positive=good. **Do not "just delete the minus signs":** SPM features are sign-*constrained* in the negative-good convention (`spm_opta.R:1079`, `spm_model.R:239`, `03_skill_spm.R:255`), and `def_rating` is stored on disk in the old sign, so a partial flip silently inverts ratings while diagnostics still look sane. Migration plan: `pannaverse/docs/plans/SIGN-CONVENTION-POSITIVE-IS-GOOD.md`. `panna_ratings.parquet` shows `defense` as "defensive value added" (xG suppression per 90).
- **Replacement Level filter at export** — `10_export_blog_data.R` drops `player_id == "replacement"` rows before publishing. The synthetic row is a model artifact (game-state confound, picks up uncontrolled variance from league-season fixed effects), not a coherent player rating.
- **SPADL's `bodypart` is a stub: every shot says "foot".** `map_opta_bodypart()` (`spadl_conversion.R`) only ever sets "head" for aerials (type 44) and "other" for keeper actions; its qualifier refinement was never written and the sole caller passes `qualifiers = NULL`. Shots are types 13/14/15/16, so **100% of shots come through as "foot"** — measured 9,782/9,782 on ENG 2015-2016 against 15.7% real headers. Anything deriving `is_header`/`is_right_foot`/`is_left_foot` from SPADL gets three constant-zero features; the xG model is trained on Opta's real `body_part` (RightFoot/LeftFoot/Head), so this was a pure train/serve skew worth **+6.30% on total xG**. Join `body_part` from `opta_shot_events` on `(match_id, original_event_id)` instead — `add_xg_to_spadl(shot_lookup = )` and `add_xgot_to_spadl()` both do. (An earlier version of this file claimed SPADL carried "head"/"foot_left"/"foot_right"; it never has.)
- **`.get_col()` warns on missing columns** — memoized warnings via `.get_col_warned` env in `utils.R`
- **`compute_match_elos()` time decay is opt-in** — pass `time_decay_halflife = N` (days) to scale K by `0.5 ^ ((max_date - match_date) / N)`. Default `NULL` = no decay (legacy behaviour). The v5 Elo optimization treated this as a tunable param and converged near "off" (~6500-day halflife), so it's not the default — but callers wanting recency weighting should set it (~720 days ≈ 0.7 weight at 1 year, matches the FIFA / SPI intuition).
- **WP model is possession-team POV, not home POV** — since retraining 2026-05-19 the WP model predicts `P(team_in_possession wins)`. `add_wp_vars()` is torp-style: `wpa = fcase(team_id_next == team_id, wp_next - wp, default = (1 - wp_next) - wp)`. **Never** consume `wp` as a fixed-POV (home) probability and subtract neighbouring rows — possession changes flip POV and the delta gets ~30× inflated. The retro of this exact bug is at `pannaverse/docs/backlog/CLAUDE_TODO_WPA_SCALE_REGRESSION.md` (moved from this repo's root — verse docs live one level up, see pannaverse `CLAUDE.md`). Sanity bounds: per-event |WPA| ≤ 0.05 typically, per-match max ~0.5-1.0 (goal-causing events in close games), per-season top players ±5-10.
- **Pipeline-script skip signal is a typed condition, not a magic string** — 10b/10c export scripts use `skip_league_cond("reason")` defined inline; outer `tryCatch(..., panna_skip_league = handler, error = ...)` dispatches on class. If you add a new league-iterating step, mirror that pattern (and the `.required_*_cols` + `validate_*_schema()` helper) rather than `stop("__skip_xxx__")` + `if (identical(e$message, ...))`. Class dispatch is robust to message drift.

## GitHub Actions

| Workflow | Trigger | Purpose |
|----------|---------|---------|
| `R-CMD-check.yaml` | Push to `dev`, PRs to `main` | Package checks |
| `opta-pipeline.yml` | Manual dispatch | Opta RAPM/SPM on GHA, auto-uploads caches. Fixed 2026-07-16 (panna#109): step 7 (`07_seasonal_ratings.R`) narrows `opta_stats`/`opta_xmetrics` to their read-list before the 14-season loop, keeping RSS flat (~14.1GB) instead of creeping to 15.9GB and OOMing at season 9/14 — verified green end-to-end via `-f start_step=7` resume-mode run (all 14 seasons, 12.1 min). The `opta-scrape-complete` auto-trigger stays removed (Pete's call, not re-enabled by this fix) — dispatch manually |
| `pkgdown.yaml` | Push | Documentation site |
| `predictions-pipeline.yml` | Wed 8 AM UTC / manual / `opta-scrape-complete` dispatch | Weekly match predictions. Runs steps 1-10c + 11 (WC2026 sim) + 12 (WC2026 blog export). Triggers `predictions-complete` repository_dispatch on `pannadata` to refresh blog data. Note: WC2026 sim defaults to FALSE in `run_predictions_opta.R` but the workflow enables it in its `run_steps` override. |
| `psr-weekly-snapshot.yml` | Weekly snapshot / manual | PSR weekly snapshot generation |
| `epv-pipeline.yml` | Daily `opta-scrape-complete` dispatch + Sunday 18:00 cron (both xmetrics_only, published models) / manual dispatch for retrains | EPV model training pipeline. Daily dispatch added 2026-07-18 (panna#150) so `opta_xmetrics_bymatch.parquet` follows every scrape — game-logs xGOT/GSAA no longer go NULL between Sundays. Own concurrency group (NOT panna-release-writer — pending-slot cancellation risk vs predictions' same-event run) |

## Documentation convention

Deep/verse-level docs (reviews, plans, incidents, reference material, backlog TODOs) live one level
up at `pannaverse/docs/{reviews,plans,incidents,reference,backlog}/`, not in this repo — see
`pannaverse/docs/HOME.md` for the index. This repo keeps only README, CLAUDE, NEWS, LICENSE,
cran-comments, DATA_DICTIONARY, DATA_ISSUES, MODELS, OPTA_REFERENCE, and ARCHITECTURE at its root.
`pannaverse/docs/NEXT-STEPS.md` and `pannaverse/docs/DECISIONS.md` are the living queue/decision-log — update
them at the end of a session. Reviews under `docs/reviews/` are immutable once written (append
addenda, don't rewrite historical prose). New idea/TODO files belong in `pannaverse/docs/backlog/`,
not at this repo's root.

