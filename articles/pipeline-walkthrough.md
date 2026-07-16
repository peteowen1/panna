# Pipeline Anatomy

Every metric in
[`vignette("player-ratings")`](https://peteowen1.github.io/panna/articles/player-ratings.md)
is produced by one of four numbered pipelines under `data-raw/`. This
vignette is a map: what each numbered step produces, where its cache
lives, how the pipelines depend on each other, and how they’re scheduled
in CI.

## The wrong-cwd trap

**Always run pipeline scripts from inside `panna/`, never from the
`pannaverse/` root.** Every script uses relative cache paths
(`data-raw/cache-opta/...`). Run one from the wrong directory and you
get a stray `data-raw/cache/` tree at the pannaverse root that silently
pollutes `git status` there – which is exactly the signal used to detect
submodule pointer moves. If you ever see `data-raw/` or `opta/`
directories at the pannaverse root, they’re regenerable junk from a
wrong-cwd run; delete them.

``` r
# WRONG -- run from pannaverse root
# Rscript panna/data-raw/player-ratings-opta/run_pipeline_opta.R

# RIGHT
# cd panna
# Rscript data-raw/player-ratings-opta/run_pipeline_opta.R
```

## The four pipelines

### 1. Opta RAPM/SPM (`data-raw/player-ratings-opta/`, entry `run_pipeline_opta.R`)

| Step | Script                    | Produces                                | Cache                                |
|------|---------------------------|-----------------------------------------|--------------------------------------|
| 01   | `01_load_opta_data.R`     | Raw events cache                        | `cache-opta/`                        |
| 02   | `02_data_processing.R`    | Processed splint-ready data             | `cache-opta/`                        |
| 03   | `03_splint_creation.R`    | Splint boundaries (chain-derived)       | `cache-opta/03_splints.rds`          |
| 04   | `04_rapm.R`               | Seasonal RAPM (ridge/glmnet)            | `cache-opta/04_rapm.rds`             |
| 05   | `05_spm.R`                | SPM model fit + predictions             | `cache-opta/`                        |
| 06   | `06_xrapm.R`              | xRAPM (SPM-shrunk RAPM)                 | `cache-opta/`                        |
| 07   | `07_seasonal_ratings.R`   | Combined seasonal ratings table         | `cache-opta/07_seasonal_ratings.rds` |
| 07b  | `07b_player_centrality.R` | Network centrality metrics              | `cache-opta/`                        |
| 08   | `08_panna_ratings.R`      | Final `panna` rating                    | `cache-opta/08_panna.rds`            |
| 09   | `09_export_ratings.R`     | Uploads `seasonal_xrapm`/`seasonal_spm` | `ratings-data` release               |

Prerequisite: pre-computed xMetrics (step 4 of the EPV pipeline, below).

### 2. Estimated Skills (`data-raw/estimated-skills/`, entry `run_skills_pipeline.R`)

| Step | Purpose                                | Cache                                        |
|------|----------------------------------------|----------------------------------------------|
| 00   | GK per-90 column prep                  | `cache-skills/`                              |
| 01   | Compute match-level stats              | `cache-skills/01_match_stats.rds`            |
| 02   | Estimate per-stat skills               | `cache-skills/`                              |
| 02b  | Optimize decay/prior params            | `cache-skills/02b_decay_params.rds`          |
| 03   | Skill-based SPM (incl. as-of variant)  | `cache-skills/03_skill_spm.rds`              |
| 04   | Skill-based xRAPM                      | `cache-skills/`                              |
| 05   | Skill-based panna ratings              | `cache-skills/`                              |
| 06   | Seasonal skill ratings                 | `cache-skills/06_seasonal_ratings.rds`       |
| 07   | Train PSR model                        | `cache-skills/`                              |
| 07b  | Position/era mean tables               | `inst/extdata/`                              |
| 07c  | Live-PSV centering constants           | `inst/extdata/psv_live_constants.csv`        |
| 08   | Export skills                          | `opta_skills.parquet`                        |
| 08b  | Export weekly PSR snapshots            | `opta_psr_weekly.parquet` (`opta-latest`)    |
| 09   | Career panna trait                     | `career_panna.parquet` (`ratings-data`)      |
| 09b  | Leak-free as-of career panna snapshots | `career_panna_asof.parquet` (`ratings-data`) |

Prerequisite: `cache-opta/03_splints.rds` and `cache-opta/04_rapm.rds`
from pipeline 1 – must run the Opta RAPM pipeline first.

### 3. Match Predictions (`data-raw/match-predictions-opta/`, entry `run_predictions_opta.R`)

| Step    | Script                                 | Produces                                                                                                                         |
|---------|----------------------------------------|----------------------------------------------------------------------------------------------------------------------------------|
| 01      | `01_build_fixture_results.R`           | Fixture/result base table                                                                                                        |
| 01b     | `01b_refresh_wc2026_squads.R` (opt-in) | Rebuilds WC2026 announced-squad minutes                                                                                          |
| 02      | `02_player_ratings_to_team.R`          | Team-aggregated player ratings                                                                                                   |
| 02b     | `02b_team_skill_features.R`            | Team-level skill aggregates                                                                                                      |
| 03      | `03_team_rolling_features.R`           | Rolling form + Elo features                                                                                                      |
| 04      | `04_build_match_dataset.R`             | Full model-ready match dataset                                                                                                   |
| 05      | `05_fit_goals_model.R`                 | XGBoost Poisson goals model                                                                                                      |
| 06      | `06_fit_outcome_model.R`               | XGBoost multinomial outcome model                                                                                                |
| 07      | `07_predict_fixtures.R`                | `predictions.parquet` (all matches, played + upcoming)                                                                           |
| 08      | `08_evaluate_model.R`                  | Backtest metrics                                                                                                                 |
| 09      | `09_upload_predictions.R` (opt-in)     | Validates predictions for step 13                                                                                                |
| 10      | `10_export_blog_data.R` (opt-in)       | `panna_ratings.parquet`, `match_predictions.parquet`                                                                             |
| 10b     | `10b_export_game_logs.R` (opt-in)      | `game_logs_*.parquet` (EPV+WPA+PSV+`piero_value`)                                                                                |
| 10c     | `10c_export_equity.R` (opt-in)         | `action_equity_*.parquet` (per-action EPV credit)                                                                                |
| 10d     | `10d_export_shootout_wpa.R` (opt-in)   | Per-player shootout WPA                                                                                                          |
| 11      | `11_simulate_wc2026.R` (opt-in)        | WC2026 Monte Carlo simulation                                                                                                    |
| 12      | `12_export_wc2026_blog.R` (opt-in)     | `wc2026_team_strength.parquet` (`tiento` column)                                                                                 |
| 12b/12c | Snapshot scripts (opt-in)              | Dated minutes/strength history diffs                                                                                             |
| 13      | `13_publish_release_data.R` (opt-in)   | Single gated [`vb_publish()`](https://peteowen1.github.io/panna/reference/vb_publish.md) of `predictions-latest` + `blog-latest` |

Prerequisite: `cache-opta/07_seasonal_ratings.rds` (pipeline 1) and,
when `use_skill_ratings = TRUE` (the default),
`cache-skills/06_seasonal_ratings.rds` (pipeline 2).

### 4. EPV / xMetrics (`data-raw/epv/`, run manually – no single entry point)

| Step     | Script                                                   | Produces                                                                  |
|----------|----------------------------------------------------------|---------------------------------------------------------------------------|
| 01       | `01_train_epv_models.R`                                  | EPV model (XGBoost)                                                       |
| 01b      | `01b_train_duel_model.R`                                 | xDuel WOE models                                                          |
| 02       | `02_calculate_player_epv.R`                              | Per-player EPV                                                            |
| 03       | `03_calculate_player_xmetrics.R`                         | xG/xA/xPass per player-match                                              |
| 04 / 04b | `04_export_xmetrics.R` / `04b_export_xmetrics_bymatch.R` | `opta_xmetrics.parquet` / `opta_xmetrics_bymatch.parquet` (`opta-latest`) |
| 05       | `05_train_wp_model.R`                                    | Win-probability model                                                     |
| 06       | `06_calculate_wpa.R`                                     | Per-action WPA                                                            |

This pipeline trains the models everything else depends on. Model
training is iterating (see `../MODELS.md`); routine refreshes run
`xmetrics_only` (score with the currently published model, no retrain).

## How to run: `run_steps` and `force_rebuild_from`

Every orchestrator (`run_pipeline_opta.R`, `run_skills_pipeline.R`,
`run_predictions_opta.R`) follows the same override pattern: config
variables are set with `if (!exists(...))` so a driver script can
override them *before*
[`source()`](https://rdrr.io/r/base/source.html)-ing the pipeline.

``` r
# From panna/:

# Run only a subset of steps
run_steps <- list(
  step_01_load_data        = TRUE,
  step_02_data_processing  = TRUE,
  step_03_splint_creation  = FALSE,  # already cached, skip
  step_04_rapm             = TRUE,
  step_05_spm              = TRUE,
  step_06_xrapm             = TRUE,
  step_07_seasonal_ratings = TRUE,
  step_08_panna_ratings    = TRUE,
  step_09_export_ratings   = FALSE  # skip upload
)
source("data-raw/player-ratings-opta/run_pipeline_opta.R")
```

``` r
# force_rebuild_from clears cache from a given step onward and re-runs it.
# NULL (default) = use whatever is cached; a step number = full refresh
# from there on.
force_rebuild_from <- 4  # e.g. re-run RAPM onward after a splint fix
source("data-raw/player-ratings-opta/run_pipeline_opta.R")
```

Simpler shortcut: `start_step <- 3` (before sourcing) auto-populates
`run_steps` to skip everything before step 3.

The Opta RAPM pipeline additionally runs each step in its own `callr`
subprocess so memory is fully released between steps – the heaviest
single step peaks at ~12.6GB, but a single R session accumulates the
high-water mark of every step it has run, which previously OOM’d a 16GB
CI runner even though no individual step needed that much.

## Cache topology

| Directory (inside `panna/`)        | Pipeline                         | Notable cross-pipeline reads                                                                          |
|------------------------------------|----------------------------------|-------------------------------------------------------------------------------------------------------|
| `data-raw/cache/`                  | EPV/xMetrics + SPADL conversions | –                                                                                                     |
| `data-raw/cache-opta/`             | Opta RAPM/SPM (steps 01-09)      | Read by Skills pipeline (`03_splints.rds`, `04_rapm.rds`) and Predictions (`07_seasonal_ratings.rds`) |
| `data-raw/cache-skills/`           | Estimated Skills                 | Read by Predictions (`06_seasonal_ratings.rds`, when `use_skill_ratings = TRUE`)                      |
| `data-raw/cache-predictions-opta/` | Match Predictions                | Read by blog export steps (10/10b/10c/12)                                                             |

All four are gitignored – they are pipeline intermediates, fully
regenerable from source data plus the published models.

## GitHub Actions mapping

| Workflow                   | Trigger                                                                                                                                                                                                    | Runs                                                                                                                          |
|----------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------|
| `opta-pipeline.yml`        | Manual dispatch only                                                                                                                                                                                       | Opta RAPM/SPM pipeline. OOMs the 16GB hosted runner since the 2026-06 league expansion – run locally (~25GB+ RAM) until fixed |
| `predictions-pipeline.yml` | Wed 8 AM UTC cron, plus a WC2026 finals-week daily backstop; also fired by `pannadata`’s daily Opta scrape via `repository_dispatch: opta-scrape-complete` (so it effectively runs daily, not just weekly) | Match Predictions pipeline, steps 1-13 as enabled                                                                             |
| `psr-weekly-snapshot.yml`  | Wed 10 AM UTC                                                                                                                                                                                              | Skills step 8b (weekly PSR snapshot) + step 07c (live-PSV centering constants)                                                |
| `epr-weekly-snapshot.yml`  | Wed 11 AM UTC                                                                                                                                                                                              | Incremental `opta_epr_weekly.parquet` rebuild                                                                                 |
| `epv-pipeline.yml`         | Weekly Sunday 18:00 UTC                                                                                                                                                                                    | EPV/xMetrics pipeline in `xmetrics_only` mode (scores with the published model; manual dispatch needed for a real retrain)    |
| `pkgdown.yaml`             | Push                                                                                                                                                                                                       | Rebuilds this documentation site                                                                                              |
| `R-CMD-check.yaml`         | Push to `dev`, PRs to `main`                                                                                                                                                                               | Package checks                                                                                                                |

## Fresh-clone bootstrap

``` r
# From panna/, one command pulls everything a fresh clone needs:
# Rscript data-raw/bootstrap.R            # data + models + prediction caches
# Rscript data-raw/bootstrap.R opta       # Opta data + models only
# Rscript data-raw/bootstrap.R models     # models only
# Rscript data-raw/bootstrap.R caches     # prediction caches only

library(panna)
pb_download_opta()  # what bootstrap.R calls under the hood for Opta data
```

See
[`vignette("data-bus")`](https://peteowen1.github.io/panna/articles/data-bus.md)
for what
[`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)
actually does and how published outputs get back out to GitHub Releases.

## Next steps

- [Player
  Ratings](https://peteowen1.github.io/panna/articles/player-ratings.md)
  – what each pipeline’s output means
- [Data Access and
  Publishing](https://peteowen1.github.io/panna/articles/data-bus.md) –
  download/publish mechanics
- [Match Prediction and Tournament
  Simulation](https://peteowen1.github.io/panna/articles/match-prediction.md)
  – pipeline 3 in depth
- [Data
  Dictionary](https://peteowen1.github.io/panna/DATA_DICTIONARY.md) –
  column definitions at each stage
