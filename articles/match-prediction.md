# Match Prediction and Tournament Simulation

Match predictions are a two-step XGBoost model (Poisson goals, then
multinomial outcome) fit on team-aggregated player ratings, rolling
form, and Elo. This vignette covers reading already-computed
predictions, the low-level scorer, and the World Cup 2026 tournament
simulator built on top.

## What feeds the model

Team-level features are built in `data-raw/match-predictions-opta/`
steps 02-04 (pipeline 3 of
[`vignette("pipeline-walkthrough")`](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md),
“Pipeline Anatomy”):

- **Player ratings -\> team**: seasonal xRAPM/SPM (or skill-based
  ratings when `use_skill_ratings = TRUE`, the default) aggregated to a
  team strength number.
- **Team skill features**: team-level aggregates from the Estimated
  Skills pipeline.
- **Rolling form + Elo**:
  [`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md),
  [`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md).
- **The `panna` feature specifically** uses the leak-free as-of-date
  career trait (`career_panna_asof.parquet`, monthly snapshots) when
  available, with an SPM fallback.

## Reading predictions (the practical entry point)

Most consumers should read already-computed predictions rather than fit
anything themselves:

``` r
library(panna)

# All available predictions (played + upcoming), downloaded from GitHub Releases
preds <- load_predictions(source = "remote")

# Only future fixtures
upcoming <- load_predictions(source = "remote", filter_future = TRUE)
```

[`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md)
returns a data frame with (among others) `home_team`, `away_team`,
`prob_H`, `prob_D`, `prob_A`, `pred_home_goals`, `pred_away_goals`.

## `predict_match()` – the low-level scorer

[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md)
takes already-fitted goals + outcome model objects and feature matrices
and returns win/draw/loss probabilities plus expected goals:

``` r
predict_match(goals_home_model, goals_away_model, outcome_model,
              X_goals, X_outcome)
```

In practice this is a documented utility, not something the shipped
pipeline calls: `data-raw/match-predictions-opta/07_predict_fixtures.R`
fits its predictions inline against the models saved by steps 05/06
rather than going through this wrapper. The model-fitting functions
themselves
([`fit_goals_xgb()`](https://peteowen1.github.io/panna/reference/fit_goals_xgb.md)/[`fit_outcome_xgb()`](https://peteowen1.github.io/panna/reference/fit_outcome_xgb.md))
are internal, not exported – so
[`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md)
is only directly usable if you already have
`goals_home_model`/`goals_away_model`/`outcome_model` objects on disk
(e.g. from running steps 05-06 of the pipeline locally with
`devtools::load_all()`). For everyday use, read predictions via
[`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md)
instead.

## Backing out a team-strength number

Given any set of match probabilities (from
[`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md),
or the knockout lookup below),
[`fit_bt_ratings()`](https://peteowen1.github.io/panna/reference/fit_bt_ratings.md)
fits a single Bradley-Terry-Davidson strength rating per team:

``` r
preds <- load_predictions(source = "remote")
bt <- fit_bt_ratings(preds, verbose = TRUE)
head(bt$ratings)  # team / rating / rank, centered to mean zero
```

## World Cup 2026 simulation

The tournament simulator
([`simulate_world_cup()`](https://peteowen1.github.io/panna/reference/simulate_world_cup.md))
is exported, but its realistic entry point is the pipeline script
`data-raw/match-predictions-opta/11_simulate_wc2026.R` (step 11, opt-in)
– it needs a hand-curated team-to-group mapping
(`inst/extdata/wc2026_groups.csv`) plus the fitted goals/outcome models
from steps 05-06. The shape of the real call:

``` r
# From data-raw/match-predictions-opta/11_simulate_wc2026.R, simplified:
match_dataset <- readRDS("data-raw/cache-predictions-opta/04_match_dataset.rds")
goals_models  <- readRDS("data-raw/cache-predictions-opta/05_goals_model.rds")
outcome_model <- readRDS("data-raw/cache-predictions-opta/06_outcome_model.rds")
groups        <- read.csv(system.file("extdata", "wc2026_groups.csv", package = "panna"))

wc_preds <- preds[preds$league == "WC" & preds$season == "2026", ]

# Full-model pairwise knockout lookup (every team vs every team, not just
# group rivals) -- avoids the "strong team in a weak group" rating artifact.
knockout <- build_knockout_lookup(
  match_dataset  = match_dataset,
  goals_models   = goals_models,
  outcome_result = outcome_model,
  verbose        = TRUE
)

sim <- simulate_world_cup(
  predictions = wc_preds,
  groups      = groups,
  knockout    = knockout,
  n_sims      = 10000L,
  elo_k       = 20,
  bracket     = "fifa2026",
  verbose     = TRUE
)

sim$summary       # per-team round-by-round probabilities
sim$group_table   # group-position probabilities
```

After simulating, step 12 (`12_export_wc2026_blog.R`) exports
`wc2026_team_strength.parquet` with the `tiento` column – a separate,
hand-weighted team-composite rating (panna 0.40 / Elo 0.30 / EPR 0.20 /
PSR 0.10) used for the WC2026 blog map, distinct from the Bradley-Terry
ratings above.

[`run_wc2026_reference_checks()`](https://peteowen1.github.io/panna/reference/run_wc2026_reference_checks.md)
runs a battery of sanity facts (e.g. host teams always show
`home_field == 1`) against the pipeline’s own output files and warns on
any that fail – designed to run at the end of the predictions pipeline,
after step 12:

``` r
run_wc2026_reference_checks(cache_dir = "data-raw/cache-predictions-opta")
```

## Where predictions land

`predictions-pipeline.yml` runs on a Wednesday 8 AM UTC cron, but in
practice fires **daily** – `pannadata`’s daily Opta scrape dispatches
`opta-scrape-complete`, which re-triggers this workflow every day the
scrape succeeds. Step 13 (`13_publish_release_data.R`) does one gated
[`vb_publish()`](https://peteowen1.github.io/panna/reference/vb_publish.md)
of both `predictions-latest` (raw predictions) and `blog-latest` (blog
export), so the two releases can never end up half-updated relative to
each other. See
[`vignette("data-bus")`](https://peteowen1.github.io/panna/articles/data-bus.md)
for how that publish step works.

## Next steps

- [Pipeline
  Anatomy](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md)
  – the full pipeline map (predictions is pipeline 3)
- [Player
  Ratings](https://peteowen1.github.io/panna/articles/player-ratings.md)
  – the ratings that feed team strength
- [Data Access and
  Publishing](https://peteowen1.github.io/panna/articles/data-bus.md) –
  [`vb_publish()`](https://peteowen1.github.io/panna/reference/vb_publish.md)
  and the release-as-data-bus pattern
