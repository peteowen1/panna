# Panna Metrics Architecture

How all the player metrics and ratings in panna connect, what each does, and where to find them.

For system-level architecture (how panna + pannadata connect, CI/CD, data flow)
see [`../ARCHITECTURE.md`](../ARCHITECTURE.md).
For column-level details see [`DATA_DICTIONARY.md`](DATA_DICTIONARY.md).
For code conventions see [`CLAUDE.md`](CLAUDE.md).

## Metric Inventory

### Action-Level Models (per touch/event)

| Metric | What it measures | Model | Key functions | File |
|--------|-----------------|-------|--------------|------|
| **xG** | Shot scoring probability | XGBoost | `fit_xg_model()`, `predict_xg()` | `xg_model.R` |
| **xPass** | Pass completion probability | XGBoost | `fit_xpass_model()`, `predict_xpass()` | `xpass_model.R` |
| **EPV** | Expected possession value: P(team scores next) - P(opponent scores next) | Multinomial XGBoost | `fit_epv_model()`, `assign_epv_credit()` | `epv_model.R`, `epv_features.R` |
| **WP** | In-match win probability: P(home win) + 0.5 * P(draw) | XGBoost (3-outcome) | `fit_wp_model()`, `create_wp_features()` | `wp_model.R` |
| **WPA** | Win probability added per action | Delta WP | `assign_wpa_credit()` | `wp_credit.R` |

### Per-Game Player Metrics

| Metric | What it measures | How computed | Key functions | File |
|--------|-----------------|-------------|--------------|------|
| **Player game EPV** | Per-match EPV contribution (off + def) | Sum of EPV credit (actor + receiver) | `aggregate_player_game_epv()` | `epv_model.R` |
| **Player game WPA** | Per-match WPA contribution | Sum of WPA credit (actor + receiver) | `aggregate_player_game_wpa()` | `wp_credit.R` |
| **PSV** (Player Stat Value) | Per-match stat-based value | Linear combination of match stats using PSR coefficients | `calculate_psv()`, `calculate_psv_components()` | `psr.R` |
| **OSV / DSV** | Offensive / defensive stat value | O/D decomposition of PSV (additive: OSV + DSV = PSV) | `calculate_psv_components()` | `psr.R` |
| **Panna Value** | Combined per-game rating | `EPR_WEIGHT * epv + PSR_WEIGHT * psv` | `build_player_game_ratings()` | `player_game_ratings.R` |

### Season-Level Player Ratings

| Rating | What it measures | How computed | Key functions | File |
|--------|-----------------|-------------|--------------|------|
| **RAPM** | Regularized adjusted plus-minus | Ridge regression on splint-level xG differential | `fit_rapm()`, `extract_rapm_ratings()` | `rapm_model.R` |
| **SPM** | Statistical plus-minus | Elastic net predicting RAPM from box score stats | `fit_spm_opta()`, `fit_spm_model()` | `spm_model.R`, `spm_opta.R` |
| **xRAPM** | RAPM shrunk toward SPM prior | Ridge with offset: `y_adj = y - X*spm; gamma = ridge(X, y_adj); xrapm = spm + gamma` | `fit_rapm_with_prior()`, `extract_xrapm_ratings()` | `rapm_model.R` |
| **Panna Rating** | Final combined rating | = xRAPM (in production pipeline) | `calculate_panna_rating()` | `panna_rating.R` |
| **O-Panna / D-Panna** | Offensive / defensive decomposition | Separate O/D design matrix with O/D priors | `calculate_od_panna()` | `offensive_defensive.R` |
| **EPR** | Expected points rating from EPV | Bayesian decay-weighted per-game EPV | `calculate_epr()` | `player_ratings_epv.R` |
| **PSR** | Player skill rating | glmnet on decay-weighted skills predicting xG differential | `calculate_psr()`, `calculate_psr_components()` | `psr.R` |
| **OSR / DSR** | Offensive / defensive skill rating | O/D decomposition of PSR | `calculate_psr_components()` | `psr.R` |
| **Estimated Skills** | Bayesian decay-weighted career stats | Per-stat exponential decay + position prior shrinkage | `estimate_player_skills()` | `estimated_skills.R` |

### Supporting Infrastructure

| Component | Purpose | Key functions | File |
|-----------|---------|--------------|------|
| **SPADL conversion** | Opta events to standardised action format | `convert_opta_to_spadl()` | `spadl_conversion.R` |
| **Possession chains** | Group SPADL actions into continuous possession blocks | `create_possession_chains()` | `possession_chains.R` |
| **Splints** | Constant-lineup match segments (between subs/goals) | `create_all_splints()` | `splint_creation.R` |
| **RAPM design matrix** | Sparse matrix with `{player_id}_off` / `{player_id}_def` columns | `create_rapm_design_matrix()` | `rapm_matrix.R` |
| **Skill config** | Structured definitions for 80+ stats (type, category, position-adjustment) | `soccer_stat_rating_definitions()` | `skill_config.R` |
| **Skill optimisation** | L-BFGS-B for decay rates, Brent for prior strength | `optimize_decay_params()` | `skill_optimization.R` |
| **Player attribution** | Zero-ablation SHAP-like player impact on predictions | `calculate_player_attribution()` | `player_attribution.R` |
| **Match prediction** | Elo + XGBoost using team-aggregated ratings | `predict_matches()` | `match_prediction.R` |

## How the Metrics Connect

### Dependency Graph

```
Opta Events
    |
    v
SPADL Actions ──────────────────────────────────────────────┐
    |                                                       |
    v                                                       v
Possession Chains ──> EPV Model ──> Per-action EPV    xG/xPass Models
    |                     |              |
    |                     v              v
    |              WP Model ──> WPA    Player Game EPV ──> EPR
    |                            |
    |                            v
    |                     Player Game WPA
    |
    v
Splints (constant lineups)
    |
    |── xG diff per splint ──────────────────> RAPM (ridge regression)
    |                                            |
    |── EPV/WPA/PSV per splint (optional) ──> Multi-target RAPM
    |
    v
Opta Box Score Stats ──> SPM (elastic net predicting RAPM)
    |                       |
    |                       v
    |                    xRAPM = RAPM shrunk toward SPM prior
    |                       |
    |                       v
    |                  PANNA RATING (= xRAPM)
    |
    v
Decay-Weighted Skills ──> Skill SPM ──> Skill xRAPM ──> Skill Panna
    |
    v
PSR Model (glmnet on skills) ──> PSR / OSR / DSR (season-level)
    |                                  |
    v                                  v
PSV / OSV / DSV (per-game)       PSR coefficients applied per match
    |
    v
build_player_game_ratings() merges EPV + WPA + PSV ──> panna_value (per game)
```

### Key Relationships

**xRAPM does NOT use PSR as its prior.** xRAPM uses SPM (predicted RAPM from box scores). PSR is a separate downstream product that uses decay-weighted skills to predict xG differential. They are cousins, not parent-child:

- **SPM**: elastic net predicting RAPM from raw season stats (prior for xRAPM)
- **PSR**: glmnet predicting xG diff from decay-weighted career skills (standalone rating)
- **Skill SPM**: elastic net predicting RAPM from decay-weighted skills (prior for skill xRAPM)

**Two parallel pipeline variants exist:**

1. **Opta Ratings Pipeline** (`data-raw/player-ratings-opta/`): Uses raw season stats for SPM
2. **Estimated Skills Pipeline** (`data-raw/estimated-skills/`): Replaces raw averages with decay-weighted career skills, then re-runs SPM/xRAPM/Panna

Both share the same RAPM design matrix (splints + xG target from step 04).

### Rating Sign Convention

- **Offense**: positive = good (more xG created)
- **Defense**: negative = good (more xG suppressed)
- **Overall**: offense - defense (so good defense contributes positively to overall)

## Production Pipelines

### Opta Ratings Pipeline

```
data-raw/player-ratings-opta/
  01_load_opta_data.R     Load Opta events + lineups, SPADL conversion, xG scoring
  02_data_processing.R    Process into pipeline-ready format
  03_splint_creation.R    Segment matches into constant-lineup splints
  04_rapm.R               Base RAPM (ridge on xG differential)
  05_spm.R                Opta SPM (80+ features, blended elastic net + XGBoost)
  06_xrapm.R              xRAPM (RAPM shrunk toward SPM) + multi-target variant
  07_seasonal_ratings.R   Per-season decay-weighted rating snapshots
  08_panna_ratings.R      Final combined ratings (panna = xRAPM)
  09_export_ratings.R     Export to parquet for GitHub Releases
```

### Estimated Skills Pipeline

```
data-raw/estimated-skills/
  01_compute_match_stats.R    Per-match player stats from Opta
  02_estimate_skills.R        Decay-weighted career skill estimates
  02b_optimize_params.R       Optimise decay rates per stat category
  03_skill_spm.R              SPM using estimated skills (not raw averages)
  04_skill_xrapm.R            xRAPM with skill-based SPM prior
  05_skill_panna_ratings.R    Skill-based Panna ratings
  06_seasonal_skill_ratings.R Per-season snapshots
  07_train_psr_model.R        Train PSR/OSR/DSR glmnet models
  08_export_skills.R          Export skills + ratings
  08b_export_psr_weekly.R     Weekly PSR snapshots for blog
```

### Match Predictions Pipeline

```
data-raw/match-predictions-opta/
  01_build_fixture_results.R    Load match fixtures and results
  02_player_ratings_to_team.R   Aggregate player ratings to team level
  02b_team_skill_features.R     Team-level skill aggregations (optional)
  03_team_rolling_features.R    Rolling average form metrics
  04_build_match_dataset.R      Combine all features for model training
  05_fit_goals_model.R          XGBoost Poisson model for goal prediction
  06_fit_outcome_model.R        XGBoost multinomial for W/D/L probabilities
  07_predict_fixtures.R         Generate predictions for upcoming fixtures
  08_evaluate_model.R           Backtesting and validation metrics
  09_upload_predictions.R       Upload predictions to GitHub Releases
  10_export_blog_data.R         Export blog parquets (ratings, predictions, standings)
  10b_export_game_logs.R        Per-match game logs (optional)
  10c_export_equity.R           Per-action EPV equity metrics (optional)
```

### EPV/WPA Pipeline

```
data-raw/epv/
  01_train_epv_models.R       Train xG, xPass, EPV models
  02_calculate_player_epv.R   Per-player EPV aggregation
  03_epv_shap_analysis.R      SHAP analysis of EPV model
```

## Test Coverage

| Area | Test Files |
|------|-----------|
| EPV system | `test-epv-pipeline`, `test-epv-adjustments`, `test-player_game_epv`, `test-epr` |
| WPA | `test-wp_model` |
| RAPM | `test-rapm-model`, `test-rapm-matrix`, `test-splints`, `test-rapm_multi_target` |
| SPM | `test-spm-model`, `test-spm-opta-helpers` |
| Estimated skills | `test-estimated-skills`, `test-skill-optimization`, `test-skill_config` |
| PSR / PSV | `test-psr`, `test-psv` |
| Unified ratings | `test-player_game_ratings` |
| End-to-end | `test-integration` |
| Match prediction | `test-match-prediction` |

All tests use synthetic data generated inline -- no external data dependencies.
