# Panna Data Dictionary

This document describes all data structures used in the panna pipelines.

## Pipeline Overview

### RAPM/SPM Pipeline — `data-raw/player-ratings-opta/`

```
01_load_opta_data.R       ->  cache-opta/01_opta_data.rds
02_data_processing.R      ->  cache-opta/02_processed_data.rds
03_splint_creation.R      ->  cache-opta/03_splints.rds
04_rapm.R                 ->  cache-opta/04_rapm.rds
05_spm.R                  ->  cache-opta/05_spm.rds
06_xrapm.R                ->  cache-opta/06_xrapm.rds
07_seasonal_ratings.R     ->  cache-opta/07_seasonal_ratings.rds
07b_player_centrality.R   ->  cache-opta/07b_centrality.rds
08_panna_ratings.R        ->  cache-opta/08_panna_ratings.rds
09_export_ratings.R       ->  seasonal_xrapm.parquet, seasonal_spm.parquet
```

### EPV/WPA Pipeline — `data-raw/epv/`

```
01_train_epv_models.R          ->  xg_model.rds, xpass_model.rds, epv_model.rds
02_calculate_player_epv.R      ->  per-player EPV aggregation
03_calculate_player_xmetrics.R ->  xmetrics/{league}/{season}.parquet
04_export_xmetrics.R           ->  opta_xmetrics.parquet (GitHub Release)
05_train_wp_model.R            ->  wp_model.rds
06_calculate_wpa.R             ->  per-player WPA credits
```

### Skills Pipeline — `data-raw/estimated-skills/`

```
00_add_gk_per90_cols.R         ->  GK feature prep
01_compute_match_stats.R       ->  cache-skills/01_match_stats.rds
02_estimate_skills.R           ->  cache-skills/02_skills.rds
02b_optimize_params.R          ->  cache-skills/02b_decay_params.rds
03_skill_spm.R                 ->  cache-skills/03_skill_spm.rds
04_skill_xrapm.R               ->  cache-skills/04_skill_xrapm.rds
05_skill_panna_ratings.R       ->  cache-skills/05_skill_panna.rds
06_seasonal_skill_ratings.R    ->  cache-skills/06_seasonal_ratings.rds
07_train_psr_model.R           ->  PSR/PSV glmnet models
08_export_skills.R             ->  opta_skills.parquet (GitHub Release)
08b_export_psr_weekly.R        ->  PSR weekly snapshots
```

### Predictions Pipeline — `data-raw/match-predictions-opta/`

```
01_build_fixture_results.R     ->  cache-predictions-opta/01_fixtures.rds
02_player_ratings_to_team.R    ->  cache-predictions-opta/02_team_ratings.rds
02b_team_skill_features.R      ->  cache-predictions-opta/02b_team_skills.rds
03_team_rolling_features.R     ->  cache-predictions-opta/03_rolling.rds
04_build_match_dataset.R       ->  cache-predictions-opta/04_dataset.rds
05_fit_goals_model.R           ->  cache-predictions-opta/05_goals_model.rds
06_fit_outcome_model.R         ->  cache-predictions-opta/06_outcome_model.rds
07_predict_fixtures.R          ->  cache-predictions-opta/07_predictions.rds
08_evaluate_model.R            ->  backtesting evaluation
09_upload_predictions.R        ->  predictions-latest (GitHub Release)
10_export_blog_data.R          ->  blog-latest (GitHub Release)
```

---

## FBref Pipeline Data Structures

> **Archived in commit 43ea7fd** — the FBref pipeline code was moved to `archive/data-raw/player-ratings-fbref/` and is no longer actively maintained. These schemas are kept for historical reference (e.g. when reading older pannadata releases). The active player ratings pipeline is Opta-based — see **Pipeline Overview** above.

The FBref pipeline shares the same RAPM/SPM methodology as Opta but uses FBref data.

### 01_raw_data.rds

Raw data from pannadata repository. All columns are snake_case after `clean_column_names()`.

### results
| Column | Type | Description |
|--------|------|-------------|
| match_url | character | FBref match URL (unique identifier) |
| home | character | Home team name |
| away | character | Away team name |
| home_goals | numeric | Home team goals |
| away_goals | numeric | Away team goals |
| home_x_g | numeric | Home team xG |
| away_x_g | numeric | Away team xG |
| date | Date | Match date |
| season_end_year | numeric | Season end year (e.g., 2024 for 2023-24) |
| league | character | League name (multi-league only) |
| country | character | Country code (multi-league only) |

### lineups (derived from stats_passing)
| Column | Type | Description |
|--------|------|-------------|
| match_url | character | FBref match URL |
| team | character | Team name |
| player_name | character | Player name |
| player_href | character | FBref player ID |
| pos | character | Position (e.g., "FW", "MF") |
| min | numeric | Minutes played |
| home_away | character | "Home" or "Away" |
| is_starter | logical | TRUE if minutes >= 45 |
| minutes | numeric | Minutes played |
| is_home | logical | TRUE if home team |

### events (from match summary)
| Column | Type | Description |
|--------|------|-------------|
| match_url | character | FBref match URL |
| team | character | Team involved |
| event_type | character | Event type from FBref |
| minute | numeric | Minute of event |
| player | character | Player name |
| is_goal | logical | Goal event |
| is_sub | logical | Substitution event |
| is_penalty | logical | Penalty goal |
| is_own_goal | logical | Own goal |
| is_red_card | logical | Red card event |

### shooting
| Column | Type | Description |
|--------|------|-------------|
| match_url | character | FBref match URL |
| squad | character | Shooting team |
| player | character | Shooter name |
| minute | numeric | Shot minute |
| x_g | numeric | Shot xG |
| outcome | character | "Goal" or other |
| body_part | character | Body part used |
| notes | character | Shot details |

### stats_summary, stats_passing, stats_defense, stats_possession
Advanced player stats per match. See `pannadata/DATA_DICTIONARY.md` for full column definitions.

---

## 02_processed_data.rds

Cleaned and standardized data.

### results
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Unique match ID (season_date_home_away hash) |
| season | numeric | Season end year |
| date | Date | Match date |
| match_url | character | FBref URL |
| home_team | character | Standardized home team name |
| away_team | character | Standardized away team name |
| home_goals | numeric | Home goals |
| away_goals | numeric | Away goals |
| goal_diff | numeric | home_goals - away_goals |
| home_xg | numeric | Home xG |
| away_xg | numeric | Away xG |
| xg_diff | numeric | home_xg - away_xg |
| league | character | League name (if multi-league) |
| country | character | Country code (if multi-league) |
| season_end_year | numeric | Season end year (if multi-league) |

### lineups
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Match ID |
| team | character | Team name |
| is_home | logical | Home team indicator |
| player_name | character | Standardized player name |
| player_id | character | FBref player ID (from player_href) |
| is_starter | logical | Started the match |
| position | character | Position |
| minutes | numeric | Minutes played |

### events
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Match ID |
| team | character | Team name |
| is_home | logical | Home team indicator |
| event_type | character | "goal", "substitution", or "red_card" |
| minute | numeric | Event minute |
| player_name | character | Player involved |
| is_penalty | logical | Penalty goal |
| is_own_goal | logical | Own goal |
| is_red_card | logical | Red card |

### shooting
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Match ID |
| team | character | Team name |
| player_name | character | Shooter name |
| minute | numeric | Shot minute |
| xg | numeric | Shot xG |
| is_goal | logical | Goal scored |
| is_penalty | logical | Penalty shot |
| body_part | character | Body part |
| shot_type | character | Shot details |

---

## 03_splints.rds

Time segments for RAPM analysis.

### splints
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Match ID |
| splint_id | character | Unique splint ID (match_id_splint_num) |
| splint_num | integer | Splint number within match |
| start_minute | numeric | Start of splint |
| end_minute | numeric | End of splint |
| duration | numeric | Splint duration in minutes |
| avg_min | numeric | (start + end) / 2 |
| gf_home | integer | Cumulative home goals at splint start |
| ga_home | integer | Cumulative away goals at splint start |
| red_home | integer | Cumulative home red cards at splint start |
| red_away | integer | Cumulative away red cards at splint start |
| n_players_home | integer | Home players on pitch (11 - red_home) |
| n_players_away | integer | Away players on pitch (11 - red_away) |
| npxg_home | numeric | Non-penalty xG for home team in splint |
| npxg_away | numeric | Non-penalty xG for away team in splint |
| npxgd | numeric | npxg_home - npxg_away |
| npxgd_per_90 | numeric | npxgd * 90 / duration |
| league | character | League name (if multi-league) |
| season_end_year | numeric | Season (if multi-league) |
| country | character | Country (if multi-league) |

### players
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Match ID |
| splint_id | character | Splint ID |
| splint_num | integer | Splint number |
| team | character | Team name |
| is_home | logical | Home team indicator |
| player_name | character | Player name |
| player_id | character | Player ID |
| start_minute | numeric | Splint start |
| end_minute | numeric | Splint end |

### match_info
| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Match ID |
| season | numeric | Season end year |
| home_team | character | Home team |
| away_team | character | Away team |
| league | character | League (if multi-league) |
| season_end_year | numeric | Season (if multi-league) |

---

## 04_rapm.rds (RAPM Design Matrix)

### rapm_data
| Component | Type | Description |
|-----------|------|-------------|
| X_players | sparse matrix | Player indicators (playerX_off, playerX_def, replacement_off, replacement_def) |
| X_full | sparse matrix | X_players + covariates |
| row_data | data.frame | See below |
| y | numeric | Target: xgf90 |
| weights | numeric | Splint duration / 90 |
| player_mapping | data.frame | player_id, player_name, total_minutes |
| player_ids | character | Player IDs (including "replacement") |
| n_players | integer | Number of regular players |
| covariate_names | character | Names of covariate columns |
| leagues | character | Unique leagues (if multi-league) |
| seasons | numeric | Unique seasons (if multi-league) |

### row_data (within rapm_data)
| Column | Type | Description |
|--------|------|-------------|
| row_id | integer | Row number |
| splint_id | character | Splint ID |
| match_id | character | Match ID |
| xgf | numeric | xG FOR (raw) |
| minutes | numeric | Splint duration |
| xgf90 | numeric | TARGET: xG FOR per 90 |
| gd | integer | Goal difference (from this team's perspective) |
| gf | integer | Goals for (from this team's perspective) |
| ga | integer | Goals against |
| avg_min | numeric | Average minute of splint |
| home_away | character | "home" or "away" |
| n_offense | integer | Players on attacking team (accounts for red cards) |
| n_defense | integer | Players on defending team |
| net_players | integer | n_offense - n_defense (man advantage) |

### Covariates in X_full
| Covariate | Description |
|-----------|-------------|
| gd | Goal difference at splint start |
| gf | Goals for at splint start |
| ga | Goals against at splint start |
| avg_min | Average minute of splint |
| is_home | 1 if home team attacking, 0 otherwise |
| n_offense | Attacking team player count (11 - red cards) |
| n_defense | Defending team player count |
| net_players | n_offense - n_defense |
| league_X | League dummies (if multi-league) |
| season_X | Season dummies (if multi-league) |

---

## 05_spm.rds

Statistical Plus-Minus model predicting RAPM from box scores.

### spm_ratings
| Column | Type | Description |
|--------|------|-------------|
| player_name | character | Player name |
| spm | numeric | Overall SPM prediction |
| total_minutes | numeric | Total minutes played |

### offense_spm_ratings
| Column | Type | Description |
|--------|------|-------------|
| player_name | character | Player name |
| offense_spm | numeric | Offensive SPM prediction |

### defense_spm_ratings
| Column | Type | Description |
|--------|------|-------------|
| player_name | character | Player name |
| defense_spm | numeric | Defensive SPM prediction |

---

## 06_xrapm.rds

RAPM with SPM prior (xRAPM).

### ratings
| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| player_name | character | Player name |
| total_minutes | numeric | Total minutes |
| xrapm | numeric | xRAPM overall rating |
| offense | numeric | Offensive xRAPM |
| defense | numeric | Defensive xRAPM |
| off_prior | numeric | Offensive SPM prior |
| def_prior | numeric | Defensive SPM prior |
| off_deviation | numeric | offense - off_prior |
| def_deviation | numeric | defense - def_prior |

---

## 07_panna.rds

Final unified ratings.

### panna_ratings
| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| player_name | character | Player name |
| total_minutes | numeric | Total minutes played |
| panna | numeric | PANNA rating (xRAPM) |
| offense | numeric | Offensive rating |
| defense | numeric | Defensive rating |
| off_deviation | numeric | Deviation from offensive prior |
| def_deviation | numeric | Deviation from defensive prior |
| off_prior | numeric | SPM offensive prior |
| def_prior | numeric | SPM defensive prior |
| base_panna | numeric | Base RAPM (no prior) |
| base_offense | numeric | Base offensive RAPM |
| base_defense | numeric | Base defensive RAPM |
| spm_overall | numeric | Overall SPM prediction |
| panna_percentile | numeric | Percentile rank (0-100) |
| panna_rank | numeric | Rank (1 = best) |

---

---

## Opta Data Schemas

Primary data source (15 leagues, 263 columns per player). Use `load_opta_*()` functions.

### Opta Player Stats (`load_opta_stats()`)

263 columns per player per match. Key columns:

| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Opta match ID |
| match_date | Date | Match date |
| player_id | character | Opta player ID |
| player_name | character | Player name |
| team_id | character | Team ID |
| team_name | character | Team name |
| team_position | character | "home" or "away" |
| minsPlayed | numeric | Minutes played |
| position | character | Position (Goalkeeper, Defender, etc.) |
| goals | numeric | Goals scored |
| totalScoringAtt | numeric | Total shots |
| ontargetScoringAtt | numeric | Shots on target |
| bigChanceScored | numeric | Big chances scored |
| bigChanceMissed | numeric | Big chances missed |
| goalAssist | numeric | Assists |
| accuratePass | numeric | Accurate passes |
| totalPass | numeric | Total passes |
| successfulFinalThirdPasses | numeric | Final third passes completed |
| wonTackle | numeric | Tackles won |
| totalTackle | numeric | Total tackles attempted |
| interceptionWon | numeric | Interceptions won |
| aerialWon | numeric | Aerial duels won |
| aerialLost | numeric | Aerial duels lost |
| duelWon | numeric | Total duels won |
| duelLost | numeric | Total duels lost |
| touches | numeric | Ball touches |
| touchesInOppBox | numeric | Touches in opponent's box |
| possWonDef3rd | numeric | Possession won in defensive third |
| possWonMid3rd | numeric | Possession won in middle third |
| possWonAtt3rd | numeric | Possession won in attacking third |
| dispossessed | numeric | Times dispossessed |
| turnover | numeric | Turnovers |
| fouls | numeric | Fouls committed |
| wasFouled | numeric | Fouls won |
| saves | numeric | Goalkeeper saves |
| goalsConceded | numeric | Goals conceded (GK) |

### Opta Events (`load_opta_events()`)

Match events with timing for splint boundaries.

| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Opta match ID |
| event_type | character | goal, substitution, red_card, yellow_card, second_yellow |
| minute | numeric | Event minute |
| second | numeric | Event second |
| team_id | character | Team involved |
| player_id | character | Player involved |
| player_name | character | Player name |
| player_on_id | character | Sub coming on (for substitutions) |
| player_on_name | character | Name of sub coming on |
| player_off_id | character | Player leaving (for substitutions) |
| player_off_name | character | Name of player leaving |
| assist_player_id | character | Assister (for goals) |
| assist_player_name | character | Assister name |

### Opta Shot Events (`load_opta_shot_events()`)

Individual shots with x/y coordinates.

| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Opta match ID |
| event_id | integer | Unique shot event ID |
| player_id | character | Shooter ID |
| player_name | character | Shooter name |
| team_id | character | Team ID |
| minute | numeric | Shot minute |
| second | numeric | Shot second |
| x | numeric | X coordinate (0-100, goal at 100) |
| y | numeric | Y coordinate (0-100, center at 50) |
| outcome | integer | 1=on target, 0=off target |
| is_goal | logical | Shot resulted in goal |
| type_id | integer | 13=saved, 14=post, 15=miss, 16=goal |
| body_part | character | Head, LeftFoot, RightFoot |
| situation | character | OpenPlay, SetPiece, Corner, Penalty |
| big_chance | logical | Big chance indicator |

Note: Raw Opta data does not include xG values. The xMetrics pipeline (steps 01-04 in `data-raw/epv/`) computes xG/xA/xPass from SPADL + XGBoost models. Pre-computed xMetrics are loaded via `load_opta_xmetrics()`. Penalty xG is overridden to 0.76.

### Opta Lineups (`load_opta_lineups()`)

Player lineup data with timing.

| Column | Type | Description |
|--------|------|-------------|
| match_id | character | Opta match ID |
| match_date | Date | Match date |
| player_id | character | Player ID |
| player_name | character | Player name |
| team_id | character | Team ID |
| team_name | character | Team name |
| team_position | character | "home" or "away" |
| position | character | Position (Goalkeeper, Defender, etc.) |
| position_side | character | Left, Right, Centre |
| formation_place | integer | 1-11 for starters |
| shirt_number | integer | Jersey number |
| is_starter | logical | Started the match |
| minutes_played | numeric | Total minutes played |
| sub_on_minute | numeric | Minute substituted on (0 if starter) |
| sub_off_minute | numeric | Minute substituted off (0 if full match) |

### Using Opta Data for Splints

```r
# Load Opta data
lineups <- load_opta_lineups("ENG", "2024-2025")
events <- load_opta_events("ENG", "2024-2025")
shots <- load_opta_shot_events("ENG", "2024-2025")

# Create processed data structure
processed <- create_opta_processed_data(
  opta_lineups = lineups,
  opta_events = events,
  opta_shot_events = shots
)

# Create splints
splints <- create_all_splints(processed)
```

xG values come from the xMetrics pipeline (`load_opta_xmetrics()`), which pre-computes xG/xA/xPass per player per match. These are joined to splints during the Opta RAPM pipeline (step 03).

---

## Opta Pipeline Cache Structures

### 07_seasonal_ratings.rds

List with seasonal aggregations of RAPM/SPM/xRAPM ratings.

#### seasonal_spm
| Column | Type | Description |
|--------|------|-------------|
| season_end_year | numeric | Season |
| player_id | character | Opta player ID |
| player_name | character | Player name |
| spm | numeric | Overall SPM |
| offense_spm | numeric | Offensive SPM |
| defense_spm | numeric | Defensive SPM |
| total_minutes | numeric | Minutes played |

#### seasonal_xrapm
| Column | Type | Description |
|--------|------|-------------|
| season_end_year | numeric | Season |
| player_name | character | Player name |
| xrapm | numeric | Overall xRAPM |
| offense | numeric | Offensive xRAPM |
| defense | numeric | Defensive xRAPM |
| off_deviation | numeric | offense - off_prior |
| def_deviation | numeric | defense - def_prior |
| total_minutes | numeric | Minutes played |

#### metadata
| Field | Description |
|-------|-------------|
| min_minutes_spm | Minimum minutes for SPM |
| min_minutes_rapm | Minimum minutes for RAPM |
| lambda | Regularization parameter |
| n_seasons | Number of seasons |
| created | Timestamp |

### 07b_centrality.rds

| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Opta player ID |
| centrality | numeric | PageRank centrality (0-1) |
| unique_opponents | integer | Number of unique opponents faced |
| matches_played | integer | Total matches |
| component_id | integer | Connected component label |
| component_size | integer | Size of player's connected component |

---

## Value Metrics Data Structures

Two-path system: EPV (action-level) + PSV (box-score-level) → `panna_value`.

### Per-Game EPV (`aggregate_player_game_epv()`)

| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| player_name | character | Player name |
| team_id | character | Team ID |
| match_id | character | Match ID |
| minutes_played | numeric | Minutes in match |
| epv_total | numeric | Total EPV credit |
| epv_offensive | numeric | Offensive EPV |
| epv_defensive | numeric | Defensive EPV |
| epv_passing | numeric | Passing EPV |
| epv_shooting | numeric | Shooting EPV |
| epv_p90 | numeric | EPV per 90 minutes |

### WPA (`aggregate_player_game_wpa()`)

| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| match_id | character | Match ID |
| wpa_total | numeric | Total WPA |
| wpa_as_actor | numeric | WPA as acting player |
| wpa_as_receiver | numeric | WPA as pass receiver |
| n_wpa_actions | integer | Number of WPA-credited actions |
| wpa_total_p90 | numeric | WPA per 90 |

### PSV (`calculate_psv()`)

| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| psv | numeric | Player Stat Value (osv + dsv) |
| osv | numeric | Offensive stat value |
| dsv | numeric | Defensive stat value |

### EPR (`calculate_epr()`)

Decay-weighted Bayesian EPV ratings.

| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| player_name | character | Player name |
| epr | numeric | Total EPR |
| epr_offensive | numeric | Offensive EPR |
| epr_defensive | numeric | Defensive EPR |
| wt_games | numeric | Effective sample size (weighted games) |
| n_games | integer | Raw game count |

### Combined Game Ratings (`build_player_game_ratings()`)

Merges EPV + WPA + PSV into unified per-game output.

| Column | Type | Description |
|--------|------|-------------|
| player_id | character | Player ID |
| player_name | character | Player name |
| match_id | character | Match ID |
| minutes_played | numeric | Minutes |
| epv_total | numeric | EPV credit |
| wpa_total | numeric | WPA credit |
| psv | numeric | Player Stat Value |
| panna_value | numeric | **Combined: 50% EPV + 50% PSV** |
| panna_value_p90 | numeric | panna_value per 90 |

---

## Rating Interpretation

### Offense Coefficient
- **Positive** = Player helps team create MORE xG when attacking (GOOD)
- Units: xG per 90 minutes above average

### Defense Coefficient
- **Positive** = Player allows MORE xG when defending (BAD)
- **Negative** = Player allows LESS xG when defending (GOOD)
- Units: xG per 90 minutes relative to average

### Final Rating
```
panna = offense - defense
```
Higher = better overall impact

### Example
- Erling Haaland: offense = +0.15, defense = +0.02
  - Helps team create +0.15 xG/90 on offense
  - Allows +0.02 xG/90 on defense (slightly below average defender)
  - panna = 0.15 - 0.02 = +0.13

- Virgil van Dijk: offense = -0.02, defense = -0.10
  - Creates -0.02 xG/90 on offense (average)
  - Allows -0.10 xG/90 on defense (excellent defender)
  - panna = -0.02 - (-0.10) = +0.08
