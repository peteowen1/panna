# Panna Data Dictionary

This document describes all data structures used in the panna pipeline.

## Pipeline Overview

```
01_data_collection.R  ->  01_raw_data.rds
02_data_processing.R  ->  02_processed_data.rds
03_splint_creation.R  ->  03_splints.rds
04_rapm.R             ->  04_rapm.rds
05_spm.R              ->  05_spm.rds
06_xrapm.R            ->  06_xrapm.rds
07_panna_ratings.R    ->  07_panna.rds
```

---

## 01_raw_data.rds

Raw data from worldfootballR. All columns are snake_case after `clean_column_names()`.

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
Advanced player stats per match. See worldfootballR documentation for full column list.

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
