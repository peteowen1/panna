# Package index

## Data Loading - Opta

Load Opta player statistics (263 columns, 15 leagues)

- [`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md)
  : Load Opta Player Stats
- [`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md)
  : Load Opta Shot Data
- [`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md)
  : Load Opta All Match Events (All Events with X/Y Coordinates)
- [`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md)
  : Load Opta Lineup Data
- [`load_opta_fixtures()`](https://peteowen1.github.io/panna/reference/load_opta_fixtures.md)
  : Load Opta Fixture Data
- [`load_opta_xmetrics()`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md)
  : Load Opta xG/xA/xPass Player Metrics
- [`load_opta_shot_events()`](https://peteowen1.github.io/panna/reference/load_opta_shot_events.md)
  : Load Opta Shot Events (Individual Shots with Coordinates)
- [`load_opta_events()`](https://peteowen1.github.io/panna/reference/load_opta_events.md)
  : Load Opta Match Events (Goals, Cards, Substitutions)
- [`load_opta_big5()`](https://peteowen1.github.io/panna/reference/load_opta_big5.md)
  : Load All Opta Data for Big 5 Leagues
- [`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md)
  : Load pre-computed Opta skill estimates
- [`load_opta_match_stats()`](https://peteowen1.github.io/panna/reference/load_opta_match_stats.md)
  : Load pre-computed Opta match-level stats

## Data Loading - Understat

Load Understat data (xGChain, xGBuildup)

- [`load_understat_roster()`](https://peteowen1.github.io/panna/reference/load_understat_roster.md)
  : Load Understat Roster Data
- [`load_understat_shots()`](https://peteowen1.github.io/panna/reference/load_understat_shots.md)
  : Load Understat Shots Data
- [`load_understat_metadata()`](https://peteowen1.github.io/panna/reference/load_understat_metadata.md)
  : Load Understat Metadata

## Data Loading - FBref

Load FBref match data from pannadata (StatsBomb xG)

- [`load_summary()`](https://peteowen1.github.io/panna/reference/load_summary.md)
  : Load Summary Data
- [`load_passing()`](https://peteowen1.github.io/panna/reference/load_passing.md)
  : Load Passing Data
- [`load_defense()`](https://peteowen1.github.io/panna/reference/load_defense.md)
  : Load Defense Data
- [`load_possession()`](https://peteowen1.github.io/panna/reference/load_possession.md)
  : Load Possession Data
- [`load_keeper()`](https://peteowen1.github.io/panna/reference/load_keeper.md)
  : Load Keeper Data
- [`load_shots()`](https://peteowen1.github.io/panna/reference/load_shots.md)
  : Load Shots Data
- [`load_metadata()`](https://peteowen1.github.io/panna/reference/load_metadata.md)
  : Load Metadata
- [`load_events()`](https://peteowen1.github.io/panna/reference/load_events.md)
  : Load Events Data

## Player Statistics

Aggregated player statistics by source

- [`player_opta_summary()`](https://peteowen1.github.io/panna/reference/player_opta_summary.md)
  : Opta Player Summary Statistics
- [`player_opta_passing()`](https://peteowen1.github.io/panna/reference/player_opta_passing.md)
  : Opta Player Passing Statistics
- [`player_opta_defense()`](https://peteowen1.github.io/panna/reference/player_opta_defense.md)
  : Opta Player Defense Statistics
- [`player_opta_possession()`](https://peteowen1.github.io/panna/reference/player_opta_possession.md)
  : Opta Player Possession Statistics
- [`player_opta_keeper()`](https://peteowen1.github.io/panna/reference/player_opta_keeper.md)
  : Opta Player Keeper Statistics
- [`player_opta_shots()`](https://peteowen1.github.io/panna/reference/player_opta_shots.md)
  : Opta Player Shooting Statistics
- [`player_opta_setpiece()`](https://peteowen1.github.io/panna/reference/player_opta_setpiece.md)
  : Opta Player Set Piece Statistics
- [`player_opta_xg()`](https://peteowen1.github.io/panna/reference/player_opta_xg.md)
  : Opta Player xG and xA Statistics
- [`player_opta_xpass()`](https://peteowen1.github.io/panna/reference/player_opta_xpass.md)
  : Opta Player xPass Statistics
- [`player_understat_summary()`](https://peteowen1.github.io/panna/reference/player_understat_summary.md)
  : Understat Player Summary Statistics
- [`player_fbref_summary()`](https://peteowen1.github.io/panna/reference/player_fbref_summary.md)
  : FBref Player Summary Statistics
- [`player_fbref_passing()`](https://peteowen1.github.io/panna/reference/player_fbref_passing.md)
  : FBref Player Passing Statistics
- [`player_fbref_defense()`](https://peteowen1.github.io/panna/reference/player_fbref_defense.md)
  : FBref Player Defense Statistics
- [`player_fbref_keeper()`](https://peteowen1.github.io/panna/reference/player_fbref_keeper.md)
  : FBref Player Keeper Statistics
- [`player_profile()`](https://peteowen1.github.io/panna/reference/player_profile.md)
  : Player Profile - Combined Statistics Across Sources

## RAPM Pipeline

Regularized Adjusted Plus-Minus model

- [`create_all_splints()`](https://peteowen1.github.io/panna/reference/create_all_splints.md)
  : Create splints for all matches
- [`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md)
  : Create RAPM design matrix (new structure)
- [`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)
  : Fit RAPM model
- [`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
  : Fit RAPM with SPM prior (xRAPM)
- [`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md)
  : Extract RAPM ratings from fitted model

## SPM Pipeline

Statistical Plus-Minus model

- [`aggregate_player_stats()`](https://peteowen1.github.io/panna/reference/aggregate_player_stats.md)
  : Aggregate player statistics to per-90 rates
- [`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md)
  : Fit SPM model
- [`fit_spm_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_xgb.md)
  : Fit SPM model using XGBoost
- [`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md)
  : Calculate SPM ratings for all players
- [`calculate_spm_ratings_xgb()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings_xgb.md)
  : Calculate SPM ratings using XGBoost model
- [`get_spm_feature_importance()`](https://peteowen1.github.io/panna/reference/get_spm_feature_importance.md)
  : Get top SPM feature importance

## SPM Pipeline - Opta

Opta-specific SPM model with 80+ features

- [`aggregate_opta_stats()`](https://peteowen1.github.io/panna/reference/aggregate_opta_stats.md)
  : Aggregate Opta player statistics to per-90 rates
- [`compute_match_level_opta_stats()`](https://peteowen1.github.io/panna/reference/compute_match_level_opta_stats.md)
  : Compute match-level Opta statistics with per-90 rates
- [`fit_spm_opta()`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md)
  : Fit SPM model using Opta features
- [`compare_spm_features()`](https://peteowen1.github.io/panna/reference/compare_spm_features.md)
  : Compare FBref and Opta SPM feature importance

## Panna Ratings

Combined RAPM + SPM ratings

- [`fit_panna_model()`](https://peteowen1.github.io/panna/reference/fit_panna_model.md)
  : Fit end-to-end panna model
- [`calculate_panna_rating()`](https://peteowen1.github.io/panna/reference/calculate_panna_rating.md)
  : Calculate panna rating
- [`get_panna_ratings()`](https://peteowen1.github.io/panna/reference/get_panna_ratings.md)
  : Get panna ratings from fitted model
- [`extract_xrapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_xrapm_ratings.md)
  : Extract xRAPM ratings (with prior)
- [`rank_players_panna()`](https://peteowen1.github.io/panna/reference/rank_players_panna.md)
  : Rank players by panna rating
- [`validate_panna_ratings()`](https://peteowen1.github.io/panna/reference/validate_panna_ratings.md)
  : Validate panna ratings

## Offensive/Defensive Analysis

Rating decomposition and analysis

- [`split_od_contributions()`](https://peteowen1.github.io/panna/reference/split_od_contributions.md)
  : Split O/D contributions from existing ratings
- [`calculate_od_panna()`](https://peteowen1.github.io/panna/reference/calculate_od_panna.md)
  : Calculate O-panna and D-panna
- [`categorize_player_profile()`](https://peteowen1.github.io/panna/reference/categorize_player_profile.md)
  : Categorize player profile
- [`get_top_offensive()`](https://peteowen1.github.io/panna/reference/get_top_offensive.md)
  : Get top offensive players
- [`get_top_defensive()`](https://peteowen1.github.io/panna/reference/get_top_defensive.md)
  : Get top defensive players
- [`visualize_od_scatter()`](https://peteowen1.github.io/panna/reference/visualize_od_scatter.md)
  : Visualize O/D scatter

## EPV (Expected Possession Value)

Action-level player valuation from Opta event data

- [`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
  : Convert Opta Match Events to SPADL Format
- [`create_possession_chains()`](https://peteowen1.github.io/panna/reference/create_possession_chains.md)
  : Create Possession Chains from SPADL Actions
- [`fit_epv_model()`](https://peteowen1.github.io/panna/reference/fit_epv_model.md)
  : Fit EPV Model
- [`calculate_action_epv()`](https://peteowen1.github.io/panna/reference/calculate_action_epv.md)
  : Calculate Action EPV Values
- [`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md)
  : Assign EPV Credit with Turnover Handling
- [`aggregate_player_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_epv.md)
  : Aggregate Player EPV Metrics
- [`save_epv_model()`](https://peteowen1.github.io/panna/reference/save_epv_model.md)
  : Save EPV Model
- [`load_epv_model()`](https://peteowen1.github.io/panna/reference/load_epv_model.md)
  : Load EPV Model
- [`pb_download_epv_models()`](https://peteowen1.github.io/panna/reference/pb_download_epv_models.md)
  : Download EPV Models from GitHub Releases
- [`validate_epv_model()`](https://peteowen1.github.io/panna/reference/validate_epv_model.md)
  : Validate EPV Model
- [`fit_xg_model()`](https://peteowen1.github.io/panna/reference/fit_xg_model.md)
  : Fit xG Model using XGBoost
- [`predict_xg()`](https://peteowen1.github.io/panna/reference/predict_xg.md)
  : Predict xG for Shots
- [`save_xg_model()`](https://peteowen1.github.io/panna/reference/save_xg_model.md)
  : Save xG Model
- [`load_xg_model()`](https://peteowen1.github.io/panna/reference/load_xg_model.md)
  : Load Pre-trained xG Model
- [`validate_xg_model()`](https://peteowen1.github.io/panna/reference/validate_xg_model.md)
  : Validate xG Model Performance
- [`aggregate_player_xmetrics()`](https://peteowen1.github.io/panna/reference/aggregate_player_xmetrics.md)
  : Aggregate Player-Level xG/xA/xPass Metrics
- [`fit_xpass_model()`](https://peteowen1.github.io/panna/reference/fit_xpass_model.md)
  : Fit xPass Model using XGBoost
- [`predict_xpass()`](https://peteowen1.github.io/panna/reference/predict_xpass.md)
  : Predict Pass Completion Probability
- [`save_xpass_model()`](https://peteowen1.github.io/panna/reference/save_xpass_model.md)
  : Save xPass Model
- [`load_xpass_model()`](https://peteowen1.github.io/panna/reference/load_xpass_model.md)
  : Load Pre-trained xPass Model
- [`split_pass_credit()`](https://peteowen1.github.io/panna/reference/split_pass_credit.md)
  : Split Pass Credit (Legacy Helper)

## Estimated Skills

Bayesian decay-weighted skill estimation with position-specific priors

- [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)
  : Estimate player skills using Bayesian conjugate priors with decay
  weighting
- [`estimate_player_skills_at_date()`](https://peteowen1.github.io/panna/reference/estimate_player_skills_at_date.md)
  : Estimate player skills at a specific date
- [`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md)
  : Generate a player's estimated skill profile
- [`inspect_skill()`](https://peteowen1.github.io/panna/reference/inspect_skill.md)
  : Inspect skill estimate breakdown for a single stat
- [`aggregate_skills_for_spm()`](https://peteowen1.github.io/panna/reference/aggregate_skills_for_spm.md)
  : Aggregate skills for SPM input (one row per player per season)
- [`backtest_skill_predictions()`](https://peteowen1.github.io/panna/reference/backtest_skill_predictions.md)
  : Backtest skill predictions against actual match performance
- [`adjust_match_stats_for_context()`](https://peteowen1.github.io/panna/reference/adjust_match_stats_for_context.md)
  : Adjust match stats for opponent quality, venue, and league level
- [`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md)
  : Get default decay parameters for skill estimation
- [`compute_position_multipliers()`](https://peteowen1.github.io/panna/reference/compute_position_multipliers.md)
  : Compute position-specific multipliers for prior centers
- [`optimize_all_priors()`](https://peteowen1.github.io/panna/reference/optimize_all_priors.md)
  : Optimize prior strengths for all stats
- [`optimize_stat_prior()`](https://peteowen1.github.io/panna/reference/optimize_stat_prior.md)
  : Optimize Bayesian prior strength for a single stat
- [`get_stat_tiers()`](https://peteowen1.github.io/panna/reference/get_stat_tiers.md)
  : Get stat tiers for optimization

## Match Prediction

XGBoost Poisson/multinomial match outcome prediction

- [`aggregate_lineup_ratings()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_ratings.md)
  : Aggregate Player Ratings to Team Level
- [`aggregate_lineup_skills()`](https://peteowen1.github.io/panna/reference/aggregate_lineup_skills.md)
  : Aggregate Player Skills to Team-Level Features
- [`init_team_elos()`](https://peteowen1.github.io/panna/reference/init_team_elos.md)
  : Initialize Team Elo Ratings
- [`update_elo()`](https://peteowen1.github.io/panna/reference/update_elo.md)
  : Update Elo Ratings After a Match
- [`compute_match_elos()`](https://peteowen1.github.io/panna/reference/compute_match_elos.md)
  : Compute Elo Ratings for All Matches
- [`compute_team_rolling_features()`](https://peteowen1.github.io/panna/reference/compute_team_rolling_features.md)
  : Compute Team Rolling Features
- [`fit_goals_xgb()`](https://peteowen1.github.io/panna/reference/fit_goals_xgb.md)
  : Fit XGBoost Poisson Model for Goal Prediction
- [`fit_outcome_xgb()`](https://peteowen1.github.io/panna/reference/fit_outcome_xgb.md)
  : Fit XGBoost Multinomial Model for Match Outcome
- [`predict_match()`](https://peteowen1.github.io/panna/reference/predict_match.md)
  : Predict Match Outcome Probabilities
- [`compute_multiclass_logloss()`](https://peteowen1.github.io/panna/reference/compute_multiclass_logloss.md)
  : Compute Multi-Class Log Loss
- [`calibration_table()`](https://peteowen1.github.io/panna/reference/calibration_table.md)
  : Create Calibration Table

## Feature Engineering

Advanced feature creation

- [`create_player_feature_matrix()`](https://peteowen1.github.io/panna/reference/create_player_feature_matrix.md)
  : Create full player feature matrix

## Data Distribution

GitHub Releases upload/download

- [`pb_download_source()`](https://peteowen1.github.io/panna/reference/pb_download_source.md)
  : Download parquet files by source type
- [`pb_download_data()`](https://peteowen1.github.io/panna/reference/pb_download_data.md)
  : Download data from GitHub Releases
- [`pb_download_parquet()`](https://peteowen1.github.io/panna/reference/pb_download_parquet.md)
  : Download parquet files from GitHub Releases
- [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)
  : Download Opta Data to Local Directory
- [`pb_download_predictions()`](https://peteowen1.github.io/panna/reference/pb_download_predictions.md)
  : Download match predictions from GitHub Releases
- [`pb_upload_source()`](https://peteowen1.github.io/panna/reference/pb_upload_source.md)
  : Upload parquet files by source type
- [`pb_upload_data()`](https://peteowen1.github.io/panna/reference/pb_upload_data.md)
  : Upload data to GitHub Releases
- [`pb_upload_parquet()`](https://peteowen1.github.io/panna/reference/pb_upload_parquet.md)
  : Upload parquet files to GitHub Releases
- [`pb_upload_consolidated()`](https://peteowen1.github.io/panna/reference/pb_upload_consolidated.md)
  : Upload Consolidated Parquet Files to GitHub Releases
- [`pb_sync_data()`](https://peteowen1.github.io/panna/reference/pb_sync_data.md)
  : Sync local data with GitHub Releases
- [`pb_list_data()`](https://peteowen1.github.io/panna/reference/pb_list_data.md)
  : List files in GitHub Release
- [`pb_list_sources()`](https://peteowen1.github.io/panna/reference/pb_list_sources.md)
  : List releases by source type
- [`pb_status()`](https://peteowen1.github.io/panna/reference/pb_status.md)
  : Check if local data is in sync with GitHub Releases
- [`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md)
  : Load match predictions

## Scraping - FBref

FBref data scraping functions

- [`scrape_fbref_matches()`](https://peteowen1.github.io/panna/reference/scrape_fbref_matches.md)
  : Scrape FBref match data directly
- [`scrape_comp_season()`](https://peteowen1.github.io/panna/reference/scrape_comp_season.md)
  : Scrape a competition-season
- [`scrape_fixtures()`](https://peteowen1.github.io/panna/reference/scrape_fixtures.md)
  : Scrape match URLs from fixtures page
- [`fbref_competitions`](https://peteowen1.github.io/panna/reference/fbref_competitions.md)
  : FBref competition metadata
- [`list_competitions()`](https://peteowen1.github.io/panna/reference/list_competitions.md)
  : List competitions by type

## Scraping - Understat

Understat data scraping functions

- [`scrape_understat_season()`](https://peteowen1.github.io/panna/reference/scrape_understat_season.md)
  : Scrape full Understat season
- [`scrape_understat_matches()`](https://peteowen1.github.io/panna/reference/scrape_understat_matches.md)
  : Scrape multiple Understat matches
- [`scrape_understat_match()`](https://peteowen1.github.io/panna/reference/scrape_understat_match.md)
  : Scrape single Understat match
- [`scrape_understat_match_range()`](https://peteowen1.github.io/panna/reference/scrape_understat_match_range.md)
  : Scrape matches by Understat ID range
- [`scrape_understat_fixtures()`](https://peteowen1.github.io/panna/reference/scrape_understat_fixtures.md)
  : Scrape fixtures from Understat league page
- [`bulk_scrape_understat()`](https://peteowen1.github.io/panna/reference/bulk_scrape_understat.md)
  : Bulk scrape Understat matches with auto-detection
- [`smart_scrape_understat()`](https://peteowen1.github.io/panna/reference/smart_scrape_understat.md)
  : Smart scrape Understat with per-league tracking
- [`load_understat_manifest()`](https://peteowen1.github.io/panna/reference/load_understat_manifest.md)
  : Load Understat manifest from parquet file
- [`save_understat_manifest()`](https://peteowen1.github.io/panna/reference/save_understat_manifest.md)
  : Save Understat manifest to parquet file
- [`understat_competitions`](https://peteowen1.github.io/panna/reference/understat_competitions.md)
  : Understat competition metadata
- [`list_understat_competitions()`](https://peteowen1.github.io/panna/reference/list_understat_competitions.md)
  : List Understat competitions

## Cache Management

Local and remote cache operations

- [`pannadata_dir()`](https://peteowen1.github.io/panna/reference/pannadata_dir.md)
  : Get or set pannadata directory
- [`opta_data_dir()`](https://peteowen1.github.io/panna/reference/opta_data_dir.md)
  : Get or Set Opta Data Directory
- [`clear_remote_cache()`](https://peteowen1.github.io/panna/reference/clear_remote_cache.md)
  : Clear Remote Data Cache

## Parquet Operations

Building and querying parquet files

- [`build_parquet()`](https://peteowen1.github.io/panna/reference/build_parquet.md)
  : Build parquet file from RDS files for a league-season
- [`build_all_parquet()`](https://peteowen1.github.io/panna/reference/build_all_parquet.md)
  : Build all parquet files
- [`build_consolidated_parquet()`](https://peteowen1.github.io/panna/reference/build_consolidated_parquet.md)
  : Build Consolidated Parquet Files for Remote Queries
- [`build_consolidated_understat_parquet()`](https://peteowen1.github.io/panna/reference/build_consolidated_understat_parquet.md)
  : Build Consolidated Understat Parquet Files

## Utilities

Helper functions

- [`clean_column_names()`](https://peteowen1.github.io/panna/reference/clean_column_names.md)
  : Clean column names to snake_case
- [`safe_divide()`](https://peteowen1.github.io/panna/reference/safe_divide.md)
  : Safe division handling division by zero
- [`per_90()`](https://peteowen1.github.io/panna/reference/per_90.md) :
  Calculate minutes per 90
- [`validate_seasons()`](https://peteowen1.github.io/panna/reference/validate_seasons.md)
  : Validate season input
- [`validate_dataframe()`](https://peteowen1.github.io/panna/reference/validate_dataframe.md)
  : Validate data frame input
- [`get_big5_leagues()`](https://peteowen1.github.io/panna/reference/get_big5_leagues.md)
  : Big 5 European league configurations

## Competition Metadata

League and competition information

- [`list_opta_seasons()`](https://peteowen1.github.io/panna/reference/list_opta_seasons.md)
  : List Available Opta Seasons
- [`list_opta_leagues()`](https://peteowen1.github.io/panna/reference/list_opta_leagues.md)
  : List Available Opta Leagues
- [`get_opta_columns()`](https://peteowen1.github.io/panna/reference/get_opta_columns.md)
  : Get Opta Column Names

## Validation

Model and data validation

- [`validate_predictive_power()`](https://peteowen1.github.io/panna/reference/validate_predictive_power.md)
  : Validate predictive power
- [`validate_spm_prediction()`](https://peteowen1.github.io/panna/reference/validate_spm_prediction.md)
  : Validate SPM prediction accuracy
- [`validate_epv_model()`](https://peteowen1.github.io/panna/reference/validate_epv_model.md)
  : Validate EPV Model
- [`validate_xg_model()`](https://peteowen1.github.io/panna/reference/validate_xg_model.md)
  : Validate xG Model Performance
- [`validate_panna_ratings()`](https://peteowen1.github.io/panna/reference/validate_panna_ratings.md)
  : Validate panna ratings

## Analysis & Reporting

Analysis helpers and reporting

- [`compare_panna_rapm_spm()`](https://peteowen1.github.io/panna/reference/compare_panna_rapm_spm.md)
  : Compare panna, RAPM, and SPM ratings
- [`generate_panna_report()`](https://peteowen1.github.io/panna/reference/generate_panna_report.md)
  : Generate prediction report
- [`report_season_ranges()`](https://peteowen1.github.io/panna/reference/report_season_ranges.md)
  : Report season ranges for all data types

## Constants

Package-wide constants and thresholds

- [`BETA_PRIOR_ALPHA`](https://peteowen1.github.io/panna/reference/BETA_PRIOR_ALPHA.md)
  : Beta prior alpha for finishing modifier (shrinkage toward 1.0)
- [`CHAIN_TIME_GAP_SECONDS`](https://peteowen1.github.io/panna/reference/CHAIN_TIME_GAP_SECONDS.md)
  : Time gap threshold for chain breaks (seconds)
- [`CONFIDENCE_LEVEL`](https://peteowen1.github.io/panna/reference/CONFIDENCE_LEVEL.md)
  : Default confidence level for statistical intervals
- [`HALFTIME_MINUTE`](https://peteowen1.github.io/panna/reference/HALFTIME_MINUTE.md)
  : Minute marking halftime (end of first half)
- [`MINUTES_PER_MATCH`](https://peteowen1.github.io/panna/reference/MINUTES_PER_MATCH.md)
  : Minutes per regulation match
- [`MIN_GAMES_FOR_PADDING`](https://peteowen1.github.io/panna/reference/MIN_GAMES_FOR_PADDING.md)
  : Default minimum games for Bayesian padding full weight
- [`MIN_MINUTES_FEATURES`](https://peteowen1.github.io/panna/reference/MIN_MINUTES_FEATURES.md)
  : Default minimum minutes for player feature matrix
- [`MIN_MINUTES_RAPM`](https://peteowen1.github.io/panna/reference/MIN_MINUTES_RAPM.md)
  : Default minimum minutes for RAPM matrix inclusion
- [`MIN_MINUTES_SPM`](https://peteowen1.github.io/panna/reference/MIN_MINUTES_SPM.md)
  : Default minimum minutes for SPM/player stats functions
- [`MIN_SEQUENCES_PER_MATCH`](https://peteowen1.github.io/panna/reference/MIN_SEQUENCES_PER_MATCH.md)
  : Minimum estimated sequences per team per match
- [`MIN_SHOTS_FOR_FINISHING`](https://peteowen1.github.io/panna/reference/MIN_SHOTS_FOR_FINISHING.md)
  : Default minimum shots for finishing modifier calculation
- [`MIN_WEIGHT_DURATION`](https://peteowen1.github.io/panna/reference/MIN_WEIGHT_DURATION.md)
  : Minimum weight threshold for duration-based weighting
- [`PENALTY_XG`](https://peteowen1.github.io/panna/reference/PENALTY_XG.md)
  : Default penalty kick xG value
- [`PLAYERS_PER_TEAM`](https://peteowen1.github.io/panna/reference/PLAYERS_PER_TEAM.md)
  : Players per team in standard lineup
- [`SIX_YARD_X_MIN`](https://peteowen1.github.io/panna/reference/SIX_YARD_X_MIN.md)
  : Six-yard box x threshold (attacking end)
- [`SIX_YARD_Y_MAX`](https://peteowen1.github.io/panna/reference/SIX_YARD_Y_MAX.md)
  : Six-yard box y upper bound
- [`SIX_YARD_Y_MIN`](https://peteowen1.github.io/panna/reference/SIX_YARD_Y_MIN.md)
  : Six-yard box y lower bound
- [`TOUCHES_PER_SEQUENCE`](https://peteowen1.github.io/panna/reference/TOUCHES_PER_SEQUENCE.md)
  : Average touches per possession sequence (approximation)
- [`XG_MAX`](https://peteowen1.github.io/panna/reference/XG_MAX.md) :
  Maximum xG value (caps extreme predictions)
- [`XG_MIN`](https://peteowen1.github.io/panna/reference/XG_MIN.md) :
  Minimum xG value (prevents 0 in log calculations)

## Deprecated

Deprecated functions (use alternatives instead)

- [`fit_epv_scoring_model()`](https://peteowen1.github.io/panna/reference/fit_epv_scoring_model.md)
  **\[deprecated\]** : Deprecated: Fit EPV Scoring Model
- [`fit_epv_conceding_model()`](https://peteowen1.github.io/panna/reference/fit_epv_conceding_model.md)
  **\[deprecated\]** : Deprecated: Fit EPV Conceding Model
- [`assign_pass_credit()`](https://peteowen1.github.io/panna/reference/assign_pass_credit.md)
  **\[deprecated\]** : Deprecated: Assign Pass Credit
