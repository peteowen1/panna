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
- [`create_match_splints()`](https://peteowen1.github.io/panna/reference/create_match_splints.md)
  : Create splints for a single match
- [`create_splint_boundaries()`](https://peteowen1.github.io/panna/reference/create_splint_boundaries.md)
  : Create splint boundaries
- [`assign_players_to_splints()`](https://peteowen1.github.io/panna/reference/assign_players_to_splints.md)
  : Assign players to splints
- [`calculate_splint_npxgd()`](https://peteowen1.github.io/panna/reference/calculate_splint_npxgd.md)
  : Calculate splint-level xG differential
- [`create_rapm_design_matrix()`](https://peteowen1.github.io/panna/reference/create_rapm_design_matrix.md)
  : Create RAPM design matrix (new structure)
- [`fit_rapm()`](https://peteowen1.github.io/panna/reference/fit_rapm.md)
  : Fit RAPM model
- [`fit_rapm_with_prior()`](https://peteowen1.github.io/panna/reference/fit_rapm_with_prior.md)
  : Fit RAPM with SPM prior (xRAPM)
- [`extract_rapm_coefficients()`](https://peteowen1.github.io/panna/reference/extract_rapm_coefficients.md)
  : Extract RAPM coefficients
- [`extract_rapm_ratings()`](https://peteowen1.github.io/panna/reference/extract_rapm_ratings.md)
  : Extract RAPM ratings from fitted model

## SPM Pipeline

Statistical Plus-Minus model

- [`aggregate_player_stats()`](https://peteowen1.github.io/panna/reference/aggregate_player_stats.md)
  : Aggregate player statistics to per-90 rates
- [`aggregate_player_season_stats()`](https://peteowen1.github.io/panna/reference/aggregate_player_season_stats.md)
  : Aggregate player season stats
- [`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md)
  : Fit SPM model
- [`fit_spm_xgb()`](https://peteowen1.github.io/panna/reference/fit_spm_xgb.md)
  : Fit SPM model using XGBoost
- [`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md)
  : Calculate SPM ratings for all players
- [`calculate_spm_ratings_xgb()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings_xgb.md)
  : Calculate SPM ratings using XGBoost model
- [`create_spm_prior()`](https://peteowen1.github.io/panna/reference/create_spm_prior.md)
  : Create SPM prior vector for RAPM
- [`build_prior_vector()`](https://peteowen1.github.io/panna/reference/build_prior_vector.md)
  : Build prior vector for RAPM from SPM predictions
- [`extract_spm_coefficients()`](https://peteowen1.github.io/panna/reference/extract_spm_coefficients.md)
  : Extract SPM coefficients
- [`get_spm_feature_importance()`](https://peteowen1.github.io/panna/reference/get_spm_feature_importance.md)
  : Get top SPM feature importance
- [`prepare_spm_regression_data()`](https://peteowen1.github.io/panna/reference/prepare_spm_regression_data.md)
  : Prepare SPM regression data

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
- [`extract_od_rapm_coefficients()`](https://peteowen1.github.io/panna/reference/extract_od_rapm_coefficients.md)
  : Extract offensive/defensive RAPM coefficients
- [`calculate_offensive_spm()`](https://peteowen1.github.io/panna/reference/calculate_offensive_spm.md)
  : Calculate offensive SPM
- [`calculate_defensive_spm()`](https://peteowen1.github.io/panna/reference/calculate_defensive_spm.md)
  : Calculate defensive SPM
- [`calculate_spm_blend()`](https://peteowen1.github.io/panna/reference/calculate_spm_blend.md)
  : Calculate blended SPM ratings from Elastic Net and XGBoost
- [`categorize_player_profile()`](https://peteowen1.github.io/panna/reference/categorize_player_profile.md)
  : Categorize player profile
- [`get_top_offensive()`](https://peteowen1.github.io/panna/reference/get_top_offensive.md)
  : Get top offensive players
- [`get_top_defensive()`](https://peteowen1.github.io/panna/reference/get_top_defensive.md)
  : Get top defensive players
- [`prepare_od_scatter_data()`](https://peteowen1.github.io/panna/reference/prepare_od_scatter_data.md)
  : Create O/D scatter plot data
- [`visualize_od_scatter()`](https://peteowen1.github.io/panna/reference/visualize_od_scatter.md)
  : Visualize O/D scatter

## EPV (Expected Possession Value)

Action-level player valuation from Opta event data

- [`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
  : Convert Opta Match Events to SPADL Format
- [`create_possession_chains()`](https://peteowen1.github.io/panna/reference/create_possession_chains.md)
  : Create Possession Chains from SPADL Actions
- [`classify_chain_outcomes()`](https://peteowen1.github.io/panna/reference/classify_chain_outcomes.md)
  : Classify Chain Outcomes
- [`label_actions_with_outcomes()`](https://peteowen1.github.io/panna/reference/label_actions_with_outcomes.md)
  : Label Actions with Chain Outcomes
- [`create_epv_features()`](https://peteowen1.github.io/panna/reference/create_epv_features.md)
  : Create Game State Features for EPV
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

## Data Processing

Raw data processing and cleaning

- [`process_all_data()`](https://peteowen1.github.io/panna/reference/process_all_data.md)
  : Process all raw data
- [`process_match_results()`](https://peteowen1.github.io/panna/reference/process_match_results.md)
  : Process and clean match results
- [`process_match_lineups()`](https://peteowen1.github.io/panna/reference/process_match_lineups.md)
  : Process match lineups
- [`process_match_events()`](https://peteowen1.github.io/panna/reference/process_match_events.md)
  : Process match events
- [`process_shooting_data()`](https://peteowen1.github.io/panna/reference/process_shooting_data.md)
  : Process shooting data
- [`process_advanced_stats()`](https://peteowen1.github.io/panna/reference/process_advanced_stats.md)
  : Process advanced match stats
- [`merge_processed_data()`](https://peteowen1.github.io/panna/reference/merge_processed_data.md)
  : Merge all processed data
- [`derive_events_from_shooting()`](https://peteowen1.github.io/panna/reference/derive_events_from_shooting.md)
  : Derive goal events from shooting data
- [`derive_lineups_from_stats()`](https://peteowen1.github.io/panna/reference/derive_lineups_from_stats.md)
  : Get match lineups from advanced stats (no scraping needed)
- [`filter_bad_xg_data()`](https://peteowen1.github.io/panna/reference/filter_bad_xg_data.md)
  : Detect and filter bad xG data from splints

## Feature Engineering

Advanced feature creation

- [`create_player_feature_matrix()`](https://peteowen1.github.io/panna/reference/create_player_feature_matrix.md)
  : Create full player feature matrix
- [`create_offensive_features()`](https://peteowen1.github.io/panna/reference/create_offensive_features.md)
  : Create offensive features
- [`create_defensive_features()`](https://peteowen1.github.io/panna/reference/create_defensive_features.md)
  : Create defensive features
- [`calculate_per_100_sequences()`](https://peteowen1.github.io/panna/reference/calculate_per_100_sequences.md)
  : Convert statistics to per-100-sequences rate
- [`calculate_team_sequences()`](https://peteowen1.github.io/panna/reference/calculate_team_sequences.md)
  : Calculate team possession sequences
- [`calculate_finishing_modifier()`](https://peteowen1.github.io/panna/reference/calculate_finishing_modifier.md)
  : Calculate player finishing modifier

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
- [`get_latest_release()`](https://peteowen1.github.io/panna/reference/get_latest_release.md)
  : Get Release Info
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
- [`fetch_match_page()`](https://peteowen1.github.io/panna/reference/fetch_match_page.md)
  : Fetch FBref match page HTML
- [`parse_match_page()`](https://peteowen1.github.io/panna/reference/parse_match_page.md)
  : Parse all tables from FBref match page
- [`extract_match_events()`](https://peteowen1.github.io/panna/reference/extract_match_events.md)
  : Extract match events with timing
- [`fbref_competitions`](https://peteowen1.github.io/panna/reference/fbref_competitions.md)
  : FBref competition metadata
- [`list_competitions()`](https://peteowen1.github.io/panna/reference/list_competitions.md)
  : List competitions by type
- [`get_fbref_comp_id()`](https://peteowen1.github.io/panna/reference/get_fbref_comp_id.md)
  : Get FBref competition ID
- [`get_fbref_schedule_url()`](https://peteowen1.github.io/panna/reference/get_fbref_schedule_url.md)
  : Get FBref schedule URL
- [`get_fbref_session()`](https://peteowen1.github.io/panna/reference/get_fbref_session.md)
  : Get or create FBref session
- [`reset_fbref_session()`](https://peteowen1.github.io/panna/reference/reset_fbref_session.md)
  : Reset FBref session

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
- [`fetch_understat_page()`](https://peteowen1.github.io/panna/reference/fetch_understat_page.md)
  : Fetch Understat page HTML
- [`aggregate_understat_data()`](https://peteowen1.github.io/panna/reference/aggregate_understat_data.md)
  : Aggregate cached Understat data
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
- [`get_understat_league_url()`](https://peteowen1.github.io/panna/reference/get_understat_league_url.md)
  : Get Understat league URL
- [`get_understat_match_url()`](https://peteowen1.github.io/panna/reference/get_understat_match_url.md)
  : Get Understat match URL
- [`get_understat_seasons()`](https://peteowen1.github.io/panna/reference/get_understat_seasons.md)
  : Get available Understat seasons
- [`get_understat_slug()`](https://peteowen1.github.io/panna/reference/get_understat_slug.md)
  : Get Understat URL slug
- [`get_understat_session()`](https://peteowen1.github.io/panna/reference/get_understat_session.md)
  : Get or create Understat session
- [`reset_understat_session()`](https://peteowen1.github.io/panna/reference/reset_understat_session.md)
  : Reset Understat session
- [`is_understat_league()`](https://peteowen1.github.io/panna/reference/is_understat_league.md)
  : Check if league is available on Understat
- [`understat_league_to_code()`](https://peteowen1.github.io/panna/reference/understat_league_to_code.md)
  : Map Understat league name to our code

## Cache Management

Local and remote cache operations

- [`pannadata_dir()`](https://peteowen1.github.io/panna/reference/pannadata_dir.md)
  : Get or set pannadata directory
- [`opta_data_dir()`](https://peteowen1.github.io/panna/reference/opta_data_dir.md)
  : Get or Set Opta Data Directory
- [`load_cached_data()`](https://peteowen1.github.io/panna/reference/load_cached_data.md)
  : Load cached data for a specific type
- [`save_cached_data()`](https://peteowen1.github.io/panna/reference/save_cached_data.md)
  : Save data to cache
- [`list_cached_matches()`](https://peteowen1.github.io/panna/reference/list_cached_matches.md)
  : List cached matches
- [`get_cached_match_urls()`](https://peteowen1.github.io/panna/reference/get_cached_match_urls.md)
  : Get cached match URLs from metadata
- [`get_cached_fbref_ids()`](https://peteowen1.github.io/panna/reference/get_cached_fbref_ids.md)
  : Get cached match IDs for a league-season (fast batch version)
- [`get_cached_understat_ids()`](https://peteowen1.github.io/panna/reference/get_cached_understat_ids.md)
  : Get cached Understat match IDs
- [`aggregate_cached_matches()`](https://peteowen1.github.io/panna/reference/aggregate_cached_matches.md)
  : Aggregate cached match data
- [`clear_remote_cache()`](https://peteowen1.github.io/panna/reference/clear_remote_cache.md)
  : Clear Remote Data Cache
- [`clear_remote_opta_cache()`](https://peteowen1.github.io/panna/reference/clear_remote_opta_cache.md)
  : Clear remote Opta data cache

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
- [`query_local_parquet()`](https://peteowen1.github.io/panna/reference/query_local_parquet.md)
  : Query Local Parquet with SQL
- [`query_remote_parquet()`](https://peteowen1.github.io/panna/reference/query_remote_parquet.md)
  : Query Remote Parquet with SQL (Primary Method)
- [`get_remote_tables()`](https://peteowen1.github.io/panna/reference/get_remote_tables.md)
  : Get Available Remote Tables

## Utilities

Helper functions

- [`clean_column_names()`](https://peteowen1.github.io/panna/reference/clean_column_names.md)
  : Clean column names to snake_case
- [`clean_player_name()`](https://peteowen1.github.io/panna/reference/clean_player_name.md)
  : Clean player name for matching
- [`standardize_player_names()`](https://peteowen1.github.io/panna/reference/standardize_player_names.md)
  : Standardize player names
- [`standardize_team_names()`](https://peteowen1.github.io/panna/reference/standardize_team_names.md)
  : Standardize team names
- [`create_match_id()`](https://peteowen1.github.io/panna/reference/create_match_id.md)
  : Create unique match ID
- [`create_player_id()`](https://peteowen1.github.io/panna/reference/create_player_id.md)
  : Create unique player ID
- [`extract_fbref_player_id()`](https://peteowen1.github.io/panna/reference/extract_fbref_player_id.md)
  : Extract FBref player ID from href
- [`extract_season_from_match_id()`](https://peteowen1.github.io/panna/reference/extract_season_from_match_id.md)
  : Extract season from match_id
- [`extract_season_end_year_from_match_id()`](https://peteowen1.github.io/panna/reference/extract_season_end_year_from_match_id.md)
  : Extract season end year from match_id
- [`safe_divide()`](https://peteowen1.github.io/panna/reference/safe_divide.md)
  : Safe division handling division by zero
- [`per_90()`](https://peteowen1.github.io/panna/reference/per_90.md) :
  Calculate minutes per 90
- [`calculate_npxg()`](https://peteowen1.github.io/panna/reference/calculate_npxg.md)
  : Calculate non-penalty xG and goals
- [`validate_seasons()`](https://peteowen1.github.io/panna/reference/validate_seasons.md)
  : Validate season input
- [`validate_dataframe()`](https://peteowen1.github.io/panna/reference/validate_dataframe.md)
  : Validate data frame input
- [`get_seasons_since()`](https://peteowen1.github.io/panna/reference/get_seasons_since.md)
  : Get all seasons since a start year
- [`get_big5_leagues()`](https://peteowen1.github.io/panna/reference/get_big5_leagues.md)
  : Big 5 European league configurations

## Season Conversion

Season format conversion between sources

- [`fbref_to_understat_season()`](https://peteowen1.github.io/panna/reference/fbref_to_understat_season.md)
  : Convert FBref season to Understat season
- [`understat_to_fbref_season()`](https://peteowen1.github.io/panna/reference/understat_to_fbref_season.md)
  : Convert Understat season to FBref season

## Competition Metadata

League and competition information

- [`get_tournament_years()`](https://peteowen1.github.io/panna/reference/get_tournament_years.md)
  : Get tournament years with available data
- [`is_tournament_competition()`](https://peteowen1.github.io/panna/reference/is_tournament_competition.md)
  : Check if competition uses tournament (year-only) format
- [`list_opta_seasons()`](https://peteowen1.github.io/panna/reference/list_opta_seasons.md)
  : List Available Opta Seasons
- [`list_opta_leagues()`](https://peteowen1.github.io/panna/reference/list_opta_leagues.md)
  : List Available Opta Leagues
- [`suggest_opta_seasons()`](https://peteowen1.github.io/panna/reference/suggest_opta_seasons.md)
  : Suggest Available Seasons for an Opta League
- [`get_opta_columns()`](https://peteowen1.github.io/panna/reference/get_opta_columns.md)
  : Get Opta Column Names

## Validation

Model and data validation

- [`validate_data_completeness()`](https://peteowen1.github.io/panna/reference/validate_data_completeness.md)
  : Check and report data completeness
- [`validate_predictive_power()`](https://peteowen1.github.io/panna/reference/validate_predictive_power.md)
  : Validate predictive power
- [`validate_spm_prediction()`](https://peteowen1.github.io/panna/reference/validate_spm_prediction.md)
  : Validate SPM prediction accuracy

## Analysis & Reporting

Analysis helpers and reporting

- [`compare_panna_rapm_spm()`](https://peteowen1.github.io/panna/reference/compare_panna_rapm_spm.md)
  : Compare panna, RAPM, and SPM ratings
- [`generate_panna_report()`](https://peteowen1.github.io/panna/reference/generate_panna_report.md)
  : Generate prediction report
- [`get_covariate_effects()`](https://peteowen1.github.io/panna/reference/get_covariate_effects.md)
  : Get covariate effects from fitted model
- [`aggregate_rapm_by_team()`](https://peteowen1.github.io/panna/reference/aggregate_rapm_by_team.md)
  : Aggregate RAPM by team
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

## Internal

Internal functions (advanced users)

- [`prepare_rapm_data()`](https://peteowen1.github.io/panna/reference/prepare_rapm_data.md)
  : Prepare RAPM data for model fitting
- [`get_splint_players()`](https://peteowen1.github.io/panna/reference/get_splint_players.md)
  : Get players on pitch for a splint
- [`apply_bayesian_padding()`](https://peteowen1.github.io/panna/reference/apply_bayesian_padding.md)
  : Apply Bayesian padding to statistics
- [`migrate_metadata_tables_available()`](https://peteowen1.github.io/panna/reference/migrate_metadata_tables_available.md)
  : Migrate old metadata files to include tables_available field
