# Pipeline Walkthrough

This vignette walks through the full panna rating pipeline using
synthetic data. No external data downloads are required – everything
runs self-contained.

## Setup

``` r
library(panna)
set.seed(42)
```

## Step 1: Create Synthetic Match Data

In production, you would load data with
[`load_summary()`](https://peteowen1.github.io/panna/reference/load_summary.md),
[`load_metadata()`](https://peteowen1.github.io/panna/reference/load_metadata.md),
etc. Here we generate synthetic match data that mirrors the structure of
processed FBref data.

``` r
n_matches <- 12
home_teams <- paste0("Team_", LETTERS[1:6])
away_teams <- paste0("Team_", LETTERS[7:12])
match_ids <- paste0("match_", seq_len(n_matches))

# Match results with xG
results <- data.frame(
  match_id = match_ids,
  home_team = rep(home_teams, length.out = n_matches),
  away_team = rep(away_teams, length.out = n_matches),
  home_score = sample(0:3, n_matches, replace = TRUE),
  away_score = sample(0:3, n_matches, replace = TRUE),
  home_xg = runif(n_matches, 0.5, 3),
  away_xg = runif(n_matches, 0.5, 3),
  stringsAsFactors = FALSE
)

# Player pool: 50 players split across teams
all_player_ids <- paste0("player_", 1:50)
all_player_names <- paste("Player", 1:50)

# Generate lineups (11 starters per team per match)
lineup_rows <- list()
for (i in seq_len(n_matches)) {
  home_idx <- sample(1:25, 11)
  away_idx <- sample(26:50, 11)

  for (j in seq_along(home_idx)) {
    lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
      match_id = match_ids[i], player_id = all_player_ids[home_idx[j]],
      player_name = all_player_names[home_idx[j]],
      team = results$home_team[i], is_home = TRUE, is_starter = TRUE,
      on_minute = 0, off_minute = 90, minutes = 90, stringsAsFactors = FALSE
    )
  }
  for (j in seq_along(away_idx)) {
    lineup_rows[[length(lineup_rows) + 1]] <- data.frame(
      match_id = match_ids[i], player_id = all_player_ids[away_idx[j]],
      player_name = all_player_names[away_idx[j]],
      team = results$away_team[i], is_home = FALSE, is_starter = TRUE,
      on_minute = 0, off_minute = 90, minutes = 90, stringsAsFactors = FALSE
    )
  }
}
lineups <- do.call(rbind, lineup_rows)

# Generate shots with xG
shot_rows <- list()
for (i in seq_len(n_matches)) {
  n_shots <- sample(15:25, 1)
  match_lineups <- lineups[lineups$match_id == match_ids[i], ]
  shot_players <- match_lineups[sample(nrow(match_lineups), n_shots, replace = TRUE), ]
  shot_rows[[i]] <- data.frame(
    match_id = rep(match_ids[i], n_shots),
    minute = sort(sample(1:90, n_shots, replace = TRUE)),
    player_id = shot_players$player_id,
    player_name = shot_players$player_name,
    team = shot_players$team, is_home = shot_players$is_home,
    xg = pmin(runif(n_shots, 0.02, 0.5), 0.95),
    is_goal = rbinom(n_shots, 1, 0.1) == 1,
    is_penalty = FALSE, is_own_goal = FALSE, stringsAsFactors = FALSE
  )
}
shooting <- do.call(rbind, shot_rows)

# Generate events (goals + substitutions)
event_rows <- list()
for (i in seq_len(n_matches)) {
  goals <- shooting[shooting$match_id == match_ids[i] & shooting$is_goal, ]
  if (nrow(goals) > 0) {
    event_rows[[length(event_rows) + 1]] <- data.frame(
      match_id = rep(match_ids[i], nrow(goals)),
      team = goals$team, is_home = goals$is_home, event_type = "goal",
      minute = goals$minute, player_name = goals$player_name,
      is_penalty = FALSE, is_own_goal = FALSE, is_red_card = FALSE,
      stringsAsFactors = FALSE
    )
  }
  # 2 substitutions per match
  starters <- lineups[lineups$match_id == match_ids[i] & lineups$is_starter, ]
  subs <- starters[sample(nrow(starters), 2), ]
  event_rows[[length(event_rows) + 1]] <- data.frame(
    match_id = rep(match_ids[i], 2),
    team = subs$team, is_home = subs$is_home, event_type = "substitution",
    minute = sample(50:75, 2), player_name = subs$player_name,
    is_penalty = FALSE, is_own_goal = FALSE, is_red_card = FALSE,
    stringsAsFactors = FALSE
  )
}
events <- do.call(rbind, event_rows)

processed_data <- list(
  results = results, lineups = lineups, shooting = shooting,
  events = events, stats_summary = NULL
)

cat(sprintf("Created %d matches with %d players\n", n_matches, length(all_player_ids)))
#> Created 12 matches with 50 players
```

## Step 2: Create Splints

Splints are time segments where the lineup is constant. Boundaries occur
at goals, substitutions, red cards, and halftime. Each splint records
the non-penalty xG differential (npxGD) that occurred during that
segment.

``` r
splint_data <- create_all_splints(processed_data, verbose = FALSE)

cat(sprintf("Splints: %d across %d matches\n",
            nrow(splint_data$splints),
            length(unique(splint_data$splints$match_id))))
#> Splints: 81 across 12 matches
cat(sprintf("Player-splint assignments: %d\n", nrow(splint_data$players)))
#> Player-splint assignments: 1782

# Example: first match's splints
first_match <- splint_data$splints[splint_data$splints$match_id == "match_1", ]
first_match[, c("splint_num", "start_minute", "end_minute", "duration")]
#>   splint_num start_minute end_minute duration
#> 1          1            0         46       46
#> 2          2           46         64       18
#> 3          3           64         74       10
#> 4          4           74         75        1
#> 5          5           75         91       16
```

## Step 3: Build RAPM Design Matrix

The design matrix encodes which players were on the field during each
splint. Each splint generates two rows: one from the home team’s
attacking perspective, one from the away team’s. Player columns are
split into offense and defense indicators.

``` r
rapm_data <- create_rapm_design_matrix(splint_data, min_minutes = 45)
#> [04:29:35] Processing 81 splints...
#> [04:29:35] Including 50 players (>= 45 minutes)
#> [04:29:35] Replacement pool: 0 players (< 45 minutes)
#> [04:29:35] Building row data (vectorized)...
#> [04:29:35] Building sparse matrix (vectorized)...
#> [04:29:35] Replacement appearances: 0 offense, 0 defense
#> [04:29:36] Design matrix: 162 rows, 100 player columns (+2 replacement), 5 covariates

# Add covariates to the player matrix for model fitting
covariates <- cbind(
  gd = rapm_data$row_data$gd,
  is_home = as.numeric(rapm_data$row_data$home_away == "home"),
  avg_min = rapm_data$row_data$avg_min
)
rapm_data$X <- cbind(rapm_data$X_players, covariates)
rapm_data$covariate_names <- colnames(covariates)

cat(sprintf("Design matrix: %d rows x %d columns\n",
            nrow(rapm_data$X), ncol(rapm_data$X)))
#> Design matrix: 162 rows x 105 columns
cat(sprintf("Players included: %d (min 45 minutes)\n", rapm_data$n_players))
#> Players included: 50 (min 45 minutes)
cat(sprintf("Target: %s\n", rapm_data$target_name))
#> Target: xgf90
```

## Step 4: Fit Base RAPM

RAPM uses ridge regression (L2 penalty) to estimate each player’s
offensive and defensive impact. Cross-validation selects the
regularization strength.

``` r
rapm_model <- fit_rapm(rapm_data, parallel = FALSE, nfolds = 3)
#> [04:29:36] Fitting RAPM: 162 observations, 105 columns
#> [04:29:36] RAPM fit complete (xG-based). Lambda.min: 140.1164, R^2: 0.840
rapm_ratings <- extract_rapm_ratings(rapm_model)

cat(sprintf("RAPM ratings for %d players\n", nrow(rapm_ratings)))
#> RAPM ratings for 51 players
head(rapm_ratings[, c("player_name", "rapm", "offense", "defense", "total_minutes")])
#>    player_name         rapm      offense       defense total_minutes
#> 1    Player 10 1.243153e-37 1.013306e-37 -2.298464e-38           728
#> 23    Player 7 1.097749e-37 7.331287e-38 -3.646203e-38           364
#> 43   Player 30 1.083924e-37 9.399345e-39 -9.899306e-38           637
#> 7    Player 22 1.029150e-37 5.422111e-38 -4.869392e-38           455
#> 3    Player 23 8.216838e-38 1.415311e-37  5.936271e-38           455
#> 45    Player 8 7.091842e-38 5.257829e-38 -1.834014e-38           273
```

The `rapm` column equals `offense - defense`. Positive offense means the
player helps create xG; negative defense means the player prevents xG.

``` r
# Verify: rapm = offense - defense
all.equal(rapm_ratings$rapm, rapm_ratings$offense - rapm_ratings$defense)
#> [1] TRUE
```

## Step 5: Fit SPM Model

SPM (Statistical Plus-Minus) predicts RAPM from box score statistics. It
captures the relationship between traditional stats and on-field impact,
providing a prior for players with limited RAPM sample size.

``` r
# Create synthetic per-90 statistics
n_players <- nrow(rapm_ratings)
player_features <- data.frame(
  player_id = rapm_ratings$player_id,
  player_name = rapm_ratings$player_name,
  total_minutes = rapm_ratings$total_minutes,
  n_matches = pmax(1, round(rapm_ratings$total_minutes / 90)),
  goals_p90 = runif(n_players, 0, 0.6),
  npxg_p90 = runif(n_players, 0, 0.5),
  xa_p90 = runif(n_players, 0, 0.4),
  tackles_p90 = runif(n_players, 0.5, 3.5),
  interceptions_p90 = runif(n_players, 0.3, 2),
  progressive_passes_p90 = runif(n_players, 1, 7),
  rapm = rapm_ratings$rapm,
  stringsAsFactors = FALSE
)

spm_model <- fit_spm_model(player_features, nfolds = 3)
#> [04:29:36] Fitting SPM model with 6 predictors on 51 players
#> [04:29:36]   Weighting by minutes (sqrt transform)
#> [04:29:36] SPM fit complete. R-squared: 0.000 (weighted in-sample)
spm_ratings <- calculate_spm_ratings(player_features, spm_model)

cat(sprintf("SPM predictions for %d players\n", nrow(spm_ratings)))
#> SPM predictions for 51 players
head(spm_ratings[, c("player_name", "spm", "total_minutes")])
#>   player_name           spm total_minutes
#> 1   Player 10 -3.089047e-40           728
#> 2    Player 7 -3.089047e-40           364
#> 3   Player 30 -3.089047e-40           637
#> 4   Player 22 -3.089047e-40           455
#> 5   Player 23 -3.089047e-40           455
#> 6    Player 8 -3.089047e-40           273
```

## Step 6: Calculate Panna Rating

The final Panna rating combines RAPM with SPM as a Bayesian prior. This
shrinks noisy RAPM estimates toward SPM predictions, providing more
stable ratings.

The formula: `panna = spm_prior + deviation`

Where `deviation` is how much RAPM departs from the SPM prediction after
regularization.

``` r
panna_result <- calculate_panna_rating(rapm_data, spm_ratings, lambda_prior = 1)
#> [04:29:36] Fitting panna model with SPM prior...
#> [04:29:36] Panna ratings calculated for 105 players
panna_ratings <- panna_result$ratings

cat(sprintf("Panna ratings for %d players\n", nrow(panna_ratings)))
#> Panna ratings for 105 players
head(panna_ratings[, c("player_name", "panna", "spm_prior", "deviation")])
#>     player_name     panna spm_prior deviation
#> 103        <NA> 1.0218390         0 1.0218390
#> 93         <NA> 0.9517237         0 0.9517237
#> 31         <NA> 0.9412482         0 0.9412482
#> 21         <NA> 0.8199820         0 0.8199820
#> 14         <NA> 0.6106109         0 0.6106109
#> 59         <NA> 0.4946201         0 0.4946201
```

``` r
# Verify: panna = spm_prior + deviation
all.equal(panna_ratings$panna, panna_ratings$spm_prior + panna_ratings$deviation)
#> [1] TRUE
```

## Step 7: Offensive/Defensive Decomposition

Each player’s overall rating can be decomposed into offensive and
defensive contributions, useful for understanding player profiles.

``` r
# Final rating summary
summary_df <- data.frame(
  player = panna_ratings$player_name,
  panna = round(panna_ratings$panna, 3),
  spm_prior = round(panna_ratings$spm_prior, 3),
  deviation = round(panna_ratings$deviation, 3)
)

# Top 5 and bottom 5
cat("Top 5 players:\n")
#> Top 5 players:
print(head(summary_df[order(-summary_df$panna), ], 5), row.names = FALSE)
#>  player panna spm_prior deviation
#>    <NA> 1.022         0     1.022
#>    <NA> 0.952         0     0.952
#>    <NA> 0.941         0     0.941
#>    <NA> 0.820         0     0.820
#>    <NA> 0.611         0     0.611

cat("\nBottom 5 players:\n")
#> 
#> Bottom 5 players:
print(tail(summary_df[order(-summary_df$panna), ], 5), row.names = FALSE)
#>  player  panna spm_prior deviation
#>    <NA> -0.475         0    -0.475
#>    <NA> -0.497         0    -0.497
#>    <NA> -0.563         0    -0.563
#>    <NA> -0.629         0    -0.629
#>    <NA> -0.855         0    -0.855
```

## Pipeline Summary

The full pipeline:

1.  **Data** – Load match results, lineups, shooting, and events
2.  **Splints** – Divide matches into constant-lineup segments
3.  **Design Matrix** – Encode player presence as sparse indicators
4.  **RAPM** – Ridge regression to isolate player impact from lineup
    context
5.  **SPM** – Predict RAPM from box score stats (elastic net)
6.  **Panna** – Combine RAPM + SPM with Bayesian shrinkage

Key properties: - `rapm = offense - defense` (exact decomposition) -
`panna = spm_prior + deviation` (exact decomposition) - Ratings are in
units of xG per 90 minutes above/below average - Stronger regularization
(`lambda_prior`) pulls panna closer to SPM
