# 05b_rapm_diagnostics.R
# Diagnostics to understand RAPM results
#
# Run after 05_multi_season_rapm.R to analyze:
# - Player sample sizes and reliability
# - Teammate effects and multicollinearity
# - Season-by-season stability
# - Team-level validation

library(dplyr)
library(tidyr)
devtools::load_all()

cache_dir <- file.path("data-raw", "cache")

# Load RAPM results
rapm_results <- readRDS(file.path(cache_dir, "05_rapm.rds"))
rapm_data <- rapm_results$rapm_data
rapm_ratings <- rapm_results$ratings

# 1. Player Sample Size ----
cat("\n=== 1. PLAYER SAMPLE SIZE ANALYSIS ===\n")

# Get splint counts per player from the sparse matrix
X <- rapm_data$X
player_splint_counts <- Matrix::colSums(X != 0)
player_minutes <- Matrix::colSums(abs(X) * rapm_data$splint_info$duration)

sample_size_df <- data.frame(
  player_id = colnames(X),
  n_splints = as.numeric(player_splint_counts),
  total_minutes = as.numeric(player_minutes)
) %>%
  left_join(rapm_ratings, by = "player_id") %>%
  arrange(desc(total_minutes))

cat("\nTop 10 players by minutes:\n")
print(head(sample_size_df %>% select(player_name, n_splints, total_minutes, rapm), 10))

cat("\nBottom 10 by minutes (among those with ratings):\n")
print(tail(sample_size_df %>% select(player_name, n_splints, total_minutes, rapm), 10))

# Check correlation between sample size and absolute RAPM
cat("\nCorrelation between minutes and |RAPM|:\n")
cat(sprintf("  r = %.3f\n", cor(sample_size_df$total_minutes, abs(sample_size_df$rapm))))
cat("  (Negative = more minutes -> more shrinkage, which is backwards)\n")
cat("  (Positive = more minutes -> ratings further from zero, expected)\n")

# 2. Extreme Ratings ----
cat("\n=== 2. EXTREME RATING ANALYSIS ===\n")

# Top 20 and bottom 20 with sample sizes
cat("\nTop 20 RAPM with sample sizes:\n")
top_20 <- sample_size_df %>%
  arrange(desc(rapm)) %>%
  head(20) %>%
  select(player_name, rapm, n_splints, total_minutes)
print(top_20)

cat("\nBottom 20 RAPM with sample sizes:\n")
bottom_20 <- sample_size_df %>%
  arrange(rapm) %>%
  head(20) %>%
  select(player_name, rapm, n_splints, total_minutes)
print(bottom_20)

# 3. Season Breakdown ----
cat("\n=== 3. SEASON BREAKDOWN ===\n")

# Extract season from splint_id
splint_info <- rapm_data$splint_info %>%
  mutate(season = substr(splint_id, 1, 9))

cat("\nSplints per season:\n")
print(table(splint_info$season))

# Calculate player appearances by season
# This requires looking at which players appear in which splints
get_player_seasons <- function(X, splint_info) {
  seasons <- unique(splint_info$season)
  player_seasons <- list()

  for (s in seasons) {
    season_rows <- which(splint_info$season == s)
    if (length(season_rows) > 0) {
      season_X <- X[season_rows, , drop = FALSE]
      players_in_season <- colnames(X)[Matrix::colSums(season_X != 0) > 0]
      for (p in players_in_season) {
        if (is.null(player_seasons[[p]])) {
          player_seasons[[p]] <- s
        } else {
          player_seasons[[p]] <- c(player_seasons[[p]], s)
        }
      }
    }
  }
  player_seasons
}

player_seasons <- get_player_seasons(X, splint_info)

# Find players who span multiple seasons
multi_season_players <- sample_size_df %>%
  mutate(n_seasons = sapply(player_id, function(p) length(player_seasons[[p]]))) %>%
  filter(n_seasons >= 5)

cat("\nPlayers appearing in 5+ seasons (most identifiable):\n")
print(
  multi_season_players %>%
    arrange(desc(rapm)) %>%
    select(player_name, rapm, n_seasons, total_minutes) %>%
    head(20)
)

cat("\nWorst rated players with 5+ seasons:\n")
print(
  multi_season_players %>%
    arrange(rapm) %>%
    select(player_name, rapm, n_seasons, total_minutes) %>%
    head(20)
)

# 4. Teammate Analysis ----
cat("\n=== 4. TEAMMATE ANALYSIS ===\n")

analyze_teammates <- function(player_name_query, X, ratings) {
  # Find player
  player_idx <- which(grepl(player_name_query, colnames(X), ignore.case = TRUE))[1]
  if (is.na(player_idx)) {
    cat(paste("Player not found:", player_name_query, "\n"))
    return(NULL)
  }

  player_id <- colnames(X)[player_idx]
  player_col <- X[, player_idx]

  # Find splints where this player appears
  player_splints <- which(player_col != 0)

  if (length(player_splints) == 0) {
    cat(paste("No splints found for:", player_name_query, "\n"))
    return(NULL)
  }

  # Get teammates (same sign = same team, opposite sign = opponent)
  player_sign <- sign(player_col[player_splints[1]])

  teammate_counts <- Matrix::colSums(X[player_splints, ] * player_sign > 0)
  opponent_counts <- Matrix::colSums(X[player_splints, ] * player_sign < 0)

  # Exclude self
 teammate_counts[player_idx] <- 0

  teammates_df <- data.frame(
    player_id = colnames(X),
    times_teammate = as.numeric(teammate_counts),
    times_opponent = as.numeric(opponent_counts)
  ) %>%
    left_join(ratings, by = "player_id") %>%
    filter(times_teammate > 0 | times_opponent > 0)

  list(
    player = player_id,
    n_splints = length(player_splints),
    top_teammates = teammates_df %>% arrange(desc(times_teammate)) %>% head(15),
    top_opponents = teammates_df %>% arrange(desc(times_opponent)) %>% head(10)
  )
}

# Analyze Son (worst rated)
cat("\n--- Son Heung-min teammate analysis ---\n")
son_analysis <- analyze_teammates("Son-Heung", X, rapm_ratings)
if (!is.null(son_analysis)) {
  cat(paste("Total splints:", son_analysis$n_splints, "\n"))
  cat("\nMost frequent teammates:\n")
  print(son_analysis$top_teammates %>% select(player_name, times_teammate, rapm))
  cat("\nAverage teammate RAPM:", mean(son_analysis$top_teammates$rapm, na.rm = TRUE), "\n")
}

# Analyze Lewis Dunk (best rated)
cat("\n--- Lewis Dunk teammate analysis ---\n")
dunk_analysis <- analyze_teammates("Lewis-Dunk", X, rapm_ratings)
if (!is.null(dunk_analysis)) {
  cat(paste("Total splints:", dunk_analysis$n_splints, "\n"))
  cat("\nMost frequent teammates:\n")
  print(dunk_analysis$top_teammates %>% select(player_name, times_teammate, rapm))
  cat("\nAverage teammate RAPM:", mean(dunk_analysis$top_teammates$rapm, na.rm = TRUE), "\n")
}

# 5. Team-Level RAPM ----
cat("\n=== 5. TEAM-LEVEL VALIDATION ===\n")

# Load processed data to get team info
processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

# Get unique team-player mappings from lineups
if (!is.null(processed_data$lineups)) {
  # Get most common team for each player
  player_teams <- processed_data$lineups %>%
    group_by(player_name, team) %>%
    summarise(appearances = n(), .groups = "drop") %>%
    group_by(player_name) %>%
    slice_max(appearances, n = 1) %>%
    ungroup() %>%
    select(player_name, primary_team = team)

  # Join with ratings
  team_ratings <- rapm_ratings %>%
    left_join(player_teams, by = "player_name") %>%
    filter(!is.na(primary_team))

  # Aggregate by team
  team_summary <- team_ratings %>%
    group_by(primary_team) %>%
    summarise(
      n_players = n(),
      mean_rapm = mean(rapm),
      sum_rapm = sum(rapm),
      top_player = player_name[which.max(rapm)],
      top_rapm = max(rapm),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_rapm))

  cat("\nTeam average RAPM (should correlate with actual performance):\n")
  print(team_summary, n = 25)
}

# 5b. Team-Level npxGD Target ----
cat("\n=== 5b. TEAM-LEVEL npxGD TOTALS (Target Variable Check) ===\n")

# We need to sum npxGD at match level by team, not splint level
# Load the original results to get match-level xG data
if (file.exists(file.path(cache_dir, "02_processed_data.rds"))) {
  processed_data <- readRDS(file.path(cache_dir, "02_processed_data.rds"))

  if (!is.null(processed_data$results)) {
    results <- processed_data$results

    # Calculate npxGD for home and away teams per match
    # Home team: home_xg - away_xg
    # Away team: away_xg - home_xg
    home_npxgd <- results %>%
      filter(!is.na(home_xg) & !is.na(away_xg)) %>%
      group_by(team = home_team) %>%
      summarise(
        matches = n(),
        total_npxgd = sum(home_xg - away_xg, na.rm = TRUE),
        .groups = "drop"
      )

    away_npxgd <- results %>%
      filter(!is.na(home_xg) & !is.na(away_xg)) %>%
      group_by(team = away_team) %>%
      summarise(
        matches = n(),
        total_npxgd = sum(away_xg - home_xg, na.rm = TRUE),
        .groups = "drop"
      )

    # Combine home and away
    team_npxgd <- home_npxgd %>%
      full_join(away_npxgd, by = "team", suffix = c("_home", "_away")) %>%
      mutate(
        total_matches = coalesce(matches_home, 0) + coalesce(matches_away, 0),
        total_npxgd = coalesce(total_npxgd_home, 0) + coalesce(total_npxgd_away, 0),
        npxgd_per_match = total_npxgd / total_matches
      ) %>%
      select(team, total_matches, total_npxgd, npxgd_per_match) %>%
      arrange(desc(total_npxgd))

    cat("\nTeam npxGD totals (from actual match data):\n")
    cat("This is what the MODEL SHOULD learn - good teams should have high npxGD\n\n")
    print(team_npxgd, n = 30)

    # Now compare with RAPM rankings
    if (exists("team_summary")) {
      comparison <- team_npxgd %>%
        left_join(
          team_summary %>% select(primary_team, mean_rapm, sum_rapm),
          by = c("team" = "primary_team")
        ) %>%
        filter(!is.na(mean_rapm))

      cat("\n\nCorrelation between actual npxGD and RAPM sum:\n")
      cat(sprintf("  r = %.3f\n", cor(comparison$total_npxgd, comparison$sum_rapm, use = "complete.obs")))

      cat("\nCorrelation between npxGD per match and mean RAPM:\n")
      cat(sprintf("  r = %.3f\n", cor(comparison$npxgd_per_match, comparison$mean_rapm, use = "complete.obs")))

      cat("\nTeams with biggest RAPM vs npxGD discrepancy:\n")
      comparison <- comparison %>%
        mutate(
          npxgd_rank = rank(-total_npxgd),
          rapm_rank = rank(-sum_rapm),
          rank_diff = npxgd_rank - rapm_rank
        ) %>%
        arrange(desc(abs(rank_diff)))

      cat("\nOverrated by RAPM (high RAPM rank, low npxGD rank):\n")
      print(comparison %>% filter(rank_diff > 0) %>% head(10) %>%
              select(team, total_npxgd, sum_rapm, npxgd_rank, rapm_rank, rank_diff))

      cat("\nUnderrated by RAPM (low RAPM rank, high npxGD rank):\n")
      print(comparison %>% filter(rank_diff < 0) %>% head(10) %>%
              select(team, total_npxgd, sum_rapm, npxgd_rank, rapm_rank, rank_diff))
    }
  }
}

# 6. Response Variable ----
cat("\n=== 6. RESPONSE VARIABLE ANALYSIS ===\n")

y <- rapm_data$y
weights <- rapm_data$weights

cat("\nUnweighted response (npxGD per 90) distribution:\n")
print(summary(y))

cat("\nWeighted response distribution:\n")
print(summary(y * sqrt(weights)))

cat("\nExtreme splints (|y| > 30):\n")
extreme_splints <- which(abs(y) > 30)
cat(paste("Count:", length(extreme_splints), "of", length(y),
          sprintf("(%.1f%%)\n", 100 * length(extreme_splints) / length(y))))

if (length(extreme_splints) > 0) {
  cat("\nExample extreme splints:\n")
  print(rapm_data$splint_info[extreme_splints[1:min(5, length(extreme_splints))], ])
}

# 7. Model Fit ----
cat("\n=== 7. MODEL FIT SUMMARY ===\n")

model <- rapm_results$model

cat(paste("Lambda.min:", round(model$lambda.min, 4), "\n"))
cat(paste("Lambda.1se:", round(model$lambda.1se, 4), "\n"))

# Get predictions at lambda.min
X_weighted <- rapm_data$X_weighted
y_weighted <- rapm_data$y_weighted

predictions <- as.numeric(predict(model, newx = X_weighted, s = "lambda.min"))
residuals <- y_weighted - predictions

cat(paste("\nR-squared:", round(1 - sum(residuals^2) / sum((y_weighted - mean(y_weighted))^2), 4), "\n"))
cat(paste("RMSE:", round(sqrt(mean(residuals^2)), 4), "\n"))

# Done ----
cat("\n=== DIAGNOSTICS COMPLETE ===\n")
