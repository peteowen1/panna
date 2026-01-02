# debug_splints.R
# Quick diagnostic of splint data quality

library(dplyr)

cache_dir <- file.path("data-raw", "cache")

# Load splint data
splint_data <- readRDS(file.path(cache_dir, "03_splints.rds"))

cat("\n=== SPLINT DATA STRUCTURE ===\n")
cat("Names:", paste(names(splint_data), collapse = ", "), "\n\n")

splints <- splint_data$splints
players <- splint_data$players

cat("Splints: ", nrow(splints), "rows\n")
cat("Columns:", paste(names(splints), collapse = ", "), "\n\n")

cat("Players: ", nrow(players), "rows\n\n")

cat("=== SPLINT RESPONSE VARIABLE ===\n")
cat("npxgd_per_90 summary:\n")
print(summary(splints$npxgd_per_90))

cat("\nDuration summary (minutes):\n")
print(summary(splints$duration))

cat("\nnpxg_home summary:\n")
print(summary(splints$npxg_home))

cat("\nnpxg_away summary:\n")
print(summary(splints$npxg_away))

# Check for issues
cat("\n=== POTENTIAL ISSUES ===\n")

# 1. How many splints have zero xG on both sides?
zero_both <- sum(splints$npxg_home == 0 & splints$npxg_away == 0, na.rm = TRUE)
cat("Splints with zero xG (both teams):", zero_both, "/", nrow(splints),
    sprintf("(%.1f%%)\n", 100 * zero_both / nrow(splints)))

# 2. How many splints have NA values?
na_npxgd <- sum(is.na(splints$npxgd_per_90))
cat("Splints with NA npxgd_per_90:", na_npxgd, "\n")

# 3. Very short splints
short_splints <- sum(splints$duration < 5, na.rm = TRUE)
cat("Splints < 5 minutes:", short_splints, "\n")

# 4. Check player assignments
cat("\n=== PLAYER ASSIGNMENTS ===\n")
players_per_splint <- players %>%
  group_by(splint_id) %>%
  summarise(n_players = n(), .groups = "drop")

cat("Players per splint summary:\n")
print(summary(players_per_splint$n_players))

# Should be ~22 (11 per team)
wrong_count <- sum(players_per_splint$n_players != 22)
cat("Splints with != 22 players:", wrong_count, "/", nrow(splints), "\n")

# 5. Sample of splints with actual xG data
cat("\n=== SAMPLE SPLINTS WITH XG ===\n")
sample_with_xg <- splints %>%
  filter(npxg_home > 0 | npxg_away > 0) %>%
  head(10) %>%
  select(match_id, splint_num, duration, npxg_home, npxg_away, npxgd, npxgd_per_90)
print(sample_with_xg)

# 6. Distribution of npxgd_per_90
cat("\n=== DISTRIBUTION OF NPXGD_PER_90 ===\n")
cat("Quantiles:\n")
print(quantile(splints$npxgd_per_90, probs = c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1), na.rm = TRUE))

# 7. Standard deviation - key for RAPM
cat("\nStandard deviation:", sd(splints$npxgd_per_90, na.rm = TRUE), "\n")
cat("This should be similar to team-level npxGD_per_90 variance\n")

cat("\n=== DONE ===\n")
