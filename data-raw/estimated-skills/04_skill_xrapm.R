# 04_skill_xrapm.R
# xRAPM with skill-based SPM prior
#
# Uses the skill-based SPM predictions (from 03_skill_spm.R) as the Bayesian
# prior for RAPM, replacing the raw-stat SPM prior.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
xrapm_lambda <- "min"

# 3. Load Data ----

cat("\n=== Loading Data ===\n")

splint_data <- readRDS(file.path(opta_cache_dir, "03_splints.rds"))
rapm_results <- readRDS(file.path(opta_cache_dir, "04_rapm.rds"))
spm_results <- readRDS(file.path(cache_dir, "03_skill_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Players with RAPM:", nrow(rapm_results$ratings), "\n")
cat("Players with skill SPM:", nrow(spm_results$spm_ratings), "\n")

# 4. Create SPM Priors ----

cat("\n=== Creating Skill-Based SPM Priors ===\n")

player_mapping <- rapm_results$rapm_data$player_mapping

offense_prior <- build_prior_vector(
  spm_data = spm_results$offense_spm_ratings,
  spm_col = "offense_spm",
  player_mapping = player_mapping
)

defense_prior <- build_prior_vector(
  spm_data = spm_results$defense_spm_ratings,
  spm_col = "defense_spm",
  player_mapping = player_mapping
)

cat("Offense priors set:", sum(offense_prior != 0), "\n")
cat("Defense priors set:", sum(defense_prior != 0), "\n")

# 5. Fit xRAPM ----

cat("\n=== Fitting xRAPM with Skill-Based Prior ===\n")

rapm_data <- rapm_results$rapm_data

xrapm_model <- fit_rapm_with_prior(
  rapm_data,
  offense_prior = offense_prior,
  defense_prior = defense_prior,
  alpha = 0,
  nfolds = 10,
  use_weights = TRUE,
  penalize_covariates = FALSE
)

# 6. Extract Ratings ----

cat("\n=== Skill-Based xRAPM Ratings ===\n")

xrapm_ratings <- extract_xrapm_ratings(xrapm_model, lambda = xrapm_lambda)

cat("\nTop 25 by skill-xRAPM:\n")
print(
  xrapm_ratings %>%
    head(25) %>%
    select(player_name, xrapm, offense, defense, off_deviation, def_deviation, total_minutes)
)

# 7. Compare with Raw-Stat xRAPM ----

cat("\n=== Skill xRAPM vs Raw xRAPM ===\n")

opta_xrapm_path <- file.path(opta_cache_dir, "06_xrapm.rds")
if (file.exists(opta_xrapm_path)) {
  raw_xrapm <- readRDS(opta_xrapm_path)

  comparison <- xrapm_ratings %>%
    select(player_name, skill_xrapm = xrapm, skill_off = offense, skill_def = defense) %>%
    inner_join(
      raw_xrapm$ratings %>%
        select(player_name, raw_xrapm = xrapm, raw_off = offense, raw_def = defense),
      by = "player_name"
    )

  cat(sprintf("Skill vs Raw xRAPM: r = %.3f\n", cor(comparison$skill_xrapm, comparison$raw_xrapm)))
  cat(sprintf("Skill vs Raw Offense: r = %.3f\n", cor(comparison$skill_off, comparison$raw_off)))
  cat(sprintf("Skill vs Raw Defense: r = %.3f\n", cor(comparison$skill_def, comparison$raw_def)))

  # Players most changed
  comparison <- comparison %>%
    mutate(diff = skill_xrapm - raw_xrapm) %>%
    arrange(desc(abs(diff)))

  cat("\nPlayers most changed by skill estimation:\n")
  print(head(comparison %>% select(player_name, skill_xrapm, raw_xrapm, diff), 15))
}

# 8. Save ----

cat("\n=== Saving Results ===\n")

xrapm_results <- list(
  model = xrapm_model,
  ratings = xrapm_ratings,
  offense_prior = offense_prior,
  defense_prior = defense_prior
)

saveRDS(xrapm_results, file.path(cache_dir, "04_skill_xrapm.rds"))
cat("Saved to cache-skills/04_skill_xrapm.rds\n")

cat("\n=== COMPLETE ===\n")
