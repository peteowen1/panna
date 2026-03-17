# 07_train_psr_model.R
# Train PSR (Player Skill Rating) models via glmnet
#
# Predicts match xG differential (primary) and goal differential (secondary)
# from team-aggregated per-match player skills.
#
# Key design choices (following torpverse methodology):
#   - Per-match lagged skills: for each match, skills are estimated using all
#     data STRICTLY BEFORE that match date (no data leakage)
#   - Separate home/away features (v2): enables clean OSR/DSR decomposition
#   - Two model families: xG diff (primary, less noisy) + goal diff (secondary)
#
# Inputs:
#   - cache-skills/01_match_stats.rds (player-match level stats)
#   - cache-opta/03_splints.rds (for match-level xG from splint data)
#   - Optimized decay params from 02b (if available)
#
# Outputs:
#   - inst/extdata/psr_coefficients.csv   (xG margin model — primary)
#   - inst/extdata/osr_coefficients.csv   (xG offensive model)
#   - inst/extdata/dsr_coefficients.csv   (xG defensive model)
#   - inst/extdata/gd_psr_coefficients.csv  (goal diff margin — secondary)
#   - inst/extdata/gd_osr_coefficients.csv  (goal diff offensive)
#   - inst/extdata/gd_dsr_coefficients.csv  (goal diff defensive)
#   - cache-skills/07_psr_model.rds (full model objects for diagnostics)

# 1. Setup ----

library(glmnet)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
extdata_dir <- file.path("inst", "extdata")
dir.create(extdata_dir, showWarnings = FALSE, recursive = TRUE)

# Match weight decay: more recent matches weighted higher in training
MATCH_WEIGHT_DECAY_DAYS <- 365  # ~1 year half-life

# Elastic net alpha grid
ALPHA_GRID <- c(0, 0.25, 0.5, 0.75, 1.0)

# Minimum players per team-match to include in training
MIN_PLAYERS_PER_TEAM <- 8

# Minimum weighted 90s for a player's skills to be included
MIN_W90_FOR_SKILLS <- 3

# Hold-out test season
if (!exists("test_season_year")) test_season_year <- NULL  # auto-detect

cat("\n")
cat(paste(rep("#", 70), collapse = ""), "\n")
cat("#  PSR MODEL TRAINING (per-match skills, xG + goals targets)\n")
cat(paste(rep("#", 70), collapse = ""), "\n\n")

# 3. Load Data ----

cat("=== Loading Data ===\n\n")

# Prefer slim version (fewer columns, less memory) if available
slim_path <- file.path(cache_dir, "01_match_stats_slim.rds")
full_path <- file.path(cache_dir, "01_match_stats.rds")
ms_path <- if (file.exists(slim_path)) slim_path else full_path
cat(sprintf("Loading: %s\n", basename(ms_path)))
match_stats <- readRDS(ms_path)
gc(verbose = FALSE)
cat(sprintf("Match stats: %s player-match rows x %d cols\n",
            format(nrow(match_stats), big.mark = ","), ncol(match_stats)))

# Load optimized decay params (if available)
decay_params_path <- file.path(cache_dir, "02b_decay_params.rds")
if (file.exists(decay_params_path)) {
  decay_params <- readRDS(decay_params_path)
  cat("Using optimized decay parameters\n")
} else {
  decay_params <- get_default_decay_params()
  cat("Using default decay parameters\n")
}

# Ensure season_end_year exists
ms_dt <- data.table::as.data.table(match_stats)
rm(match_stats); gc(verbose = FALSE)
if (!"season_end_year" %in% names(ms_dt)) {
  ms_dt[, season_end_year := data.table::fifelse(
    data.table::month(as.Date(match_date)) >= 7L,
    data.table::year(as.Date(match_date)) + 1L,
    data.table::year(as.Date(match_date))
  )]
}
ms_dt[, match_date := as.Date(match_date)]

# 4. Derive Match Results (goals) ----

cat("\n=== Deriving Match Results ===\n\n")

# Sum goals per team per match from player stats
team_goals <- ms_dt[, .(
  team_goals = sum(as.numeric(goals), na.rm = TRUE),
  team_name = team_name[1],
  is_home = is_home[1],
  match_date = match_date[1],
  season_end_year = season_end_year[1],
  n_players = .N
), by = .(match_id, team_name)]

home_side <- team_goals[is_home == 1, .(match_id, home_team = team_name,
                                          home_goals = team_goals,
                                          match_date, season_end_year)]
away_side <- team_goals[is_home == 0, .(match_id, away_team = team_name,
                                          away_goals = team_goals)]

match_outcomes <- home_side[away_side, on = "match_id", nomatch = NULL]
match_outcomes[, goal_diff := home_goals - away_goals]

cat(sprintf("Matches with goal data: %s\n",
            format(nrow(match_outcomes), big.mark = ",")))

# 5. Load Match-Level xG from Splint Data ----

cat("\n=== Loading xG Data ===\n\n")

has_xg <- FALSE
splint_path <- file.path(opta_cache_dir, "03_splints.rds")
if (file.exists(splint_path)) {
  splint_data <- readRDS(splint_path)
  splints <- data.table::as.data.table(splint_data$splints)

  # Accept either home_xg/away_xg or npxg_home/npxg_away column names
  xg_home_col <- intersect(c("home_xg", "npxg_home"), names(splints))[1]
  xg_away_col <- intersect(c("away_xg", "npxg_away"), names(splints))[1]

  if ("match_id" %in% names(splints) && !is.na(xg_home_col) && !is.na(xg_away_col)) {
    cat(sprintf("Using xG columns: %s, %s\n", xg_home_col, xg_away_col))
    # Aggregate xG per match from splints (sum across all splints in a match)
    match_xg <- splints[, .(
      home_xg = sum(.SD[[xg_home_col]], na.rm = TRUE),
      away_xg = sum(.SD[[xg_away_col]], na.rm = TRUE)
    ), .SDcols = c(xg_home_col, xg_away_col), by = match_id]
    match_xg[, xg_diff := home_xg - away_xg]

    # Join to match outcomes
    match_outcomes <- match_xg[match_outcomes, on = "match_id"]
    has_xg <- sum(!is.na(match_outcomes$xg_diff)) > 100
    cat(sprintf("Matches with xG: %d / %d\n",
                sum(!is.na(match_outcomes$xg_diff)), nrow(match_outcomes)))

    if (has_xg) {
      cat(sprintf("xG diff range: [%.1f, %.1f], mean: %.2f\n",
                  min(match_outcomes$xg_diff, na.rm = TRUE),
                  max(match_outcomes$xg_diff, na.rm = TRUE),
                  mean(match_outcomes$xg_diff, na.rm = TRUE)))
    }
  } else {
    cat("Splint data missing xG columns (checked: home_xg, npxg_home, away_xg, npxg_away)\n")
  }
  rm(splint_data, splints)  # free memory
} else {
  cat("No splint data found — will train goal-diff model only\n")
}

# 6. Compute Pre-Match Skills at Each Match Date ----

cat("\n=== Computing Pre-Match Skills ===\n\n")

# Use weekly date bins to reduce computation (~400 calls vs ~3000)
# Each match uses skills from the most recent weekly bin BEFORE its date
unique_match_dates <- sort(unique(match_outcomes$match_date))
cat(sprintf("Unique match dates: %d (%s to %s)\n",
            length(unique_match_dates),
            min(unique_match_dates), max(unique_match_dates)))

# Create weekly reference dates (every 7 days)
SKILL_BIN_DAYS <- 7L
all_dates_num <- as.numeric(unique_match_dates)
bin_breaks <- seq(min(all_dates_num), max(all_dates_num) + SKILL_BIN_DAYS,
                  by = SKILL_BIN_DAYS)
weekly_dates <- as.Date(bin_breaks, origin = "1970-01-01")
cat(sprintf("Weekly skill dates: %d (every %d days)\n",
            length(weekly_dates), SKILL_BIN_DAYS))

# Map each match date to its weekly bin (latest bin <= match_date)
match_date_to_bin <- function(md) {
  idx <- findInterval(as.numeric(md), as.numeric(weekly_dates))
  idx[idx < 1] <- 1
  weekly_dates[idx]
}

# Batch-estimate skills at each weekly date
prematch_skills <- tryCatch(
  .estimate_prematch_skills_batch(
    match_stats = ms_dt,
    ref_dates = weekly_dates,
    decay_params = decay_params,
    min_weighted_90s = MIN_W90_FOR_SKILLS,
    verbose = TRUE
  ),
  error = function(e) {
    cat(sprintf("\n!!! BATCH ESTIMATION ERROR: %s\n", e$message))
    cat(sprintf("Traceback:\n"))
    traceback()
    list()
  }
)
gc(verbose = FALSE)

cat(sprintf("\nSkills computed for %d / %d weekly dates\n",
            length(prematch_skills), length(weekly_dates)))

# 7. Join Pre-Match Skills to Player-Matches ----
# Memory-safe: process one weekly date at a time instead of stacking all
# skills into one giant table. Avoids ~4GB peak from rbindlist.

cat("\n=== Joining Skills to Matches (chunked) ===\n\n")

psr_cols <- .get_psr_skill_cols()

# For each player-match, look up their pre-match skills by weekly bin date
player_match_ids <- ms_dt[, .(match_id, player_id, team_name, is_home,
                               total_minutes, match_date)]
rm(ms_dt); gc(verbose = FALSE)
# Map each match to its weekly bin
player_match_ids[, skill_date := match_date_to_bin(match_date)]

# Determine skill_keep_cols from the first non-empty result
first_sk <- NULL
for (d in names(prematch_skills)) {
  if (!is.null(prematch_skills[[d]]) && nrow(prematch_skills[[d]]) > 0) {
    first_sk <- data.table::as.data.table(prematch_skills[[d]])
    break
  }
}
if (is.null(first_sk)) {
  stop("No skills were computed. Check match_stats and decay_params.")
}
skill_keep_cols <- intersect(psr_cols, names(first_sk))
rm(first_sk)

# Save the latest date's skills for validation BEFORE freeing prematch_skills
latest_skill_date <- max(names(prematch_skills))
latest_skills_for_validation <- prematch_skills[[latest_skill_date]]

# Chunked join: for each weekly date, join its skills with matching
# player-matches, then discard. Never hold all dates in memory at once.
skill_join_cols <- c("player_id", skill_keep_cols)
matched_chunks <- vector("list", length(prematch_skills))
dates_processed <- 0L

for (j in seq_along(prematch_skills)) {
  d <- names(prematch_skills)[j]
  sk <- prematch_skills[[d]]
  # Free this slot immediately after extracting
  prematch_skills[[j]] <- NULL

  if (is.null(sk) || nrow(sk) == 0) next

  sk <- data.table::as.data.table(sk)
  # Only keep needed columns
  sk_cols <- intersect(skill_join_cols, names(sk))
  sk <- sk[, ..sk_cols]

  # Find player-matches mapped to this weekly bin
  bin_date <- as.Date(d)
  pm_subset <- player_match_ids[skill_date == bin_date]
  if (nrow(pm_subset) == 0) next

  # Join: attach skills to player-matches for this date
  matched <- sk[pm_subset, on = "player_id", nomatch = NA]
  matched_chunks[[j]] <- matched

  dates_processed <- dates_processed + 1L
  if (dates_processed %% 100 == 0) {
    cat(sprintf("  Joined %d / %d dates\n", dates_processed, length(names(prematch_skills)) + dates_processed))
    gc(verbose = FALSE)
  }
}

rm(prematch_skills)
gc(verbose = FALSE)

pm_with_skills <- data.table::rbindlist(matched_chunks, fill = TRUE, use.names = TRUE)
rm(matched_chunks)
gc(verbose = FALSE)

# Add any player-matches that had no matching skill date (fill with NA)
missing_matches <- player_match_ids[!match_id %in% pm_with_skills$match_id |
                                     !player_id %in% pm_with_skills$player_id]
if (nrow(missing_matches) > 0) {
  # These will get NA skills, which get imputed to 0 below
  pm_with_skills <- data.table::rbindlist(
    list(pm_with_skills, missing_matches), fill = TRUE, use.names = TRUE
  )
}
rm(player_match_ids)
gc(verbose = FALSE)

n_with_skills <- sum(!is.na(pm_with_skills[[skill_keep_cols[1]]]))
n_total <- nrow(pm_with_skills)
cat(sprintf("Player-matches with skills: %d / %d (%.1f%%)\n",
            n_with_skills, n_total, 100 * n_with_skills / n_total))
cat(sprintf("Dates processed: %d\n", dates_processed))

# Impute missing skills with 0 (player has no prior data)
for (col in skill_keep_cols) {
  data.table::set(pm_with_skills, which(is.na(pm_with_skills[[col]])), col, 0)
}

# 8. Aggregate Team-Level Skills Per Match ----

cat("\n=== Aggregating Team Skills ===\n\n")

# Weight skills by minutes / 90 (90-min player contributes 1x, sub 0.5x)
weight <- pm_with_skills$total_minutes / 90
weight[is.na(weight) | weight <= 0] <- 0

for (col in skill_keep_cols) {
  data.table::set(pm_with_skills, j = col,
                  value = pm_with_skills[[col]] * weight)
}

team_skills <- pm_with_skills[, c(
  lapply(.SD, sum, na.rm = TRUE),
  list(n_players = .N)
), by = .(match_id, team_name, is_home), .SDcols = skill_keep_cols]

cat(sprintf("Team-match skill rows: %s\n",
            format(nrow(team_skills), big.mark = ",")))

# Filter matches with too few players
team_skills <- team_skills[n_players >= MIN_PLAYERS_PER_TEAM]

# 9. Create Home/Away Feature Matrix ----

cat("\n=== Creating Home/Away Feature Matrix ===\n\n")

home_skills <- team_skills[is_home == 1]
away_skills <- team_skills[is_home == 0]

# Rename with home_/away_ prefix
home_renamed <- data.table::copy(home_skills[, c("match_id", skill_keep_cols), with = FALSE])
data.table::setnames(home_renamed, skill_keep_cols, paste0("home_", skill_keep_cols))

away_renamed <- data.table::copy(away_skills[, c("match_id", skill_keep_cols), with = FALSE])
data.table::setnames(away_renamed, skill_keep_cols, paste0("away_", skill_keep_cols))

# Join home + away + outcomes
train_data <- home_renamed[away_renamed, on = "match_id", nomatch = NULL]
train_data <- match_outcomes[train_data, on = "match_id", nomatch = NULL]

cat(sprintf("Training data: %s matches x %d features\n",
            format(nrow(train_data), big.mark = ","),
            2 * length(skill_keep_cols)))

# Season breakdown
season_counts <- train_data[, .N, by = season_end_year]
data.table::setorder(season_counts, season_end_year)
cat("\nMatches per season:\n")
for (i in seq_len(nrow(season_counts))) {
  cat(sprintf("  %d: %d matches\n", season_counts$season_end_year[i],
              season_counts$N[i]))
}

# 10. Train/Test Split ----

cat("\n=== Train/Test Split ===\n\n")

if (is.null(test_season_year)) {
  test_season_year <- max(train_data$season_end_year)
}

is_train <- train_data$season_end_year < test_season_year
is_test <- train_data$season_end_year >= test_season_year

cat(sprintf("Train: seasons < %d (%d matches)\n", test_season_year, sum(is_train)))
cat(sprintf("Test: seasons >= %d (%d matches)\n", test_season_year, sum(is_test)))

if (sum(is_train) < 100) {
  stop(sprintf("Insufficient training data: only %d matches.", sum(is_train)))
}

# 11. Prepare Features ----

feature_cols <- c(paste0("home_", skill_keep_cols),
                   paste0("away_", skill_keep_cols))

X_train <- as.matrix(train_data[is_train, ..feature_cols])
X_test <- if (sum(is_test) > 0) as.matrix(train_data[is_test, ..feature_cols]) else NULL

# Replace NA/Inf
X_train[is.na(X_train) | is.infinite(X_train)] <- 0
if (!is.null(X_test)) X_test[is.na(X_test) | is.infinite(X_test)] <- 0

# Exponential decay weights
anchor_date <- max(train_data$match_date[is_train])
train_dates <- as.Date(train_data$match_date[is_train])
days_ago <- as.numeric(anchor_date - train_dates)
weights <- exp(-days_ago / MATCH_WEIGHT_DECAY_DAYS)
weights <- weights / mean(weights)

# Standardize using training SDs
train_sds <- apply(X_train, 2, sd, na.rm = TRUE)
train_sds[train_sds == 0 | is.na(train_sds)] <- 1
X_train_std <- sweep(X_train, 2, train_sds, "/")
X_train_std[is.na(X_train_std) | is.infinite(X_train_std)] <- 0
if (!is.null(X_test)) {
  X_test_std <- sweep(X_test, 2, train_sds, "/")
  X_test_std[is.na(X_test_std) | is.infinite(X_test_std)] <- 0
}

cat(sprintf("\n%d features standardized\n", length(feature_cols)))

# CV folds: one per season (temporal CV)
fold_ids <- as.integer(factor(train_data$season_end_year[is_train]))
n_folds <- length(unique(fold_ids))
cat(sprintf("Temporal CV: %d folds\n", n_folds))

# 12. Train Model Helper ----

train_model <- function(X, y, w, foldid, alpha_grid, model_name) {
  cat(sprintf("\n--- Training %s model ---\n", model_name))

  best_alpha <- 0
  best_cvm <- Inf
  best_fit <- NULL

  for (alpha in alpha_grid) {
    fit <- tryCatch(
      glmnet::cv.glmnet(
        x = X, y = y, weights = w,
        alpha = alpha, foldid = foldid,
        standardize = FALSE,
        type.measure = "mse"
      ),
      error = function(e) {
        cat(sprintf("  alpha=%.2f: ERROR - %s\n", alpha, e$message))
        NULL
      }
    )

    if (!is.null(fit)) {
      min_cvm <- min(fit$cvm)
      cat(sprintf("  alpha=%.2f: CV MSE = %.4f\n", alpha, min_cvm))
      if (min_cvm < best_cvm) {
        best_cvm <- min_cvm
        best_alpha <- alpha
        best_fit <- fit
      }
    }
  }

  if (is.null(best_fit)) stop(sprintf("All alphas failed for %s", model_name))
  cat(sprintf("  Best: alpha=%.2f, CV MSE=%.4f\n", best_alpha, best_cvm))

  n_nonzero <- sum(as.matrix(coef(best_fit, s = "lambda.min"))[-1, 1] != 0)
  cat(sprintf("  Non-zero: %d / %d\n", n_nonzero, ncol(X)))

  list(fit = best_fit, alpha = best_alpha, cvm = best_cvm)
}

# 13. Extract Symmetric Coefficients Helper ----

extract_symmetric_coefs <- function(model, skill_cols, train_sds, type) {
  coefs <- as.numeric(coef(model$fit, s = "lambda.min"))[-1]
  n <- length(skill_cols)
  home_coefs <- coefs[1:n]
  away_coefs <- coefs[(n + 1):(2 * n)]
  home_sds <- train_sds[paste0("home_", skill_cols)]

  player_beta <- if (type == "margin") {
    (home_coefs - away_coefs) / 2
  } else if (type == "offense") {
    (home_coefs + away_coefs) / 2
  } else {
    -(home_coefs + away_coefs) / 2
  }

  data.frame(
    stat_name = skill_cols,
    beta = player_beta,
    sd = as.numeric(home_sds),
    stringsAsFactors = FALSE
  )
}

# 14. Train and Evaluate Helper ----

evaluate_model <- function(model, X_test_std, y_test, name) {
  if (is.null(X_test_std) || length(y_test) == 0) return(invisible(NULL))
  pred <- predict(model$fit, X_test_std, s = "lambda.min")
  rmse <- sqrt(mean((y_test - pred)^2))
  r <- cor(y_test, as.numeric(pred))
  cat(sprintf("  %s: RMSE=%.3f, r=%.3f\n", name, rmse, r))
}

train_and_save <- function(y_margin, y_off, y_def, y_margin_test, y_off_test,
                            y_def_test, prefix, label) {
  cat(sprintf("\n%s\n=== Training %s Models ===\n%s\n",
              paste(rep("=", 50), collapse = ""), label,
              paste(rep("=", 50), collapse = "")))

  margin_m <- train_model(X_train_std, y_margin, weights, fold_ids,
                            ALPHA_GRID, paste(label, "Margin"))
  off_m <- train_model(X_train_std, y_off, weights, fold_ids,
                         ALPHA_GRID, paste(label, "Offense"))
  def_m <- train_model(X_train_std, y_def, weights, fold_ids,
                         ALPHA_GRID, paste(label, "Defense"))

  if (sum(is_test) > 0) {
    cat(sprintf("\n--- %s Test Set ---\n", label))
    evaluate_model(margin_m, X_test_std, y_margin_test, "Margin")
    evaluate_model(off_m, X_test_std, y_off_test, "Offense")
    evaluate_model(def_m, X_test_std, y_def_test, "Defense")
    baseline_rmse <- sqrt(mean(y_margin_test^2))
    cat(sprintf("  Baseline (predict 0): RMSE=%.3f\n", baseline_rmse))
  }

  # Extract coefficients
  margin_coefs <- extract_symmetric_coefs(margin_m, skill_keep_cols, train_sds, "margin")
  osr_coefs <- extract_symmetric_coefs(off_m, skill_keep_cols, train_sds, "offense")
  dsr_coefs <- extract_symmetric_coefs(def_m, skill_keep_cols, train_sds, "defense")

  # Top coefficients
  cat(sprintf("\nTop 15 %s margin coefficients:\n", label))
  top <- margin_coefs[order(-abs(margin_coefs$beta)), ]
  print(head(top, 15), row.names = FALSE)

  # Save
  write.csv(margin_coefs, file.path(extdata_dir, paste0(prefix, "psr_coefficients.csv")),
            row.names = FALSE)
  write.csv(osr_coefs, file.path(extdata_dir, paste0(prefix, "osr_coefficients.csv")),
            row.names = FALSE)
  write.csv(dsr_coefs, file.path(extdata_dir, paste0(prefix, "dsr_coefficients.csv")),
            row.names = FALSE)

  cat(sprintf("\nSaved: %spsr_coefficients.csv (%d non-zero)\n",
              prefix, sum(margin_coefs$beta != 0)))

  list(
    margin = margin_m, offense = off_m, defense = def_m,
    margin_coefs = margin_coefs, osr_coefs = osr_coefs, dsr_coefs = dsr_coefs
  )
}

# 15. Train xG Models (Primary) ----

xg_models <- NULL
if (has_xg) {
  # Filter to matches with xG data
  has_xg_mask_train <- is_train & !is.na(train_data$xg_diff)
  has_xg_mask_test <- is_test & !is.na(train_data$xg_diff)

  X_xg_train <- X_train_std[has_xg_mask_train[is_train], ]
  w_xg <- weights[has_xg_mask_train[is_train]]
  fold_xg <- fold_ids[has_xg_mask_train[is_train]]
  # Reindex folds to be sequential
  fold_xg <- as.integer(factor(fold_xg))

  xg_models <- train_and_save(
    y_margin = train_data$xg_diff[has_xg_mask_train],
    y_off = train_data$home_xg[has_xg_mask_train],
    y_def = train_data$away_xg[has_xg_mask_train],
    y_margin_test = train_data$xg_diff[has_xg_mask_test],
    y_off_test = train_data$home_xg[has_xg_mask_test],
    y_def_test = train_data$away_xg[has_xg_mask_test],
    prefix = "",
    label = "xG"
  )
}

# 16. Train Goal Diff Models (Secondary) ----

gd_models <- train_and_save(
  y_margin = train_data$goal_diff[is_train],
  y_off = train_data$home_goals[is_train],
  y_def = train_data$away_goals[is_train],
  y_margin_test = if (sum(is_test) > 0) train_data$goal_diff[is_test] else numeric(0),
  y_off_test = if (sum(is_test) > 0) train_data$home_goals[is_test] else numeric(0),
  y_def_test = if (sum(is_test) > 0) train_data$away_goals[is_test] else numeric(0),
  prefix = "gd_",
  label = "Goal Diff"
)

# 17. Validate: PSR for Latest Skills ----

cat("\n=== Validation: PSR for Latest Date ===\n\n")

# Use the saved latest skills (prematch_skills was freed in section 7)
latest_date <- latest_skill_date
latest_skills <- latest_skills_for_validation

if (!is.null(latest_skills) && nrow(latest_skills) > 0) {
  # Use xG coefficients if available, otherwise goal diff
  coefs <- if (!is.null(xg_models)) xg_models else gd_models

  psr_result <- tryCatch(
    calculate_psr_components(
      latest_skills, coefs$margin_coefs, coefs$osr_coefs, coefs$dsr_coefs
    ),
    error = function(e) {
      cat(sprintf("PSR validation failed: %s\n", e$message))
      NULL
    }
  )

  if (!is.null(psr_result) && nrow(psr_result) > 0) {
    cat(sprintf("PSR computed for %d players (as of %s)\n",
                nrow(psr_result), latest_date))
    cat(sprintf("PSR range: [%.3f, %.3f], sd: %.3f\n",
                min(psr_result$psr), max(psr_result$psr), sd(psr_result$psr)))

    cat("\nTop 20 by PSR:\n")
    top20 <- psr_result[order(-psr)][1:min(20, nrow(psr_result))]
    print(top20[, .(player_name, primary_position,
                     psr = round(psr, 3), osr = round(osr, 3),
                     dsr = round(dsr, 3))],
          row.names = FALSE)
  }
}

# 18. Save Full Model Objects ----

cat("\n=== Saving Model Objects ===\n\n")

psr_model_data <- list(
  xg_models = xg_models,
  gd_models = gd_models,
  skill_cols = skill_keep_cols,
  train_sds = train_sds,
  feature_cols = feature_cols,
  test_season = test_season_year,
  has_xg = has_xg,
  metadata = list(
    n_train_matches = sum(is_train),
    n_test_matches = sum(is_test),
    n_features = length(feature_cols),
    n_skill_dates = dates_processed,
    alpha_grid = ALPHA_GRID,
    decay_days = MATCH_WEIGHT_DECAY_DAYS,
    min_w90 = MIN_W90_FOR_SKILLS,
    created = Sys.time()
  )
)

saveRDS(psr_model_data, file.path(cache_dir, "07_psr_model.rds"))
cat(sprintf("Saved to %s/07_psr_model.rds\n", cache_dir))

cat("\n=== COMPLETE ===\n")
