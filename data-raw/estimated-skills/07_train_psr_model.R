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
library(Matrix)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
extdata_dir <- file.path("inst", "extdata")
dir.create(extdata_dir, showWarnings = FALSE, recursive = TRUE)

# Match weight decay: more recent matches weighted higher in training
MATCH_WEIGHT_DECAY_DAYS <- 365  # ~1 year half-life

# Elastic net alpha grid.
# Reduced from c(0, 0.25, 0.5, 0.75, 1.0) for FE-augmented training: the FE
# block (~4K cols) makes each cv.glmnet fit ~3 min, so 5 alphas x 9 models
# x 3 min ~ 2h. Pre-FE alpha sweeps showed differences <1% in CV MSE, so a
# single elastic-net alpha is enough. Restore the grid for a final tuned run.
ALPHA_GRID <- c(0.5)

# Minimum players per team-match to include in training
MIN_PLAYERS_PER_TEAM <- 8

# Minimum weighted 90s for a player's skills to be included
MIN_W90_FOR_SKILLS <- 3

# Fixed-effect granularity for league-quality control.
#   - "league_season": one dummy per (competition, season). Absorbs the
#     league-season baseline xG diff (e.g., Belgian league lower than EPL)
#     without stripping individual player effects from team strength.
#     Recommended — clean identification via bridging in UCL/UEL/internationals.
#   - "team_season": one dummy per (team, season). Stronger control but
#     over-corrects: team strength is partly CAUSED by player skill, so
#     absorbing team FE strips elite players (Mbappé, Haaland) of their
#     contribution to their team. Empirically zeros out most offensive betas.
FE_GRANULARITY <- "league_season"

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
  competition = competition[1],
  n_players = .N
), by = .(match_id, team_name)]

home_side <- team_goals[is_home == 1, .(match_id, home_team = team_name,
                                          home_goals = team_goals,
                                          match_date, season_end_year,
                                          competition)]
away_side <- team_goals[is_home == 0, .(match_id, away_team = team_name,
                                          away_goals = team_goals)]

match_outcomes <- home_side[away_side, on = "match_id", nomatch = NULL]
match_outcomes[, goal_diff := home_goals - away_goals]

cat(sprintf("Matches with goal data: %s\n",
            format(nrow(match_outcomes), big.mark = ",")))

# Join team-strength ratings (off/def from RAPM-derived team aggregates).
# These act as unpenalized continuous controls for OPPONENT QUALITY, so the
# skill betas only capture residual effect after controlling for league
# baseline (already handled by league-season FE) AND opponent strength.
#
# Without this, a high-stat-rate player in a weaker league (Veerman in
# Eredivisie, Tavernier in Scottish) gets a high PSR β because the model
# can't tell whether his per-90 productivity is from skill or from facing
# weak defenders. opp_def_rating is calibrated cross-league via RAPM, so
# it provides cleanly comparable opposition-quality signal.
ts_path <- file.path("data-raw", "cache-opta", "team_season_strength.parquet")
if (file.exists(ts_path) && requireNamespace("arrow", quietly = TRUE)) {
  ts <- data.table::as.data.table(arrow::read_parquet(ts_path))
  match_outcomes <- merge(match_outcomes,
                            ts[, .(team_name, season_end_year,
                                    home_off_rating = off_rating,
                                    home_def_rating = def_rating)],
                            by.x = c("home_team","season_end_year"),
                            by.y = c("team_name","season_end_year"), all.x = TRUE)
  match_outcomes <- merge(match_outcomes,
                            ts[, .(team_name, season_end_year,
                                    away_off_rating = off_rating,
                                    away_def_rating = def_rating)],
                            by.x = c("away_team","season_end_year"),
                            by.y = c("team_name","season_end_year"), all.x = TRUE)
  for (c in c("home_off_rating","home_def_rating","away_off_rating","away_def_rating")) {
    match_outcomes[is.na(get(c)), (c) := 0]
  }
  cat(sprintf("Team-strength controls joined: %d / %d matches have any rating\n",
              sum(match_outcomes$home_off_rating != 0 |
                    match_outcomes$away_off_rating != 0),
              nrow(match_outcomes)))
} else {
  match_outcomes[, `:=`(home_off_rating = 0, home_def_rating = 0,
                          away_off_rating = 0, away_def_rating = 0)]
}

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
    cat(sprintf("Condition: %s\n", conditionMessage(e)))
    list()
  }
)
gc(verbose = FALSE)

if (length(prematch_skills) == 0) {
  stop("Batch skill estimation produced no results. Cannot proceed with PSR training.")
}
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

cat(sprintf("\n%d skill features standardized\n", length(feature_cols)))

# CV folds: temporal blocks of seasons. With the FE block bloating each fit's
# wall time, one-fold-per-season (12 folds) becomes prohibitive. Grouping into
# 4 blocks of ~3 seasons each keeps temporal CV intact while cutting wall time
# ~3x. Block boundaries: 2014-2016, 2017-2019, 2020-2022, 2023-2025.
season_blocks <- function(yr) {
  data.table::fcase(
    yr <= 2016L, 1L,
    yr <= 2019L, 2L,
    yr <= 2022L, 3L,
    default = 4L
  )
}
fold_ids <- season_blocks(train_data$season_end_year[is_train])
n_folds <- length(unique(fold_ids))
cat(sprintf("Temporal CV: %d folds (season-block CV)\n", n_folds))

# 11b. Build Competition-Season Fixed Effects ----
#
# Why: the existing skill regression pools across leagues, so a player whose
# stats come from Belgian/Super_Lig matches has the same beta-weighted PSR as
# an EPL player. To make skill betas league-neutral we add an unpenalized
# FE column per (competition, season).
#
# We deliberately do NOT use team-season FE. Team strength is partly CAUSED
# by player skill, so absorbing team FE strips elite players (Mbappé,
# Haaland) of their contribution to their team's attack. The league-season
# baseline is exogenous to individual player skill in a way team strength
# is not, and is the actual confound we care about.
#
# Identification: each match contributes to exactly one (competition, season)
# FE. Cross-league bridging happens via UCL/UEL/international matches where
# teams from different domestic leagues face off. The bridging matches let
# the regression simultaneously estimate league-specific baselines and
# league-invariant skill betas.

build_league_season_fe <- function(rows_dt, all_keys) {
  # rows_dt: data.table with columns season_end_year, competition.
  # all_keys: character vector of "<competition>_<season>" keys (column order).
  # Returns a sparse dgCMatrix with one column per (competition, season).
  n <- nrow(rows_dt)
  row_key <- paste0(rows_dt$competition, "_", rows_dt$season_end_year)
  j_idx <- match(row_key, all_keys)
  keep <- !is.na(j_idx)
  Matrix::sparseMatrix(
    i = seq_len(n)[keep],
    j = j_idx[keep],
    x = 1, dims = c(n, length(all_keys)),
    dimnames = list(NULL, paste0("ls_", all_keys))
  )
}

cat(sprintf("\n=== Building competition-season fixed effects (FE_GRANULARITY=%s) ===\n",
            FE_GRANULARITY))

if (FE_GRANULARITY != "league_season") {
  stop(sprintf("FE_GRANULARITY=%s not supported in this build", FE_GRANULARITY))
}

# Build the universe of (competition, season) keys from training data
fe_rows_train <- train_data[is_train, .(season_end_year, competition)]
ls_keys <- sort(unique(paste0(fe_rows_train$competition, "_", fe_rows_train$season_end_year)))
cat(sprintf("League-seasons: %d distinct keys\n", length(ls_keys)))

FE_train <- build_league_season_fe(fe_rows_train, ls_keys)
cat(sprintf("FE matrix: %d rows x %d cols (sparse)\n",
            nrow(FE_train), ncol(FE_train)))

if (sum(is_test) > 0) {
  fe_rows_test <- train_data[is_test, .(season_end_year, competition)]
  FE_test <- build_league_season_fe(fe_rows_test, ls_keys)
  n_unmapped <- sum(rowSums(FE_test) == 0)
  cat(sprintf("FE test matrix: %d rows x %d cols (%d rows have no FE match)\n",
              nrow(FE_test), ncol(FE_test), n_unmapped))
} else {
  FE_test <- NULL
}

# Stitch FE + opponent-strength controls + skills into a single sparse matrix
n_fe <- ncol(FE_train)
n_skill <- length(feature_cols)

# Opponent-strength controls (continuous, unpenalized) — mirrors EPR's
# opp_def_rating. We deliberately only include each side's OPPONENT defense
# rating (the home team's opponent is the away team, so home faces away_def;
# vice-versa). Including the team's OWN ratings would absorb player-skill
# signal at the team level — exactly the over-control problem we hit when
# we tried team-season FE.
team_strength_cols <- c("away_def_rating","home_def_rating")
n_ts <- length(team_strength_cols)
TS_train <- methods::as(as.matrix(train_data[is_train, ..team_strength_cols]),
                          "CsparseMatrix")
TS_test  <- if (sum(is_test) > 0) {
  methods::as(as.matrix(train_data[is_test, ..team_strength_cols]),
                "CsparseMatrix")
} else NULL

X_train_full <- cbind(FE_train, TS_train, methods::as(X_train_std, "CsparseMatrix"))
if (!is.null(FE_test) && !is.null(TS_test)) {
  X_test_full <- cbind(FE_test, TS_test, methods::as(X_test_std, "CsparseMatrix"))
} else {
  X_test_full <- NULL
}

# penalty.factor: 0 for FE + team-strength controls, 1 for skills
penalty_factors <- c(rep(0, n_fe), rep(0, n_ts), rep(1, n_skill))

cat(sprintf("Combined matrix: %d rows x %d cols (%d FE + %d team-strength + %d skill)\n",
            nrow(X_train_full), ncol(X_train_full), n_fe, n_ts, n_skill))

# 12. Train Model Helper ----

train_model <- function(X, y, w, foldid, alpha_grid, model_name, pf = NULL) {
  cat(sprintf("\n--- Training %s model ---\n", model_name))

  best_alpha <- 0
  best_cvm <- Inf
  best_fit <- NULL

  for (alpha in alpha_grid) {
    cv_args <- list(
      x = X, y = y, weights = w,
      alpha = alpha, foldid = foldid,
      standardize = FALSE,
      type.measure = "mse",
      # With ~4K unpenalized FE columns + 180 penalized skills, the default
      # nlambda=100 path is overkill and dominates wall time. nlambda=30 still
      # explores enough of the path to find lambda.min reliably.
      nlambda = 30
    )
    if (!is.null(pf)) cv_args$penalty.factor <- pf

    t0 <- Sys.time()
    fit <- tryCatch(
      do.call(glmnet::cv.glmnet, cv_args),
      error = function(e) {
        cat(sprintf("  alpha=%.2f: ERROR - %s\n", alpha, e$message))
        NULL
      }
    )
    elapsed <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    if (!is.null(fit)) cat(sprintf("  alpha=%.2f: fit in %ss\n", alpha, elapsed))

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
#
# With FE prepended, the full coefficient vector is:
#   [intercept, FE_1..FE_{n_fe}, home_skill_1..home_skill_n, away_skill_1..away_skill_n]
# We want only the skill block (FE coefs are nuisance controls and discarded).

extract_symmetric_coefs <- function(model, skill_cols, train_sds, type, n_fe = 0L) {
  coefs <- as.numeric(coef(model$fit, s = "lambda.min"))[-1]  # drop intercept
  skill_start <- n_fe + 1L
  n <- length(skill_cols)
  home_coefs <- coefs[skill_start:(skill_start + n - 1L)]
  away_coefs <- coefs[(skill_start + n):(skill_start + 2L * n - 1L)]
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
                            y_def_test, prefix, label,
                            X = X_train_full, w = weights, fids = fold_ids,
                            skill_cols = skill_keep_cols, sds = train_sds,
                            test_mask = is_test, X_test = X_test_full,
                            pf = penalty_factors, fe_count = n_fe + n_ts) {
  cat(sprintf("\n%s\n=== Training %s Models ===\n%s\n",
              paste(rep("=", 50), collapse = ""), label,
              paste(rep("=", 50), collapse = "")))

  margin_m <- train_model(X, y_margin, w, fids, ALPHA_GRID, paste(label, "Margin"), pf = pf)
  off_m    <- train_model(X, y_off,    w, fids, ALPHA_GRID, paste(label, "Offense"), pf = pf)
  def_m    <- train_model(X, y_def,    w, fids, ALPHA_GRID, paste(label, "Defense"), pf = pf)

  if (sum(test_mask) > 0) {
    cat(sprintf("\n--- %s Test Set ---\n", label))
    evaluate_model(margin_m, X_test, y_margin_test, "Margin")
    evaluate_model(off_m, X_test, y_off_test, "Offense")
    evaluate_model(def_m, X_test, y_def_test, "Defense")
    baseline_rmse <- sqrt(mean(y_margin_test^2))
    cat(sprintf("  Baseline (predict 0): RMSE=%.3f\n", baseline_rmse))
  }

  # Extract coefficients (skip FE block)
  margin_coefs <- extract_symmetric_coefs(margin_m, skill_cols, sds, "margin",  n_fe = fe_count)
  osr_coefs    <- extract_symmetric_coefs(off_m,    skill_cols, sds, "offense", n_fe = fe_count)
  dsr_coefs    <- extract_symmetric_coefs(def_m,    skill_cols, sds, "defense", n_fe = fe_count)

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
  # Use which() for unambiguous integer indexing — avoids NA-in-logical edge cases
  train_rows <- which(is_train)
  test_rows  <- which(is_test)

  xg_in_train <- which(!is.na(train_data$xg_diff[train_rows]))
  xg_in_test  <- which(!is.na(train_data$xg_diff[test_rows]))

  X_xg_train <- X_train_full[xg_in_train, ]
  X_xg_test  <- if (length(xg_in_test) > 0 && !is.null(X_test_full)) X_test_full[xg_in_test, ] else NULL
  w_xg       <- weights[xg_in_train]
  fold_xg    <- as.integer(factor(fold_ids[xg_in_train]))

  xg_models <- train_and_save(
    y_margin = train_data$xg_diff[train_rows][xg_in_train],
    y_off    = train_data$home_xg[train_rows][xg_in_train],
    y_def    = train_data$away_xg[train_rows][xg_in_train],
    y_margin_test = train_data$xg_diff[test_rows][xg_in_test],
    y_off_test    = train_data$home_xg[test_rows][xg_in_test],
    y_def_test    = train_data$away_xg[test_rows][xg_in_test],
    prefix = "",
    label  = "xG",
    X = X_xg_train, w = w_xg, fids = fold_xg, X_test = X_xg_test
  )

  # Blended-target models (for the DISPLAYED value PSV): alpha*xG + (1-alpha)*goals,
  # on the same rows/design as the xG model. xG-diff is stable/predictive; goal-diff
  # rewards finishing — the blend credits finishing without pure-goal noise. The ""
  # (xG) and gd_ sets above stay as-is for the RAPM target / other consumers.
  a <- if (exists("psv_blend_alpha")) psv_blend_alpha else 0.6
  cat(sprintf("\n=== Training blended-target models (alpha=%.2f xG / %.2f goals) ===\n",
              a, 1 - a))
  blend_models <- train_and_save(
    y_margin = a * train_data$xg_diff[train_rows][xg_in_train] +
               (1 - a) * train_data$goal_diff[train_rows][xg_in_train],
    y_off    = a * train_data$home_xg[train_rows][xg_in_train] +
               (1 - a) * train_data$home_goals[train_rows][xg_in_train],
    y_def    = a * train_data$away_xg[train_rows][xg_in_train] +
               (1 - a) * train_data$away_goals[train_rows][xg_in_train],
    y_margin_test = a * train_data$xg_diff[test_rows][xg_in_test] +
                    (1 - a) * train_data$goal_diff[test_rows][xg_in_test],
    y_off_test    = a * train_data$home_xg[test_rows][xg_in_test] +
                    (1 - a) * train_data$home_goals[test_rows][xg_in_test],
    y_def_test    = a * train_data$away_xg[test_rows][xg_in_test] +
                    (1 - a) * train_data$away_goals[test_rows][xg_in_test],
    prefix = "blend_",
    label  = sprintf("Blend a=%.2f", a),
    X = X_xg_train, w = w_xg, fids = fold_xg, X_test = X_xg_test
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

# 18. Train GK Sub-Model ----
#
# Separate model for goalkeepers using:
#   - GK-specific features (.get_gk_skill_cols())
#   - Goal differential as target (not xG diff) — GK value is in the
#     goals-saved-above-xG residual, which xG deliberately strips out
#
# Same architecture: team-aggregated GK skills → glmnet → coefficients.
# Since outfield players have ~0 for GK stats (saves, claims, etc.),
# the team sum is effectively just the GK's values.

cat("\n")
cat(paste(rep("=", 50), collapse = ""), "\n")
cat("=== Training GK Sub-Model (Goal Diff Target) ===\n")
cat(paste(rep("=", 50), collapse = ""), "\n\n")

gk_models <- NULL
gk_psr_cols <- .get_gk_skill_cols()
gk_skill_keep_cols <- character(0)

# If we still have pm_with_skills in memory, try to use GK cols from it.
# Otherwise re-derive from the pre-match skills cache.
# The pm_with_skills was freed after section 8, so we need to rebuild
# from the same weekly skill chunks.

# Check if GK features exist in the already-joined data
# If pm_with_skills was freed, we need to reload match_stats and re-join
# pm_with_skills only has outfield skill_keep_cols — GK features were never
# joined. Re-load match stats and re-join GK-specific skills from prematch_skills.
{
  cat("Loading match stats for GK feature extraction...\n")
  ms_dt_gk <- data.table::as.data.table(readRDS(ms_path))
  ms_dt_gk[, match_date := as.Date(match_date)]
  if (!"season_end_year" %in% names(ms_dt_gk)) {
    ms_dt_gk[, season_end_year := data.table::fifelse(
      data.table::month(match_date) >= 7L,
      data.table::year(match_date) + 1L,
      data.table::year(match_date)
    )]
  }

  # Reload prematch_skills if freed
  if (!exists("prematch_skills") || length(prematch_skills) == 0) {
    cat("Re-computing pre-match skills for GK features...\n")
    prematch_skills <- .estimate_prematch_skills_batch(
      match_stats = ms_dt_gk,
      ref_dates = weekly_dates,
      decay_params = decay_params,
      min_weighted_90s = MIN_W90_FOR_SKILLS,
      verbose = TRUE
    )
  }

  # Determine available GK skill columns from first non-empty result
  gk_first_sk <- NULL
  for (d in names(prematch_skills)) {
    if (!is.null(prematch_skills[[d]]) && nrow(prematch_skills[[d]]) > 0) {
      gk_first_sk <- data.table::as.data.table(prematch_skills[[d]])
      break
    }
  }

  if (!is.null(gk_first_sk)) {
    gk_skill_keep_cols <- intersect(gk_psr_cols, names(gk_first_sk))
    cat(sprintf("GK features available: %d / %d\n",
                length(gk_skill_keep_cols), length(gk_psr_cols)))

    if (length(gk_skill_keep_cols) >= 3) {
      # Join GK skills to player-matches (same chunked approach as section 7)
      gk_pm <- ms_dt_gk[, .(match_id, player_id, team_name, is_home,
                              total_minutes, match_date)]
      gk_pm[, skill_date := match_date_to_bin(match_date)]

      gk_skill_col_set <- c("player_id", gk_skill_keep_cols)
      gk_dates_done <- 0L

      for (d_chr in names(prematch_skills)) {
        sk_d <- prematch_skills[[d_chr]]
        if (is.null(sk_d) || nrow(sk_d) == 0) next
        sk_dt <- data.table::as.data.table(sk_d)
        avail <- intersect(gk_skill_col_set, names(sk_dt))
        if (length(avail) < 2) next
        sk_dt <- sk_dt[, avail, with = FALSE]

        d_date <- as.Date(d_chr)
        rows_for_date <- which(gk_pm$skill_date == d_date)
        if (length(rows_for_date) == 0) next

        matched <- sk_dt[gk_pm[rows_for_date], on = "player_id", nomatch = NA]
        for (col in gk_skill_keep_cols) {
          if (col %in% names(matched)) {
            data.table::set(gk_pm, i = rows_for_date, j = col,
                            value = matched[[col]])
          }
        }
        gk_dates_done <- gk_dates_done + 1L
      }

      cat(sprintf("GK skills joined for %d weekly dates\n", gk_dates_done))

      # Impute missing with 0
      for (col in gk_skill_keep_cols) {
        if (col %in% names(gk_pm)) {
          data.table::set(gk_pm, which(is.na(gk_pm[[col]])), col, 0)
        }
      }

      # Weight by minutes and aggregate per team
      gk_weight <- gk_pm$total_minutes / 90
      gk_weight[is.na(gk_weight) | gk_weight <= 0] <- 0
      for (col in gk_skill_keep_cols) {
        if (col %in% names(gk_pm)) {
          data.table::set(gk_pm, j = col, value = gk_pm[[col]] * gk_weight)
        }
      }

      gk_team_skills <- gk_pm[, c(
        lapply(.SD, sum, na.rm = TRUE),
        list(n_players = .N)
      ), by = .(match_id, team_name, is_home), .SDcols = gk_skill_keep_cols]

      gk_team_skills <- gk_team_skills[n_players >= MIN_PLAYERS_PER_TEAM]

      # Home/away feature matrix
      gk_home <- data.table::copy(
        gk_team_skills[is_home == 1, c("match_id", gk_skill_keep_cols), with = FALSE]
      )
      data.table::setnames(gk_home, gk_skill_keep_cols, paste0("home_", gk_skill_keep_cols))

      gk_away <- data.table::copy(
        gk_team_skills[is_home == 0, c("match_id", gk_skill_keep_cols), with = FALSE]
      )
      data.table::setnames(gk_away, gk_skill_keep_cols, paste0("away_", gk_skill_keep_cols))

      gk_train_data <- gk_home[gk_away, on = "match_id", nomatch = NULL]
      gk_train_data <- match_outcomes[gk_train_data, on = "match_id", nomatch = NULL]

      cat(sprintf("GK training data: %s matches x %d features\n",
                  format(nrow(gk_train_data), big.mark = ","),
                  2 * length(gk_skill_keep_cols)))

      # Train/test split (same as outfield)
      gk_is_train <- gk_train_data$season_end_year < test_season_year
      gk_is_test <- gk_train_data$season_end_year >= test_season_year

      gk_feature_cols <- c(paste0("home_", gk_skill_keep_cols),
                            paste0("away_", gk_skill_keep_cols))
      gk_X_train <- as.matrix(gk_train_data[gk_is_train, ..gk_feature_cols])
      gk_X_test <- if (sum(gk_is_test) > 0) {
        as.matrix(gk_train_data[gk_is_test, ..gk_feature_cols])
      } else NULL

      gk_X_train[is.na(gk_X_train) | is.infinite(gk_X_train)] <- 0
      if (!is.null(gk_X_test)) {
        gk_X_test[is.na(gk_X_test) | is.infinite(gk_X_test)] <- 0
      }

      # Weights (same decay)
      gk_anchor_date <- max(gk_train_data$match_date[gk_is_train])
      gk_train_dates <- as.Date(gk_train_data$match_date[gk_is_train])
      gk_days_ago <- as.numeric(gk_anchor_date - gk_train_dates)
      gk_weights <- exp(-gk_days_ago / MATCH_WEIGHT_DECAY_DAYS)
      gk_weights <- gk_weights / mean(gk_weights)

      # Standardize
      gk_train_sds <- apply(gk_X_train, 2, sd, na.rm = TRUE)
      gk_train_sds[gk_train_sds == 0 | is.na(gk_train_sds)] <- 1
      gk_X_train_std <- sweep(gk_X_train, 2, gk_train_sds, "/")
      gk_X_train_std[is.na(gk_X_train_std) | is.infinite(gk_X_train_std)] <- 0
      if (!is.null(gk_X_test)) {
        gk_X_test_std <- sweep(gk_X_test, 2, gk_train_sds, "/")
        gk_X_test_std[is.na(gk_X_test_std) | is.infinite(gk_X_test_std)] <- 0
      }

      # Season folds for CV
      gk_fold_ids <- as.integer(factor(season_blocks(gk_train_data$season_end_year[gk_is_train])))

      cat(sprintf("GK train: %d matches, test: %d matches\n",
                  sum(gk_is_train), sum(gk_is_test)))

      # Build league-season FE for GK matches (using the same helper as the
      # outfield model). Re-use the outfield ls_keys universe to keep columns
      # aligned in case any downstream tooling treats this as the same FE space.
      gk_fe_rows_train <- gk_train_data[gk_is_train, .(season_end_year, competition)]
      FE_gk_train <- build_league_season_fe(gk_fe_rows_train, ls_keys)
      if (sum(gk_is_test) > 0) {
        gk_fe_rows_test <- gk_train_data[gk_is_test, .(season_end_year, competition)]
        FE_gk_test <- build_league_season_fe(gk_fe_rows_test, ls_keys)
      } else {
        FE_gk_test <- NULL
      }
      gk_n_fe <- ncol(FE_gk_train)
      gk_n_skill <- length(gk_feature_cols)
      # Same 4 team-strength controls for the GK model
      gk_TS_train <- methods::as(as.matrix(gk_train_data[gk_is_train, ..team_strength_cols]),
                                    "CsparseMatrix")
      gk_TS_test  <- if (sum(gk_is_test) > 0) {
        methods::as(as.matrix(gk_train_data[gk_is_test, ..team_strength_cols]),
                      "CsparseMatrix")
      } else NULL
      gk_n_ts <- length(team_strength_cols)

      gk_X_train_full <- cbind(FE_gk_train, gk_TS_train,
                                  methods::as(gk_X_train_std, "CsparseMatrix"))
      gk_X_test_full <- if (!is.null(FE_gk_test) && !is.null(gk_TS_test)) {
        cbind(FE_gk_test, gk_TS_test, methods::as(gk_X_test_std, "CsparseMatrix"))
      } else NULL
      gk_penalty_factors <- c(rep(0, gk_n_fe), rep(0, gk_n_ts), rep(1, gk_n_skill))

      cat(sprintf("GK combined matrix: %d rows x %d cols (%d FE + %d team-strength + %d skill)\n",
                  nrow(gk_X_train_full), ncol(gk_X_train_full),
                  gk_n_fe, gk_n_ts, gk_n_skill))

      # Train GK models on GOAL DIFF (not xG diff)
      # GK value is in the goals-saved-above-xG residual
      gk_models <- tryCatch(
        train_and_save(
          y_margin = gk_train_data$goal_diff[gk_is_train],
          y_off = gk_train_data$home_goals[gk_is_train],
          y_def = gk_train_data$away_goals[gk_is_train],
          y_margin_test = if (sum(gk_is_test) > 0) gk_train_data$goal_diff[gk_is_test] else numeric(0),
          y_off_test = if (sum(gk_is_test) > 0) gk_train_data$home_goals[gk_is_test] else numeric(0),
          y_def_test = if (sum(gk_is_test) > 0) gk_train_data$away_goals[gk_is_test] else numeric(0),
          prefix = "gk_",
          label = "GK (Goal Diff)",
          X = gk_X_train_full, w = gk_weights, fids = gk_fold_ids,
          skill_cols = gk_skill_keep_cols, sds = gk_train_sds,
          test_mask = gk_is_test,
          X_test = gk_X_test_full,
          pf = gk_penalty_factors, fe_count = gk_n_fe + gk_n_ts
        ),
        error = function(e) {
          cat(sprintf("GK model training failed: %s\n", e$message))
          NULL
        }
      )

      rm(gk_pm, gk_team_skills, gk_home, gk_away, gk_train_data,
         gk_X_train, gk_X_train_std, gk_X_train_full)
      gc(verbose = FALSE)
    } else {
      cat("Too few GK features available. Skipping GK sub-model.\n")
    }
  }
  rm(ms_dt_gk); gc(verbose = FALSE)
}

if (is.null(gk_models)) {
  cat("\nGK sub-model not trained. GK PSR will be zero until retrained.\n")
} else {
  cat("\nGK sub-model trained successfully.\n")
}


# 19. Save Full Model Objects ----

cat("\n=== Saving Model Objects ===\n\n")

psr_model_data <- list(
  xg_models = xg_models,
  gd_models = gd_models,
  gk_models = gk_models,
  skill_cols = skill_keep_cols,
  gk_skill_cols = gk_skill_keep_cols,
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
