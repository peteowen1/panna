# 11_simulate_wc2026.R
# Simulate the 2026 World Cup using the latest match predictions.
#
# Inputs:
#   cache-predictions-opta/07_predictions.rds   — fixture predictions (incl. WC)
#   cache-predictions-opta/04_match_dataset.rds — per-team feature rows
#   cache-predictions-opta/05_goals_model.rds   — goals models
#   cache-predictions-opta/06_outcome_model.rds — outcome model
#   cache-predictions-opta/wc2026_groups.rds    — hand-curated team-to-group map
#
# Outputs:
#   cache-predictions-opta/wc2026_bt_ratings.parquet         — BT strength (diagnostic)
#   cache-predictions-opta/wc2026_simulation.parquet         — per-team trophy/exit probs
#   cache-predictions-opta/wc2026_group_expectations.parquet — group standings probabilities
#
# Pipeline:
#   1. Filter predictions to WC 2026 group-stage matches
#   2. Build the full-model knockout matchup lookup (all 1128 pairs)
#   3. Fit Bradley-Terry ratings (kept only as a reported diagnostic)
#   4. Simulate `wc_sim_n` tournaments with run-hot Elo momentum
#   5. Save ratings, summary, and group expectations

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!exists("wc_sim_n")) wc_sim_n <- 10000L
if (!exists("wc_sim_seed")) wc_sim_seed <- 2026L
# Run-hot momentum strength (Elo K-factor). 0 disables momentum.
if (!exists("wc_sim_elo_k")) wc_sim_elo_k <- 20

library(data.table)
devtools::load_all()   # WC2026_LEAGUE / WC2026_SEASON_LABEL from R/constants.R

# 2. Load inputs ----

preds_path  <- file.path(cache_dir, "07_predictions.rds")
stopifnot(file.exists(preds_path))

# WC group assignments — try the cache first (lets devs override locally for
# what-if scenarios), then fall back to the package asset shipped at
# inst/extdata/wc2026_groups.csv. The CSV is the source of truth (human-
# editable, version-controlled); the legacy cache RDS is a dev convenience.
groups_cache_rds <- file.path(cache_dir, "wc2026_groups.rds")
groups_pkg_csv   <- system.file("extdata", "wc2026_groups.csv", package = "panna")
groups <- if (file.exists(groups_cache_rds)) {
  as.data.table(readRDS(groups_cache_rds))
} else if (nzchar(groups_pkg_csv) && file.exists(groups_pkg_csv)) {
  data.table::fread(groups_pkg_csv)
} else {
  stop("wc2026_groups not found: neither ", groups_cache_rds,
       " nor inst/extdata/wc2026_groups.csv is available")
}

preds <- as.data.table(readRDS(preds_path))
match_dataset <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models  <- readRDS(file.path(cache_dir, "05_goals_model.rds"))
outcome_model <- readRDS(file.path(cache_dir, "06_outcome_model.rds"))

wc <- preds[league == WC2026_LEAGUE & season == WC2026_SEASON_LABEL &
              home_team != "" & away_team != ""]
n_teams <- uniqueN(c(wc$home_team, wc$away_team))
message(sprintf("Simulating WC2026 from %d group-stage predictions across %d teams",
                nrow(wc), n_teams))

# 3. Full-model knockout matchup lookup ----
# Every knockout tie is predicted with the same 170-feature goals + outcome
# models as the group stage (no Bradley-Terry compression).

knockout <- build_knockout_lookup(
  match_dataset  = match_dataset,
  goals_models   = goals_models,
  outcome_result = outcome_model,
  verbose        = TRUE
)

# 4. Bradley-Terry ratings — diagnostic only ----
# No longer used by the simulator; kept as a published team-strength summary
# (wc2026_team_full_ranks.R consumes wc2026_bt_ratings.parquet).
#
# Fit on the FULL 1128-pair knockout lookup, not the 72 group fixtures: with
# group fixtures only, each team is compared solely against its 3 group
# rivals, so the comparison graph is 12 disconnected components and
# cross-group rating levels are optimizer artifacts (a dominant team in a
# weak group floats to #1 — the "Switzerland tops BT" bug, blog #269). The
# lookup compares every team with every other team through the same blended
# match model, making the graph fully connected and the ratings globally
# comparable. Lookup probs are orientation-averaged; host home_field is
# baked into pairings involving USA/CAN/MEX, which is intentional — BT now
# reads as "expected strength at THIS tournament".

bt_pairs <- data.frame(
  home_team = knockout$probs$t1,
  away_team = knockout$probs$t2,
  prob_H    = knockout$probs$p_t1,
  prob_D    = knockout$probs$p_draw,
  prob_A    = knockout$probs$p_t2
)
bt <- fit_bt_ratings(bt_pairs, neutral = TRUE, verbose = TRUE)
bt_with_groups <- as.data.table(merge(bt$ratings, groups, by = "team"))
setorder(bt_with_groups, rank)
arrow::write_parquet(bt_with_groups,
                       file.path(cache_dir, "wc2026_bt_ratings.parquet"))

cat("\n=== Top 20 by BT rating ===\n")
top_bt <- copy(bt_with_groups)
setorder(top_bt, -rating)
print(top_bt[1:20, .(rank, group, team, rating)])

# 5. Tournament simulation (full-model knockouts + run-hot momentum) ----

set.seed(wc_sim_seed)
sim <- simulate_world_cup(
  predictions = wc,
  groups      = groups,
  knockout    = knockout,
  n_sims      = wc_sim_n,
  elo_k       = wc_sim_elo_k,
  verbose     = TRUE
)

arrow::write_parquet(sim$summary,
                       file.path(cache_dir, "wc2026_simulation.parquet"))
arrow::write_parquet(sim$group_table,
                       file.path(cache_dir, "wc2026_group_expectations.parquet"))

cat("\n=== Top 16 by Champion probability ===\n")
print(head(as.data.table(sim$summary), 16))

cat("\n=== Expected group standings (advance = pos1 + pos2) ===\n")
gt <- as.data.table(sim$group_table)
gt[, advance := round(pos1 + pos2, 1)]
print(gt[, .(group, team, pos1, pos2, pos3, pos4, advance)], nrows = 50)

invisible(sim)
