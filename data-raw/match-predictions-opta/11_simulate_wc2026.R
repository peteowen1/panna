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
  g <- as.data.table(readRDS(groups_cache_rds))
  message("  wc2026 groups: cache RDS (", groups_cache_rds, ")")
  # Group letters drive the FIFA 2026 knockout bracket, so a stale RDS
  # silently shadowing the corrected CSV produces wrong knockout paths
  # with no structural symptom (still 12 groups A-L of 4). Compare loudly.
  if (nzchar(groups_pkg_csv) && file.exists(groups_pkg_csv)) {
    csv_g <- data.table::fread(groups_pkg_csv)
    chk <- merge(g[, .(team, group_rds = group)],
                 csv_g[, .(team, group_csv = group)], by = "team", all = TRUE)
    bad <- chk[is.na(group_rds) | is.na(group_csv) | group_rds != group_csv]
    if (nrow(bad) > 0L) {
      warning(sprintf(paste(
        "wc2026_groups.rds cache DISAGREES with inst/extdata/wc2026_groups.csv",
        "for %d team(s): %s — delete the stale RDS unless this is a",
        "deliberate what-if run"),
        nrow(bad), paste(bad$team, collapse = ", ")),
        call. = FALSE, immediate. = TRUE)
    }
  }
  g
} else if (nzchar(groups_pkg_csv) && file.exists(groups_pkg_csv)) {
  message("  wc2026 groups: package CSV (inst/extdata/wc2026_groups.csv)")
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

# Nothing left to simulate? Stop here, loudly.
#
# Post-final this script used to run to completion off zero rows -- fitting
# Bradley-Terry ratings and reporting champion probabilities for a tournament
# already won -- and reported SUCCESS. Then build_knockout_lookup()'s
# constant-aggregates invariant (added 2026-08-12) started aborting on it
# instead, with a message about Argentina's feature values that says nothing
# about the real cause. Name the real cause.
#
# Call the SAME helper the driver's gate calls rather than recomputing the
# count here. A local `sum(wc$status == "fixture")` looks equivalent and is
# not: without `na.rm` a single NA status makes the sum NA, so
# `isTRUE(NA == 0L)` is FALSE and this guard would fail OPEN exactly where the
# gate fails closed. The helper also applies the same blank-team-name filter
# `wc` uses above, so both sides agree about unresolved knockout placeholders.
# Two checks that disagree about the same question are worse than one.
n_remaining <- .wc2026_fixtures_remaining(cache_dir)
if (nrow(wc) == 0L || isTRUE(n_remaining == 0L)) {
  stop(sprintf(paste0(
    "WC2026 has no unplayed fixtures in %s (%d WC row(s), %s still to play).\n",
    "  The tournament is over -- there is nothing to simulate. Steps 11/12/12b/12c\n",
    "  are for a live tournament only."),
    preds_path, nrow(wc),
    if (is.na(n_remaining)) "unknown how many" else as.character(n_remaining)),
    call. = FALSE)
}

# 3. Full-model knockout matchup lookup ----
# Every knockout tie is predicted with the same 170-feature goals + outcome
# models as the group stage (no Bradley-Terry compression).

# Commit marker for step 12 (panna#180 review). Step 11 writes its three
# outputs at different points -- wc2026_bt_ratings.parquet BEFORE
# simulate_world_cup() runs, wc2026_simulation.parquet and
# wc2026_group_expectations.parquet after -- so a failure inside the
# simulation leaves a FRESH bt file beside STALE sim files, all three
# present. Step 12 used to gate on file.exists() across the three, which
# passes in exactly that state and merges two vintages into one published
# wc2026_team_strength.parquet.
#
# Step 11 is non-fatal since panna#194, so "failed partway" no longer stops
# the pipeline -- it is a routine event. Hence a marker, deleted here and
# rewritten only after all three files are on disk: its presence means one
# run produced the complete set.
.wc11_marker <- file.path(cache_dir, ".wc11_outputs_complete")
unlink(.wc11_marker)

knockout <- build_knockout_lookup(
  match_dataset  = match_dataset,
  goals_models   = goals_models,
  outcome_result = outcome_model,
  verbose        = TRUE
)

# Publish the full pairwise lookup (1,128 pairs) — the blog's in-browser
# simulator uses it for real knockout-tie probabilities instead of a
# logistic approximation. Step 12 uploads it to blog-latest.
arrow::write_parquet(
  as.data.frame(knockout$probs)[, c("t1", "t2", "p_t1", "p_draw", "p_t2",
                                    "lambda_t1", "lambda_t2")],
  file.path(cache_dir, "wc2026_knockout_probs.parquet"))

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

# All three outputs are now on disk from THIS run -- see the marker comment
# above build_knockout_lookup(). Written last, deliberately.
writeLines(format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3Z", tz = "UTC"), .wc11_marker)

cat("\n=== Top 16 by Champion probability ===\n")
print(head(as.data.table(sim$summary), 16))

cat("\n=== Expected group standings (advance = pos1 + pos2) ===\n")
gt <- as.data.table(sim$group_table)
gt[, advance := round(pos1 + pos2, 1)]
print(gt[, .(group, team, pos1, pos2, pos3, pos4, advance)], nrows = 50)

invisible(sim)
