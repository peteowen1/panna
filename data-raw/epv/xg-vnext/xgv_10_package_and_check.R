# xG next round, step 10: package the new models for scoring, and prove the
# scoring path reproduces the training features exactly.
# =============================================================================
# 1. Adds to xg_model_v5.rds / xgot_model_v3.rds the metadata the scoring code
#    reads: na_is_missing = TRUE (predict_xg keeps NA), penalty_xg (current
#    season) and penalty_xg_by_season (pooled earlier-seasons rate, xgv_09).
# 2. End to end on one league-season: SPADL from raw events -> add_xg_to_spadl()
#    and add_xgot_to_spadl() with the new models, events and foot history -> the
#    same shots scored directly from shot_features.parquet. Must agree to 1e-6.
#
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_10_package_and_check.R
suppressMessages({library(data.table); library(arrow); library(dplyr); devtools::load_all(quiet = TRUE)})
X <- "data-raw/cache/epv/xg-vnext"; OD <- "C:/dev/pannaverse/pannadata/data/opta/"
LG <- "ENG"; SE <- "2024-2025"

pr <- fread(file.path(X, "penalty_rates.csv"))
by_season <- unique(pr[, .(season_num, prior)])[order(season_num)]
tab <- setNames(round(by_season$prior, 5), by_season$season_num)
cur <- tab[[as.character(max(as.integer(names(tab))) - 1L)]]   # the season being played: 2026-27 -> 2027
for (f in c("xg_model_v5.rds", "xgot_model_v3.rds")) {
  m <- readRDS(file.path(X, f))
  m$panna_metadata$na_is_missing <- TRUE
  m$panna_metadata$penalty_xg <- cur
  m$panna_metadata$penalty_xg_by_season <- tab
  saveRDS(m, file.path(X, f))
  cat(f, ": na_is_missing TRUE, penalty_xg", cur, ", by season", length(tab), "rows\n")
}
xg5 <- readRDS(file.path(X, "xg_model_v5.rds")); xgot3 <- readRDS(file.path(X, "xgot_model_v3.rds"))

# ---- the scoring path, as a pipeline would run it -------------------------------
events <- load_opta_match_events(LG, season = SE, source = "local")
shot_ev <- load_opta_shot_events(LG, season = SE, source = "local")
fx <- unique(as.data.table(collect(open_dataset(paste0(OD, "opta_fixtures.parquet")) |> select(match_id, match_date))), by = "match_id")
all_shots <- as.data.table(collect(open_dataset(paste0(OD, "opta_shot_events.parquet")) |>
                                     select(match_id, player_id, body_part, is_own_goal)))
all_shots <- merge(all_shots, fx, by = "match_id")
foot_hist <- .shot_foot_history(all_shots)
spadl <- convert_opta_to_spadl(events)
lk <- .epv_shot_lookup(LG, SE)
s1 <- add_xg_to_spadl(spadl, xg5, season = SE, shot_lookup = lk, events = events, foot_history = foot_hist)
s2 <- add_xgot_to_spadl(s1, xgot3, lk, season = SE, events = events, foot_history = foot_hist)
sc <- as.data.table(s2)[action_type == "shot", .(match_id, event_id = as.character(original_event_id), xg_path = xg, xgot_path = xgot)]

# ---- the same shots scored straight from the training features ------------------
sf <- as.data.table(read_parquet(file.path(X, "shot_features.parquet")))
sf <- sf[competition == "EPL" & season == SE & is_penalty == 0]
for (v in xg5$feature_names) if (is.logical(sf[[v]])) set(sf, j = v, value = as.integer(sf[[v]]))
fs <- xg5$feature_names
sf[, xg_train := predict(xg5$model, xgboost::xgb.DMatrix(as.matrix(sf[, ..fs]), missing = NA))]
cmp <- merge(sc, sf[, .(match_id, event_id, xg_train)], by = c("match_id", "event_id"))
d <- abs(cmp$xg_path - cmp$xg_train)
cat(sprintf("\nxG END TO END (%s %s): %d shots compared | max |diff| %.2e | over 1e-6: %d\n", LG, SE, nrow(cmp), max(d), sum(d > 1e-6)))
if (sum(d > 1e-6)) { cmp[, diff := xg_path - xg_train]; print(head(cmp[order(-abs(diff))], 10)) }
cat(sprintf("xGOT scored on %d shots (mean %.3f) | penalties at %.4f\n", sum(!is.na(sc$xgot_path) & sc$xgot_path > 0),
            mean(sc$xgot_path[sc$xgot_path > 0], na.rm = TRUE), cur))
