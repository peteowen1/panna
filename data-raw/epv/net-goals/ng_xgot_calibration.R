# Is the xGOT model the ledger uses still calibrated? (2026-09-23)
# xGOT (pannamodels `epv` xgot_model.rds, 2026-07-23 -- no newer build exists)
# splits every shot into strike and finish and names keepers on the finish.
# On-target shots only (xGOT > 0), own goals out. Calibrated = goals / xGOT
# near 1 in every cut. Read-only. Run from panna/: Rscript data-raw/epv/net-goals/ng_xgot_calibration.R
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
x <- readRDS("data-raw/cache/epv/net-goals/ng_inputs_ENG_2024-2025.rds"); ep <- as.data.table(x$ep)
sev <- as.data.table(load_opta_shot_events("ENG", season = "2024-2025", source = "local"))
s <- ep[action_type == "shot" & is.finite(xgot) & xgot > 0 & !(is_own_goal %in% TRUE)]
s <- merge(s, sev[, .(match_id, original_event_id = event_id, body_part, situation)],
           by = c("match_id", "original_event_id"), all.x = TRUE)
s[, goal := result %in% "success"]
cat("on-target shots:", nrow(s), "| body part found on", round(100 * mean(!is.na(s$body_part)), 1), "%\n\n")
cal <- function(by) s[, .(shots = .N, goals = sum(goal), xgot = round(sum(xgot), 1),
                          goals_per_xgot = round(sum(goal) / sum(xgot), 3)), by = by][order(-shots)]
cat("All:\n"); print(cal(NULL))
cat("\nBy body part:\n"); print(cal("body_part"))
cat("\nBy situation:\n"); print(cal("situation"))
s[, band := cut(xgot, c(0, .1, .2, .4, .6, .8, 1), include.lowest = TRUE)]
cat("\nBy xGOT band:\n"); print(s[, .(shots = .N, mean_xgot = round(mean(xgot), 3), goal_rate = round(mean(goal), 3)), keyby = band])
