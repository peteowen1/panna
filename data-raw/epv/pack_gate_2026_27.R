# Fair out-of-time gate for the pack's xG/xGOT (2026-09-23): 2026-27, a season
# neither the new final models (trained on the 170 chunk league-seasons, to
# 2025-26) nor the current ones (2026-09-12 / 2026-07-23) trained on -- the
# 2025-26 holdout was unfair because Opta's big-chance tag rate rose 14.0% ->
# 16.5% over 2021-26 while goals per shot stayed flat (drift the current model
# had already seen). Goals / predicted, non-penalty; 1.00 is perfect.
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
P <- "data-raw/cache/epv/pack-2026-09"
f <- file.path(opta_data_dir(), "opta_shot_events.parquet")
s <- as.data.table(arrow::read_parquet(f))[season == "2026-2027"]
cal <- function(ft, p, lab) {
  d <- data.table(goal = ft$is_goal, p = p, h = ft$is_header, pen = ft$is_penalty)[pen %in% 0]
  rbind(d[, .(cut = "all", shots = .N, goals = sum(goal), pred = round(sum(p), 1))],
        d[, .(shots = .N, goals = sum(goal), pred = round(sum(p), 1)), by = .(cut = fifelse(h %in% 1, "header", "foot"))])[
    , `:=`(model = lab, goals_per_pred = round(goals / pred, 3))][]
}
ft <- prepare_shots_for_xg(as.data.frame(s))
cat("xG, 2026-27:\n")
print(rbind(cal(ft, predict_xg(readRDS(file.path(P, "xg_model.rds")), ft), "new"),
            cal(ft, predict_xg(readRDS("data-raw/cache/epv/xg_model.rds"), ft), "current")))
fo <- prepare_shots_for_xgot(as.data.frame(s))
cat("\nxGOT, 2026-27 on-target:\n")
print(rbind(cal(fo, predict_xgot(readRDS(file.path(P, "xgot_model.rds")), fo), "new"),
            cal(fo, suppressWarnings(predict_xgot(readRDS(file.path(opta_data_dir(), "models", "xgot_model.rds")), fo)), "current")))
