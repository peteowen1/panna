# Situation-label skew: what does this season's xG look like to a model trained
# on transposed labels? (2026-09-23)
# pannadata#66: scrapes before 2026-06 labelled open play "SetPiece", set
# pieces "Corner", and let corners/free kicks fall to "OpenPlay". The scraper
# was fixed; old data was not repaired; the xG/xGOT models learned the OLD
# labels. So shots scraped since (2026-27, WC 2026) reach the models with
# flags that mean something else. Score them both ways against real goals.
# Calibrated = goals / xG near 1. Read-only.
# Run: powershell.exe -Command 'Rscript "data-raw/epv/net-goals/ng_situation_skew.R"' (from panna/)
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
f <- file.path(opta_data_dir(), "opta_shot_events.parquet")
s <- as.data.table(arrow::read_parquet(f))
xgm <- load_xg_model()
old_label <- function(x) data.table::fcase(x == "OpenPlay", "SetPiece", x == "SetPiece", "Corner",
                                           x == "Corner", "OpenPlay", default = x)
score <- function(d) {
  ft <- prepare_shots_for_xg(as.data.frame(d))
  ft$xg <- predict_xg(xgm, ft)
  ft
}
say <- function(...) cat(..., "\n", sep = "")
for (lab in c("2026-2027", "2025-2026")) {
  d <- s[season == lab & !(situation %in% "Penalty")]
  if ("is_own_goal" %in% names(d)) d <- d[!(is_own_goal %in% TRUE)]   # scoring drops them; keep rows aligned
  a <- score(d)
  say("\n", lab, " (", format(nrow(d), big.mark = ","), " non-penalty shots; label mix: ",
      paste(names(table(d$situation)), round(100 * prop.table(table(d$situation)), 1), sep = " ", collapse = "%, "), "%)")
  goal_col <- intersect(c("is_goal", "goal"), names(a))[1]
  if (lab == "2026-2027") {
    b <- score(copy(d)[, situation := old_label(situation)])
    say("  goals ", sum(a[[goal_col]]), " | xG as labelled now ", round(sum(a$xg), 1), " (goals/xG ", round(sum(a[[goal_col]]) / sum(a$xg), 3),
        ") | xG with labels mapped back to the convention the model learned ", round(sum(b$xg), 1),
        " (", round(sum(b[[goal_col]]) / sum(b$xg), 3), ")")
    cmp <- data.table(situation = d$situation, now = a$xg, old = b$xg, goal = a[[goal_col]])
    print(cmp[, .(shots = .N, goals = sum(goal), xg_now = round(sum(now), 1), xg_old_labels = round(sum(old), 1),
                  g_per_xg_now = round(sum(goal) / sum(now), 3), g_per_xg_old = round(sum(goal) / sum(old), 3)), by = situation][order(-shots)])
  } else {
    say("  control (old labels, as trained): goals ", sum(a[[goal_col]]), " | xG ", round(sum(a$xg), 1),
        " | goals/xG ", round(sum(a[[goal_col]]) / sum(a$xg), 3))
  }
}
