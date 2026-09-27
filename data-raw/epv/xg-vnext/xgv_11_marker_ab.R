suppressMessages(devtools::load_all(quiet = TRUE))
library(data.table)
MARKERS <- c(90L, 91L)
epv_model <- readRDS("data-raw/cache/epv/pack-2026-09/epv_model_pubv0.rds")
ev <- as.data.table(suppressMessages(load_opta_match_events("ENG2", season = "2026-2027")))
score <- function(e) {
  s <- suppressMessages(convert_opta_to_spadl(e))
  ch <- suppressMessages(create_possession_chains(s)); co <- suppressMessages(add_next_chain_outcome(classify_chain_outcomes(ch)))
  lab <- suppressMessages(create_next_goal_labels(label_actions_with_outcomes(ch, co)))
  as.data.table(suppressMessages(calculate_action_epv(lab, features = NULL, epv_model, league = "ENG2", season = "2026-2027")))
}
A <- score(ev); B <- score(ev[!as.integer(type_id) %in% MARKERS])
A[, oe := as.character(original_event_id)]; B[, oe := as.character(original_event_id)]
A[, is_marker := opta_type_id %in% MARKERS]
setorder(A, match_id, action_id)
A[, near := shift(is_marker, 1, fill = FALSE) | shift(is_marker, -1, fill = FALSE), by = match_id]
m <- merge(A[!(is_marker), .(oe, near, a = epv_delta, type = action_type)], B[, .(oe, b = epv_delta)], by = "oe")
cat(sprintf("actions: A %d, B %d; markers in A %d (EPV delta on markers: sum %.2f, sum|.| %.2f)\n",
            nrow(A), nrow(B), sum(A$is_marker), sum(A[is_marker == TRUE, epv_delta], na.rm = TRUE), sum(abs(A[is_marker == TRUE, epv_delta]), na.rm = TRUE)))
print(m[, .(n = .N, changed = sum(abs(a - b) > 1e-9, na.rm = TRUE), mean_abs_change = signif(mean(abs(a - b), na.rm = TRUE), 3),
            sum_abs_change = round(sum(abs(a - b), na.rm = TRUE), 2), sum_change = round(sum(a - b, na.rm = TRUE), 2),
            sum_abs_delta = round(sum(abs(a), na.rm = TRUE), 1)), by = near])
print(m[near == TRUE][order(-abs(a - b))][1:6, .(type, before = round(a, 3), without_marker = round(b, 3))])
