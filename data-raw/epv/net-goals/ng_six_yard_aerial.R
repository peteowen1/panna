# Anchor check (Pete, 2026-09-23): the smoke-test EPV says a header won in the
# 6-yard box is worth 0.061. What actually follows one? ENG 2024-25, attacking
# side's won aerials in the 6-yard box (x > 94.8, 36.8 < y < 63.2 in SPADL's
# 0-100 frame), and the real xG / goals of what came next. Read-only.
suppressPackageStartupMessages(library(data.table))
x <- readRDS("data-raw/cache/epv/net-goals/ng_inputs_ENG_2024-2025.rds"); ep <- as.data.table(x$ep)
setorder(ep, match_id, action_id)
ep[, `:=`(n1_type = shift(action_type, -1), n1_team = shift(team_id, -1), n1_pl = shift(player_id, -1),
          n1_xg = shift(epv, -1), n1_res = shift(result, -1)), by = match_id]
a <- ep[action_type == "aerial" & result %in% "success" & start_x > 94.8 & start_y > 36.8 & start_y < 63.2]
cat("won aerials in the 6-yard box (attacking side):", nrow(a), "\n")
a[, nxt := fcase(n1_type == "shot" & n1_pl == player_id, "his own header at goal",
                 n1_type == "shot" & n1_team == team_id, "a team-mate's shot",
                 n1_team == team_id, "his side keeps it, no shot yet",
                 default = "the other side has it next")]
print(a[, .(n = .N, share = round(.N / nrow(a), 3), mean_shot_xg = round(mean(fifelse(n1_type == "shot", n1_xg, NA_real_), na.rm = TRUE), 3),
            goals = sum(n1_type == "shot" & n1_res %in% "success")), by = nxt][order(-n)])
own <- a[nxt == "his own header at goal"]
cat("\nhis own header at goal:", nrow(own), "| mean real xG", round(mean(own$n1_xg), 3), "| goal rate", round(mean(own$n1_res %in% "success"), 3), "\n")
cat("expected goals from the next action, over ALL", nrow(a), "won aerials:",
    round(sum(fifelse(a$n1_type == "shot" & a$n1_team == a$team_id, a$n1_xg, 0)) / nrow(a), 3),
    "| actual goals per won aerial:", round(sum(a$n1_type == "shot" & a$n1_res %in% "success" & a$n1_team == a$team_id) / nrow(a), 3), "\n")
cat("current EPV (position-only labels) at these rows, mean:", round(mean(a$epv), 3), "\n")
