# Step 5 anchor (set by Pete before the result, 2026-09-23): the new EPV,
# averaged over the 234 real won 6-yard-box aerials of ENG 2024-25 (attacking
# side), must land in 0.08-0.14. Truth there: 0.081 goals and 0.12 xG from the
# next action (ng_six_yard_aerial.R). Also the won-aerial-then-own-header rows.
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
x <- readRDS("data-raw/cache/epv/net-goals/ng_inputs_ENG_2024-2025.rds"); ep <- as.data.table(x$ep)
setorder(ep, match_id, action_id)
ep[, `:=`(n1_type = shift(action_type, -1), n1_pl = shift(player_id, -1)), by = match_id]
ft <- as.data.table(suppressMessages(create_epv_features_simple(ep)))
M <- list(old_canonical = "data-raw/cache/epv/epv_model_xg_clean_full.rds",
          new_pubxg = "data-raw/cache/epv/pack-2026-09/epv_model_pubxg.rds")
for (n in names(M)) ep[[n]] <- predict_epv_probs(readRDS(M[[n]]), ft)$expected_xg
six <- ep[action_type == "aerial" & result %in% "success" & start_x > 94.8 & start_y > 36.8 & start_y < 63.2]
hdr <- ep[action_type == "aerial" & result %in% "success" & n1_type == "shot" & n1_pl == player_id]
cat("won 6-yard-box aerials (n =", nrow(six), "): mean EPV old", round(mean(six$old_canonical), 3),
    "| new", round(mean(six$new_pubxg), 3), "| ANCHOR 0.08-0.14:",
    if (mean(six$new_pubxg) >= 0.08 && mean(six$new_pubxg) <= 0.14) "PASS" else "FAIL", "\n")
cat("won aerial then own header (n =", nrow(hdr), "): mean EPV at the aerial old", round(mean(hdr$old_canonical), 3),
    "| new", round(mean(hdr$new_pubxg), 3), "| mean real xG of the header", round(mean(shift(ep$epv, -1)[ep$action_type == "aerial" & ep$result %in% "success" & ep$n1_type == "shot" & ep$n1_pl == ep$player_id]), 3), "\n")
