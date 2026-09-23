# Are EPV's training labels priced with a head-blind xG? (2026-09-23)
# create_next_xg_labels() logs "No xG values found, estimating from position":
# the EPV model then learns chance values from estimate_simple_xg(x, y), which
# cannot tell a header from a shot. If headers are over-valued there, that is
# why a won header in the box is worth more to EPV than the header's real xG.
# Real goals as the referee. Read-only. Run from panna/.
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
x <- readRDS("data-raw/cache/epv/net-goals/ng_inputs_ENG_2024-2025.rds"); ep <- as.data.table(x$ep)
sev <- as.data.table(load_opta_shot_events("ENG", season = "2024-2025", source = "local"))
s <- merge(ep[action_type == "shot" & !(is_own_goal %in% TRUE)],
           sev[, .(match_id, original_event_id = event_id, body_part, situation)],
           by = c("match_id", "original_event_id"))
s <- s[!(situation %in% "Penalty")]
s[, `:=`(simple = estimate_simple_xg(start_x, start_y), goal = result %in% "success",
         kind = fifelse(body_part %in% "Head", "Header", "Foot"))]
cat("ENG 2024-25 non-penalty shots:", nrow(s), "\n")
print(s[, .(shots = .N, goals = sum(goal), goal_rate = round(mean(goal), 3),
            real_xg = round(mean(xg), 3), position_only_xg = round(mean(simple), 3),
            position_only_over_goals = round(sum(simple) / sum(goal), 2)), by = kind])
