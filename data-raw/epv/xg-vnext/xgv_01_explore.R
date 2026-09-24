# xG / xGOT next round, step 1: look at the data with Pete before fitting anything.
# =============================================================================
# A. Penalty conversion, in-match only, shrunk by league (replaces PENALTY_XG = 0.80)
# B. Big-chance tagging rate and conversion by league-season
# C. Every Opta qualifier that appears on shots: how often, and how often it scores
# D. Example shots: similar spots, one tagged big chance and one not
#
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_01_explore.R
# Writes CSVs to data-raw/cache/epv/xg-vnext/ (gitignored).
suppressMessages({library(data.table); library(arrow); library(dplyr)})
OD  <- "C:/dev/pannaverse/pannadata/data/opta/"
OUT <- "data-raw/cache/epv/xg-vnext"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
say <- function(...) cat("\n", ..., "\n", sep = "")

# ---- shots joined to their event rows (period, qualifiers) -------------------
sh <- as.data.table(collect(open_dataset(paste0(OD, "opta_shot_events.parquet"))))
ev <- as.data.table(collect(open_dataset(list.files(paste0(OD, "events_consolidated"), "^events_.*[.]parquet$", full.names = TRUE)) |>
  filter(type_id %in% c(13, 14, 15, 16)) |> select(match_id, event_id, period_id, qualifier_json)))
sh <- merge(sh, ev, by = c("match_id", "event_id"), all.x = TRUE)
fx <- as.data.table(collect(open_dataset(paste0(OD, "opta_fixtures.parquet")) |> select(match_id, match_date)))
fx <- unique(fx, by = "match_id"); sh <- merge(sh, fx, by = "match_id", all.x = TRUE)
sh[, goal := as.integer(type_id == 16)]
sh[, yr := suppressWarnings(as.integer(substr(season, nchar(season) - 3, nchar(season))))]
say("shots ", nrow(sh), " | with event row ", sum(!is.na(sh$period_id)), " | with a match date ", sum(!is.na(sh$match_date)),
    " | own goals ", sum(sh$is_own_goal %in% TRUE))
sh <- sh[!(is_own_goal %in% TRUE)]
# goal-only feeds: a league-season with shots but not one miss/save/post
cov <- sh[, .(shots = .N, nongoal = sum(goal == 0)), by = .(competition, season)]
goal_only <- cov[nongoal == 0]
say("goal-only league-seasons dropped: ", nrow(goal_only), " (", sum(goal_only$shots), " shots)")
sh <- sh[!goal_only, on = .(competition, season)]

# ---- A. penalties -------------------------------------------------------------
pk <- sh[situation == "Penalty" & period_id %in% 1:4]
p0 <- mean(pk$goal)
lg <- pk[, .(pens = .N, goals = sum(goal)), by = competition]
# beta-binomial method of moments: how much real spread is there between leagues?
lg[, r := goals / pens]
v_obs <- lg[, sum(pens * (r - p0)^2) / sum(pens)]            # weighted observed variance
v_bin <- p0 * (1 - p0) * nrow(lg) / sum(lg$pens)              # expected from binomial noise alone
tau2  <- max(v_obs - v_bin, 0)
k     <- if (tau2 > 0) p0 * (1 - p0) / tau2 - 1 else Inf       # prior strength, in penalties
lg[, shrunk := (goals + k * p0) / (pens + k)]
say("A. PENALTIES in-match (periods 1-4), full-coverage feeds: ", nrow(pk), " taken, ", sum(pk$goal), " scored, rate ",
    round(p0, 5))
say("   between-league spread: observed var ", signif(v_obs, 3), ", binomial noise ", signif(v_bin, 3),
    " -> real sd ", round(sqrt(tau2), 4), " | prior strength k = ", round(k), " penalties")
print(lg[order(-pens)][1:25, .(competition, pens, raw = round(r, 4), shrunk = round(shrunk, 4))])
cat("shrunk range across all", nrow(lg), "leagues:", paste(round(range(lg$shrunk), 4), collapse = " to "), "\n")
fwrite(lg, file.path(OUT, "penalty_by_league.csv"))

# ---- B. big chances by league-season -----------------------------------------
np <- sh[situation != "Penalty" & period_id %in% 1:4]
np[, bc := as.integer(big_chance %in% c(TRUE, 1))]
bcs <- np[, .(shots = .N, bc_rate = round(mean(bc), 4), conv_bc = round(mean(goal[bc == 1]), 4),
              conv_other = round(mean(goal[bc == 0]), 4)), by = .(competition, yr)]
fwrite(bcs, file.path(OUT, "big_chance_by_league_season.csv"))
say("B. BIG CHANCES, all leagues by season end year (share of shots tagged; conversion when tagged / not)")
print(np[, .(shots = .N, bc_rate = round(mean(bc), 4), conv_bc = round(mean(goal[bc == 1]), 4),
             conv_other = round(mean(goal[bc == 0]), 4)), by = yr][order(yr)])
big <- np[, .N, by = competition][N > 40000]$competition
say("   the biggest leagues, tagging rate by season (rows = season end year)")
print(dcast(bcs[competition %in% big & yr >= 2016], yr ~ competition, value.var = "bc_rate"))
say("   same leagues, conversion of a TAGGED big chance")
print(dcast(bcs[competition %in% big & yr >= 2016], yr ~ competition, value.var = "conv_bc"))
say("   across league-seasons (1,000+ shots): does a league that tags more convert its tags less?")
b2 <- bcs[shots >= 1000]
cat("   n =", nrow(b2), " cor(tag rate, conversion of tagged) =", round(cor(b2$bc_rate, b2$conv_bc), 3),
    " | cor(tag rate, conversion of untagged) =", round(cor(b2$bc_rate, b2$conv_other), 3), "\n")

# ---- C. qualifiers on shots ----------------------------------------------------
qs <- np[!is.na(qualifier_json), .(id = .I, goal, q = regmatches(qualifier_json, gregexpr('"[0-9]+"(?=:)', qualifier_json, perl = TRUE)))]
ql <- qs[, .(q = gsub('"', "", unlist(q))), by = .(id, goal)]
qsum <- ql[, .(shots = .N, share = round(.N / nrow(qs), 4), goal_rate = round(mean(goal), 4)), by = q][order(-shots)]
say("C. QUALIFIERS on non-penalty shots (", nrow(qs), " shots with an event row; base goal rate ", round(mean(qs$goal), 4), ")")
print(qsum[shots >= 1000], nrows = 200)
fwrite(qsum, file.path(OUT, "shot_qualifiers.csv"))

# ---- D. example shots ------------------------------------------------------------
ex <- np[competition == "EPL" & yr == 2025 & !is.na(x)]
ex[, dist := round(sqrt(((100 - x) * 1.05)^2 + ((50 - y) * 0.68)^2), 1)]
ex[, band := cut(dist, c(0, 8, 12, 16, 25))]
set.seed(24)
pick <- ex[, .SD[sample(.N, min(.N, 3))], by = .(band, bc)][order(band, -bc)]
say("D. EXAMPLES, EPL 2024-25: shots in the same distance band, tagged big chance (bc = 1) and not")
print(pick[, .(band, bc, player_name, minute, dist, body_part, situation, xg = round(xg, 3), goal)])
fwrite(pick, file.path(OUT, "examples_big_chance.csv"))
