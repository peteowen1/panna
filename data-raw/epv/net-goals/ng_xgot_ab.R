# The xGOT shot split and the shot chain: before vs after, ENG 2024-25
# =============================================================================
# Compares the per-player page data written before the change
# (ng_categories_baseline_pre_xgot.json) with the current build. Checks fixed
# BEFORE looking (2026-09-23):
#   1. every team still sums to its goal difference (gated inside the build);
#   2. the anchor's share falls: the shot chain books 337 goals of |value| that
#      used to go unbooked after shots;
#   3. Pickford, the consensus best shot-stopper of 2024-25, ranks top-5 of
#      keepers on goalkeeping;
#   4. players who neither shoot much nor keep barely move.
#
# Run from panna/: Rscript data-raw/epv/net-goals/ng_xgot_ab.R
suppressPackageStartupMessages({library(data.table); library(jsonlite)})
say <- function(...) cat(..., "\n", sep = "")
D <- "data-raw/cache/epv/net-goals"
rd <- function(f) {
  j <- fromJSON(file.path(D, f))
  p <- as.data.table(j$players)
  list(p = p, m = j$methods)
}
a <- rd("ng_categories_baseline_pre_xgot.json"); b <- rd("ng_categories_artifact.json")
anc <- "Anchor to the real goal difference"
share <- function(x) {
  v <- as.matrix(x$p[, x$m, with = FALSE])
  sum(abs(x$p[[anc]]) * x$p$gms) / sum(abs(v) * x$p$gms)
}
say("anchor share of |value| on the page: before ", round(100 * share(a), 2), "%, after ", round(100 * share(b), 2), "%")

k <- merge(a$p[, .(player_id, name, pos, gms, net0 = net)], b$p[, .(player_id, net1 = net)], by = "player_id")
say("players in both: ", nrow(k), " | per-game net r = ", round(cor(k$net0, k$net1), 4),
    ", rank r = ", round(cor(k$net0, k$net1, method = "spearman"), 4))
k[, move := net1 - net0]
say("\nmean change by position (goals/game):")
print(k[, .(n = .N, before = round(mean(net0), 4), after = round(mean(net1), 4), move = round(mean(move), 4),
            sd_move = round(sd(move), 4)), by = pos][order(move)])

gk <- merge(b$p[pos == "Goalkeeper", .(player_id, name, gms,
                                        gk = get("Stopping shots") + get("Keeper: rebound after a save"))],
            a$p[, .(player_id, gk0 = get("Stopping shots") + get("Keeper: rebound after a save"))],
            by = "player_id")
gk[, rank := frank(-gk)]
say("\nkeepers by goalkeeping per game (stopping shots + rebounds), after vs before:")
print(gk[order(rank)][1:10, .(rank, name, gms, after = round(gk, 3), before = round(gk0, 3))])
say("check 3, Pickford top-5 of ", nrow(gk), " keepers: ", isTRUE(gk[grepl("Pickford", name), rank] <= 5))

sh <- merge(b$p[, .(player_id, name, pos, s1 = Shooting)], a$p[, .(player_id, s0 = Shooting)], by = "player_id")
say("\nshooting per game, biggest movers:")
print(sh[order(-abs(s1 - s0))][1:10, .(name, pos, before = round(s0, 3), after = round(s1, 3))])

other <- k[!pos %in% c("Goalkeeper", "Striker")]
say("\ncheck 4, non-strikers/non-keepers: mean |move| ", round(mean(abs(other$move)), 4),
    " goals/game (net sd ", round(sd(other$net0), 4), ")")
