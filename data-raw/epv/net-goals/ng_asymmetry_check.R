# Football twin of torp's np_asymmetry_check.R (2026-09-23): on each player's
# OWN actions, the share of the row he keeps when it gained vs when it lost,
# and what team-mates carry of his losses beyond an even share ("excused").
# Per game, by position, ENG 2024-25, current rules. Read-only.
# Run from panna/: Rscript data-raw/epv/net-goals/ng_asymmetry_check.R
suppressPackageStartupMessages(library(data.table))
say <- function(...) cat(..., "\n", sep = "")
x <- readRDS("data-raw/cache/epv/net-goals/ng_ledger_ENG_2024-2025.rds")
raw <- as.data.table(x$raw); ep <- as.data.table(x$ep)
off <- raw[entry == "offence"]
v <- off[, .(v = sum(value_own)), by = .(match_id, action_id)]
v <- merge(v, ep[, .(match_id, action_id, actor = player_id, action_type)], by = c("match_id", "action_id"))
kept <- off[!is.na(player_id), .(kept = sum(value_own)), by = .(match_id, action_id, player_id)]
own <- merge(v, kept, by.x = c("match_id", "action_id", "actor"), by.y = c("match_id", "action_id", "player_id"), all.x = TRUE)
own[is.na(kept), kept := 0]
say("rows: ", format(nrow(own), big.mark = ","), " | actor paid on ", round(100 * mean(own$kept != 0), 1), "%")
lu <- as.data.table(x$lineups)
gms <- lu[suppressWarnings(as.numeric(minutes_played)) > 0, .(gms = uniqueN(match_id)), by = player_id]
pos <- as.data.table(x$positions)
say("positions cols: ", paste(names(pos), collapse = ", "))
pcol <- intersect(c("position_group", "pos", "position"), names(pos))[1]
pos <- unique(pos[, .(player_id, pos = get(pcol))], by = "player_id")
for (lab in c("all actions", "shots only", "everything but shots")) {
  o <- switch(lab, "all actions" = own, "shots only" = own[action_type == "shot"],
              "everything but shots" = own[action_type != "shot"])
  pl <- o[, .(gain = sum(v[v > 0]), loss = sum(v[v < 0]), kg = sum(kept[v > 0]), kl = sum(kept[v < 0])), by = .(player_id = actor)]
  pl <- merge(merge(pl, pos, by = "player_id"), gms, by = "player_id")[gms >= 10]
  say("\n", lab, ": league share kept of own gains ", round(sum(pl$kg) / sum(pl$gain), 3),
      ", of own losses ", round(sum(pl$kl) / sum(pl$loss), 3))
  out <- pl[, .(players = .N, gain_pg = sum(gain) / sum(gms), loss_pg = sum(loss) / sum(gms),
                kept_of_gains = sum(kg) / sum(gain), kept_of_losses = sum(kl) / sum(loss)), by = pos]
  out[, excused_pg := (kept_of_gains - kept_of_losses) * -loss_pg]
  print(out[order(-excused_pg)][, lapply(.SD, function(z) if (is.numeric(z)) round(z, 3) else z)])
}
