# How much value sits in what a receiver does before his next action?
# =============================================================================
# torp books a disposal row (split passer / receiver) AND a reception row
# (receipt -> the receiver's next disposal, all his). panna's pass row runs from
# the pass start to the receiver's NEXT action, so his carry is split with the
# passer by xPass. Before building a carry row, size it (Pete, 2026-09-23).
#
# The model has no value at the spot a pass ends, so this uses a first-order
# stand-in: the season's mean model EPV in each 5 x 5 zone of the pitch (every
# action's start position, in its own attacking direction). Carry value =
# zone value where the receiver's next action starts - zone value where the
# pass ended. Compared with the pass rows' own |epv_delta|.
#
# Run from panna/: Rscript data-raw/epv/net-goals/ng_carry_sizing.R
suppressPackageStartupMessages(library(data.table))
say <- function(...) cat(..., "\n", sep = "")
x <- readRDS("data-raw/cache/epv/net-goals/ng_ledger_ENG_2024-2025.rds")
ep <- x$ep[order(match_id, action_id)]
stopifnot(all(c("start_x", "start_y", "end_x", "end_y", "epv") %in% names(ep)))
say("actions ", nrow(ep), " | matches ", uniqueN(ep$match_id))

zone <- function(v) pmin(pmax(floor(v / 5), 0), 19)
grid <- ep[is.finite(start_x) & is.finite(epv),
           .(v = mean(epv), n = .N), by = .(zx = zone(start_x), zy = zone(start_y))]
say("zones ", nrow(grid), " (median n ", median(grid$n), ")")

ep[, `:=`(nx = shift(start_x, -1), ny = shift(start_y, -1), nteam = shift(team_id, -1),
          nplayer = shift(player_id, -1)), by = match_id]
p <- ep[action_type == "pass" & result %in% "success" & nteam == team_id &
          is.finite(end_x) & is.finite(nx)]
p[, gap_m := sqrt(((nx - end_x) * 1.05)^2 + ((ny - end_y) * 0.68)^2)]
p[, `:=`(ezx = zone(end_x), ezy = zone(end_y), nzx = zone(nx), nzy = zone(ny))]
p <- merge(p, grid[, .(ezx = zx, ezy = zy, v_end = v)], by = c("ezx", "ezy"), all.x = TRUE)
p <- merge(p, grid[, .(nzx = zx, nzy = zy, v_next = v)], by = c("nzx", "nzy"), all.x = TRUE)
p[, carry := v_next - v_end]
stopifnot(nrow(p) > 100000, mean(is.finite(p$carry)) > 0.95)

say("\ncompleted passes followed by a team-mate's action: ", nrow(p))
say("gap between pass end and next action start (metres): median ",
    round(median(p$gap_m), 1), ", share over 5 m ", round(100 * mean(p$gap_m > 5), 1),
    "%, over 15 m ", round(100 * mean(p$gap_m > 15), 1), "%")
say("sum |pass epv_delta| ", round(sum(abs(p$epv_delta), na.rm = TRUE), 1),
    " goals | sum |carry (zone estimate)| ", round(sum(abs(p$carry), na.rm = TRUE), 1),
    " goals | ratio ", round(sum(abs(p$carry), na.rm = TRUE) / sum(abs(p$epv_delta), na.rm = TRUE), 3))
say("net carry value ", round(sum(p$carry, na.rm = TRUE), 1), " goals over the season")
# Who would it move? Receivers' carry value per player, top 10
nm <- unique(ep[!is.na(player_name), .(player_id, player_name)], by = "player_id")
r <- p[, .(carries = .N, carry = sum(carry, na.rm = TRUE)), by = .(player_id = nplayer)]
r <- merge(r, nm, by = "player_id", all.x = TRUE)
say("\nreceivers with the most carry value (season goals, zone estimate):")
print(r[order(-carry)][1:10, .(player_name, carries, carry = round(carry, 2))])

# What is actually MISALLOCATED today: the pass row already gives the receiver
# xPass of the whole delta, carry included, so the passer holds (1 - xPass) of
# the receiver's carry. That slice is what a carry row would move.
m <- p[is.finite(xpass) & is.finite(carry)]
m[, mis := carry * (1 - xpass)]
say("\nmisallocated to the passer, carry x (1 - xPass): sum |.| ", round(sum(abs(m$mis)), 1),
    " goals, net ", round(sum(m$mis), 1), " | as share of all |pass delta| ",
    round(sum(abs(m$mis)) / sum(abs(m$epv_delta), na.rm = TRUE), 3))
pp <- m[, .(to_receiver = sum(mis)), by = .(player_id = nplayer)]
pf <- m[, .(from_passer = -sum(mis)), by = player_id]
mv <- merge(pp, pf, by = "player_id", all = TRUE)
mv[is.na(to_receiver), to_receiver := 0][is.na(from_passer), from_passer := 0]
mv[, move := to_receiver + from_passer]
mv <- merge(mv, nm, by = "player_id", all.x = TRUE)
say("season goals each player would gain (+) or lose (-): sd ", round(sd(mv$move), 3),
    ", max ", round(max(mv$move), 2), ", min ", round(min(mv$move), 2))
print(mv[order(-abs(move))][1:10, .(player_name, move = round(move, 2))])
