# xG next round, step 3a: context for every shot, from the full event stream.
# =============================================================================
# One parquet per competition file under data-raw/cache/epv/xg-vnext/context/,
# written as each file finishes; a re-run skips files already done.
#
# Everything here is known BEFORE the shot is struck:
#   shot tags   23 fast break, 89 1 on 1, 215 individual play, 29 assisted,
#               154 intentional assist, 217 2nd assisted, 117 lob, 214 big chance,
#               and the undocumented 328 / 388 / 458 / 468 (presence only; they
#               start in 2017 / 2020 / 2021 / 2022, see xgv_02)
#   assist      last pass by the shooting team tagged 210 (Assist) in the 20 s
#               before the shot: cross 2, through ball 4, pull back 195,
#               chipped 155, lay-off 156, flick-on 168, long ball 1, free kick 5,
#               corner 6, throw-in 107, its length (212) and where it started
#   possession  seconds since the other team last had the ball, and completed
#               passes by the shooting team since
#   rebound     a shot in the same match and period within 5 s before this one
#   game state  shooting team's goals minus the other team's before this shot
#               (own goals, qualifier 28, count for the other side), minute
# Events are ordered by period, minute, second, then event_id; ties inside one
# second keep that order, which is Opta's id order.
#
# Run from panna/:  Rscript data-raw/epv/xg-vnext/xgv_03_shot_context.R
suppressMessages({library(data.table); library(arrow); library(dplyr)})
source(file.path(Sys.getenv("USERPROFILE"), ".claude/lib/runtime_log.R"))
ED  <- "C:/dev/pannaverse/pannadata/data/opta/events_consolidated"
OUT <- "data-raw/cache/epv/xg-vnext/context"; dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
files <- list.files(ED, "^events_.*[.]parquet$", full.names = TRUE)
if (exists("ONLY_FILES")) files <- files[basename(files) %in% ONLY_FILES]   # smoke test
SHOT <- c(13L, 14L, 15L, 16L)
# types where a side has the ball; used to find when the other team last had it
ONBALL <- c(1L, 3L, 12L, 13L, 14L, 15L, 16L, 61L, 50L)
has_q <- function(json, id) grepl(sprintf('"%s":', id), json, fixed = TRUE)
num_q <- function(json, id) suppressWarnings(as.numeric(sub(sprintf('.*"%s":"?([-0-9.]+).*', id), "\\1",
                                                            ifelse(has_q(json, id), json, NA))))

one_file <- function(f) {
  ds <- open_dataset(f)
  ev <- as.data.table(collect(ds |> select(match_id, event_id, type_id, team_id, period_id, minute, second, outcome)))
  ev <- ev[period_id %in% 1:4]
  setorder(ev, match_id, period_id, minute, second, event_id)
  ev[, `:=`(idx = .I, t = minute * 60 + second)]
  sj <- as.data.table(collect(ds |> filter(type_id %in% SHOT) |> select(match_id, event_id, qualifier_json)))
  gl <- sj[, .(match_id, event_id, og = has_q(qualifier_json, "28"))]
  shots <- merge(ev[type_id %in% SHOT], sj, by = c("match_id", "event_id"))
  if (!nrow(shots)) return(NULL)
  teams <- ev[, .(teams = list(unique(team_id))), by = match_id]
  shots[teams, on = "match_id", other := mapply(function(tm, a) setdiff(a, tm)[1], team_id, i.teams)]

  # ---- shot tags
  for (id in c("23", "89", "215", "29", "154", "217", "117", "214", "328", "388", "458", "468"))
    set(shots, j = paste0("q", id), value = as.integer(has_q(shots$qualifier_json, id)))

  # ---- assist pass (tagged 210) by the shooting team, within 20 s
  ap <- as.data.table(collect(ds |> filter(type_id == 1L, grepl('"210":', qualifier_json, fixed = TRUE)) |>
                                select(match_id, event_id, team_id, x, y, qualifier_json)))
  ap <- merge(ap, ev[, .(match_id, event_id, idx, t, period_id)], by = c("match_id", "event_id"))
  for (id in c("2", "4", "195", "155", "156", "168", "1", "5", "6", "107"))
    set(ap, j = paste0("a", id), value = as.integer(has_q(ap$qualifier_json, id)))
  ap[, a_len := num_q(qualifier_json, "212")][, qualifier_json := NULL]
  setnames(ap, c("x", "y", "t", "idx", "period_id"), c("a_x", "a_y", "a_t", "a_idx", "a_period"))
  shots[, j_idx := idx - 0.5]
  asst <- ap[shots[, .(match_id, team_id, event_id, j_idx)], on = .(match_id, team_id, a_idx = j_idx), roll = Inf]
  shots <- cbind(shots, asst[, .SD, .SDcols = patterns("^a[0-9]+$|^a_len$|^a_x$|^a_y$|^a_t$|^a_period$")])
  shots[, has_assist := !is.na(a_t) & a_period == period_id & (t - a_t) <= 20]
  shots[has_assist == FALSE, c(grep("^a[0-9]+$|^a_len$|^a_x$|^a_y$", names(shots), value = TRUE)) := NA]
  shots[, c("a_t", "a_period") := NULL]

  # ---- possession: when did the other team last have the ball?
  # a rolling join returns the LOOKUP's value in the join column, so the matched
  # row's own position rides in a second column (o_pos)
  ob <- ev[type_id %in% ONBALL, .(match_id, team_id, o_idx = idx, o_pos = idx, o_t = t, o_period = period_id)]
  po <- ob[shots[, .(match_id, team_id = other, j_idx)], on = .(match_id, team_id, o_idx = j_idx), roll = Inf]
  shots[, `:=`(opp_idx = po$o_pos, opp_t = po$o_t, opp_period = po$o_period)]
  shots[, poss_secs := fifelse(!is.na(opp_t) & opp_period == period_id, t - opp_t, NA_real_)]
  # completed passes by the shooting team since then (running count per match-team)
  pc <- ev[type_id == 1L & outcome == 1L, .(match_id, team_id, p_idx = idx)]
  pc[, n := seq_len(.N), by = .(match_id, team_id)]
  n_at <- function(key_idx) pc[data.table(match_id = shots$match_id, team_id = shots$team_id, p_idx = key_idx),
                               on = .(match_id, team_id, p_idx), roll = Inf]$n
  shots[, poss_passes := fcoalesce(n_at(j_idx), 0L) - fcoalesce(n_at(fcoalesce(as.numeric(opp_idx), 0) + 0.5), 0L)]
  shots[is.na(poss_secs), poss_passes := NA]

  # ---- rebound: any shot in the same match and period in the 5 s before
  ps <- shots[, .(match_id, r_idx = idx, r_t = t, r_period = period_id)]
  rb <- ps[shots[, .(match_id, j_idx)], on = .(match_id, r_idx = j_idx), roll = Inf]
  shots[, rebound := as.integer(!is.na(rb$r_t) & rb$r_period == period_id & (t - rb$r_t) <= 5)]

  # ---- game state before the shot
  g <- merge(ev[type_id == 16L, .(match_id, event_id, team_id, g_idx = idx)], gl, by = c("match_id", "event_id"))
  g[teams, on = "match_id", scorer := ifelse(og, mapply(function(tm, a) setdiff(a, tm)[1], team_id, i.teams), team_id)]
  gs <- g[, .(match_id, g_idx, scorer)]
  setorder(gs, match_id, g_idx)
  sb <- merge(shots[, .(match_id, event_id, team_id, other, j_idx)], gs, by = "match_id", allow.cartesian = TRUE)
  sb <- sb[g_idx < j_idx, .(for_ = sum(scorer == team_id), against = sum(scorer == other)), by = .(match_id, event_id)]
  shots[sb, on = .(match_id, event_id), `:=`(goals_for = i.for_, goals_against = i.against)]
  shots[is.na(goals_for), `:=`(goals_for = 0L, goals_against = 0L)]
  shots[, score_diff := goals_for - goals_against]

  shots[, c("qualifier_json", "j_idx", "opp_idx", "opp_t", "opp_period", "other", "idx", "t", "outcome") := NULL]
  shots[]
}

done <- sub("^ctx_", "events_", list.files(OUT, "^ctx_.*[.]parquet$"))
todo <- files[!basename(files) %in% done]
cat("competition files:", length(files), "| done:", length(done), "| to do:", length(todo), "\n")
for (f in todo) {
  t0 <- Sys.time()
  r <- tryCatch(rt_stage(paste("context", basename(f)), one_file(f)),
                error = function(e) { message("FAILED ", basename(f), ": ", conditionMessage(e)); NULL })
  if (is.null(r)) next
  write_parquet(r, file.path(OUT, sub("^events_", "ctx_", basename(f))))
  message(sprintf("%-45s %8d shots  %.1f min", basename(f), nrow(r), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
n_done <- length(list.files(OUT, "^ctx_.*[.]parquet$"))
message("finished: ", n_done, " of ", length(files), " competition files have context")
if (n_done < length(files)) message("MISSING: ", paste(setdiff(basename(files), sub("^ctx_", "events_", list.files(OUT))), collapse = ", "))
