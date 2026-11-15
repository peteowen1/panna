# panna#273 gate: structural WP model (SE = scoreboard + pre-match Elo; SEP = + who has the ball)
# against the published WP model, every league with a local SPADL cache.
#
# Grain: one row per on-ball SPADL action. Target: match result from the home side (1/0.5/0).
# Train: seasons ending 2022-2024 (1 in TRAIN_EVERY rows per match, to fit in memory).
# Hold-out: seasons ending 2026 (all rows), scored one league-season at a time.
# Metrics: log loss over hold-out rows (lower is better), goal / turnover swing ratio
# (EPV's is ~35), SEP's possession premium at 0-0 in the first 15 min.
#
# Env: WPA273_DIR (models/ with the published epv/xg/wp models; outputs go here),
#      GATE_STAGE = "feats" | "fit" | "all" (default "all"), GATE_UNITS = "ENG:2023-2024,..." (optional, for timing).
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
stage <- Sys.getenv("GATE_STAGE", "all")
TRAIN_EVERY <- as.integer(Sys.getenv("TRAIN_EVERY", "8"))
TRAIN_YEARS <- 2022:2024; HOLD_YEARS <- 2026L
M <- file.path(out, "models"); fd <- file.path(out, "feats_all"); dir.create(fd, showWarnings = FALSE)
sp_dir <- "C:/dev/pannaverse/panna/data-raw/cache/epv/spadl"
say <- function(...) { message(format(Sys.time(), "%H:%M:%S "), sprintf(...)) }

wm <- readRDS(file.path(M, "wp_model.rds"))
units <- data.table(f = list.files(sp_dir, pattern = "^spadl_.*\\.rds$"))
units[, key := sub("^spadl_(.*)\\.rds$", "\\1", f)]
# Split on the LAST underscore: league names can hold spaces ("Copa America_2024 USA").
units[, league := sub("_[^_]*$", "", key)][, season := sub("^.*_", "", key)]
units[, end_year := extract_season_end_year(season)]
units[, role := fcase(end_year %in% TRAIN_YEARS, "train", end_year %in% HOLD_YEARS, "hold", default = NA_character_)]
units <- units[!is.na(role)]
if (nzchar(Sys.getenv("GATE_UNITS"))) {
  want <- sub(":", "_", strsplit(Sys.getenv("GATE_UNITS"), ",")[[1]])
  units <- units[key %in% want]
}
say("units: %d train, %d hold-out", units[role == "train", .N], units[role == "hold", .N])

# Pre-match Elo: every league from elo_all_leagues.R when present, else the 14 competitions in 04_match_dataset.
elo_all <- file.path(out, "match_elo_all.parquet")
elo <- if (file.exists(elo_all)) {
  say("Elo source: %s", elo_all)
  as.data.table(arrow::read_parquet(elo_all, col_select = c("match_id", "elo_diff")))
} else {
  say("Elo source: 04_match_dataset.rds (14 competitions only)")
  as.data.table(readRDS("C:/dev/pannaverse/panna/data-raw/cache-predictions-opta/04_match_dataset.rds"))[, .(match_id, elo_diff)]
}
elo <- unique(elo[!is.na(elo_diff)], by = "match_id")

keep <- unique(c("match_id", "action_id", "team_id", "period_id", "time_seconds", "score_diff", "is_home",
                 "red_card_diff", "time_remaining", "is_second_half", "is_extra_time", "epv", "wp_label",
                 wm$feature_names))
feat_path <- function(u) file.path(fd, sprintf("wpfeat_%s.parquet", u$key))

# ---- Stage 1: features per league-season (cached; skips files that exist) ----
cov_log <- list()
if (stage %in% c("feats", "all")) {
  epv_model <- readRDS(file.path(M, "epv_model.rds")); xg_model <- readRDS(file.path(M, "xg_model.rds"))
  for (i in seq_len(nrow(units))) {
    u <- units[i]; fp <- feat_path(u)
    if (file.exists(fp)) next
    t0 <- Sys.time()
    res <- tryCatch({
      events <- load_opta_match_events(u$league, season = u$season, source = "local")
      lineups <- load_opta_lineups(u$league, season = u$season, source = "local")
      spadl <- readRDS(file.path(sp_dir, u$f)); spadl <- spadl[spadl$match_id %in% unique(events$match_id), ]
      ch <- create_possession_chains(spadl)
      # Same shot lookup as 05_train_wp_model.R, so headers and set pieces get their real xG.
      ch <- calculate_action_epv(ch, features = NULL, epv_model, xg_model = xg_model, league = u$league,
                                 season = u$season, shot_lookup = .epv_shot_lookup(u$league, u$season),
                                 events = events)
      ch <- add_red_card_to_chains(ch, events)
      mr <- .build_match_results_from_events(events, lineups)
      wf <- as.data.table(create_wp_features(ch, mr))[!is.na(wp_label)]
      wf <- wf[, intersect(keep, names(wf)), with = FALSE]
      if (u$role == "train") {
        setorder(wf, match_id, period_id, time_seconds)
        wf <- wf[, .SD[seq(1L, .N, by = TRAIN_EVERY)], by = match_id]
      }
      arrow::write_parquet(wf, fp)
      sprintf("%d rows, %d matches, epv non-NA %.1f%%", nrow(wf), uniqueN(wf$match_id), 100 * mean(!is.na(wf$epv)))
    }, error = function(e) paste("FAILED:", conditionMessage(e)))
    say("%s %s [%s]: %s, %.1f min", u$league, u$season, u$role, res, as.numeric(difftime(Sys.time(), t0, units = "mins")))
    suppressWarnings(rm(events, lineups, spadl, ch, mr, wf)); gc(verbose = FALSE)
  }
}
if (!stage %in% c("fit", "all", "score")) quit(save = "no")

# ---- Stage 2: fit SE and SEP on the training units ----
rd <- function(u) {
  fp <- feat_path(u); if (!file.exists(fp)) return(NULL)
  d <- as.data.table(arrow::read_parquet(fp)); n_m <- uniqueN(d$match_id)
  d <- d[elo, elo_diff := i.elo_diff, on = "match_id"][]
  cov <- d[, uniqueN(match_id[!is.na(elo_diff)])] / n_m
  cov_log[[u$key]] <<- data.table(league = u$league, season = u$season, role = u$role, matches = n_m, elo_cov = cov)
  if (cov < 0.9) return(NULL)  # reported below; not silently scored on a partial Elo set
  d[!is.na(elo_diff)][, `:=`(league = u$league, season = u$season)]
}
tr <- rbindlist(lapply(split(units[role == "train"], seq_len(units[role == "train", .N])), rd), fill = TRUE)
say("train: %d rows (1 in %d), %d matches, %d league-seasons", nrow(tr), TRAIN_EVERY, uniqueN(tr$match_id), uniqueN(tr[, paste(league, season)]))
if (stage == "score") {  # reuse the models a previous fit stage saved
  fits <- readRDS(file.path(out, "wp_structural_gate_models.rds")); say("loaded saved SE/SEP models")
} else {
  fits <- list(SE = train_wp_structural(tr, use_possession = FALSE),
               SEP = train_wp_structural(tr, use_possession = TRUE))
  saveRDS(fits, file.path(out, "wp_structural_gate_models.rds"))
}
rm(tr); gc(verbose = FALSE)

# ---- Stage 3: score every hold-out league-season, one at a time ----
ll_sum <- function(p, y) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); sum(-(y * log(p) + (1 - y) * log(1 - p))) }
swings <- function(d, p) {
  d <- copy(d)[, p := p]
  d[, `:=`(pn = shift(p, type = "lead"), tn = shift(team_id, type = "lead"), sdn = shift(score_diff, type = "lead")), by = match_id]
  d[, chg := sdn - score_diff][, same := tn == team_id]
  d[, kind := fcase(is.na(tn), NA_character_, chg != 0, "goal", same, "jitter", default = "turnover")]
  d[, sw := abs(fifelse(same, pn - p, (1 - pn) - p))]
  d[!is.na(kind), .(sw = sum(sw, na.rm = TRUE), n = .N), by = kind]
}
acc <- list(); prem <- list()
for (i in which(units$role == "hold")) {
  u <- units[i]; d <- rd(u); if (is.null(d)) next
  setorder(d, match_id, period_id, time_seconds)
  preds <- list(published = predict_wp(wm, d), SE = predict_wp(fits$SE, d), SEP = predict_wp(fits$SEP, d))
  z <- .wp_structural_frame(d[score_diff == 0 & time_seconds < 900]); fn <- fits$SEP$feature_names
  ph <- predict(fits$SEP$model, as.matrix(copy(z)[, home_has_ball := 1L][, ..fn])) -
        predict(fits$SEP$model, as.matrix(copy(z)[, home_has_ball := 0L][, ..fn]))
  prem[[u$key]] <- data.table(s = sum(ph), n = length(ph))
  for (nm in names(preds)) {
    s <- swings(d, preds[[nm]])
    acc[[paste(u$key, nm)]] <- data.table(league = u$league, season = u$season, model = nm, rows = nrow(d),
                                          matches = uniqueN(d$match_id), ll = ll_sum(preds[[nm]], d$wp_label),
                                          goal_sw = s[kind == "goal", sum(sw)], goal_n = s[kind == "goal", sum(n)],
                                          to_sw = s[kind == "turnover", sum(sw)], to_n = s[kind == "turnover", sum(n)])
  }
  say("scored %s %s: %d rows", u$league, u$season, nrow(d))
}
A <- rbindlist(acc)
cov_tab <- rbindlist(cov_log)
fwrite(A, file.path(out, "wp_structural_gate_by_unit.csv")); fwrite(cov_tab, file.path(out, "wp_structural_gate_elo_coverage.csv"))
cat("\nElo coverage below 90% (league-seasons left out):\n"); print(cov_tab[elo_cov < 0.9])
tot <- A[, .(rows = sum(rows), matches = sum(matches), logloss = round(sum(ll) / sum(rows), 4),
             goal_per_turnover = round((sum(goal_sw) / sum(goal_n)) / (sum(to_sw) / sum(to_n)), 1)), by = model]
P <- rbindlist(prem)
cat("\n=== Hold-out, all leagues (seasons ending 2026): log loss lower is better; goal_per_turnover: EPV ~35 ===\n")
print(tot)
cat(sprintf("SEP possession premium (home win chance with the ball minus without, 0-0, first 15 min): %.4f over %d rows\n",
            sum(P$s) / sum(P$n), sum(P$n)))
by_lg <- dcast(A[, .(ll = sum(ll) / sum(rows)), by = .(league, model)], league ~ model, value.var = "ll")
by_lg[, `:=`(SE_minus_pub = round(SE - published, 4), SEP_minus_SE = round(SEP - SE, 4))]
cat("\nBy league (log loss; negative SE_minus_pub = SE better than published; positive SEP_minus_SE = possession hurts):\n")
print(by_lg[order(SE_minus_pub)])
say("DONE")
