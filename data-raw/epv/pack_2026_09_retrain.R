# pack_2026_09_retrain.R -- model-pack retrain, steps 3-5 of
# pannaverse/docs/plans/EPV-RETRAIN-SCOPE.md "The plan (2026-09-23)".
#
# ONE change against the canonical EPV: its training labels are priced by the
# real xG model instead of estimate_simple_xg(x, y), which prices headers at
# 3.3x their goal rate (ng_label_xg_check.R). Same 170 labelled chunks
# (data-raw/cache/epv/labeled_chunks, 2026-07-13) and the same fit recipe as
# data-raw/debug/retrain_clean_epv_full.R, so the label pricing is the only
# axis that moves. xG and xGOT are retrained on the repaired situation labels
# (step 1) for the same league-seasons.
#
# Checkpointed: every stage writes to OUT and is skipped on a re-run when its
# file exists, so an interrupted run loses only the stage in flight. Nothing
# here overwrites a model the ledger or the loaders read today.
#
#   powershell.exe -Command 'Rscript "data-raw/epv/pack_2026_09_retrain.R"'
#   (launch detached with Start-Process for the full run)

suppressPackageStartupMessages({library(data.table); devtools::load_all(quiet = TRUE)})
source(file.path(Sys.getenv("USERPROFILE"), ".claude/lib/runtime_log.R"))
say <- function(...) { cat(format(Sys.time(), "%H:%M:%S"), ..., "\n", sep = ""); flush.console() }

CHUNKS  <- "data-raw/cache/epv/labeled_chunks"
OUT     <- if (exists("PACK_OUT")) PACK_OUT else "data-raw/cache/epv/pack-2026-09"
HOLDOUT <- "2025-2026"          # scored by the gates, never trained on by the gate models
# Which xG prices the EPV labels. "published" (default since 2026-09-23): the
# xG the ledger itself prices shots with, so EPV and the ledger agree on what a
# chance is worth by construction. On 2026-27 (unseen by both) the published
# xG beat the pack's new one -- goals/xG 0.985 vs 0.963, headers 0.948 vs 0.853
# (pack_gate_2026_27.R) -- because Opta's big-chance tag rate drifted up.
LABEL_XG <- if (exists("LABEL_XG")) LABEL_XG else "published"
LBL_DIR  <- if (LABEL_XG == "published") "chunks_pubxg" else "chunks_realxg"
EPV_FILE <- if (LABEL_XG == "published") "epv_model_pubxg.rds" else "epv_model.rds"
dir.create(file.path(OUT, LBL_DIR), recursive = TRUE, showWarnings = FALSE)
ck <- function(f) file.path(OUT, f)

# ---- which league-seasons: exactly the ones the chunks hold -----------------
chunk_files <- list.files(CHUNKS, pattern = "^chunk_.*\\.parquet$", full.names = TRUE)
key_of <- function(lg, s) sprintf("chunk_%s_%s.parquet", gsub("[^A-Za-z0-9]", "_", lg), gsub("[^A-Za-z0-9]", "_", s))
units <- rbindlist(lapply(names(OPTA_LEAGUES), function(lg) {
  av <- tryCatch(list_opta_seasons(lg, source = "local"), error = function(e) character(0))
  if (!length(av)) return(NULL)
  d <- data.table(league = lg, season = av)
  d[, file := file.path(CHUNKS, key_of(league, season))][file.exists(file)]
}))
if (exists("PACK_UNITS")) units <- units[eval(PACK_UNITS)]   # smoke test only: a quoted filter
say("chunks on disk ", length(chunk_files), " | matched to a league-season ", nrow(units))
if (!exists("PACK_UNITS")) stopifnot(nrow(units) >= 0.95 * length(chunk_files))

# ---- A. shots for those league-seasons (repaired situation labels) ----------
if (!file.exists(ck("shots.rds"))) {
  shots <- rt_stage("load shots", rbindlist(lapply(seq_len(nrow(units)), function(i)
    as.data.table(load_opta_shot_events(units$league[i], season = units$season[i], source = "local"))[
      , `:=`(unit_league = units$league[i], unit_season = units$season[i])]), fill = TRUE))
  saveRDS(shots, ck("shots.rds"))
}
shots <- readRDS(ck("shots.rds"))
say("shots ", format(nrow(shots), big.mark = ","), " | holdout (", HOLDOUT, ") ", sum(shots$unit_season == HOLDOUT))

cal_table <- function(ft, p, label) {
  d <- data.table(goal = ft$is_goal, p = p, body = ft$is_header, pen = ft$is_penalty)
  d <- d[pen %in% 0]
  rbind(d[, .(cut = "all", shots = .N, goals = sum(goal), pred = round(sum(p), 1))],
        d[, .(shots = .N, goals = sum(goal), pred = round(sum(p), 1)), by = .(cut = fifelse(body %in% 1, "header", "foot"))])[
    , `:=`(model = label, goals_per_pred = round(goals / pred, 3))][]
}
logloss <- function(y, p) { p <- pmin(pmax(p, 1e-6), 1 - 1e-6); -mean(y * log(p) + (1 - y) * log(1 - p)) }

# ---- B. xG: gate model (train minus holdout) vs current, then final ---------
if (!file.exists(ck("xg_model.rds"))) {
  ft <- prepare_shots_for_xg(as.data.frame(shots))
  hold <- ft$match_id %in% shots[unit_season == HOLDOUT]$match_id
  gm <- rt_stage("xG fit (gate, no holdout)", fit_xg_model(ft[!hold, ], nrounds = 1000, early_stopping_rounds = 50, verbose = 0))
  cur <- readRDS("data-raw/cache/epv/xg_model.rds")
  pn <- predict_xg(gm, ft[hold, ]); pc <- predict_xg(cur, ft[hold, ])
  g <- rbind(cal_table(ft[hold, ], pn, "new"), cal_table(ft[hold, ], pc, "current 2026-09-12"))
  np <- ft$is_penalty[hold] %in% 0
  say("xG GATE on ", HOLDOUT, " (goals / predicted; 0.97-1.03 to pass). log-loss new ",
      round(logloss(ft$is_goal[hold][np], pn[np]), 5), " vs current ", round(logloss(ft$is_goal[hold][np], pc[np]), 5))
  print(g)
  fwrite(g, ck("gate_xg.csv"))
  fm <- rt_stage("xG fit (final, all)", fit_xg_model(ft, nrounds = 1000, early_stopping_rounds = 50, verbose = 0))
  saveRDS(fm, ck("xg_model.rds"))
}
xg_model <- readRDS(if (LABEL_XG == "published") "data-raw/cache/epv/xg_model.rds" else ck("xg_model.rds"))
say("EPV labels priced by the ", LABEL_XG, " xG -> ", LBL_DIR, ", ", EPV_FILE)

# ---- C. xGOT: same shape, header gate ---------------------------------------
if (!file.exists(ck("xgot_model.rds"))) {
  ft <- prepare_shots_for_xgot(as.data.frame(shots))
  hold <- ft$match_id %in% shots[unit_season == HOLDOUT]$match_id
  gm <- rt_stage("xGOT fit (gate)", fit_xgot_model(ft[!hold, ], nrounds = 3000L, early_stopping_rounds = 50, verbose = 0))
  cur <- readRDS(file.path(opta_data_dir(), "models", "xgot_model.rds"))
  pn <- predict_xgot(gm, ft[hold, ]); pc <- suppressWarnings(predict_xgot(cur, ft[hold, ]))
  g <- rbind(cal_table(ft[hold, ], pn, "new"), cal_table(ft[hold, ], pc, "current 2026-07-23"))
  say("xGOT GATE on ", HOLDOUT, " (headers 0.95-1.05 to pass)"); print(g)
  fwrite(g, ck("gate_xgot.csv"))
  fm <- rt_stage("xGOT fit (final)", fit_xgot_model(ft, nrounds = 3000L, early_stopping_rounds = 50, verbose = 0))
  saveRDS(fm, ck("xgot_model.rds"))
}

# ---- D. re-price every chunk's labels with the new xG -----------------------
lk_cols <- intersect(c("match_id", "event_id", "body_part", "situation", "is_big_chance"), names(shots))
done <- 0L
for (i in seq_len(nrow(units))) {
  out <- file.path(OUT, LBL_DIR, basename(units$file[i]))
  if (file.exists(out)) next
  ch <- as.data.table(arrow::read_parquet(units$file[i]))
  lk <- as.data.frame(shots[unit_league == units$league[i] & unit_season == units$season[i], ..lk_cols])
  sx <- suppressMessages(as.data.table(add_xg_to_spadl(ch, xg_model, shot_lookup = lk)))
  xv <- sx[action_type == "shot" & is.finite(xg), .(match_id, action_id, xg)]
  ch[, next_xg_label_position_only := next_xg_label]
  ch[, next_xg_label := NULL]
  ch <- suppressMessages(as.data.table(create_next_xg_labels(ch, xg_values = xv)))
  arrow::write_parquet(ch, out)
  done <- done + 1L
  if (done %% 20 == 0) say("  relabelled ", done, " chunks")
}
say("relabel stage complete: ", length(list.files(file.path(OUT, LBL_DIR))), " chunks")

# ---- E. EPV, the canonical recipe on the re-priced chunks -------------------
if (!file.exists(ck(EPV_FILE))) {
  set.seed(1)
  files <- list.files(file.path(OUT, LBL_DIR), pattern = "parquet$", full.names = TRUE)
  dat <- rt_stage("EPV training rows", rbindlist(lapply(files, function(f) {
    d <- as.data.table(arrow::read_parquet(f))
    if (nrow(d) > 1) d <- d[sample(.N, ceiling(.N * 0.35))]
    fe <- suppressMessages(as.data.table(create_epv_features_simple(d)))
    fe[d, next_xg_label := i.next_xg_label, on = .(match_id, action_id)]
    fe
  }), fill = TRUE))
  dat <- dat[!is.na(next_xg_label)]
  say("EPV training rows ", format(nrow(dat), big.mark = ","))
  m <- rt_stage("EPV fit", fit_epv_model(dat, dat, method = "xg", nrounds = 1000, early_stopping_rounds = 50, verbose = 0))
  m$panna_metadata$feature_mode <- "simple"
  m$panna_metadata$labels <- paste0("next shot's xG from the ", LABEL_XG, " xG model (not estimate_simple_xg)")
  saveRDS(m, ck(EPV_FILE))
}

# ---- F. what moved: label means by the kind of shot that ends the label -----
new <- readRDS(ck(EPV_FILE)); old <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")
fc <- old$panna_metadata$feature_cols
mkrow <- function(sx, sy, act) {
  d2g <- sqrt((100 - sx)^2 + (50 - sy)^2)
  ang <- abs(atan2(56 - sy, max(100 - sx, .1)) - atan2(44 - sy, max(100 - sx, .1)))
  v <- c(start_x = sx, start_y = sy, distance_to_goal = d2g, angle_to_goal = ang, time_remaining = 0.9,
         is_extra_time = 0, time_in_half_remaining = 0.9, prev_x = sx, prev_y = sy, prev_dx = 0, prev_dy = 0,
         same_team_prev = 1, action_cat = act, league_id = 1)
  matrix(v[fc], 1, dimnames = list(NULL, fc))
}
spots <- data.table(spot = c("won header, 6-yard box (95,50)", "won header, penalty spot (89,50)",
                             "cross from byline (98,20)", "pass, box centre (88,50)", "pass, edge of box (83,50)",
                             "pass, midfield (50,50)"),
                    x = c(95, 89, 98, 88, 83, 50), y = c(50, 50, 20, 50, 50, 50), act = c(8, 8, 2, 1, 1, 1))
spots[, `:=`(old = round(sapply(seq_len(.N), function(i) predict(old$model, mkrow(x[i], y[i], act[i]))), 3),
             new = round(sapply(seq_len(.N), function(i) predict(new$model, mkrow(x[i], y[i], act[i]))), 3))]
say("EPV state values, old (position-only labels) vs new (real-xG labels); ENG, time 0.9:")
print(spots[, .(spot, old, new)])
fwrite(spots, ck(sub(".rds", "_spots.csv", EPV_FILE, fixed = TRUE)))
say("done. Outputs in ", OUT)
