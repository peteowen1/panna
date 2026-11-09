# Build WP training features for one or more league-seasons, reading the shared SPADL cache
# read-only (never get_or_build_spadl, which can write it). Saves one parquet per league-season.
# Usage: set Sys.getenv("WP_UNITS") = "ENG:2023-2024,ESP:2023-2024" before running.
suppressMessages(devtools::load_all("C:/dev/pannaverse/panna", quiet = TRUE))
library(data.table)
out <- Sys.getenv("WPA273_DIR", "data-raw/cache/wpa273")
M <- file.path(out, "models"); fd <- file.path(out, "feats"); dir.create(fd, showWarnings = FALSE)
epv_model <- readRDS(file.path(M, "epv_model.rds")); xg_model <- readRDS(file.path(M, "xg_model.rds"))
wm <- readRDS(file.path(M, "wp_model.rds"))
cat("published WP model: features =", paste(wm$feature_names, collapse = ","), "| nrounds =", wm$optimal_nrounds,
    "| objective =", tryCatch(xgboost::xgb.config(wm$model)$learner$objective$name, error = function(e) "?"), "\n")
units <- strsplit(strsplit(Sys.getenv("WP_UNITS"), ",")[[1]], ":")
for (u in units) {
  league <- u[1]; season <- u[2]; t0 <- Sys.time()
  f_out <- file.path(fd, sprintf("wpfeat_%s_%s.parquet", league, season))
  if (file.exists(f_out)) { cat("exists:", basename(f_out), "\n"); next }
  sp_path <- file.path("C:/dev/pannaverse/panna/data-raw/cache/epv/spadl", sprintf("spadl_%s_%s.rds", league, season))
  if (!file.exists(sp_path)) { cat("NO SPADL CACHE:", sp_path, "\n"); next }
  events <- load_opta_match_events(league, season = season, source = "local")
  lineups <- load_opta_lineups(league, season = season, source = "local")
  spadl <- readRDS(sp_path)
  spadl <- spadl[spadl$match_id %in% unique(events$match_id), ]
  ch <- create_possession_chains(spadl)
  ch <- calculate_action_epv(ch, features = NULL, epv_model, xg_model = xg_model, league = league,
                             season = season, shot_lookup = NULL, events = events)
  ch <- add_red_card_to_chains(ch, events)
  mr <- .build_match_results_from_events(events, lineups)
  wf <- as.data.table(create_wp_features(ch, mr))
  wf[, `:=`(league = league, season = season)]
  keep <- intersect(c("match_id", "team_id", "home_team_id", "period_id", "time_seconds", "score_diff",
                      wm$feature_names, "wp_label", "league", "season"), names(wf))
  arrow::write_parquet(wf[, ..keep], f_out)
  cat(sprintf("%s %s: %d rows, %d matches, epv non-NA %.1f%%, %.1f min\n", league, season, nrow(wf),
              uniqueN(wf$match_id), 100 * mean(!is.na(wf$epv)), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  rm(events, lineups, spadl, ch, wf); gc(verbose = FALSE)
}
