# xG v5 / v5.1 release, WP gate: can the LIVE WP stay when EPV and xG both move?
# =============================================================================
# Same held-out set and scoring as pack_gate_wp.R (Big 5, 2024-25, outside WP's
# 2020-24 training). WP reads two things that change in this release: `epv` (the
# EPV priced on the new xG) and `xg_diff` (the new xG). So the new arm's features
# are built with BOTH, and the live WP is scored on them.
#
# Gate, set before the result (same bar as step 6): within +0.0005 MSE and +0.002
# ECE of the live WP on today's features. Checked for the live WP kept as is, and
# for the WP retrained on the pack in the live configuration when it exists.
#
# PACK_TAG selects the pack: "v5" (2026-09-27) or "v51" (xG v5.1, full event feeds
# only, 2026-09-28). Each tag has its own feature folder and output file.
# Run from panna/:  PACK_TAG=v51 Rscript data-raw/epv/pack_gate_wp_v5.R
suppressMessages({library(arrow); library(data.table); library(xgboost); devtools::load_all(".", quiet = TRUE)})
options(width = 130)
P <- "data-raw/cache/epv/pack-2026-09/"
TAG <- Sys.getenv("PACK_TAG", "v5")
PACK <- switch(TAG,
  v5  = list(epv = "epv_model_v5v0.rds",  xg = "xg_model_v5.rds",   wp = "wp_v5",  label = "v5"),
  v51 = list(epv = "epv_model_v51v0.rds", xg = "xg_model_v5_1.rds", wp = "wp_v51", label = "v5.1"),
  stop("PACK_TAG must be v5 or v51"))
stopifnot(length(list.files(file.path(P, "wf_big5_canonical"))) == 5)   # today's features, built by pack_gate_wp.R
SE <- "2024-2025"; LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")
epv_new <- readRDS(paste0(P, PACK$epv))
xg_new  <- readRDS(file.path("data-raw/cache/epv/xg-vnext", PACK$xg))
fh      <- .load_shot_foot_history()

build_new <- function(LG) {
  events  <- load_opta_match_events(LG, season = SE, source = "local")
  lineups <- load_opta_lineups(LG, season = SE, source = "local")
  chains <- create_possession_chains(convert_opta_to_spadl(events))
  chains <- calculate_action_epv(chains, features = NULL, epv_new, xg_model = xg_new, league = LG, season = SE,
                                 shot_lookup = .epv_shot_lookup(LG, SE), events = events, foot_history = fh)
  chains <- add_red_card_to_chains(chains, events)
  wf <- as.data.table(create_wp_features(chains, .build_match_results_from_events(events, lineups)))[!is.na(wp_label)]
  wf[, `:=`(league = LG, season = SE)]
  keep <- intersect(c("match_id", "period_id", "time_seconds", "time_remaining", "time_elapsed_frac",
    "xmargin", "epv", "xg_diff", "red_card_diff", "is_home", "is_second_half", "is_extra_time",
    "wp_label", "league", "season"), names(wf))
  wf[, ..keep]
}
fd <- file.path(P, paste0("wf_big5_", TAG))
dir.create(fd, showWarnings = FALSE)
for (LG in LEAGUES) {
  f <- sprintf("%s/%s_%s.parquet", fd, LG, SE)
  if (file.exists(f)) next
  t0 <- Sys.time(); arrow::write_parquet(build_new(LG), f)
  cat(sprintf("built %s %s %s [%.1f min]\n", TAG, LG, SE, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

score <- function(model_path, feat_dir, label) {
  m <- readRDS(model_path); feats <- m$feature_names
  te <- rbindlist(lapply(sprintf("%s/%s_%s.parquet", feat_dir, LEAGUES, SE),
                         function(f) as.data.table(arrow::read_parquet(f))), fill = TRUE)
  te[, `:=`(xmargin_x_time = xmargin * time_elapsed_frac, epv_x_time = epv * time_elapsed_frac)]
  te[, pred := pmin(pmax(predict(m$model, as.matrix(te[, ..feats])), 0), 1)]
  ece <- te[, .(n = .N, p = mean(pred), a = mean(wp_label)),
            by = cut(pred, seq(0, 1, .1), include.lowest = TRUE)][, sum(n / sum(n) * abs(a - p))]
  data.table(arm = label, games = uniqueN(paste(te$league, te$match_id)), actions = nrow(te),
             mse = mean((te$pred - te$wp_label)^2), ece = ece)
}
LIVE <- "C:/dev/_model-backups/2026-09-23/pannamodels-epv/wp_model.rds"
lab <- PACK$label
a <- score(LIVE, file.path(P, "wf_big5_canonical"), "live WP, today's EPV + xG")
b <- score(LIVE, fd, sprintf("live WP, %s EPV + xG %s", lab, lab))
# The WP retrained on the pack in the live configuration (pack_run_wp_v5.R /
# pack_run_wp_v51.R), held to the same bar.
CAND <- paste0(P, PACK$wp, "/wp_model.rds")
d <- if (file.exists(CAND)) score(CAND, fd, sprintf("%s WP (live config), %s EPV + xG %s", lab, lab, lab))
tab <- rbind(a, b, d)
cat(sprintf("\n=== WP held-out 2024-25, Big 5, pack %s (MSE and ECE: lower is better) ===\n", lab)); print(tab, digits = 5)
gate <- function(x, what) {
  d_mse <- x$mse - a$mse; d_ece <- x$ece - a$ece
  cat(sprintf("GATE (%s): dMSE %+.5f (<= +0.0005), dECE %+.5f (<= +0.002) -> %s\n",
              what, d_mse, d_ece, if (d_mse <= 0.0005 && d_ece <= 0.002) "PASS" else "FAIL"))
}
cat("\n"); gate(b, "live WP kept")
if (!is.null(d)) gate(d, sprintf("%s WP replaces live", lab))
fwrite(tab, file.path(P, paste0("gate_wp_", TAG, ".csv")))
