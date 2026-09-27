# xG v5 release, WP gate: can the LIVE WP stay when EPV and xG both move to v5?
# =============================================================================
# Same held-out set and scoring as pack_gate_wp.R (Big 5, 2024-25, outside WP's
# 2020-24 training). WP reads two things that change in this release: `epv`
# (now the v5-priced EPV, epv_model_v5v0.rds) and `xg_diff` (now xG v5). So the
# new arm's features are built with BOTH, and the live WP is scored on them.
#
# Gate, set before the result (same bar as step 6): live WP on the v5 features
# within +0.0005 MSE and +0.002 ECE of live WP on today's features. Pass = keep
# the live WP; fail = retrain WP on the v5 pack before publishing.
#
# Run from panna/:  Rscript data-raw/epv/pack_gate_wp_v5.R
suppressMessages({library(arrow); library(data.table); library(xgboost); devtools::load_all(".", quiet = TRUE)})
options(width = 130)
P <- "data-raw/cache/epv/pack-2026-09/"
stopifnot(length(list.files(file.path(P, "wf_big5_canonical"))) == 5)   # today's features, built by pack_gate_wp.R
SE <- "2024-2025"; LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")
epv_v5 <- readRDS(paste0(P, "epv_model_v5v0.rds"))
xg_v5  <- readRDS("data-raw/cache/epv/xg-vnext/xg_model_v5.rds")
fh     <- .load_shot_foot_history()

build_v5 <- function(LG) {
  events  <- load_opta_match_events(LG, season = SE, source = "local")
  lineups <- load_opta_lineups(LG, season = SE, source = "local")
  chains <- create_possession_chains(convert_opta_to_spadl(events))
  chains <- calculate_action_epv(chains, features = NULL, epv_v5, xg_model = xg_v5, league = LG, season = SE,
                                 shot_lookup = .epv_shot_lookup(LG, SE), events = events, foot_history = fh)
  chains <- add_red_card_to_chains(chains, events)
  wf <- as.data.table(create_wp_features(chains, .build_match_results_from_events(events, lineups)))[!is.na(wp_label)]
  wf[, `:=`(league = LG, season = SE)]
  keep <- intersect(c("match_id", "period_id", "time_seconds", "time_remaining", "time_elapsed_frac",
    "xmargin", "epv", "xg_diff", "red_card_diff", "is_home", "is_second_half", "is_extra_time",
    "wp_label", "league", "season"), names(wf))
  wf[, ..keep]
}
fd <- file.path(P, "wf_big5_v5")
dir.create(fd, showWarnings = FALSE)
for (LG in LEAGUES) {
  f <- sprintf("%s/%s_%s.parquet", fd, LG, SE)
  if (file.exists(f)) next
  t0 <- Sys.time(); arrow::write_parquet(build_v5(LG), f)
  cat(sprintf("built v5 %s %s [%.1f min]\n", LG, SE, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
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
a <- score(LIVE, file.path(P, "wf_big5_canonical"), "live WP, today's EPV + xG")
b <- score(LIVE, fd, "live WP, v5 EPV + xG v5")
c <- score(paste0(P, "wp_pubv0/wp_model.rds"), fd, "pack WP (pubv0), v5 EPV + xG v5")
# The WP retrained on the v5 pack in the live configuration (pack_run_wp_v5.R),
# held to the same bar against the live WP on today's features.
V5 <- paste0(P, "wp_v5/wp_model.rds")
d <- if (file.exists(V5)) score(V5, fd, "v5 WP (live config), v5 EPV + xG v5")
tab <- rbind(a, b, c, d)
cat("\n=== WP held-out 2024-25, Big 5 (MSE and ECE: lower is better) ===\n"); print(tab, digits = 5)
d_mse <- b$mse - a$mse; d_ece <- b$ece - a$ece
cat(sprintf("\nGATE (live WP kept): dMSE %+.5f (<= +0.0005), dECE %+.5f (<= +0.002) -> %s\n",
            d_mse, d_ece, if (d_mse <= 0.0005 && d_ece <= 0.002) "PASS" else "FAIL"))
if (!is.null(d)) {
  d_mse <- d$mse - a$mse; d_ece <- d$ece - a$ece
  cat(sprintf("GATE (v5 WP replaces live): dMSE %+.5f (<= +0.0005), dECE %+.5f (<= +0.002) -> %s
",
              d_mse, d_ece, if (d_mse <= 0.0005 && d_ece <= 0.002) "PASS" else "FAIL"))
}
fwrite(tab, file.path(P, "gate_wp_v5.csv"))
