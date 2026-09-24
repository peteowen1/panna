# Model-pack step 6 gate: does WP on the new EPV predict as well as the canonical WP?
# =============================================================================
# Held-out season 2024-25, Big 5 (WP trains on 2020-21 to 2023-24). Each arm is
# scored with ITS OWN EPV underneath the `epv` feature: the canonical WP on
# features built with the canonical EPV, the candidate on the same matches
# built with the pack's EPV. Scoring follows data-raw/debug/validate_final_wp.R.
#
# Gate, set before the result: candidate MSE within +0.0005 of canonical and
# ECE within +0.002 (June's d4 -> d2 swap moved MSE 0.0005 and was called noise).
# The favourite-band table is reported, not gated.
#
# Run from panna/:  Rscript data-raw/epv/pack_gate_wp.R
suppressMessages({library(arrow); library(data.table); library(xgboost); devtools::load_all(".", quiet = TRUE)})
options(width = 130)
P <- "data-raw/cache/epv/pack-2026-09/"
EPVS <- list(canonical = readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds"),
             pack      = readRDS(paste0(P, "epv_model_pubv0.rds")))
SE <- "2024-2025"; LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")

# Features built exactly as 05_train_wp_model.R builds them today (season-aware
# xG, shot lookup, own-goal-aware results), for BOTH arms, so the only thing
# that differs between arms is the EPV model. June's saved set
# (data-raw/debug/wf_big5) predates the season-aware xG and is not used.
build_one <- function(LG, SE, epv_model) {
  events  <- load_opta_match_events(LG, season = SE, source = "local")
  lineups <- load_opta_lineups(LG, season = SE, source = "local")
  chains <- create_possession_chains(convert_opta_to_spadl(events))
  chains <- calculate_action_epv(chains, features = NULL, epv_model, league = LG, season = SE,
                                 shot_lookup = .epv_shot_lookup(LG, SE))
  chains <- add_red_card_to_chains(chains, events)
  wf <- as.data.table(create_wp_features(chains, .build_match_results_from_events(events, lineups)))[!is.na(wp_label)]
  wf[, `:=`(league = LG, season = SE)]
  keep <- intersect(c("match_id", "period_id", "time_seconds", "time_remaining", "time_elapsed_frac",
    "xmargin", "epv", "xg_diff", "red_card_diff", "is_home", "is_second_half", "is_extra_time",
    "wp_label", "league", "season"), names(wf))
  wf[, ..keep]
}
featdir <- function(arm) file.path(P, paste0("wf_big5_", arm))
for (arm in names(EPVS)) for (LG in LEAGUES) {
  dir.create(featdir(arm), showWarnings = FALSE)
  f <- sprintf("%s/%s_%s.parquet", featdir(arm), LG, SE)
  if (file.exists(f)) next
  t0 <- Sys.time(); arrow::write_parquet(build_one(LG, SE, EPVS[[arm]]), f)
  cat(sprintf("built %s %s %s [%.1f min]\n", arm, LG, SE, as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

score <- function(model_path, feat_dir, label) {
  m <- readRDS(model_path); fn <- m$feature_names
  te <- rbindlist(lapply(sprintf("%s/%s_%s.parquet", feat_dir, LEAGUES, SE),
                         function(f) as.data.table(arrow::read_parquet(f))), fill = TRUE)
  te[, `:=`(xmargin_x_time = xmargin * time_elapsed_frac, epv_x_time = epv * time_elapsed_frac,
            minute = time_seconds / 60, gid = paste(league, match_id))]
  setorder(te, league, match_id, period_id, time_seconds)
  te[, pred := pmin(pmax(predict(m$model, as.matrix(te[, ..fn])), 0), 1)]
  ece <- te[, .(n = .N, p = mean(pred), a = mean(wp_label)),
            by = cut(pred, seq(0, 1, .1), include.lowest = TRUE)][, sum(n / sum(n) * abs(a - p))]
  fav <- rbindlist(lapply(c(15, 30, 45, 60, 75), function(M) {
    s <- te[minute >= M][, .SD[1], by = gid][pred >= 0.8 & pred < 0.9]
    data.table(arm = label, min = M, games = nrow(s), diff = round(mean(s$wp_label) - mean(s$pred), 3))
  }))
  list(row = data.table(arm = label, games = uniqueN(te$gid), actions = nrow(te),
                        mse = mean((te$pred - te$wp_label)^2), ece = ece), fav = fav)
}
a <- score("data-raw/cache/epv/wp_final_d2repl_reg/wp_model.rds", featdir("canonical"), "canonical")
b <- score(paste0(P, "wp_pubv0/wp_model.rds"), featdir("pack"), "pack")
# the WP actually live (pannamodels / worker, pre-June features), which a publish replaces;
# the published EPV is byte-identical to the canonical one, so it reads the canonical features
l <- score("C:/dev/_model-backups/2026-09-23/pannamodels-epv/wp_model.rds", featdir("canonical"), "live")
tab <- rbind(l$row, a$row, b$row)
cat("\n=== WP held-out 2024-25, Big 5 (MSE and ECE: lower is better) ===\n"); print(tab, digits = 5)
cat("\n=== favourite band 0.8-0.9: actual minus predicted win rate, by minute (0 is perfect) ===\n")
print(dcast(rbind(l$fav, a$fav, b$fav), min ~ arm, value.var = "diff"))
d_mse <- b$row$mse - a$row$mse; d_ece <- b$row$ece - a$row$ece
ok <- d_mse <= 0.0005 && d_ece <= 0.002
cat(sprintf("vs live: dMSE %+.5f, dECE %+.5f (reported, not gated)\n", b$row$mse - l$row$mse, b$row$ece - l$row$ece))
cat(sprintf("\nGATE: dMSE %+.5f (<= +0.0005), dECE %+.5f (<= +0.002) -> %s\n",
            d_mse, d_ece, if (ok) "PASS" else "FAIL"))
