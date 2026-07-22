# 11c_face_validity.R
#
# Wave 2 face-validity battery (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 5.3,
# veto class) for the winning glmnet candidate S4a, on the 2026 vintage
# (current window). Names joined from 05_spm.rds combined_ratings.
#
# Battery (runbook additions from NEXT-STEPS 2026-07-21 included):
#   1. Top-30 / bottom-15 by predicted net -- eyeball vs panna/PSR.
#   2. Luuk-de-Jong career-rank smell test (name-matched rank report).
#   3. def-sd-ratio: var(pred_def)/var(pred_off) -- the PSV
#      var(dsv)/var(psv) balance analog (the #163 obox failure detector).
#   4. Role coherence: per-role-group mean/sd of predictions (uncentered
#      panel scale -- means need not be 0, but role ORDER should be sane
#      and no group should dominate variance).
#   5. Top-10 by predicted DEFENSE (the DPM degeneracy probe -- C3's
#      six_yard_block failure mode would surface here).
#
# Output: printed report + data-raw/spm-redesign/wave2_face_validity_top.csv
#
# Run from panna/:
#   Rscript data-raw/spm-redesign/11c_face_validity.R

source(file.path("data-raw", "spm-redesign", "05c_candidates.R"))
suppressMessages(library(data.table))

cache_dir <- file.path("data-raw", "cache-opta")
out_dir <- file.path("data-raw", "spm-redesign")

panel_bundle <- readRDS(file.path(cache_dir, "spm_panel.rds"))
panel <- panel_bundle$panel
attr(panel, "target_provenance") <- panel_bundle$target_provenance

names_dt <- data.table::as.data.table(readRDS(file.path(cache_dir, "05_spm.rds"))$combined_ratings)[
  , .(player_id, player_name)]

cli::cli_h2("Fitting S4a on the full panel")
fits <- run_candidate(panel, candidate_configs$S4a, seed = 1)

panel_2026 <- panel[vintage_year == max(vintage_year)]
off <- predict_spm_panel(fits$offense, panel_2026)
def <- predict_spm_panel(fits$defense, panel_2026)
pred <- data.table(player_id = panel_2026$player_id,
                   role_group = panel_2026$role_group,
                   window_minutes = panel_2026$window_minutes,
                   pred_off = off$pred, pred_def = def$pred)
pred[, pred_net := pred_off - pred_def]
pred <- merge(pred, names_dt, by = "player_id", all.x = TRUE)
pred_rated <- pred[window_minutes >= 2000]
cli::cli_alert_info(sprintf("2026 vintage: %d rows, %d with >=2000 window minutes",
                            nrow(pred), nrow(pred_rated)))

cli::cli_h1("1. Top 30 by predicted net (>=2000 window min)")
print(pred_rated[order(-pred_net)][1:30, .(player_name, role_group, window_minutes,
                                           pred_net, pred_off, pred_def)], digits = 3)
cli::cli_h1("   Bottom 15")
print(pred_rated[order(pred_net)][1:15, .(player_name, role_group, window_minutes,
                                          pred_net, pred_off, pred_def)], digits = 3)

pred_rated[, rank_net := frank(-pred_net)]

cli::cli_h1("2. Luuk de Jong smell test")
ldj <- pred[grepl("luuk de jong", tolower(player_name))]
if (nrow(ldj) > 0) {
  ldj_rank <- pred_rated[grepl("luuk de jong", tolower(player_name))]
  if (nrow(ldj_rank) > 0) {
    print(ldj_rank[, .(player_name, role_group, window_minutes, pred_net,
                       rank_net, n_rated = nrow(pred_rated))], digits = 3)
  } else {
    print(ldj[, .(player_name, role_group, window_minutes, pred_net)], digits = 3)
    cli::cli_alert_info("(below the 2000-min rated cut)")
  }
} else {
  cli::cli_alert_warning("Luuk de Jong not found in the 2026 vintage panel.")
}

cli::cli_h1("3. def-sd-ratio (var pred_def / var pred_off)")
vr <- var(pred_rated$pred_def) / var(pred_rated$pred_off)
cli::cli_alert_info(sprintf("var(def)/var(off) = %.3f  (PSV post-fix analog: 0.254; pre-fix failure: 0.83)", vr))

cli::cli_h1("4. Role coherence (per-role mean/sd of net prediction)")
print(pred_rated[, .(n = .N, mean_net = mean(pred_net), sd_net = sd(pred_net),
                     mean_off = mean(pred_off), mean_def = mean(pred_def)),
                 by = role_group][order(-mean_net)], digits = 3)

cli::cli_h1("5. Top 10 by predicted DEFENSE (DPM degeneracy probe)")
print(pred_rated[order(pred_def)][1:10, .(player_name, role_group, window_minutes,
                                          pred_def, pred_net)], digits = 3)

fwrite(pred_rated[order(-pred_net)][1:100, .(player_name, role_group, window_minutes,
                                             pred_net, pred_off, pred_def, rank_net)],
       file.path(out_dir, "wave2_face_validity_top.csv"))
cli::cli_alert_success("Top-100 written to wave2_face_validity_top.csv")
