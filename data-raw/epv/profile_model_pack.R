# Profile ONE league-season through the model-pack retrain stages (2026-09-23)
# before launching the full run (global long-run rule). Stages in the NEW
# order: shots -> xG fit -> SPADL -> real xG on shot actions -> chains ->
# labels priced by real xG. Also counts the league-seasons and shots in the
# training window so the per-unit times can be scaled. Writes nothing to the
# model caches. Run: powershell.exe -Command 'Rscript "data-raw/epv/profile_model_pack.R"'
suppressPackageStartupMessages({library(data.table); devtools::load_all(quiet = TRUE)})
source(file.path(Sys.getenv("USERPROFILE"), ".claude/lib/runtime_log.R"))
say <- function(...) cat(..., "\n", sep = "")
L <- "ENG"; S <- "2024-2025"

events <- rt_stage("load events", load_opta_match_events(L, season = S, source = "local"))
shots  <- rt_stage("load shots", as.data.table(load_opta_shot_events(L, season = S, source = "local")))
sf     <- rt_stage("xG features", prepare_shots_for_xg(shots))
xgm    <- rt_stage("xG fit (one season)", fit_xg_model(sf, nrounds = 1000, early_stopping_rounds = 50, verbose = 0))
spadl  <- rt_stage("SPADL convert", convert_opta_to_spadl(events))
spadl$league <- L
lk     <- as.data.frame(shots)[, intersect(c("match_id", "event_id", "body_part", "situation", "is_big_chance"), names(shots))]
spx    <- rt_stage("real xG on shot actions", as.data.table(add_xg_to_spadl(spadl, xgm, season = S, shot_lookup = lk)))
ch     <- rt_stage("chains", create_possession_chains(spx))
lab    <- rt_stage("outcome labels", { o <- add_next_chain_outcome(classify_chain_outcomes(ch)); label_actions_with_outcomes(ch, o) })
lab    <- rt_stage("next-goal labels", create_next_goal_labels(lab))
xv     <- spx[action_type == "shot" & !is.na(xg), .(match_id, action_id, xg)]
lab2   <- rt_stage("next-xG labels (REAL xG)", create_next_xg_labels(lab, xg_values = xv))
lab0   <- rt_stage("next-xG labels (position-only, as today)", create_next_xg_labels(lab))
say("\nlabel check, ENG 2024-25: mean |next_xg_label| real ", round(mean(abs(lab2$next_xg_label), na.rm = TRUE), 4),
    " vs position-only ", round(mean(abs(lab0$next_xg_label), na.rm = TRUE), 4), " (", nrow(lab2), " actions)")

# scale: how many league-seasons and shots are in the window the script trains on
MIN_END <- 2021
ls_n <- 0L; shots_n <- 0
for (lg in names(OPTA_LEAGUES)) {
  av <- tryCatch(list_opta_seasons(lg, source = "local"), error = function(e) character(0))
  av <- av[vapply(av, function(z) isTRUE(extract_season_end_year(z) >= MIN_END), logical(1))]
  ls_n <- ls_n + length(av)
}
say("training window (end year >= ", MIN_END, ", OPTA_LEAGUES = ", length(OPTA_LEAGUES), " leagues): ", ls_n, " league-seasons")
say("one-season shots: ", nrow(shots), " | ENG events: ", nrow(events))
