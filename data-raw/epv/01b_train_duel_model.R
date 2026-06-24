# 01b. Train the xDuel (expected-duel) models — FIVE contest sub-models
# =====================================================================
# Player-AGNOSTIC contextual win-probability for the five duel contests (see
# R/duel_model.R): aerial_win, aerial_poss, takeon (foul-aware), tackle_poss,
# containment. Each feeds a "won above expected" count into PSR/PSV, replacing the
# scale-free accuracy ratios. Run BEFORE 03_calculate_player_xmetrics.R.
#
# Memory-safe: loads the FULL event stream ONE LEAGUE AT A TIME (aerial_poss and
# containment look at neighbouring rows of any type), extracts the small per-contest
# feature tables, and discards the raw events before the next league.
#
# Output: pannadata/data/opta/models/duel_model.rds (republish to pannamodels).
#   cd panna && Rscript data-raw/epv/01b_train_duel_model.R
#
# Overrides (set before sourcing): duel_train_leagues, duel_save (default TRUE).

suppressMessages(devtools::load_all(".", quiet = TRUE))
library(data.table)

# A broad cross-section is enough for a context-only model; the saved model then
# scores ALL leagues in step 03.
if (!exists("duel_train_leagues")) {
  duel_train_leagues <- c(
    "Premier_League", "La_Liga", "Bundesliga", "Serie_A", "Ligue_1",
    "Eredivisie", "Primeira_Liga", "Belgian_First_Division", "Championship",
    "Brazilian_Serie_A", "Argentine_Liga_Profesional", "MLS",
    "UEFA_Champions_League", "World_Cup"
  )
}
if (!exists("duel_save")) duel_save <- TRUE

opta_dir <- file.path(pannadata_dir(), "opta")
avail <- list.files(opta_dir, pattern = "^events_.*\\.parquet$")
want <- paste0("events_", duel_train_leagues, ".parquet")
have <- intersect(want, avail)
if (length(have) == 0) {
  cli::cli_alert_warning("None of the requested leagues found; using all available event files.")
  have <- avail
}
# Full event stream needed (aerial_poss next-possession + containment row-adjacency).
need_cols <- c("match_id", "player_id", "team_id", "type_id", "outcome",
               "x", "y", "period_id", "minute", "second")
cli::cli_alert_info("Building duel features from {length(have)} leagues (one at a time)...")

acc <- NULL
for (fn in have) {
  ev <- tryCatch(
    as.data.table(arrow::read_parquet(file.path(opta_dir, fn), col_select = all_of(need_cols))),
    error = function(e) NULL)
  if (is.null(ev) || nrow(ev) == 0) { cli::cli_alert_warning("  skip {fn}"); next }
  cli::cli_alert_info("  {fn}: {format(nrow(ev), big.mark=',')} events")
  preps <- compute_all_duel_preps(ev)
  rm(ev); gc(verbose = FALSE)
  if (is.null(acc)) {
    acc <- preps
  } else {
    acc <- setNames(lapply(names(acc), function(k)
      rbindlist(list(acc[[k]], preps[[k]]), fill = TRUE)), names(acc))
  }
  rm(preps); gc(verbose = FALSE)
}

cli::cli_alert_info("Accumulated contest rows: {paste(names(acc), vapply(acc, nrow, integer(1)), sep='=', collapse=' | ')}")

duel_model <- fit_duel_model(acc, verbose = 1)
if (isTRUE(duel_save)) save_duel_model(duel_model)

cli::cli_alert_success("xDuel models trained{if (duel_save) ' & saved' else ' (not saved)'}.")
for (cst in c("aerial_win", "aerial_poss", "takeon", "tackle_poss", "containment")) {
  m <- duel_model[[cst]]
  if (is.null(m)) { cat("\n--", cst, "(no model)\n"); next }
  cat(sprintf("\n-- %s  (win-rate %.3f, n=%s) --\n", cst, m$win_rate,
              format(m$n_contests, big.mark = ",")))
  print(m$importance)
}
