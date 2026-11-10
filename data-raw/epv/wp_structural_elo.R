# wp_structural_elo.R -- pre-match Elo for every league, for the structural WP model (panna#273).
#
# Sourced by 05d_train_wp_structural.R (training) and 10b_export_game_logs.R (serving), so the Elo
# the model trains on and the Elo it scores with are built by the same code. Defines functions only.
#
# Why its own build: the predictions run's step 01/03 Elo covers 29 competitions; WPA is published
# for every league with events, so this builds step 01 for those 29 plus the 14 others, and runs
# compute_match_elos() over all of them together with step 03's settings.

WP_ELO_BASE_LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA", "NED", "POR", "TUR", "ENG2", "SCO", "UCL", "UEL", "UECL",
                         "WC", "EURO", "AFCON", "COPA", "GOLD", "ACUP", "GULF", "WCQ_UEFA", "WCQ_CONMEBOL", "WCQ_CAF",
                         "WCQ_AFC", "EUROQ", "AFCONQ", "ACUPQ", "NL", "INTL_FR")
WP_ELO_EXTRA_LEAGUES <- c("BEL", "MEX", "SAU", "AUS", "MLS", "ARG", "BRA", "LIB", "SUD", "ACLE", "CAFCL", "CCC", "LGC", "CWC")

#' Build (or read) the all-league pre-match Elo table.
#' @param out_dir Folder for the step-01 fixture tables and match_elo.parquet.
#' @param rebuild TRUE rebuilds from the current fixtures (serving: new results every week);
#'   FALSE reuses match_elo.parquet when present (training reruns).
#' @return data.table, one row per played match: match_id, match_date, home/away Elo, elo_diff.
wp_structural_match_elo <- function(out_dir, rebuild = FALSE) {
  elo_path <- file.path(out_dir, "match_elo.parquet")
  if (!rebuild && file.exists(elo_path)) return(data.table::as.data.table(arrow::read_parquet(elo_path)))
  t0 <- Sys.time()
  fx_list <- list()
  for (grp in c("base", "extra")) {
    cd <- file.path(out_dir, "fixtures", grp)
    dir.create(cd, recursive = TRUE, showWarnings = FALSE)
    rds <- file.path(cd, "01_fixture_results.rds")
    if (rebuild || !file.exists(rds)) {
      local({
        leagues <- if (grp == "base") WP_ELO_BASE_LEAGUES else WP_ELO_EXTRA_LEAGUES
        cache_dir <- cd
        # Pinned to step 01's defaults: its exists() guards would otherwise pick up a caller's globals.
        seasons <- NULL; min_season <- "2013-2014"
        source("data-raw/match-predictions-opta/01_build_fixture_results.R", local = TRUE)
      })
    }
    fx_list[[grp]] <- data.table::as.data.table(readRDS(rds))
  }
  allr <- data.table::rbindlist(fx_list, fill = TRUE)
  allr <- unique(allr[match_status == "Played"][order(match_date)], by = "match_id")
  er <- compute_match_elos(as.data.frame(allr), k = 20, home_advantage = 88, initial_elo = 1500,
                           k_table = ELO_MATCH_TYPE_K, cross_conf_mult = ELO_CROSS_CONF_MULT,
                           conf_priors = ELO_CONFEDERATION_PRIORS, use_venue_factor = TRUE)
  pm <- unique(data.table::as.data.table(er$per_match), by = "match_id")
  if (!nrow(pm) || mean(!is.na(pm$elo_diff)) <= 0.99) {
    cli::cli_abort("Elo build: {nrow(pm)} matches, elo_diff present on {round(100 * mean(!is.na(pm$elo_diff)), 1)}%.")
  }
  arrow::write_parquet(pm, elo_path)
  message(sprintf("  WP Elo: %d played matches, %d competitions, last match %s (%.1f min)", nrow(pm),
                  data.table::uniqueN(allr$league), max(allr$match_date),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  pm
}
