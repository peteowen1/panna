# run_multitarget_eval.R
# FABLE-PRIOR-FIX-PLAN.md Step 6 (execution): isolated-run driver for the
# multi-target EPV/WPA RAPM/SPM/xRAPM chain.
#
# Steps 1-5 of the plan landed the plumbing/methodology fixes (C3/C1) inside
# 04_rapm.R/05_spm.R/06_xrapm.R/07_seasonal_ratings.R, all gated behind a
# script-level `run_multi_target <- TRUE` switch that defaults FALSE (D6) so
# the cloud pipeline never runs them until promotion. This driver flips that
# switch LOCALLY against an ISOLATED copy of the cache -- never the shared
# production `data-raw/cache-opta/` -- so a run here can never (a) corrupt a
# production artifact the panna#87 heartbeat glob would upload, or (b)
# collide with a concurrent production pipeline run.
#
# What it does NOT do: run steps 01-03 (data load / processing / splint
# creation) themselves. It reads the EXISTING production 03_splints.rds,
# filters it down to the benchmark universe (7 leagues x 5 seasons), and
# replicates 03_splint_creation.R's gated multi-target enrichment block
# (lines ~275-345: add_value_metrics_to_splints() from the Step 2 per-action
# EPV/WPA stream parquets) against that filtered copy -- because the real
# gated block in 03_splint_creation.R saves back over the SHARED
# 03_splints.rds, which we deliberately never run here.
#
# TWO outputs, do not conflate them:
#   1. The pooled 04->07 chain run (Step 3 below) fits ONE xRAPM per target
#      across ALL benchmark seasons combined -- this is PLUMBING VALIDATION
#      ONLY (it exercises the D5 tripwires, the D4 prior-match guard, and
#      the full SPM-prior chain end-to-end against real data, which is part
#      of Step 6's job). Its 07_seasonal_{epv,wpa}.rds artifacts must NOT be
#      used as the D6 benchmark candidate: a pooled 2022-2026 fit trains on
#      season S+1's own splints, which is hindsight leakage against the
#      S -> S+1 backtest eval_nextseason.R runs.
#   2. Step 5 below fits a SEPARATE, PRIOR-FREE, per-season RAPM per target
#      (season S's splints only, no SPM prior -- plain fit_rapm(), not
#      fit_rapm_with_prior()) and writes candidate_seasonal_{epv,wpa}.parquet.
#      THIS is the real D6 benchmark candidate eval_nextseason.R consumes.
#
# Usage (from the panna/ package root):
#   Rscript data-raw/player-ratings-opta/run_multitarget_eval.R
#
# Then benchmark the result:
#   Rscript data-raw/player-ratings-opta/eval_nextseason.R

suppressMessages(library(dplyr))
devtools::load_all()

if (!requireNamespace("callr", quietly = TRUE)) {
  stop("callr is required to run pipeline steps in isolation (matches run_pipeline_opta.R's pattern).",
       call. = FALSE)
}

if (!file.exists("DESCRIPTION") || !dir.exists("data-raw/player-ratings-opta")) {
  stop("Run this script from the panna package root (the directory containing DESCRIPTION), ",
       "e.g. `cd panna` first -- relative cache/script paths below assume it.", call. = FALSE)
}

# 1. Configuration ---- (exists() pattern per repo convention, inherits = FALSE
# so a same-named object from an enclosing/parent scope can't silently flip
# these)

if (!exists("production_cache", inherits = FALSE)) production_cache <- file.path("data-raw", "cache-opta")
if (!exists("eval_cache", inherits = FALSE)) eval_cache <- file.path("data-raw", "cache-opta-mteval")
if (!exists("benchmark_leagues", inherits = FALSE)) benchmark_leagues <- c("ENG", "ESP", "GER", "ITA", "FRA", "UCL", "UEL")
if (!exists("benchmark_seasons", inherits = FALSE)) benchmark_seasons <- 2022:2026
if (!exists("epv_action_dir", inherits = FALSE)) epv_action_dir <- file.path("data-raw", "cache", "epv", "players")
# Seasons the Step 5 PER-SEASON candidate stage fits (the real D6 benchmark
# input) -- a subset of benchmark_seasons matching eval_nextseason.R's own
# default eval_seasons (S end-years; each needs S+1 in benchmark_seasons for
# the production S+1 raw-RAPM target to exist, so 2026 -- the last
# benchmark_seasons entry -- is deliberately excluded here).
if (!exists("eval_seasons_for_candidates", inherits = FALSE)) eval_seasons_for_candidates <- 2023:2025

panna_root <- normalizePath(getwd(), winslash = "/")

cli::cli_h1("FABLE-PRIOR-FIX-PLAN.md Step 6: isolated multi-target eval driver")
cli::cli_alert_info("Production cache (read-only): {.path {production_cache}}")
cli::cli_alert_info("Isolated eval cache (all writes go here): {.path {eval_cache}}")
cli::cli_alert_info(sprintf(
  "Benchmark universe: %d leagues x %d seasons (%s; end-years %d-%d)",
  length(benchmark_leagues), length(benchmark_seasons),
  paste(benchmark_leagues, collapse = ", "), min(benchmark_seasons), max(benchmark_seasons)))

if (!dir.exists(eval_cache)) dir.create(eval_cache, recursive = TRUE)

pipeline_t0 <- Sys.time()

# ============================================================================
# 2. Copy the non-chain cache_dir files 04-07 read ----
#
# 03/04/05/06 (*.rds) are CHAIN artifacts -- each step's own output feeds the
# next, and step 3 (below) produces the isolated 03_splints.rds fresh, so
# those are never copied from production. Everything else 04-07 read from
# cache_dir, found by inspecting each script directly:
#   - 01_config.rds: not actually read by 04-07 (only by 01_load_opta_data.R,
#     which this driver never runs) -- copied anyway for provenance/parity
#     with the production cache, per the plan's instruction.
#   - 02_opta_stats.rds: read by 05_spm.R (opta_stats/opta_xmetrics for SPM
#     feature aggregation) AND 07_seasonal_ratings.R (same, per season).
#   - 02_processed_data.rds: read UNCONDITIONALLY by 06_xrapm.R's Section 7
#     "Team-Level Validation" (lineups/results, for an informational
#     npxGD-vs-sum-xRAPM correlation print) -- this is NOT gated behind
#     run_multi_target and has no file.exists() guard, so its absence
#     aborts 06_xrapm.R entirely before ANY multi-target artifact is saved.
#     Easy to miss because it's a chain-artifact-SHAPED filename (it looks
#     like a step-02 output) but it is not one of 03/04/05/06 -- it is a
#     genuine external input every downstream numbered step after 02
#     depends on. NOT currently present in this machine's production
#     cache-opta/ (checked live 2026-07-17) -- the abort below will fire
#     until it's regenerated (see the abort message for how).
# xMetrics itself is not read from outside cache_dir anywhere in 04-07 (it
# travels inside 02_opta_stats.rds, already listed above) -- there is no
# pannadata-loader case to special-case here.
# ============================================================================

cli::cli_h2("Step 1: copying non-chain cache files into the isolated cache")

.copy_required_cache_file <- function(fname, reason) {
  src <- file.path(production_cache, fname)
  dst <- file.path(eval_cache, fname)
  if (!file.exists(src)) {
    cli::cli_abort(c(
      "Required production cache file missing: {.path {src}}",
      "x" = "{reason}",
      "i" = "Regenerate it by resuming the production pipeline from step 2, e.g. from the panna root: `start_step <- 2; source('data-raw/player-ratings-opta/run_pipeline_opta.R')` (do not do this lightly -- it re-runs data processing over the full production league/season set)."
    ))
  }
  file.copy(src, dst, overwrite = TRUE)
  cli::cli_alert_success(sprintf("Copied %s (%.1f MB) -- %s", fname, file.size(src) / 1e6, reason))
}

.copy_optional_cache_file <- function(fname, reason) {
  src <- file.path(production_cache, fname)
  if (!file.exists(src)) {
    cli::cli_warn(c(
      "Optional production cache file missing: {.path {src}}",
      "i" = "{reason}"
    ))
    return(invisible(FALSE))
  }
  file.copy(src, file.path(eval_cache, fname), overwrite = TRUE)
  cli::cli_alert_success(sprintf("Copied %s (%.1f MB) -- %s", fname, file.size(src) / 1e6, reason))
  invisible(TRUE)
}

.copy_required_cache_file("01_config.rds", "not read by 04-07; copied for provenance/parity only")
.copy_required_cache_file("02_opta_stats.rds", "opta_stats/opta_xmetrics for SPM feature aggregation (05, 07)")
# 06_xrapm.R Section 7 now file.exists()-guards this read (skips its
# informational Team-Level Validation with a warning when absent), so a
# missing copy no longer aborts anything -- optional.
.copy_optional_cache_file("02_processed_data.rds", "06_xrapm.R Section 7 Team-Level Validation skips (guarded) without it")

# ============================================================================
# 3. Filter production splints to the benchmark universe + add per-splint
#    EPV/WPA value metrics ----
#
# Mirrors 03_splint_creation.R's gated multi-target block (lines ~275-345):
# same add_value_metrics_to_splints() call, same argument wiring (including
# the PSV load, which is NOT cache_dir-relative in the original either --
# data-raw/cache-skills/player_game_psv.rds is a fixed path there too, so no
# copy is needed for it). The one difference: the real block enriches and
# saves back over the SHARED, unfiltered 03_splints.rds; this driver filters
# FIRST (to the benchmark universe only) and saves to the ISOLATED cache.
# ============================================================================

cli::cli_h2("Step 2: filtering production splints to benchmark universe + adding per-splint EPV/WPA")

splint_data <- readRDS(file.path(production_cache, "03_splints.rds"))
cli::cli_alert_info(sprintf("Production splints: %s total (%s leagues, %s seasons)",
                            format(nrow(splint_data$splints), big.mark = ","),
                            length(unique(splint_data$splints$league)),
                            length(unique(splint_data$splints$season_end_year))))

keep_splints <- splint_data$splints$league %in% benchmark_leagues &
  splint_data$splints$season_end_year %in% benchmark_seasons
splints_f <- splint_data$splints[keep_splints, , drop = FALSE]
if (nrow(splints_f) == 0) {
  cli::cli_abort("No splints matched the benchmark universe (leagues: {paste(benchmark_leagues, collapse = ', ')}; seasons: {paste(benchmark_seasons, collapse = ', ')}).")
}
players_f <- splint_data$players[splint_data$players$splint_id %in% splints_f$splint_id, , drop = FALSE]
match_info_f <- splint_data$match_info[splint_data$match_info$match_id %in% splints_f$match_id, , drop = FALSE]

splint_data_eval <- list(splints = splints_f, players = players_f, match_info = match_info_f)
rm(splint_data); gc(verbose = FALSE)

cli::cli_alert_success(sprintf(
  "Filtered to %s splints / %s player-splint rows / %s matches",
  format(nrow(splints_f), big.mark = ","), format(nrow(players_f), big.mark = ","),
  format(nrow(match_info_f), big.mark = ",")))
print(table(splints_f$league, splints_f$season_end_year))

# Locate + load the EXACT benchmark-universe stream parquets (not a blanket
# glob -- the driver must fail loudly if the universe isn't fully backfilled
# rather than silently fitting on a partial one).
.season_label <- function(end_year) paste0(end_year - 1L, "-", end_year)
season_labels <- vapply(benchmark_seasons, .season_label, character(1))
combos <- expand.grid(league = benchmark_leagues, season_label = season_labels,
                      stringsAsFactors = FALSE)

epv_files <- file.path(epv_action_dir, sprintf("player_action_epv_%s_%s.parquet",
                                               combos$league, combos$season_label))
wpa_files <- file.path(epv_action_dir, sprintf("match_action_wpa_%s_%s.parquet",
                                               combos$league, combos$season_label))

missing_epv <- epv_files[!file.exists(epv_files)]
missing_wpa <- wpa_files[!file.exists(wpa_files)]
n_missing <- length(missing_epv) + length(missing_wpa)
if (n_missing > 0) {
  missing_bullets <- setNames(c(missing_epv, missing_wpa), rep("x", n_missing))
  cli::cli_abort(c(
    "Missing {n_missing} of {2 * nrow(combos)} per-action stream parquet{?s} for the benchmark universe:",
    missing_bullets,
    "i" = "Run data-raw/epv/02_calculate_player_epv.R and data-raw/epv/06_calculate_wpa.R for every (league, season) in the benchmark universe first (FABLE-PRIOR-FIX-PLAN.md Step 2)."
  ))
}
cli::cli_alert_success(sprintf("Found all %d EPV + %d WPA stream files for the benchmark universe",
                               length(epv_files), length(wpa_files)))

player_action_epv <- data.table::rbindlist(lapply(epv_files, arrow::read_parquet), fill = TRUE)
match_action_wpa <- data.table::rbindlist(lapply(wpa_files, arrow::read_parquet), fill = TRUE)
cli::cli_alert_info(sprintf("Loaded %s EPV action rows, %s WPA action rows",
                            format(nrow(player_action_epv), big.mark = ","),
                            format(nrow(match_action_wpa), big.mark = ",")))

# PSV: same fixed (non cache_dir-relative) path as the real gated block.
psv_cache <- file.path("data-raw", "cache-skills", "player_game_psv.rds")
player_game_psv <- if (file.exists(psv_cache)) readRDS(psv_cache) else NULL

splint_data_eval <- add_value_metrics_to_splints(
  splint_data_eval,
  player_action_epv = player_action_epv,
  match_action_wpa = match_action_wpa,
  player_game_psv = player_game_psv
)

added <- c()
if ("epv_home" %in% names(splint_data_eval$splints)) added <- c(added, "EPV")
if ("wpa_home" %in% names(splint_data_eval$splints)) added <- c(added, "WPA")
if ("psv_home" %in% names(splint_data_eval$splints)) added <- c(added, "PSV")
cli::cli_alert_success(sprintf("Value metrics added to isolated splints: %s", paste(added, collapse = ", ")))

saveRDS(splint_data_eval, file.path(eval_cache, "03_splints.rds"))
cli::cli_alert_success(sprintf("Saved isolated splint cache: %s", file.path(eval_cache, "03_splints.rds")))
rm(splint_data_eval, player_action_epv, match_action_wpa, player_game_psv); gc(verbose = FALSE)

# ============================================================================
# 4. Source 04 -> 05 -> 06 -> 07 in order, isolated ----
#
# Each step runs in its OWN fresh callr subprocess -- mirrors
# run_pipeline_opta.R's run_step_opta()/isolated() pattern (memory is fully
# released to the OS between steps; the numbered scripts' own
# `library(dplyr); devtools::load_all()` re-attaches the package fresh each
# time). One important divergence from run_pipeline_opta.R, found while
# building this: that orchestrator wraps `source(file, local = TRUE)` inside
# an extra closure (`function() { source(...) }`) passed across the callr
# boundary -- `local = TRUE` evaluates the sourced script's top-level code in
# THAT closure's own execution frame, not .GlobalEnv, so each numbered
# script's own `if (!exists("cache_dir", inherits = FALSE)) cache_dir <-
# ...` config guard (inherits = FALSE deliberately restricts the check to
# exactly one frame) can never see an override injected into .GlobalEnv --
# it silently keeps the hardcoded default every time. This is invisible in
# production only because the injected value there always EQUALS that
# default. This driver's override (a different eval_cache path, and
# run_multi_target flipped TRUE) must not be silently dropped the same way,
# so the child function below assigns directly into globalenv() and then
# calls source() with its DEFAULT (local = FALSE), which source() itself
# hardcodes to .GlobalEnv regardless of caller nesting -- the sourced
# script's exists(inherits = FALSE) checks then see exactly what was
# assigned.
# ============================================================================

cli::cli_h2("Step 3: running 04_rapm.R -> 05_spm.R -> 06_xrapm.R -> 07_seasonal_ratings.R (isolated, run_multi_target = TRUE)")
cli::cli_alert_warning("This is PLUMBING VALIDATION, NOT the D6 benchmark candidate -- see the header comment. It exercises the D5 tripwires / D4 prior-match guard / full SPM-prior chain end-to-end on real data (Step 6's job), but its pooled 07_seasonal_epv/wpa.rds output trains on season S+1's own splints for every S -- hindsight leakage. The real candidate is built fresh, per-season, prior-free, in Step 5 below.")

.run_isolated_step <- function(step_num, step_name, script_rel_path, eval_cache, panna_root) {
  cli::cli_h3(sprintf("[step %s] %s (%s)", step_num, step_name, script_rel_path))
  t0 <- Sys.time()
  step_error <- NULL
  tryCatch({
    callr::r(
      func = function(script_path, cache_dir, panna_root) {
        setwd(panna_root)
        assign("cache_dir", cache_dir, envir = globalenv())
        assign("run_multi_target", TRUE, envir = globalenv())
        # Pipeline helpers (validate_step_output/save_cache_with_meta/...)
        # live in data-raw/pipeline_utils.R, NOT the R/ package -- 04 and 07
        # call them directly. Re-source inside the child (run_pipeline_opta.R
        # does the same for the same reason: load_all() only attaches R/).
        utils_path <- file.path("data-raw", "pipeline_utils.R")
        if (file.exists(utils_path)) source(utils_path)
        source(script_path)
        invisible(NULL)
      },
      args = list(script_path = script_rel_path, cache_dir = eval_cache, panna_root = panna_root),
      wd = panna_root, show = TRUE, spinner = FALSE
    )
  }, error = function(e) {
    step_error <<- conditionMessage(e)
  })
  dt <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
  if (!is.null(step_error)) {
    cli::cli_abort(c(
      "Step {step_num} ({step_name}) FAILED after {dt}s.",
      "x" = "{step_error}",
      "i" = "All multi-target tripwires (D5) and the run_multi_target gate (D6) stayed active -- this is either a real degenerate-output abort or a genuine upstream problem, not a driver bug to route around."
    ))
  }
  cli::cli_alert_success(sprintf("Step %s (%s) complete in %ss", step_num, step_name, dt))
  invisible(TRUE)
}

steps <- list(
  list(4, "04_rapm",            file.path("data-raw", "player-ratings-opta", "04_rapm.R")),
  list(5, "05_spm",              file.path("data-raw", "player-ratings-opta", "05_spm.R")),
  list(6, "06_xrapm",            file.path("data-raw", "player-ratings-opta", "06_xrapm.R")),
  list(7, "07_seasonal_ratings", file.path("data-raw", "player-ratings-opta", "07_seasonal_ratings.R"))
)

for (s in steps) {
  .run_isolated_step(s[[1]], s[[2]], s[[3]], eval_cache = eval_cache, panna_root = panna_root)
}

# ============================================================================
# 5. Summary ----
# ============================================================================

cli::cli_h2("Step 4: pooled-run artifact summary (plumbing validation, NOT the benchmark candidate)")
cli::cli_alert_info(sprintf("Isolated cache dir: %s", normalizePath(eval_cache)))
artifact_files <- list.files(eval_cache, pattern = "^0[4-7].*\\.rds$")
cli::cli_ul(artifact_files)

splint_eval_check <- readRDS(file.path(eval_cache, "03_splints.rds"))
splints_lu <- data.table::as.data.table(splint_eval_check$splints)[, .(splint_id, season_end_year)]
players_lu <- data.table::as.data.table(splint_eval_check$players)[, .(splint_id, player_id)]
player_season_lookup <- unique(merge(players_lu, splints_lu, by = "splint_id")[, .(player_id, season_end_year)])
rm(splint_eval_check, splints_lu, players_lu)

for (tgt in c("epv", "wpa")) {
  tgt_path <- file.path(eval_cache, sprintf("07_seasonal_%s.rds", tgt))
  if (!file.exists(tgt_path)) {
    cli::cli_alert_warning(sprintf("%s: no 07_seasonal_%s.rds artifact (target skipped, or its D5 tripwire fired upstream and aborted the whole step -- see the step log above)", toupper(tgt), tgt))
    next
  }
  ratings_tgt <- readRDS(tgt_path)
  cli::cli_alert_success(sprintf(
    "%s: %d players rated. NOTE this is a POOLED xRAPM fit across all %d benchmark seasons combined (04_rapm.R's multi-target section runs prepare_rapm_data() on the whole filtered splint_data, no per-season loop like the base RAPM path's fit_season_ratings_opta()) -- there is no season_end_year column on this artifact, and it is LEAKAGE-CONTAMINATED as a benchmark candidate (trains on every S+1 too). Breakdown below is 'players active in each benchmark season', not a per-season refit -- plumbing validation only.",
    toupper(tgt), nrow(ratings_tgt), length(benchmark_seasons)))
  by_season <- player_season_lookup[player_id %in% ratings_tgt$player_id, .N, by = season_end_year][order(season_end_year)]
  print(by_season)
}

# ============================================================================
# 6. Per-season, prior-free candidate ratings -- the REAL D6 benchmark input ----
#
# Fits fit_rapm() directly (no SPM prior -- fit_rapm_with_prior() is never
# called here) on season-S-ONLY splints, one fit per (target, season). This
# is deliberately NOT the same object 04_rapm.R's multi-target section
# builds: that one is pooled across the whole benchmark window (Step 3/4
# above), so a season-S row's fit has already seen season S+1's own splints
# by the time it's evaluated against season S+1's raw RAPM -- hindsight
# leakage on exactly the axis eval_nextseason.R measures. A per-season,
# splints-restricted-to-S fit cannot see S+1 at all.
#
# Runs directly in this process (not via callr/source() like Step 3) --
# devtools::load_all() at the top of this script already attached
# prepare_rapm_data()/fit_rapm()/extract_rapm_ratings(), and each season's
# design matrix is a ~1/5 slice of the pooled one Step 3 already fit
# successfully in-subprocess, so isolation is not needed here.
# ============================================================================

cli::cli_h2("Step 5: per-season, prior-free candidate ratings (the ACTUAL D6 benchmark input)")

splint_data_eval <- readRDS(file.path(eval_cache, "03_splints.rds"))

# Same package constant 04_rapm.R's own base fit uses (R/constants.R) --
# read directly rather than re-declaring it, so this never drifts from
# production's own min_minutes.
candidate_min_minutes <- MIN_MINUTES_RAPM_FIT
cli::cli_alert_info(sprintf("Candidate seasons: %s | min_minutes = %d (MIN_MINUTES_RAPM_FIT)",
                            paste(eval_seasons_for_candidates, collapse = ", "), candidate_min_minutes))

for (tgt in c("epv", "wpa")) {
  cli::cli_h3(sprintf("Per-season prior-free RAPM for target: %s", tgt))
  tgt_mode <- if (tgt == "wpa") "net" else "od"
  home_col <- paste0(tgt, "_home")

  if (!home_col %in% names(splint_data_eval$splints)) {
    cli::cli_alert_warning(sprintf("%s: splints have no %s column (value metrics missing) -- skipping candidate_seasonal_%s.parquet", toupper(tgt), home_col, tgt))
    next
  }

  season_ratings <- vector("list", length(eval_seasons_for_candidates))
  names(season_ratings) <- as.character(eval_seasons_for_candidates)

  for (S in eval_seasons_for_candidates) {
    t0 <- Sys.time()
    s_splints <- splint_data_eval$splints[splint_data_eval$splints$season_end_year == S, , drop = FALSE]
    s_players <- splint_data_eval$players[splint_data_eval$players$splint_id %in% s_splints$splint_id, , drop = FALSE]
    s_match_info <- splint_data_eval$match_info[splint_data_eval$match_info$match_id %in% s_splints$match_id, , drop = FALSE]
    splint_data_S <- list(splints = s_splints, players = s_players, match_info = s_match_info)

    if (nrow(s_splints) < 100) {
      cli::cli_alert_warning(sprintf("  %s season %d: only %d splints -- skipping", toupper(tgt), S, nrow(s_splints)))
      next
    }

    rapm_data_S <- prepare_rapm_data(
      splint_data_S,
      min_minutes = candidate_min_minutes,
      target_type = tgt,
      include_covariates = TRUE,
      mode = tgt_mode
    )

    if (rapm_data_S$n_players < 20) {
      cli::cli_alert_warning(sprintf("  %s season %d: only %d players meet min_minutes -- skipping", toupper(tgt), S, rapm_data_S$n_players))
      next
    }

    n_obs_S <- if (!is.null(rapm_data_S$X_full)) nrow(rapm_data_S$X_full) else nrow(rapm_data_S$X)
    n_folds <- max(3, min(10, floor(n_obs_S / 20)))

    # PRIOR-FREE: fit_rapm(), never fit_rapm_with_prior() -- no SPM prior is
    # built or consulted anywhere in this stage.
    model_S <- fit_rapm(
      rapm_data_S,
      alpha = 0,
      nfolds = n_folds,
      use_weights = TRUE,
      penalize_covariates = FALSE,
      parallel = FALSE
    )

    ratings_S <- extract_rapm_ratings(model_S)
    ratings_S$season_end_year <- S
    season_ratings[[as.character(S)]] <- ratings_S

    dt_S <- round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1)
    cli::cli_alert_success(sprintf("  %s season %d: %d players rated (prior-free, mode=%s, %d folds) in %ss",
                                   toupper(tgt), S, nrow(ratings_S), tgt_mode, n_folds, dt_S))
    rm(rapm_data_S, model_S); gc(verbose = FALSE)
  }

  season_ratings <- Filter(Negate(is.null), season_ratings)
  if (length(season_ratings) == 0) {
    cli::cli_alert_warning(sprintf("%s: no seasons produced a candidate fit -- no candidate_seasonal_%s.parquet written", toupper(tgt), tgt))
    next
  }

  candidate_dt <- data.table::rbindlist(season_ratings, fill = TRUE)
  # extract_rapm_ratings()'s primary rating column is `rapm` (od mode:
  # offense - defense; net mode: the single net coefficient, offense/defense
  # NA) -- renamed `rating` here per the D6 benchmark's column contract.
  candidate_out <- candidate_dt[, .(player_id, player_name, total_minutes,
                                    rating = rapm, offense, defense, season_end_year)]

  out_path <- file.path(eval_cache, sprintf("candidate_seasonal_%s.parquet", tgt))
  arrow::write_parquet(candidate_out, out_path)
  cli::cli_alert_success(sprintf("Saved %s (%d rows across %d seasons)", out_path, nrow(candidate_out), length(season_ratings)))
}

rm(splint_data_eval); gc(verbose = FALSE)

cli::cli_h1("Isolated multi-target eval driver complete")
cli::cli_alert_info(sprintf("Total wall time: %.1f min", as.numeric(difftime(Sys.time(), pipeline_t0, units = "mins"))))
cli::cli_alert_info(sprintf("Next: Rscript data-raw/player-ratings-opta/eval_nextseason.R  (candidate_cache defaults to %s, reads candidate_seasonal_epv/wpa.parquet)", eval_cache))
