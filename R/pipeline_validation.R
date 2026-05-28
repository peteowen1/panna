# Pipeline validation: domain-truth assertions on pipeline outputs.
#
# Designed around the observation that today's worst bugs (EPR/PSR
# silently zeroed for WC2026 fixtures, Norway stuck at initial Elo 1500
# despite topping their qualifying group, EPL routed to the international
# specialist model) were ALL invisible to unit tests because the functions
# returned structurally-valid data — they were only caught by looking at
# the SEMANTIC VALUES in the output and comparing against "how should
# football actually look?".
#
# `assert_step_output()` is a thin wrapper that runs a named list of
# boolean checks against a pipeline output and emits one consolidated
# warning (or aborts) when any check fails. `WC2026_REFERENCE_FACTS` is a
# library of those checks for the WC2026 pipeline.
#
# Pattern: when you fix a bug, ALSO add the assertion that would have
# caught it. The assertion ships in the codebase as the falsifiable
# claim "this is what the output should look like."


#' Assert Pipeline Output Against Named Expectations
#'
#' Runs a list of named boolean checks against a data frame (or any
#' object) and reports any failures via cli::cli_warn or stop, depending
#' on `severity`. Each check is a function that takes the data and
#' returns TRUE if the expectation holds, FALSE otherwise. Errors
#' thrown inside a check are treated as failures (not propagated).
#'
#' @param data Pipeline output to validate (data frame, list, anything).
#' @param step_name Short identifier for the step being validated; appears
#'   in failure messages.
#' @param expectations Named list of single-argument functions returning
#'   logical(1). Names describe what the check asserts; they appear in
#'   failure messages.
#' @param severity One of "warn" (default; cli::cli_warn) or "abort"
#'   (stop). Use "abort" for invariants that mean the pipeline output is
#'   unsafe to publish; "warn" for soft signals that should be visible
#'   but not block.
#'
#' @return Invisibly returns a character vector of failed check names
#'   (length 0 means all passed).
#' @export
#' @examples
#' \dontrun{
#' assert_step_output(team_ratings, "step 02", list(
#'   "WC2026 fixtures have nonzero EPR" = function(d)
#'     all(d[league == "WC" & season == WC2026_SEASON_LABEL,
#'           home_sum_epr] != 0),
#'   "row count plausible" = function(d) nrow(d) > 10000L
#' ))
#' }
assert_step_output <- function(data, step_name, expectations,
                                severity = c("warn", "abort")) {
  severity <- match.arg(severity)
  if (!is.list(expectations) || is.null(names(expectations)) ||
      any(nchar(names(expectations)) == 0L)) {
    cli::cli_abort("`expectations` must be a named list of check functions.")
  }

  failed <- character(0)
  for (name in names(expectations)) {
    ok <- tryCatch(
      isTRUE(expectations[[name]](data)),
      error = function(e) {
        # Capture the error message so the user sees WHY the check
        # couldn't run (not just that it failed silently).
        attr(name, "error") <<- conditionMessage(e)
        FALSE
      }
    )
    if (!ok) failed <- c(failed, name)
  }

  if (length(failed) > 0L) {
    # cli bullet format: names are bullet types ("x", "i", "*"), values
    # are the message text. Order matters for setNames().
    bullets <- stats::setNames(failed, rep("x", length(failed)))
    msg <- c(
      sprintf("[%s] %d / %d output assertions failed.",
              step_name, length(failed), length(expectations)),
      bullets
    )
    if (severity == "abort") {
      cli::cli_abort(msg)
    } else {
      cli::cli_warn(msg)
    }
  } else {
    cli::cli_alert_success(sprintf(
      "[%s] all %d output assertions passed.",
      step_name, length(expectations)))
  }

  invisible(failed)
}


# =============================================================================
# WC2026 Reference Facts
# =============================================================================
# A library of domain-truth assertions for the WC2026 prediction pipeline.
# Each entry is a list with `fact` (human-readable rationale) and `check`
# (a function returning TRUE iff the fact holds against the data passed).
#
# These are facts about how WC2026 actually works — not about how the
# code is implemented. They should remain true regardless of how the
# pipeline is restructured. When a fact starts returning FALSE, either:
#  (a) the pipeline has a regression — most common case, fix the code
#  (b) the world changed — e.g., a host nation announced a name variant
#      Opta now serves differently — update the fact and document why.
#
# When adding a new fact, anchor it to a CONCRETE bug or domain-truth
# that motivated it. "All teams have non-zero Elo" is too vague to add
# value. "Norway's Elo should be > 1500 because they topped UEFA WCQ
# Group I with 24 points" is a specific falsifiable claim that traces to
# an observed-in-the-world fact.

#' WC2026 Reference Facts — Domain Assertions on Pipeline Outputs
#'
#' @format Named list of `list(fact, check_team_strength)` where
#'   `fact` is a one-line rationale string and `check_team_strength` is a
#'   function applied to `wc2026_team_strength.parquet` returning TRUE
#'   iff the fact holds.
#' @keywords internal
WC2026_REFERENCE_FACTS <- list(

  norway_elo_nontrivial = list(
    fact = paste(
      "Norway topped UEFA WC Qualifying Group I with 24 points (8W-0D-2L,",
      "+25 GD), beating Italy + 8 others. Their team-strength Elo must",
      "reflect this — anything near the 1500 initial means the intl",
      "qualifier matches didn't reach the Elo iteration."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      norway <- ts[ts$team == "Norway", "elo"]
      length(norway) == 1L && !is.na(norway) && norway > 1550
    }
  ),

  intl_teams_have_meaningful_elo = list(
    fact = paste(
      "Of the 48 WC2026 teams, no more than a small handful (debutants",
      "with little intl history) should have Elo within 5 of the initial",
      "1500. A larger count means the intl Elo coverage broke."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      n_stuck <- sum(abs(ts$elo - 1500) < 5, na.rm = TRUE)
      n_stuck <= 6L  # Curacao, NZ, Jordan, etc. may legitimately float
    }
  ),

  no_team_has_zero_elo = list(
    fact = paste(
      "Elo cannot legitimately be 0 — the system is bounded below by",
      "~1000 in practice and would never produce an exact zero. Exact",
      "0 means an NA-fill happened somewhere downstream (the 2026-05-28",
      "step 04 NA-fill turned every poisoned NA Elo into 0)."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      !any(ts$elo == 0, na.rm = TRUE)
    }
  ),

  big_teams_have_above_average_elo = list(
    fact = paste(
      "France, Germany, Brazil, Spain, Argentina — these teams should",
      "have well-above-average Elo (>1550) based on their international",
      "tournament + qualifier history. If any of them shows ~1500 or",
      "lower, the iteration was poisoned (the bug we caught 2026-05-28)."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      big <- c("France", "Germany", "Brazil", "Spain", "Argentina")
      vals <- ts[ts$team %in% big, "elo"]
      if (length(vals) == 0L) return(TRUE)
      # At least 4 out of 5 (allow one degraded for some upstream issue)
      sum(vals > 1550, na.rm = TRUE) >= 4L
    }
  ),

  epr_nonzero_for_european_squads = list(
    fact = paste(
      "European WC2026 teams (Germany, France, Spain, England, ...) all",
      "have squads dominated by tracked European-league players, so",
      "their team-aggregate EPR must be non-zero. All-zero EPR for",
      "these teams means the fixture-ratings EPR join silently failed",
      "(this happened 2026-05-28; player_id dropped by upstream select)."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      eu <- c("Germany", "France", "Spain", "England", "Portugal",
              "Netherlands", "Belgium", "Croatia", "Switzerland", "Norway")
      eu_in_data <- intersect(eu, ts$team)
      if (length(eu_in_data) < 6L) return(TRUE)  # not enough to assert
      all(ts[ts$team %in% eu_in_data, "epr"] != 0)
    }
  ),

  psr_nonzero_for_european_squads = list(
    fact = paste(
      "Same as EPR but for PSR — separate snapshot, separate join, but",
      "the same upstream player_id-drop bug zeroed both."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      eu <- c("Germany", "France", "Spain", "England", "Portugal",
              "Netherlands", "Belgium", "Croatia", "Switzerland", "Norway")
      eu_in_data <- intersect(eu, ts$team)
      if (length(eu_in_data) < 6L) return(TRUE)
      all(ts[ts$team %in% eu_in_data, "psr"] != 0)
    }
  ),

  top8_includes_six_of_eight_giants = list(
    fact = paste(
      "Of the eight perennial favourites (France, Germany, Brazil, Spain,",
      "Netherlands, England, Portugal, Argentina) — at LEAST 6 should",
      "appear in the model's top-8 by champion probability. Fewer signals",
      "a major segmentation or feature bug (e.g., EPL mis-classified as",
      "intl, which happened 2026-05-28)."
    ),
    check_simulation = function(sim) {
      sim <- as.data.frame(sim)
      sim <- sim[order(-sim$p_champ), ]
      top8 <- head(sim$team, 8L)
      giants <- c("France", "Germany", "Brazil", "Spain",
                  "Netherlands", "England", "Portugal", "Argentina")
      sum(top8 %in% giants) >= 6L
    }
  ),

  rank_columns_have_variance = list(
    fact = paste(
      "rank_epr, rank_psr columns should NOT have every team tied at 1.",
      "All-tied-at-1 happens when the underlying values are all equal",
      "(typically all zero — e.g., the EPR-all-zero bug of 2026-05-28).",
      "Detecting this catches the bug class at the published-table layer."
    ),
    check_team_strength = function(ts) {
      ts <- as.data.frame(ts)
      rank_cols <- intersect(c("rank_epr", "rank_psr"), names(ts))
      if (length(rank_cols) == 0L) return(TRUE)
      all(vapply(rank_cols, function(c) length(unique(ts[[c]])) > 1L,
                 logical(1)))
    }
  ),

  champ_sums_to_100 = list(
    fact = paste(
      "Champion probabilities across all 48 teams must sum to 100 ± 0.1.",
      "Any deviation indicates a sim arithmetic bug (lost tournaments,",
      "double-counted teams, etc.)."
    ),
    check_simulation = function(sim) {
      sim <- as.data.frame(sim)
      abs(sum(sim$p_champ) - 100) < 0.1
    }
  ),

  host_advantage_applied = list(
    fact = paste(
      "USA, Canada, Mexico should each appear with home_field = 1 in",
      "their 3 group-stage matches. This was a known silent-failure mode",
      "(host names hardcoded as strings — 2026-05-28); team_id-based",
      "lookup with stop()-on-missing was the fix."
    ),
    check_match_dataset = function(md) {
      md <- as.data.frame(md)
      wc_ids <- get0("WC2026_HOST_TEAM_IDS")
      if (is.null(wc_ids)) return(TRUE)
      hosts_home <- md[md$home_team_id %in% wc_ids &
                         md$league == "WC", , drop = FALSE]
      hosts_away <- md[md$away_team_id %in% wc_ids &
                         md$league == "WC", , drop = FALSE]
      all(hosts_home$home_field == 1L) && all(hosts_away$home_field == -1L)
    }
  )
)


#' Run the WC2026 Reference Facts Against Pipeline Outputs
#'
#' Loads the canonical output files (wc2026_team_strength.parquet,
#' wc2026_simulation.parquet, 04_match_dataset.rds), runs every
#' `WC2026_REFERENCE_FACTS` entry whose `check_*` field matches an
#' available data source, and emits one consolidated cli warning naming
#' the failures + the human-readable fact that motivated each check.
#'
#' Designed to be called at the end of the predictions pipeline (after
#' step 12). Returns the number of failed checks invisibly.
#'
#' @param cache_dir Directory holding the pipeline output files.
#'   Defaults to the pipeline's standard location.
#' @return Invisibly: the named list of failed checks (one element per
#'   failed fact, value = the `fact` string).
#' @export
run_wc2026_reference_checks <- function(
  cache_dir = file.path("data-raw", "cache-predictions-opta")
) {
  cli::cli_h2("WC2026 reference-fact validation")

  # Load whatever outputs are present; skip facts whose data isn't there.
  ts_path <- file.path(cache_dir, "wc2026_team_strength.parquet")
  sim_path <- file.path(cache_dir, "wc2026_simulation.parquet")
  md_path <- file.path(cache_dir, "04_match_dataset.rds")

  ts  <- if (file.exists(ts_path))  arrow::read_parquet(ts_path)  else NULL
  sim <- if (file.exists(sim_path)) arrow::read_parquet(sim_path) else NULL
  md  <- if (file.exists(md_path))  readRDS(md_path)              else NULL

  results <- list()
  for (name in names(WC2026_REFERENCE_FACTS)) {
    entry <- WC2026_REFERENCE_FACTS[[name]]
    checks_run <- 0L
    check_failed <- FALSE
    for (slot in c("check_team_strength", "check_simulation",
                   "check_match_dataset")) {
      if (!is.null(entry[[slot]])) {
        src <- switch(slot,
                      check_team_strength = ts,
                      check_simulation    = sim,
                      check_match_dataset = md)
        if (!is.null(src)) {
          checks_run <- checks_run + 1L
          ok <- tryCatch(isTRUE(entry[[slot]](src)),
                         error = function(e) FALSE)
          if (!ok) check_failed <- TRUE
        }
      }
    }
    if (checks_run == 0L) next  # no data source for this fact
    results[[name]] <- list(passed = !check_failed, fact = entry$fact)
  }

  n_pass <- sum(vapply(results, function(r) r$passed, logical(1)))
  n_fail <- length(results) - n_pass

  if (n_fail == 0L) {
    cli::cli_alert_success("All {length(results)} WC2026 reference fact{?s} hold.")
    return(invisible(list()))
  }

  failed <- results[!vapply(results, function(r) r$passed, logical(1))]
  cli::cli_alert_danger(
    "{n_fail} / {length(results)} WC2026 reference fact{?s} FAILED:")
  for (name in names(failed)) {
    cli::cli_text("")
    cli::cli_text("{.strong [{name}]}")
    cli::cli_text(failed[[name]]$fact)
  }
  invisible(lapply(failed, function(r) r$fact))
}
