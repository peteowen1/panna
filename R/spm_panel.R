# SPM panel-training machinery (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2, sec
# 3.1, Wave 2).
#
# One training row per (player, vintage year Y): box+xMetrics per-90
# aggregates over the SAME [Y-window_years, Y) window as the windowed
# prior-free RAPM target (rapm_window_targets.rds, built by
# 04b_rapm_window_targets.R) -- window-alignment on both sides of the
# regression is the single biggest structural upgrade over today's
# all-history-vs-all-history SPM (05_spm.R). Do NOT modify 05_spm.R or the
# existing spm_opta.R functions -- this file is new, additive machinery that
# reuses their canonical helpers (.spm_opta_predictor_cols(),
# .get_opta_col_mapping(), .spm_xmetrics_per90_cols(), classify_role()).
#
# Circularity discipline (sec 2.4): every entry point that consumes a target
# artifact calls assert_prior_free_target() (R/spm_asof.R) before fitting.
# build_spm_panel() stamps the panel with the target's provenance;
# fit_spm_panel() re-checks that stamp via the SAME assert function rather
# than re-deriving the check, so the guard can never drift between the two
# call sites.


# ============================================================================
# Role taxonomy
# ============================================================================

#' Map a 16-role code (`classify_role()` output) to the plan's 6-group role
#' taxonomy
#'
#' BOX-SCORE-VALUE-SPM-REDESIGN.md sec 3.1: GK, CB, FB/WB \{LB,RB,LWB,RWB\},
#' DM/CM \{DM,CM\}, AM/Wide \{CAM,LM,RM,LW,RW\}, CF \{CF,LF,RF\}. Group codes are
#' alphanumeric-only (no `/`) so they're safe to use inside design-matrix
#' column names (`dev__<group>__<feature>`, see `.build_panel_design_matrix()`).
#' `"UNK"` (blank/unrecognized `classify_role()` output) and any other
#' unmatched code map to `NA` -- those rows get zero role-deviation
#' contribution (pure global-feature pricing) rather than being forced into
#' a group they don't belong to.
#'
#' @param role Character vector of `classify_role()` 16-role codes.
#' @return Character vector of 6-group codes (`"GK"`, `"CB"`, `"FBWB"`,
#'   `"DMCM"`, `"AMWIDE"`, `"CF"`), or `NA` for unmatched input.
#' @family spm panel
#' @export
classify_role_group <- function(role) {
  role <- as.character(role)
  out <- rep(NA_character_, length(role))
  out[role == "GK"] <- "GK"
  out[role == "CB"] <- "CB"
  out[role %in% c("LB", "RB", "LWB", "RWB")] <- "FBWB"
  out[role %in% c("DM", "CM")] <- "DMCM"
  out[role %in% c("CAM", "LM", "RM", "LW", "RW")] <- "AMWIDE"
  out[role %in% c("CF", "LF", "RF")] <- "CF"
  out
}

#' The 5 outfield role groups eligible for partial-pooling deviation columns
#' @keywords internal
.spm_panel_outfield_role_groups <- function() {
  c("CB", "FBWB", "DMCM", "AMWIDE", "CF")
}

#' The plan's role-ambivalent restricted feature set for deviation columns
#'
#' BOX-SCORE-VALUE-SPM-REDESIGN.md sec 3.1: "touches, passes, clearances,
#' aerials, crosses". Mapped to the box-score per-90 column names
#' (`aerial_won_p90` for "aerials" -- the won side is the unambiguous-volume
#' half; `aerial_lost_p90` is already a bad-defense sign-constrained feature
#' on its own, not part of the role-ambivalent set).
#' @keywords internal
.spm_panel_role_ambivalent_cols <- function() {
  c("touches_p90", "passes_p90", "clearances_p90", "aerial_won_p90", "crosses_p90")
}


# ============================================================================
# Panel builder
# ============================================================================

#' Build one (player, vintage year) window of raw counting-stat totals
#'
#' Sums the raw Opta counting columns (`.get_opta_col_mapping()` panna
#' names, already present on `match_stats` -- it is
#' `compute_match_level_opta_stats()` output) over matches with
#' `season_end_year` in `[min_year, cutoff_year)`, derives a minutes-weighted
#' modal `classify_role()`/`classify_role_group()` per player, then runs the
#' SAME per-90 + derived-feature pipeline `aggregate_opta_stats()` uses
#' (`.calculate_opta_per90()`, `.calculate_opta_derived_features()`) so the
#' window-level feature columns are byte-identical in name/construction to
#' the career-level SPM's -- the column contract `.spm_opta_predictor_cols()`
#' selects against.
#'
#' @param match_stats data.table, `compute_match_level_opta_stats()` shape
#'   (needs `player_id`, `match_id`, `season_end_year`, `position`,
#'   `position_side`, `total_minutes`, `league`, plus the raw counting
#'   columns).
#' @param cutoff_year Integer; rows from seasons `< cutoff_year` are kept.
#' @param min_year Integer; rows from seasons `< min_year` are dropped
#'   (window is `[min_year, cutoff_year)`).
#' @param leagues Optional character vector to restrict to (`NULL` = all).
#' @param include_xmetrics Whether to attempt xMetrics enrichment (best-effort
#'   -- see `build_spm_panel()`).
#' @param xmetrics_source `"local"` or `"remote"`, passed to
#'   `enrich_match_stats_with_xmetrics()`.
#' @return data.frame, one row per player, feature columns + `role`,
#'   `role_group`, `window_minutes`, `n_matches`. `NULL` if no rows survive
#'   the window/league filter. Attribute `xmetrics_included` (logical).
#' @keywords internal
.spm_panel_window_features <- function(match_stats, cutoff_year, min_year,
                                       leagues = NULL,
                                       include_xmetrics = TRUE,
                                       xmetrics_source = "local") {
  window <- match_stats[season_end_year >= min_year & season_end_year < cutoff_year]
  if (!is.null(leagues)) {
    window <- window[league %in% leagues]
  }
  if (nrow(window) == 0) return(NULL)

  window <- data.table::copy(window)

  # TWO SEPARATE position derivations, deliberately -- not silent drift:
  #  1. `primary_position` (below): the COUNT-modal `position` string over
  #     window rows, byte-for-byte the same recipe `aggregate_opta_stats()`
  #     uses (spm_opta.R). Feeding this into `.calculate_opta_derived_features()`
  #     (further down) makes it create is_gk/is_df/is_mf/is_fw the EXACT
  #     career-SPM way, and lets `build_spm_panel()` route GK EXCLUSION
  #     through the canonical `.detect_gk_rows()` (R/psr.R) -- the
  #     one-source-of-truth GK detector shared with compute_player_psv()/
  #     the 07b sd_match build. Two independent GK detectors WILL drift.
  #  2. `role`/`role_group` (below): the MINUTES-weighted modal
  #     `classify_role()` 16-role -> 6-group taxonomy, used ONLY for the
  #     role-group partial-pooling design matrix (`fit_spm_panel()`'s
  #     `role_pooling`). Per panna/CLAUDE.md's stated preference for the
  #     16-role taxonomy over the legacy 4-bucket for new feature work --
  #     but the legacy 4-bucket dummies stay on the career-SPM recipe above
  #     since they're existing `.spm_opta_predictor_cols()` columns, not new.
  pos_mode <- NULL
  if ("position" %in% names(window)) {
    pos_mode <- window[!is.na(position) & position != "", {
      tbl <- table(position)
      list(primary_position = names(tbl)[which.max(tbl)])
    }, by = player_id]
  }

  window[, row_role := classify_role(position, position_side)]
  role_minutes <- window[, .(mins = sum(total_minutes, na.rm = TRUE)),
                         by = .(player_id, row_role)]
  data.table::setorder(role_minutes, player_id, -mins)
  primary_role <- role_minutes[, .SD[1L], by = player_id][, .(player_id, role = row_role)]
  primary_role[, role_group := classify_role_group(role)]

  # Optional, best-effort xMetrics enrichment -- adds PER-MATCH per90 rate
  # columns (that match's own minutes as denominator); reconstruct per-match
  # totals (rate * minutes / 90) via set() (never get()-in-bracket, see
  # r-datatable-gotchas) so they can be summed to a window total and
  # re-rated at window minutes, matching .aggregate_xmetrics_for_spm()'s
  # volume-based (not rate-averaged) window construction.
  xmetrics_included <- FALSE
  if (isTRUE(include_xmetrics)) {
    # NOTE: do NOT diff names(window) before/after enrich_match_stats_with_xmetrics()
    # to detect which columns it added. enrich_...() uses data.table::set()
    # to add columns BY REFERENCE, and data.table's over-allocation trick for
    # O(1) column-add can mutate the SAME underlying character vector a
    # previously-captured `names(window)` snapshot points to -- a `before <-
    # names(window)` "snapshot" silently grew to match the post-enrichment
    # column count in testing (confirmed via address()/length() before and
    # after), making a before/after diff always empty. Checking membership
    # in the canonical, fixed .spm_xmetrics_per90_cols() list is both
    # correct and immune to this: match_stats/01_match_stats.rds carries
    # none of these column names before enrichment.
    window <- tryCatch(
      enrich_match_stats_with_xmetrics(window, verbose = FALSE,
                                       source = xmetrics_source,
                                       fail_if_missing_frac = Inf),
      error = function(e) {
        cli::cli_warn("xMetrics enrichment failed for vintage {cutoff_year}: {conditionMessage(e)} -- proceeding box-only.")
        window
      }
    )
    xm_names <- .spm_xmetrics_per90_cols()
    present_xm <- intersect(xm_names, names(window))
    if (length(present_xm) > 0) {
      xmetrics_included <- TRUE
      tot_cols <- paste0(present_xm, "__tot")
      for (i in seq_along(present_xm)) {
        data.table::set(window, j = tot_cols[i],
                        value = window[[present_xm[i]]] * window$total_minutes / 90)
      }
    }
  }

  raw_cols <- intersect(names(.get_opta_col_mapping()), names(window))
  xm_tot_cols <- if (xmetrics_included) grep("__tot$", names(window), value = TRUE) else character(0)

  agg <- window[, c(
    lapply(.SD, function(x) sum(as.numeric(x), na.rm = TRUE)),
    list(n_matches = data.table::uniqueN(match_id))
  ), by = player_id, .SDcols = c(raw_cols, xm_tot_cols)]

  if (length(xm_tot_cols) > 0) {
    for (tc in xm_tot_cols) {
      rate_col <- sub("__tot$", "", tc)
      agg[[rate_col]] <- ifelse(agg$total_minutes > 0, agg[[tc]] / agg$total_minutes * 90, 0)
      agg[[tc]] <- NULL
    }
  }

  # merge(), not a X[Y, on=] bracket join -- see build_spm_panel()'s note.
  agg <- merge(agg, primary_role, by = "player_id")
  if (!is.null(pos_mode)) {
    agg <- merge(agg, pos_mode, by = "player_id", all.x = TRUE)
  }

  df <- data.table::setDF(data.table::copy(agg))
  df <- .calculate_opta_per90(df)
  df <- .calculate_opta_derived_features(df)
  df <- .clean_numeric_na(df, check_inf = TRUE)

  df$window_minutes <- df$total_minutes
  attr(df, "xmetrics_included") <- xmetrics_included
  df
}


#' Build the SPM training panel: one row per (player, vintage year)
#'
#' BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/3.1: for each vintage `Y` in
#' `vintage_years`, features are the player's box+xMetrics per-90 aggregates
#' over `[Y - window_years, Y)` (`.spm_panel_window_features()`), target is
#' the SAME-window prior-free RAPM for `Y` (`rapm_window_targets.rds`,
#' 04b_rapm_window_targets.R). Feature/target window alignment is checked,
#' not assumed (`strict_window_check`).
#'
#' Circularity guard: calls `assert_prior_free_target(rapm_window_targets)`
#' before touching any rating, and stamps the returned panel's
#' `target_provenance` attribute so `fit_spm_panel()` can re-verify it
#' without needing the raw target object again.
#'
#' GK rows are excluded by default (`include_gk = FALSE`) -- panna#159's
#' keeper-rows-only design is the intended home for GK `spm_value`/SPM
#' pricing (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 3.1, R7); this panel is
#' outfield-only until that lands.
#'
#' @param match_stats data.table, `compute_match_level_opta_stats()` shape
#'   (e.g. `cache-skills/01_match_stats.rds`). Must have `season_end_year`
#'   OR `season` (the latter is converted via `extract_season_end_year()`).
#' @param rapm_window_targets The list from `rapm_window_targets.rds`
#'   (04b_rapm_window_targets.R), keyed by vintage year.
#' @param vintage_years Integer vector of vintage years to build (default
#'   `2019:2026`, matching Wave 1).
#' @param window_years Window length (default 5, matching sec 2.1). Must
#'   match the window `rapm_window_targets` was built with, or
#'   `strict_window_check` aborts.
#' @param leagues Optional character vector to restrict to (`NULL` = all --
#'   full build; pass a small set for a smoke-scale build).
#' @param include_xmetrics Attempt best-effort xMetrics enrichment per
#'   vintage (default `TRUE`). Degrades gracefully (box-only, `cli_warn`) if
#'   local/remote xMetrics coverage is unavailable for a vintage's window --
#'   see `.spm_panel_window_features()`.
#' @param xmetrics_source `"local"` or `"remote"`.
#' @param include_gk Include GK rows in the returned panel (default `FALSE`
#'   -- see above). When `FALSE`, dropped-row counts are reported via
#'   `cli::cli_inform()`.
#' @param strict_window_check Abort if a vintage's actual window (min_year,
#'   cutoff_year) doesn't match the corresponding `rapm_window_targets`
#'   entry's `window` (default `TRUE` -- window misalignment defeats the
#'   whole point of the panel design; sec 2.2).
#'
#' @return data.table, one row per (player_id, vintage_year), with feature
#'   columns, `role`, `role_group`, `window_minutes`, `n_matches`,
#'   `offense_target`, `defense_target`, `rapm_target` (net), `vintage_year`,
#'   `window_min_year`, `window_max_year`. Attributes: `target_provenance`
#'   (`"prior_free_rapm_window"`), `builder_params` (list of the arguments
#'   above + `built_at`).
#' @family spm panel
#' @export
build_spm_panel <- function(match_stats, rapm_window_targets,
                            vintage_years = 2019:2026, window_years = 5L,
                            leagues = NULL, include_xmetrics = TRUE,
                            xmetrics_source = c("local", "remote"),
                            include_gk = FALSE, strict_window_check = TRUE) {
  xmetrics_source <- match.arg(xmetrics_source)
  assert_prior_free_target(rapm_window_targets)

  match_stats <- data.table::as.data.table(match_stats)
  if (!"season_end_year" %in% names(match_stats)) {
    if (!"season" %in% names(match_stats)) {
      cli::cli_abort("match_stats has neither {.field season_end_year} nor {.field season}.")
    }
    uniq_seasons <- unique(match_stats$season)
    sey_map <- stats::setNames(vapply(uniq_seasons, extract_season_end_year, numeric(1)), uniq_seasons)
    match_stats[, season_end_year := sey_map[season]]
  }

  panel_list <- vector("list", length(vintage_years))
  names(panel_list) <- as.character(vintage_years)
  xmetrics_flags <- stats::setNames(rep(NA, length(vintage_years)), as.character(vintage_years))
  n_gk_dropped <- 0L

  for (Y in vintage_years) {
    min_year <- Y - window_years
    tgt_entry <- rapm_window_targets[[as.character(Y)]]
    if (is.null(tgt_entry)) {
      cli::cli_warn("build_spm_panel: no target for vintage {Y} in rapm_window_targets -- skipping.")
      next
    }
    if (isTRUE(strict_window_check) && !is.null(tgt_entry$window)) {
      if (!identical(as.numeric(tgt_entry$window), as.numeric(c(min_year, Y)))) {
        cli::cli_abort(c(
          "build_spm_panel: window mismatch for vintage {Y}.",
          "x" = "Panel window computed as [{min_year}, {Y}); target window is [{tgt_entry$window[1]}, {tgt_entry$window[2]}).",
          "i" = "Feature/target window alignment is the panel design's central invariant (sec 2.2) -- pass matching window_years or strict_window_check = FALSE to override deliberately."
        ))
      }
    }

    feats <- .spm_panel_window_features(match_stats, cutoff_year = Y, min_year = min_year,
                                        leagues = leagues, include_xmetrics = include_xmetrics,
                                        xmetrics_source = xmetrics_source)
    if (is.null(feats) || nrow(feats) == 0) {
      cli::cli_warn("build_spm_panel: no window rows for vintage {Y} -- skipping.")
      next
    }
    xmetrics_flags[[as.character(Y)]] <- isTRUE(attr(feats, "xmetrics_included"))

    feats <- data.table::as.data.table(feats)
    if (!isTRUE(include_gk)) {
      # GK exclusion routes through the canonical .detect_gk_rows() (R/psr.R)
      # -- the one-source-of-truth GK detector shared with
      # compute_player_psv()/the 07b sd_match build -- NOT role_group (which
      # is classify_role()-based and kept only for the pooling design; see
      # .spm_panel_window_features()'s header comment). Two independent GK
      # detectors WILL drift.
      is_gk_row <- .detect_gk_rows(feats)
      n_gk_dropped <- n_gk_dropped + sum(is_gk_row)
      feats <- feats[!is_gk_row]
    }

    tgt <- data.table::as.data.table(tgt_entry$ratings)[, .(player_id, offense_target = offense,
                                                             defense_target = defense, rapm_target = rapm)]
    # merge(), not a X[Y, on=] bracket join -- the bracket form returns only
    # the calling table's columns (a common data.table trap); merge() gives
    # the full inner-join column set from both sides (all = FALSE default).
    row <- merge(feats, tgt, by = "player_id")
    row[, `:=`(vintage_year = Y, window_min_year = min_year, window_max_year = Y)]
    panel_list[[as.character(Y)]] <- row
  }

  panel <- data.table::rbindlist(Filter(Negate(is.null), panel_list), use.names = TRUE, fill = TRUE)
  if (nrow(panel) == 0) {
    cli::cli_abort("build_spm_panel: no rows built across any vintage -- check match_stats/rapm_window_targets coverage.")
  }

  if (!isTRUE(include_gk) && n_gk_dropped > 0) {
    cli::cli_inform("build_spm_panel: excluded {n_gk_dropped} GK player-vintage row(s) (include_gk = FALSE; panna#159 owns GK pricing design).")
  }

  vintages_with_xm <- names(xmetrics_flags)[!is.na(xmetrics_flags) & xmetrics_flags]
  cli::cli_inform(sprintf(
    "build_spm_panel: %d rows, %d players, %d vintages [%s]. xMetrics included for %d/%d vintages [%s].",
    nrow(panel), data.table::uniqueN(panel$player_id), data.table::uniqueN(panel$vintage_year),
    paste(sort(unique(panel$vintage_year)), collapse = ","),
    length(vintages_with_xm), length(Filter(Negate(is.na), xmetrics_flags)),
    paste(vintages_with_xm, collapse = ",")
  ))

  attr(panel, "target_provenance") <- "prior_free_rapm_window"
  attr(panel, "builder_params") <- list(
    vintage_years = vintage_years, window_years = window_years, leagues = leagues,
    include_xmetrics = include_xmetrics, xmetrics_source = xmetrics_source,
    include_gk = include_gk, strict_window_check = strict_window_check,
    xmetrics_included_by_vintage = xmetrics_flags,
    n_gk_dropped = n_gk_dropped, built_at = Sys.time(),
    panna_version = as.character(utils::packageVersion("panna"))
  )
  panel
}


# ============================================================================
# Sign constraints (offense NEW per sec 3.1; defense copied verbatim from
# 05_spm.R's hand-curated lists -- 05_spm.R itself is NOT modified)
# ============================================================================

#' NEW offense sign constraints (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 3.1):
#' unambiguous-direction offensive stats get a non-negative coefficient.
#' @keywords internal
.panel_offense_sign_constraints <- function() {
  list(lower = c(
    goals_p90 = 0, xg_per90 = 0, npg_minus_npxg_per90 = 0,
    big_chance_scored_p90 = 0, assists_p90 = 0, xa_per90_xmetrics = 0,
    key_passes_p90 = 0, penalty_won_p90 = 0
  ))
}

#' Defense sign constraints -- verbatim copy of 05_spm.R's
#' `defense_good_features`/`defense_bad_features` (Section 10). Duplicated
#' here (not sourced from 05_spm.R, which is a pipeline script, not a
#' package function) so this file has no dependency on script execution
#' order -- mirrors the existing `.skill_spm_defense_constraints()`
#' precedent (R/spm_opta.R) of a dedicated copy for a different column set.
#' @keywords internal
.panel_defense_sign_constraints <- function() {
  good <- c(
    "tackles_p90", "tackles_won_p90",
    "interceptions_p90", "interceptions_won_p90",
    "clearances_p90", "clearances_effective_p90",
    "blocks_p90", "blocked_passes_p90",
    "last_man_tackle_p90", "six_yard_block_p90", "clearance_off_line_p90",
    "aerial_won_p90",
    "ball_recovery_p90", "poss_won_def3rd_p90", "poss_won_mid3rd_p90",
    "tackle_poss_woe_per90", "containment_woe_per90",
    "aerial_woe_per90", "aerial_poss_woe_per90", "gsaa_per90",
    "fifty_fifty_won_p90", "fifty_fifty_success",
    "back_zone_pass_accuracy"
  )
  bad <- c(
    "fouls_p90", "penalty_conceded_p90",
    "error_lead_to_shot_p90", "error_lead_to_goal_p90", "errors_total_p90",
    "unsuccessful_touch_p90", "aerial_lost_p90",
    "pen_goals_conceded_p90",
    "poss_lost_ctrl_p90", "poss_lost_ctrl_per_touch"
  )
  list(lower = stats::setNames(rep(0, length(bad)), bad),
       upper = stats::setNames(rep(0, length(good)), good))
}


# ============================================================================
# Design matrix, grouped CV folds, fitting
# ============================================================================

#' Build the panel design matrix: global feature columns + optional
#' role-group x feature deviation columns
#'
#' Deviation column naming: `dev__<role_group>__<feature>` (role_group codes
#' from `classify_role_group()` are alphanumeric-only, so this is a safe,
#' unambiguous split point -- `.panel_base_feature_of()` reverses it).
#'
#' @param panel data.table/data.frame with `global_cols` and (if
#'   `deviation_cols` non-empty) a `role_group` column.
#' @param global_cols Character vector of predictor column names.
#' @param deviation_cols Character vector (subset of `global_cols`) to
#'   generate role-group deviation columns for. `character(0)` = no pooling.
#' @param role_groups Character vector of role-group codes to build
#'   deviations for (typically `.spm_panel_outfield_role_groups()`).
#' @return List: `X` (numeric matrix, columns = `global_cols` then all
#'   `dev__*` columns in group-major order), `dev_names` (character vector,
#'   possibly empty).
#' @keywords internal
.build_panel_design_matrix <- function(panel, global_cols, deviation_cols = character(0),
                                       role_groups = character(0)) {
  # data.table NSE bare-symbol subsetting (dt[, global_cols]) fails with "j
  # is a single symbol but column name not found" -- with = FALSE (not the
  # ..global_cols prefix, which needs global_cols to exist as a variable in
  # the calling frame, not just this one) selects by the character vector
  # regardless of whether panel arrived as a data.table or plain data.frame
  # (data.frame's `[.data.frame` also accepts a character j vector directly,
  # and silently ignores an unmatched `with` isn't a risk since we never
  # pass it there -- branch explicitly instead).
  X_global <- if (data.table::is.data.table(panel)) {
    as.matrix(panel[, global_cols, with = FALSE])
  } else {
    as.matrix(panel[, global_cols, drop = FALSE])
  }
  storage.mode(X_global) <- "double"
  colnames(X_global) <- global_cols

  if (length(deviation_cols) == 0 || length(role_groups) == 0) {
    return(list(X = X_global, dev_names = character(0)))
  }

  role_group_vec <- panel[["role_group"]]
  dev_blocks <- vector("list", length(role_groups))
  for (i in seq_along(role_groups)) {
    r <- role_groups[i]
    ind <- as.numeric(!is.na(role_group_vec) & role_group_vec == r)
    blk <- X_global[, deviation_cols, drop = FALSE] * ind
    colnames(blk) <- paste0("dev__", r, "__", deviation_cols)
    dev_blocks[[i]] <- blk
  }
  X_dev <- do.call(cbind, dev_blocks)
  list(X = cbind(X_global, X_dev), dev_names = colnames(X_dev))
}

#' Recover the base feature name from a `dev__<role_group>__<feature>`
#' design-matrix column name (role_group codes are alphanumeric-only, see
#' `classify_role_group()`); non-deviation names pass through unchanged.
#' @keywords internal
.panel_base_feature_of <- function(col_names) {
  ifelse(grepl("^dev__[A-Za-z]+__", col_names),
        sub("^dev__[A-Za-z]+__", "", col_names), col_names)
}

#' Assign player-grouped CV fold ids: every row for a given `player_id`
#' lands in the same fold
#'
#' BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/R5: overlapping vintage windows
#' make rows for the same player heavily dependent (a player's 2023 and 2024
#' panel rows share 4 of 5 window seasons); random row-level CV folds leak
#' across folds and inflate CV-selected lambda's apparent fit. Folds are
#' assigned by shuffling UNIQUE player ids into `nfolds` buckets
#' (`rep_len()` + `sample()`, balanced), then broadcasting per row.
#'
#' @param player_id Character vector, one entry per panel row. Coerced via
#'   `as.character()`; aborts on any `NA` (an `NA` player_id can't be
#'   consistently grouped -- every occurrence would collide into the same
#'   "fold", silently defeating the R5 guarantee this function exists for).
#' @param nfolds Number of folds.
#' @param seed Optional RNG seed.
#' @return Integer vector (1..nfolds), same length as `player_id`.
#' @family spm panel
#' @export
make_grouped_player_foldid <- function(player_id, nfolds = 5, seed = NULL) {
  player_id <- as.character(player_id)
  if (anyNA(player_id)) {
    cli::cli_abort(c(
      "make_grouped_player_foldid: {sum(is.na(player_id))} NA {.field player_id} value(s).",
      "i" = "Grouped CV folds require a complete player_id column -- an NA player_id can't be assigned a stable fold."
    ))
  }
  uid <- unique(player_id)
  if (!is.null(seed)) set.seed(seed)
  fold_map <- stats::setNames(sample(rep_len(seq_len(nfolds), length(uid))), uid)
  unname(fold_map[player_id])
}

#' Assert that no player straddles more than one CV fold
#'
#' The R5 checklist item made executable: fails loudly if any `player_id`
#' appears with more than one distinct `foldid` value. Intended to be called
#' on every `foldid` before it reaches `glmnet::cv.glmnet()` -- `fit_spm_panel()`
#' calls this on its own grouped assignment as a self-check, and it is
#' exported so any other candidate/eval script constructing folds by hand
#' can reuse it as a tripwire.
#'
#' @param foldid Integer vector of fold assignments.
#' @param player_id Character vector, same length as `foldid`.
#' @return Invisibly `TRUE` if grouped; aborts otherwise.
#' @family spm panel
#' @export
assert_grouped_player_folds <- function(foldid, player_id) {
  if (length(foldid) != length(player_id)) {
    cli::cli_abort("assert_grouped_player_folds: {.arg foldid} and {.arg player_id} must be the same length.")
  }
  dt <- data.table::data.table(player_id = player_id, foldid = foldid)
  n_folds_per_player <- dt[, data.table::uniqueN(foldid), by = player_id]
  offenders <- n_folds_per_player[V1 > 1]
  if (nrow(offenders) > 0) {
    cli::cli_abort(c(
      "assert_grouped_player_folds: {nrow(offenders)} player(s) straddle more than one CV fold.",
      "x" = "e.g. {.val {offenders$player_id[1]}} appears in {offenders$V1[1]} folds.",
      "i" = "Overlapping-window panel rows are dependent per player (sec 2.2/R5) -- use make_grouped_player_foldid(), never random row-level folds."
    ))
  }
  invisible(TRUE)
}


#' Assert that an as-of training panel for eval vintage `Y` contains no rows
#' from a LATER vintage
#'
#' The structural leak guard for the held-out next-window eval gate (sec
#' 5.2): a candidate being scored on vintage `Y`'s rows against the `Y+1`
#' target must be fit ONLY on `vintage_year <= Y` rows. Fitting on the WHOLE
#' panel (all vintages pooled) and then scoring vintage `Y` against `Y+1`'s
#' target is the exact hindsight leak `eval_nextseason.R`'s header documents
#' as banned for its own pooled-vs-per-season candidate distinction: the
#' pooled fit would have TRAINED on vintage `Y+1`'s own panel row (whose
#' label literally IS the `Y+1` target being scored against), and vintage
#' `Y`'s window overlaps vintage `Y+1`'s window in 4 of 5 seasons, so the
#' pooled model's coefficients partially encode information that shouldn't
#' be visible yet. `data-raw/spm-redesign/05c_candidates.R`'s
#' `run_candidate_asof()` calls this on its own `vintage_year <= Y` subset
#' before fitting -- this is the assertion made executable, not just a
#' comment.
#'
#' @param train_panel A panel (or panel subset) about to be passed to
#'   `fit_spm_panel()`/`run_candidate()` for an as-of fit targeting eval
#'   vintage `Y`.
#' @param Y The eval vintage the fit is being restricted to (features/target
#'   for `Y` must come from `vintage_year <= Y` only).
#' @return Invisibly `TRUE` if no row exceeds `Y`; aborts otherwise.
#' @family spm panel
#' @export
assert_asof_panel_window <- function(train_panel, Y) {
  if (!"vintage_year" %in% names(train_panel)) {
    cli::cli_abort("assert_asof_panel_window: {.arg train_panel} has no {.field vintage_year} column.")
  }
  offenders <- train_panel$vintage_year[train_panel$vintage_year > Y]
  if (length(offenders) > 0) {
    cli::cli_abort(c(
      "assert_asof_panel_window: {length(offenders)} training row(s) are from a vintage LATER than {Y}.",
      "x" = "Max offending vintage_year = {max(offenders)}.",
      "i" = "A candidate scored on vintage {Y} against the Y+1 target must be fit ONLY on vintage_year <= {Y} rows -- fitting on the whole (pooled) panel leaks the Y+1 target into training (sec 5.2's leak-free discipline)."
    ))
  }
  invisible(TRUE)
}


#' Fit a role-pooled elastic-net SPM model on the panel, predicting the
#' windowed prior-free RAPM target
#'
#' The Wave-2 estimator (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2/2.3/3.1):
#' plain `glmnet::cv.glmnet()` with (a) player-grouped CV folds (never
#' random rows -- R5), (b) an optional role-group x feature interaction
#' block for the restricted role-ambivalent feature set, shrunk toward 0 via
#' a higher `penalty.factor` (partial pooling -- no new machinery beyond
#' glmnet), (c) minutes weighting (`sqrt`/`linear`), (d) an optional
#' errors-in-variables target rescale, (e) sign constraints extending
#' 05_spm.R's defense lists with sec 3.1's new offense set.
#'
#' Circularity guard: re-checks the panel's `target_provenance` attribute
#' via `assert_prior_free_target()` (the SAME function `build_spm_panel()`
#' calls on the raw target) before fitting -- a training entry point that
#' trusts an un-stamped panel is exactly the gap sec 2.4.1 closes.
#'
#' @param panel Output of `build_spm_panel()` (or any data.table/data.frame
#'   carrying the same `target_provenance` attribute + columns).
#' @param target One of `"offense"`, `"defense"`, `"net"` -- which target
#'   column (`offense_target`/`defense_target`/`rapm_target`) to fit.
#' @param role_pooling Add role-group deviation columns (default `TRUE`).
#'   `FALSE` = global-only (candidate S1).
#' @param role_ambivalent_cols Feature columns to build deviations for
#'   (default `.spm_panel_role_ambivalent_cols()`).
#' @param deviation_penalty_mult Multiplier applied to deviation columns'
#'   `penalty.factor` relative to global columns (default 5 -- higher
#'   shrinkage, sec 3.1's partial-pooling design).
#' @param alpha Elastic-net mixing (default 0.5).
#' @param weight_transform `"sqrt"` (default) or `"linear"` (sec 2.3.1 --
#'   the plan restricts this study to these two; `fit_spm_model()`'s
#'   `"log"`/`"none"` are out of scope here).
#' @param min_window_minutes Drop panel rows below this window-minutes floor
#'   before fitting (default 0 = no floor; sec 2.3.1's blunt alternative to
#'   weighting).
#' @param eiv_rescale Apply the errors-in-variables target rescale `y /
#'   max(r_hat, eiv_floor)`, `r_hat = window_minutes / (window_minutes +
#'   eiv_m0)` (default `FALSE`; sec 2.3.2).
#' @param eiv_m0 Implied prior minutes at 0 the ridge penalty is equivalent
#'   to (default 8000 -- the Wave-1 attenuation study's empirical estimate,
#'   `data-raw/spm-redesign/03_attenuation_diagnostics.R` /
#'   `attenuation_band_summary.csv`; only used when `eiv_rescale = TRUE`).
#' @param eiv_floor Minimum `r_hat` before rescaling (default 0.4, per sec
#'   2.3.2).
#' @param sign_constraints Apply sec 3.1's sign constraints for the chosen
#'   `target` (default `TRUE`; no-op for `target = "net"`, which has no
#'   hand-curated list in 05_spm.R either).
#' @param predictor_cols Global predictor columns (default `NULL` = the
#'   canonical `.spm_opta_predictor_cols(panel)` selector, guaranteeing
#'   feature-set parity with the existing career-level SPM).
#' @param nfolds CV folds (default 5).
#' @param seed RNG seed for the grouped fold assignment (default `NULL`).
#'
#' @return A `cv.glmnet` object with `panna_metadata` (type `"spm_panel"`,
#'   `target`, `predictor_cols` (global), `dev_names`, `role_groups`,
#'   config echo, `feature_sds`, `n_observations`, `lambda_min`) --
#'   `predict_spm_panel()` scores new panel rows against it.
#' @family spm panel
#' @export
fit_spm_panel <- function(panel, target = c("offense", "defense", "net"),
                          role_pooling = TRUE,
                          role_ambivalent_cols = .spm_panel_role_ambivalent_cols(),
                          deviation_penalty_mult = 5, alpha = 0.5,
                          weight_transform = c("sqrt", "linear"),
                          min_window_minutes = 0,
                          eiv_rescale = FALSE, eiv_m0 = 8000, eiv_floor = 0.4,
                          sign_constraints = TRUE, predictor_cols = NULL,
                          nfolds = 5, seed = NULL) {
  target <- match.arg(target)
  weight_transform <- match.arg(weight_transform)

  assert_prior_free_target(list(target_provenance = attr(panel, "target_provenance")))

  panel <- data.table::as.data.table(panel)
  # role_group == NA rows (classify_role() "UNK") are kept: global-only
  # pricing, no deviation contribution. GK rows are dropped defensively even
  # though build_spm_panel() already excludes them by default.
  panel <- panel[is.na(role_group) | role_group != "GK"]

  if (min_window_minutes > 0) {
    panel <- panel[window_minutes >= min_window_minutes]
  }
  if (nrow(panel) < 20) {
    cli::cli_abort("fit_spm_panel: only {nrow(panel)} rows after filtering -- too few to fit.")
  }

  target_col <- switch(target, offense = "offense_target", defense = "defense_target", net = "rapm_target")
  if (!target_col %in% names(panel)) {
    cli::cli_abort("fit_spm_panel: panel has no {.field {target_col}} column.")
  }

  if (is.null(predictor_cols)) predictor_cols <- .spm_opta_predictor_cols(panel)
  predictor_cols <- intersect(predictor_cols, names(panel))
  if (length(predictor_cols) == 0) {
    cli::cli_abort("fit_spm_panel: no predictor columns resolved from the panel.")
  }

  dev_cols <- if (isTRUE(role_pooling)) intersect(role_ambivalent_cols, predictor_cols) else character(0)
  role_groups <- if (length(dev_cols) > 0) .spm_panel_outfield_role_groups() else character(0)

  design <- .build_panel_design_matrix(panel, predictor_cols, dev_cols, role_groups)
  X <- design$X
  y <- panel[[target_col]]

  mins <- panel$window_minutes
  weights <- switch(weight_transform, sqrt = sqrt(mins), linear = mins)
  weights <- weights / mean(weights, na.rm = TRUE)

  if (isTRUE(eiv_rescale)) {
    r_hat <- mins / (mins + eiv_m0)
    r_hat_capped <- pmax(r_hat, eiv_floor)
    y <- y / r_hat_capped
  }

  complete_idx <- stats::complete.cases(X, y)
  X <- X[complete_idx, , drop = FALSE]
  y <- y[complete_idx]
  weights <- weights[complete_idx]
  # as.character() here too (not just inside make_grouped_player_foldid()):
  # a factor/numeric player_id would otherwise silently pass through to the
  # foldid call and only get coerced (and NA-checked) one level down.
  player_id <- as.character(panel$player_id[complete_idx])

  foldid <- make_grouped_player_foldid(player_id, nfolds = nfolds, seed = seed)
  assert_grouped_player_folds(foldid, player_id)

  col_names <- colnames(X)
  penalty_vec <- rep(1, ncol(X))
  penalty_vec[col_names %in% design$dev_names] <- deviation_penalty_mult

  lower_vec <- rep(-Inf, ncol(X)); names(lower_vec) <- col_names
  upper_vec <- rep(Inf, ncol(X)); names(upper_vec) <- col_names
  if (isTRUE(sign_constraints) && target %in% c("offense", "defense")) {
    sc <- if (target == "offense") .panel_offense_sign_constraints() else .panel_defense_sign_constraints()
    base_of <- .panel_base_feature_of(col_names)
    if (!is.null(sc$lower)) {
      m <- match(base_of, names(sc$lower))
      lower_vec[!is.na(m)] <- sc$lower[m[!is.na(m)]]
    }
    if (!is.null(sc$upper)) {
      m <- match(base_of, names(sc$upper))
      upper_vec[!is.na(m)] <- sc$upper[m[!is.na(m)]]
    }
  }

  progress_msg(sprintf("fit_spm_panel: target=%s, %d predictors (%d global + %d deviation), %d rows",
                       target, ncol(X), length(predictor_cols), length(design$dev_names), nrow(X)))

  cv_fit <- glmnet::cv.glmnet(
    x = X, y = y, weights = weights, alpha = alpha, standardize = TRUE,
    foldid = foldid, type.measure = "mse",
    lower.limits = lower_vec, upper.limits = upper_vec,
    penalty.factor = penalty_vec
  )

  feature_sds <- apply(X, 2, stats::sd, na.rm = TRUE)
  feature_sds[feature_sds == 0 | is.na(feature_sds)] <- 1

  cv_fit$panna_metadata <- list(
    type = "spm_panel", target = target, target_col = target_col,
    predictor_cols = predictor_cols, dev_names = design$dev_names,
    role_groups = role_groups, role_ambivalent_cols = role_ambivalent_cols,
    deviation_penalty_mult = deviation_penalty_mult, alpha = alpha,
    weight_transform = weight_transform, min_window_minutes = min_window_minutes,
    eiv_rescale = eiv_rescale, eiv_m0 = eiv_m0, eiv_floor = eiv_floor,
    sign_constraints = sign_constraints, feature_sds = feature_sds,
    n_observations = nrow(X), lambda_min = cv_fit$lambda.min, lambda_1se = cv_fit$lambda.1se
  )

  progress_msg(sprintf("fit_spm_panel complete. lambda.min=%.5f, CV RMSE=%.4f",
                       cv_fit$lambda.min, sqrt(cv_fit$cvm[cv_fit$lambda == cv_fit$lambda.min])))

  cv_fit
}


#' Score panel rows against a `fit_spm_panel()` model
#'
#' Rebuilds the design matrix using the SAME global/deviation column spec
#' the model was fit with (`model$panna_metadata`), so prediction-time
#' columns are guaranteed identical in name/order to fit-time -- required
#' whenever `newdata` is a different panel slice (e.g. the eval harness
#' scoring vintage `Y`'s rows against a model trained through `Y`).
#'
#' @param model A `fit_spm_panel()` result.
#' @param newdata Panel-shaped data.table/data.frame (needs `model`'s
#'   `predictor_cols` and, if the model used role pooling, `role_group`).
#' @param lambda `"min"` (default) or `"1se"`.
#' @return data.table(player_id, vintage_year (if present), pred).
#' @family spm panel
#' @export
predict_spm_panel <- function(model, newdata, lambda = c("min", "1se")) {
  lambda <- match.arg(lambda)
  meta <- model$panna_metadata
  if (is.null(meta) || !identical(meta$type, "spm_panel")) {
    cli::cli_abort("predict_spm_panel: {.arg model} is not a fit_spm_panel() result.")
  }
  newdata <- data.table::as.data.table(newdata)
  missing_cols <- setdiff(meta$predictor_cols, names(newdata))
  for (col in missing_cols) newdata[[col]] <- 0

  # Deviation columns must be rebuilt with the SAME base-feature spec + role
  # groups the model was fit with, not re-derived from dev_names alone.
  if (length(meta$dev_names) > 0) {
    if (!"role_group" %in% names(newdata)) {
      cli::cli_abort(c(
        "predict_spm_panel: {.arg model} was fit with role-group partial pooling ({length(meta$dev_names)} deviation columns), but {.arg newdata} has no {.field role_group} column.",
        "i" = "Without role_group, .build_panel_design_matrix() can't rebuild the deviation columns the model expects -- score panel-shaped data (build_spm_panel() output), or refit with role_pooling = FALSE if role_group genuinely isn't available."
      ))
    }
    dev_base_cols <- intersect(meta$role_ambivalent_cols, meta$predictor_cols)
    design <- .build_panel_design_matrix(newdata, meta$predictor_cols, dev_base_cols, meta$role_groups)
  } else {
    design <- .build_panel_design_matrix(newdata, meta$predictor_cols, character(0), character(0))
  }

  s_val <- if (lambda == "min") model$lambda.min else model$lambda.1se
  pred <- as.numeric(stats::predict(model, newx = design$X, s = s_val))

  out <- data.table::data.table(player_id = newdata$player_id, pred = pred)
  if ("vintage_year" %in% names(newdata)) out[, vintage_year := newdata$vintage_year]
  out
}


#' Score panel rows against a fitted offense/defense pair, combining to a
#' net prediction
#'
#' `fit_spm_panel()` is fit separately per target (offense/defense have
#' different sign constraints and, for a real RAPM O/D split, different
#' underlying signal). The targets are stored in the RAW internal
#' convention (`defense_target` = contribution to opponent xG, positive =
#' concedes more = bad), and net RAPM = offense - defense
#' (`extract_rapm_ratings()`, R/rapm_model.R "RAPM rating = offense -
#' defense") -- so the net prediction is `pred_offense - pred_defense`.
#' (An earlier version summed the two, which flipped the defense half's
#' contribution at eval time and tanked every candidate's next-window
#' correlation -- caught in the 2026-07-22 full-panel bake-off.)
#'
#' @param fits List with `offense` and `defense` elements, each a
#'   `fit_spm_panel()` result (as produced by the candidate configs in
#'   `data-raw/spm-redesign/05c_candidates.R`).
#' @param newdata Panel-shaped data.table/data.frame.
#' @param lambda `"min"` (default) or `"1se"`.
#' @return data.table(player_id, vintage_year (if present), pred_offense,
#'   pred_defense, pred_net).
#' @family spm panel
#' @export
predict_spm_panel_net <- function(fits, newdata, lambda = c("min", "1se")) {
  lambda <- match.arg(lambda)
  if (!all(c("offense", "defense") %in% names(fits))) {
    cli::cli_abort("predict_spm_panel_net: {.arg fits} must have {.field offense} and {.field defense} elements.")
  }
  off <- predict_spm_panel(fits$offense, newdata, lambda = lambda)
  def <- predict_spm_panel(fits$defense, newdata, lambda = lambda)
  data.table::setnames(off, "pred", "pred_offense")
  data.table::setnames(def, "pred", "pred_defense")
  join_cols <- intersect(c("player_id", "vintage_year"), names(off))
  out <- merge(off, def[, c(join_cols, "pred_defense"), with = FALSE], by = join_cols)
  out[, pred_net := pred_offense - pred_defense]
  out
}


#' Fit the XGBoost half of the S6 panel SPM (player-grouped CV)
#'
#' The panel repeats players across vintages, so `fit_spm_xgb()`'s random
#' `xgb.cv` folds would leak players between train/test and overfit the
#' round count (the plan's R5 hazard). This fit builds its folds with
#' `make_grouped_player_foldid()` (every player in exactly one fold,
#' asserted) and mirrors `fit_spm_panel()`'s weighting/complete-case
#' handling. Promoted to the package after three script copies
#' (09c/13c/05_spm integration, 2026-07-22 Wave 4).
#'
#' @param panel Output of `build_spm_panel()` (or a `vintage_year`-subset).
#' @param target `"offense"` or `"defense"` (fits `<target>_target`).
#' @param predictor_cols Feature columns (default: canonical
#'   `.spm_opta_predictor_cols(panel)`).
#' @param weight_transform `"linear"` (S4a/S6 parity, default) or `"sqrt"`.
#' @param nfolds,seed Grouped-CV config.
#' @param max_depth,eta,subsample,colsample_bytree,nrounds,early_stopping_rounds
#'   XGBoost knobs (defaults = the Wave-2-validated panel config, identical
#'   to the 09c/13c bake-off scripts; NB production 05_spm.R's legacy
#'   `fit_spm_xgb()` calls use eta=0.02/nrounds=1000 — different model,
#'   different tuning).
#' @return An `xgb.Booster` with `panna_metadata` (type "spm_panel_xgb",
#'   target, predictor_cols, best_nrounds, cv_rmse).
#' @family spm panel
#' @export
fit_spm_panel_xgb <- function(panel, target = c("offense", "defense"),
                              predictor_cols = NULL,
                              weight_transform = c("linear", "sqrt"),
                              nfolds = 5, seed = 1,
                              max_depth = 4, eta = 0.1, subsample = 0.8,
                              colsample_bytree = 0.8, nrounds = 500,
                              early_stopping_rounds = 20) {
  target <- match.arg(target)
  weight_transform <- match.arg(weight_transform)
  if (!requireNamespace("xgboost", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg xgboost} is required.")
  }
  assert_prior_free_target(panel)
  if (is.null(predictor_cols)) predictor_cols <- .spm_opta_predictor_cols(panel)

  X <- as.matrix(as.data.frame(panel)[, predictor_cols, drop = FALSE])
  y <- panel[[paste0(target, "_target")]]
  mins <- panel$window_minutes
  w <- switch(weight_transform, linear = mins, sqrt = sqrt(mins))
  w <- w / mean(w, na.rm = TRUE)

  ok <- stats::complete.cases(X, y)
  X <- X[ok, , drop = FALSE]; y <- y[ok]; w <- w[ok]
  pid <- as.character(panel$player_id[ok])
  foldid <- make_grouped_player_foldid(pid, nfolds = nfolds, seed = seed)
  assert_grouped_player_folds(foldid, pid)

  params <- list(objective = "reg:squarederror", max_depth = max_depth,
                 eta = eta, subsample = subsample,
                 colsample_bytree = colsample_bytree, eval_metric = "rmse")
  dtrain <- xgboost::xgb.DMatrix(data = X, label = y, weight = w)
  cv <- xgboost::xgb.cv(params = params, data = dtrain, nrounds = nrounds,
                        folds = split(seq_along(foldid), foldid),
                        early_stopping_rounds = early_stopping_rounds,
                        verbose = 0)
  best_n <- cv$best_iteration
  if (is.null(best_n) || length(best_n) == 0) {
    best_n <- which.min(cv$evaluation_log$test_rmse_mean)
  }
  cv_rmse <- cv$evaluation_log$test_rmse_mean[best_n]
  progress_msg(sprintf("fit_spm_panel_xgb (%s): best_nrounds=%d, CV RMSE=%.4f",
                       target, best_n, cv_rmse))

  model <- xgboost::xgb.train(params = params, data = dtrain, nrounds = best_n)
  attr(model, "panna_metadata") <- list(
    type = "spm_panel_xgb", target = target, predictor_cols = predictor_cols,
    weight_transform = weight_transform, best_nrounds = best_n,
    cv_rmse = cv_rmse, n_observations = nrow(X)
  )
  model
}


#' Score panel-shaped rows with a `fit_spm_panel_xgb()` model
#'
#' @param model A `fit_spm_panel_xgb()` result.
#' @param newdata Panel-shaped data with the model's `predictor_cols`.
#' @return data.table(player_id, vintage_year (if present), pred).
#' @family spm panel
#' @export
predict_spm_panel_xgb <- function(model, newdata) {
  meta <- attr(model, "panna_metadata")
  if (is.null(meta) || !identical(meta$type, "spm_panel_xgb")) {
    cli::cli_abort("predict_spm_panel_xgb: {.arg model} is not a fit_spm_panel_xgb() result.")
  }
  # 0-fill training-time columns absent from newdata — parity with
  # predict_spm_panel(), so cross-vintage scoring against a schema-drifted
  # source degrades the same way in both halves of the blend.
  newdata <- data.table::as.data.table(newdata)
  missing_cols <- setdiff(meta$predictor_cols, names(newdata))
  for (col in missing_cols) newdata[[col]] <- 0
  X <- as.matrix(as.data.frame(newdata)[, meta$predictor_cols, drop = FALSE])
  out <- data.table::data.table(
    player_id = newdata$player_id,
    pred = as.numeric(stats::predict(model, xgboost::xgb.DMatrix(X)))
  )
  if ("vintage_year" %in% names(newdata)) out[, vintage_year := newdata$vintage_year]
  out
}
