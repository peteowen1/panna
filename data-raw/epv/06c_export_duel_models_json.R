#!/usr/bin/env Rscript
# 06c_export_duel_models_json.R
# Export the 5 xDuel (duel-WOE) XGBoost boosters (R/duel_model.R) to a single
# JSON bundle for the inthegame-blog Cloudflare Worker's live scoring of the
# 5 duel-WOE PSR features: aerial_woe, aerial_poss_woe, takeon_woe,
# tackle_poss_woe, containment_woe (LIVE-PSV-UNBLOCK-2026-07-20.md, task A3).
#
# Model shape: FIVE independently-calibrated binary:logistic XGBoost boosters
# (aerial_win, aerial_poss, takeon, tackle_poss, containment), each scoring
# P(win) for its contest from 4 team-relative context features
# (start_x, start_y, dist_own_goal, dist_opp_goal) — see `.DUEL_FEATURE_COLS`
# in R/duel_model.R. Same 4 features, same order, for all 5 boosters in the
# current artifact, but each booster's `feature_cols` is exported independently
# (do not assume they stay identical on a future retrain).
#
# How a per-contest P(win) becomes a *_woe_per90 xmetric (the worker MUST
# reproduce this, verified against real shipped data below in section 5):
#   1. For every contest event of a given type in a match, score P(win) with
#      the matching booster on that event's 4 features.
#   2. Per player (+ team, + match for the by-match artifact), accumulate
#      won = sum(actual outcome in {0,1}), exp = sum(P(win)).
#   3. woe = won - exp  (the "wins above expected" count for that contest).
#   4. woe_per90 = round(woe / minutes * 90, 3)  (0 if minutes <= 0 or NA).
#   This exact recipe (R/duel_model.R::compute_duel_woe + the per90 step in
#   data-raw/epv/03_calculate_player_xmetrics.R) reproduces the shipped
#   xmetrics_bymatch *_woe_per90 columns to 3 decimals (see section 5).
#
# Serialization (per LIVE-PSV-UNBLOCK-2026-07-20.md design decision D2):
#   xgb.save.raw(booster, raw_format = "json") -> rawToChar, embedded VERBATIM
#   (byte-for-byte, via string concatenation — never reparsed/re-serialized)
#   in the bundle. NO xgb.dump (corrupts split thresholds). The outer envelope
#   scalars (base_score, feature order, etc.) are written with digits = 17 so
#   no double-precision value is truncated.
#
#   NB: jsonlite's `class(x) <- "json"` "insert raw JSON" trick does NOT embed
#   unescaped when nested inside a list passed to write_json/toJSON (verified
#   empirically 2026-07-20 — it double-encodes as an escaped string instead).
#   So the bundle is assembled by hand via string concatenation, not jsonlite
#   list serialization, for the parts that must stay byte-identical.
#
# Outputs:
#   - <out_dir>/duel_woe_bundle.json      (the 5-booster bundle; large, R2-bound)
#   - data-raw/epv/duel_woe_fixtures.json (small, committable: ~10 real
#     contests/booster for the worker's float32 parity harness + the
#     xmetrics_bymatch cross-check report)
#
# Run from panna directory:
#   Rscript data-raw/epv/06c_export_duel_models_json.R
#
# Overrides (set before sourcing): duel_model_path, duel_json_out_dir,
# duel_fixture_league, duel_fixture_season, duel_fixture_n, duel_xcheck_n.

suppressPackageStartupMessages({
  library(cli)
  library(jsonlite)
  library(xgboost)
  library(data.table)
})
devtools::load_all(".", quiet = TRUE)

cli_h1("Export xDuel (5-booster WOE) models to JSON")

# ------------------------------------------------------------------------------
# 0. Config overrides
# ------------------------------------------------------------------------------
if (!exists("duel_model_path", inherits = FALSE)) duel_model_path <- NULL
if (!exists("duel_json_out_dir", inherits = FALSE)) {
  duel_json_out_dir <- file.path(opta_data_dir(), "models")
}
if (!exists("duel_fixture_league", inherits = FALSE)) duel_fixture_league <- "EPL"
if (!exists("duel_fixture_season", inherits = FALSE)) duel_fixture_season <- "2024-2025"
if (!exists("duel_fixture_n", inherits = FALSE)) duel_fixture_n <- 10L
if (!exists("duel_xcheck_n", inherits = FALSE)) duel_xcheck_n <- 3L

dir.create(duel_json_out_dir, recursive = TRUE, showWarnings = FALSE)
bundle_path <- file.path(duel_json_out_dir, "duel_woe_bundle.json")
fixtures_path <- file.path("data-raw", "epv", "duel_woe_fixtures.json")

# ------------------------------------------------------------------------------
# 1. Load the xDuel model (5 sub-models + panna_metadata)
# ------------------------------------------------------------------------------

dm <- load_duel_model(path = duel_model_path)
contests <- setdiff(names(dm), "panna_metadata")
cli_alert_info("Loaded xDuel model: {length(contests)} contests ({paste(contests, collapse = ', ')})")

# Contest -> output-column prefix (mirrors .DUEL_CONTESTS in R/duel_model.R;
# re-declared here rather than exported since it's a static, tiny lookup and
# this script must not touch R/duel_model.R).
woe_prefix_map <- c(aerial_win = "aerial", aerial_poss = "aerial_poss",
                     takeon = "takeon", tackle_poss = "tackle_poss",
                     containment = "containment")

# ------------------------------------------------------------------------------
# 2. Build each booster's envelope (metadata via jsonlite, learner JSON VERBATIM)
# ------------------------------------------------------------------------------

booster_entries <- character(length(contests))
names(booster_entries) <- contests
learner_raw <- vector("list", length(contests)); names(learner_raw) <- contests
booster_meta <- vector("list", length(contests)); names(booster_meta) <- contests

for (cst in contests) {
  sub <- dm[[cst]]
  stopifnot(!is.null(sub$model), !is.null(sub$feature_cols))

  raw <- xgboost::xgb.save.raw(sub$model, raw_format = "json")
  chr <- rawToChar(raw)
  learner_raw[[cst]] <- chr

  cfg <- xgboost::xgb.config(sub$model)
  if (is.character(cfg)) cfg <- jsonlite::fromJSON(cfg, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  objective <- cfg$learner$objective$name %||% cfg$objective$name %||% "binary:logistic"
  bs_raw <- cfg$learner$learner_model_param$base_score
  base_score <- as.numeric(gsub("[][]", "", bs_raw, perl = TRUE))

  # Ground-truth tree count from the exported learner JSON itself (not the CV
  # best_iteration bookkeeping field, which should agree but isn't the source
  # of truth for what's actually embedded).
  learner_parsed <- jsonlite::fromJSON(chr, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  num_trees <- as.integer(learner_parsed$learner$gradient_booster$model$gbtree_model_param$num_trees)
  if (!identical(num_trees, as.integer(sub$best_nrounds))) {
    cli_alert_warning("{cst}: num_trees in learner JSON ({num_trees}) != stored best_nrounds ({sub$best_nrounds})")
  }

  cli_alert_info(
    "{cst}: objective={objective}, base_score={signif(base_score, 6)}, nrounds={num_trees}, feats=[{paste(sub$feature_cols, collapse = ', ')}]"
  )

  meta <- list(
    name = cst,
    woe_prefix = unname(woe_prefix_map[[cst]]),
    objective = objective,
    base_score = base_score,
    feature_names = sub$feature_cols,
    nrounds = num_trees,
    win_rate = unname(sub$win_rate),
    n_contests_trained = unname(sub$n_contests)
  )
  booster_meta[[cst]] <- meta
  meta_json <- as.character(jsonlite::toJSON(meta, auto_unbox = TRUE, digits = 17))
  # meta_json is a compact JSON object "{...}"; splice the verbatim learner
  # JSON in as the last key by replacing the trailing '}' with ',"learner":<chr>}'.
  stopifnot(substr(meta_json, nchar(meta_json), nchar(meta_json)) == "}")
  meta_prefix <- substr(meta_json, 1, nchar(meta_json) - 1)
  booster_entries[[cst]] <- paste0(meta_prefix, ',"learner":', chr, "}")
}

# ------------------------------------------------------------------------------
# 3. Assemble the bundle (hand-spliced string — see header note on why)
# ------------------------------------------------------------------------------

exported_at <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
envelope_head <- as.character(jsonlite::toJSON(
  list(model_type = "duel_woe_bundle", contests = unname(contests),
       woe_prefixes = unname(woe_prefix_map[contests]), exported_at = exported_at),
  auto_unbox = TRUE, digits = 17
))
stopifnot(substr(envelope_head, nchar(envelope_head), nchar(envelope_head)) == "}")
envelope_prefix <- substr(envelope_head, 1, nchar(envelope_head) - 1)

boosters_obj <- paste0(
  "{", paste(sprintf('"%s":%s', contests, booster_entries), collapse = ","), "}"
)
bundle_text <- paste0(envelope_prefix, ',"boosters":', boosters_obj, "}")

if (!jsonlite::validate(bundle_text)) cli_abort("Assembled bundle is not valid JSON")
writeLines(bundle_text, bundle_path, useBytes = TRUE)
cli_alert_success("Wrote {bundle_path} ({round(file.info(bundle_path)$size / 1024, 1)} KB)")

# ------------------------------------------------------------------------------
# 4. Round-trip verification: re-read the FILE (not the in-memory string),
#    extract each booster's learner JSON byte-for-byte, reload via
#    xgb.load.raw, and confirm predictions match the original booster exactly
#    (max abs diff 0) on real fixture rows built in section 5 below.
# ------------------------------------------------------------------------------

file_text <- paste(readLines(bundle_path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
# writeLines(..., useBytes=TRUE) adds no extra chars beyond the trailing \n
# collapse reintroduces here; re-derive without assuming byte offsets from
# `bundle_text` — search the FILE text directly.
reloaded_boosters <- list()
roundtrip_ok <- logical(length(contests)); names(roundtrip_ok) <- contests

for (cst in contests) {
  name_marker <- sprintf('"name":"%s"', cst)
  name_pos <- regexpr(name_marker, file_text, fixed = TRUE)
  if (name_pos < 0) cli_abort("Could not locate booster '{cst}' in written file")
  tail_text <- substr(file_text, name_pos, nchar(file_text))
  learner_key_pos <- regexpr('"learner":{', tail_text, fixed = TRUE)
  if (learner_key_pos < 0) cli_abort("Could not locate learner JSON for '{cst}'")
  brace_pos_in_tail <- learner_key_pos + nchar('"learner":')
  start_pos <- name_pos + brace_pos_in_tail - 1L
  expected_len <- nchar(learner_raw[[cst]])
  extracted <- substr(file_text, start_pos, start_pos + expected_len - 1L)

  byte_identical <- identical(extracted, learner_raw[[cst]])
  if (!byte_identical) {
    cli_alert_danger("{cst}: extracted learner JSON is NOT byte-identical to xgb.save.raw output")
  }

  booster2 <- xgboost::xgb.load.raw(charToRaw(extracted))
  reloaded_boosters[[cst]] <- booster2
  roundtrip_ok[[cst]] <- byte_identical
}
cli_alert_info("Byte-identical extraction for all 5 boosters: {all(roundtrip_ok)}")

# ------------------------------------------------------------------------------
# 5. Fixtures: ~duel_fixture_n REAL contests per booster + xmetrics_bymatch
#    cross-check, from real Opta events (never synthetic).
# ------------------------------------------------------------------------------

cli_h2("Building fixtures from real {duel_fixture_league} {duel_fixture_season} events")

events <- as.data.table(load_opta_match_events(duel_fixture_league, source = "local"))
if ("season" %in% names(events)) {
  events <- events[season == duel_fixture_season]
}
cli_alert_info("{nrow(events)} events, {length(unique(events$match_id))} matches")

set.seed(20260720L)
fixtures_list <- list()
roundtrip_maxdiff <- numeric(length(contests)); names(roundtrip_maxdiff) <- contests

for (cst in contests) {
  sub <- dm[[cst]]
  feat <- prepare_duels_from_events(events, contest = cst)
  n_take <- min(duel_fixture_n, nrow(feat))
  idx <- sort(sample(seq_len(nrow(feat)), n_take))
  f <- feat[idx]

  X <- as.matrix(as.data.frame(f)[, sub$feature_cols, drop = FALSE])
  p_orig <- predict(sub$model, X)
  p_reloaded <- predict(reloaded_boosters[[cst]], X)
  maxdiff <- max(abs(p_orig - p_reloaded))
  roundtrip_maxdiff[[cst]] <- maxdiff
  cli_alert_info("{cst}: {n_take} fixture rows, round-trip max|diff| = {maxdiff}")

  rows <- lapply(seq_len(nrow(f)), function(i) {
    feats <- as.list(f[i, sub$feature_cols, with = FALSE])
    c(list(match_id = f$match_id[i], player_id = f$player_id[i],
           team_id = f$team_id[i], won = as.integer(f$won[i])),
      feats,
      list(expected_p_win = p_orig[i]))
  })
  fixtures_list[[cst]] <- rows
}

if (!all(roundtrip_maxdiff == 0)) {
  cli_abort("Round-trip prediction mismatch (expected max|diff| == 0): {paste(names(roundtrip_maxdiff), roundtrip_maxdiff, sep = '=', collapse = ', ')}")
}
cli_alert_success("Round-trip verified: all 5 boosters match to max|diff| = 0 on real fixture rows")

# ------------------------------------------------------------------------------
# 6. xmetrics_bymatch cross-check: accumulate actual - expected over real
#    player-matches, per90-normalize, compare to the SHIPPED xmetrics_bymatch
#    *_woe_per90 columns (built by 03_calculate_player_xmetrics.R in production).
# ------------------------------------------------------------------------------

cli_h2("Cross-checking against shipped xmetrics_bymatch")

lineups <- as.data.table(load_opta_lineups(duel_fixture_league, season = duel_fixture_season, source = "local"))
lu <- unique(lineups[, .(match_id, player_id, team_id, minutes = minutes_played)])

woe_bm <- compute_duel_woe(events, dm, by_match = TRUE)
setDT(woe_bm)
wb <- lu[woe_bm, on = c("match_id", "player_id", "team_id")]

for (cst in contests) {
  p <- unname(woe_prefix_map[[cst]])
  wcol <- paste0(p, "_woe"); p90col <- paste0(p, "_woe_per90")
  wb[[p90col]] <- ifelse(!is.na(wb[[wcol]]) & wb$minutes > 0,
                         round(wb[[wcol]] / wb$minutes * 90, 3), 0)
}

# Pick real player-matches with meaningful minutes and non-trivial WOE signal.
score <- rowSums(abs(as.matrix(wb[, paste0(unname(woe_prefix_map), "_woe_per90"), with = FALSE])), na.rm = TRUE)
cand <- wb[minutes >= 60][order(-score[minutes >= 60])][seq_len(min(duel_xcheck_n, .N))]

xm <- as.data.table(load_opta_xmetrics(duel_fixture_league, season = duel_fixture_season,
                                        source = "local", by_match = TRUE))
xm_cols <- c("minutes", paste0(unname(woe_prefix_map), "_woe"),
             paste0(unname(woe_prefix_map), "_woe_per90"))
xm_cols <- intersect(xm_cols, names(xm))
cmp <- xm[cand[, .(match_id, player_id)], on = c("match_id", "player_id")]

cross_check_rows <- list()
max_p90_diff <- 0
for (i in seq_len(nrow(cand))) {
  mid <- cand$match_id[i]; pid <- cand$player_id[i]
  shipped <- xm[match_id == mid & player_id == pid]
  mine <- cand[i]
  row_diffs <- list()
  for (p in unname(woe_prefix_map)) {
    p90col <- paste0(p, "_woe_per90")
    s_val <- if (nrow(shipped) == 1 && p90col %in% names(shipped)) shipped[[p90col]] else NA_real_
    m_val <- mine[[p90col]]
    d <- abs((s_val %||% NA_real_) - m_val)
    if (!is.na(d)) max_p90_diff <- max(max_p90_diff, d)
    row_diffs[[p90col]] <- list(shipped = s_val, recomputed = m_val)
  }
  cli_alert_info(
    "match={mid} player={pid} minutes={mine$minutes}: {paste(sprintf('%s(shipped=%s,mine=%s)', names(row_diffs), sapply(row_diffs, `[[`, 'shipped'), sapply(row_diffs, `[[`, 'recomputed')), collapse = ' | ')}"
  )
  cross_check_rows[[i]] <- list(match_id = mid, player_id = pid, minutes = mine$minutes,
                                 woe_per90 = row_diffs)
}
cli_alert_success("xmetrics_bymatch cross-check: max |shipped - recomputed| woe_per90 across {nrow(cand)} player-matches x 5 contests = {max_p90_diff}")

# ------------------------------------------------------------------------------
# 7. Write fixtures file (small, committable)
# ------------------------------------------------------------------------------

fixtures_envelope <- list(
  model_type = "duel_woe_bundle_fixtures",
  source_league = duel_fixture_league,
  source_season = duel_fixture_season,
  formula = list(
    p_win = "predict(booster, features[feature_names])  # binary:logistic, sigmoid already applied",
    woe = "sum(won) - sum(p_win)  # per player(+team[+match])",
    woe_per90 = "round(woe / minutes * 90, 3); 0 if minutes<=0 or NA"
  ),
  exported_at = exported_at,
  fixtures = fixtures_list,
  xmetrics_bymatch_cross_check = list(
    league = duel_fixture_league, season = duel_fixture_season,
    n_player_matches = nrow(cand),
    max_abs_diff_woe_per90 = max_p90_diff,
    rows = cross_check_rows
  )
)

jsonlite::write_json(fixtures_envelope, fixtures_path, auto_unbox = TRUE, digits = 17, pretty = FALSE)
cli_alert_success("Wrote {fixtures_path} ({round(file.info(fixtures_path)$size / 1024, 1)} KB)")

cli_h1("Complete")
cli_alert_success("Bundle:   {bundle_path}")
cli_alert_success("Fixtures: {fixtures_path}")
cli_alert_info("Booster summary: {paste(sprintf('%s(nrounds=%s,feats=%d)', contests, sapply(booster_meta, `[[`, 'nrounds'), sapply(booster_meta, function(m) length(m$feature_names))), collapse = ', ')}")
