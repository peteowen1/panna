#!/usr/bin/env Rscript
# Export the xGOT (post-shot xG) model (panna/R/xgot_model.R,
# data-raw/cache/epv/xgot_model.rds) to JSON for the Cloudflare Worker's live
# placement_added scoring (the ±0.481/event live-PSV feature). Mirrors
# 06_export_xg_model_json.R's envelope STRUCTURE only -- serialization
# follows pannaverse/docs/plans/LIVE-PSV-UNBLOCK-2026-07-20.md design
# decision D2, which supersedes 06's xgb.dump+digits=10 approach:
#   xgb.save.raw(booster, raw_format = "json") -> rawToChar -> the resulting
#   learner JSON is spliced into the envelope VERBATIM (byte-for-byte, no
#   parse+rewrite of tree internals -- xgb.dump quantizes split thresholds
#   and would corrupt them). All OTHER envelope numerics are written with
#   digits = 17.
#
# Feature set: xGOT reuses xG's pre-shot features (base_cols, the SAME
# geometry: x, y, distance_to_goal, angle_to_goal, in_penalty_area,
# in_six_yard_box, body-part/situation dummies, is_big_chance) PLUS 4
# placement features derived from the goal-mouth crossing point
# (gm_y, gm_z, dist_to_near_post, dist_to_top_corner -- see
# .create_placement_features() in R/xgot_model.R). The worker can reuse its
# existing xG pre-shot feature code for the base_cols; this export adds the
# goal_frame geometry constants needed to compute the 4 placement features
# from raw gm_y/gm_z.
#
# xGOT has NO penalty-override constant analogous to PENALTY_XG: training
# excludes penalties (exclude_penalties = TRUE, the fitted default) but
# add_xgot_to_spadl() does NOT special-case them at serve time -- a penalty
# is scored through the model like any other on-target shot. Confirmed via
# grep: no PENALTY_XGOT constant exists anywhere in R/. Not included in this
# envelope; flagging for the ITG session in case the worker wants one.
#
# Run from panna/:
#   Rscript data-raw/epv/06b_export_xgot_model_json.R

suppressPackageStartupMessages({
  library(cli)
  library(xgboost)
  library(jsonlite)
})
devtools::load_all(quiet = TRUE)

cli_h1("Export xGOT model to JSON")

# ------------------------------------------------------------------------
# 1. Load model
# ------------------------------------------------------------------------

rds_path <- if (exists("xgot_model_path")) xgot_model_path else "data-raw/cache/epv/xgot_model.rds"
if (!file.exists(rds_path)) cli_abort("Missing xGOT model: {rds_path}")

obj <- readRDS(rds_path)
booster <- obj$model
feature_names <- obj$panna_metadata$feature_cols
placement_cols <- obj$panna_metadata$placement_cols
cli_alert_info("Loaded xGOT model ({length(feature_names)} features): {paste(feature_names, collapse = ', ')}")
cli_alert_info("Training: {obj$panna_metadata$n_shots} shots, {obj$panna_metadata$n_goals} goals, exclude_penalties={obj$panna_metadata$exclude_penalties}")

# ------------------------------------------------------------------------
# 2. D2 serialization: raw learner JSON, embedded verbatim
# ------------------------------------------------------------------------

raw_bytes <- xgb.save.raw(booster, raw_format = "json")
booster_json_text <- rawToChar(raw_bytes)
cli_alert_success("xgb.save.raw(raw_format='json') produced {nchar(booster_json_text)} bytes")

# Parse READ-ONLY for metadata extraction. This parsed structure is used
# ONLY to read scalar fields (objective, base_score, nrounds, num_feature)
# for sanity-checks and the envelope header -- it is never reserialized;
# the tree internals reach the output file only via the verbatim text
# splice in step 3.
meta <- fromJSON(booster_json_text, simplifyDataFrame = FALSE, simplifyVector = FALSE)
obj_name <- meta$learner$objective$name
bs_raw <- meta$learner$learner_model_param$base_score
base_score <- as.numeric(gsub("[][]", "", bs_raw))
n_feature <- as.integer(meta$learner$learner_model_param$num_feature)
nrounds <- length(meta$learner$gradient_booster$model$trees)
if (n_feature != length(feature_names)) {
  cli_abort("Booster num_feature ({n_feature}) != panna_metadata$feature_cols length ({length(feature_names)})")
}
cli_alert_info("objective: {obj_name} | base_score: {format(base_score, digits = 10)} | nrounds: {nrounds} | num_feature: {n_feature}")

# ------------------------------------------------------------------------
# 3. Build envelope. Non-tree fields via jsonlite at digits=17 (lossless
#    round-trip precision for IEEE-754 doubles); the raw learner JSON is
#    spliced in as literal text (untouched).
# ------------------------------------------------------------------------

out_dir <- if (exists("xgot_json_out_dir")) xgot_json_out_dir else file.path(opta_data_dir(), "models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
json_path <- file.path(out_dir, "xgot_model.json")

envelope_meta <- list(
  model_type = "xgot_soccer",
  objective = obj_name,
  num_class = 1L,
  feature_names = feature_names,
  placement_feature_names = placement_cols,
  nrounds = nrounds,
  base_score = base_score,
  # On-target Opta shot type_ids (15=saved, 16=goal); off-target -> xgot=0
  # without ever calling the model (see add_xgot_to_spadl()).
  on_target_type_ids = list(15L, 16L),
  # Goal-frame geometry (R/xgot_model.R constants) needed to derive the 4
  # placement features from raw gm_y/gm_z -- see .create_placement_features().
  goal_frame = list(
    post_y_left = GOAL_POST_Y_LEFT,
    post_y_right = GOAL_POST_Y_RIGHT,
    post_y_mid = GOAL_POST_Y_MID,
    crossbar_z = GOAL_CROSSBAR_Z
  ),
  min_season_end_year = XGOT_MIN_SEASON_END_YEAR,
  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)
meta_json <- as.character(toJSON(envelope_meta, auto_unbox = TRUE, digits = 17, pretty = FALSE))
stopifnot(endsWith(meta_json, "}"))
final_json <- paste0(
  substr(meta_json, 1, nchar(meta_json) - 1),
  ',"booster_json":', booster_json_text, "}"
)
writeLines(final_json, json_path, useBytes = TRUE)
cli_alert_success("Wrote {json_path} ({round(file.info(json_path)$size / 1024 / 1024, 2)} MB)")

# ------------------------------------------------------------------------
# 4. Verify the splice is byte-identical, then functionally round-trip the
#    embedded learner JSON through a fresh booster.
# ------------------------------------------------------------------------

file_text <- paste(readLines(json_path, warn = FALSE), collapse = "\n")
key <- '"booster_json":'
pos <- regexpr(key, file_text, fixed = TRUE)
start <- pos + attr(pos, "match.length")
extracted_booster_json <- substr(file_text, start, nchar(file_text) - 1)
byte_identical <- identical(extracted_booster_json, booster_json_text)
if (!byte_identical) cli_abort("booster_json splice is NOT byte-identical to xgb.save.raw output")
cli_alert_success("booster_json embedding is byte-identical to xgb.save.raw() output (verbatim, no reformatting)")

fresh_booster <- xgb.load.raw(charToRaw(extracted_booster_json))
cli_alert_success("Reloaded booster from embedded JSON via xgb.load.raw()")

# ------------------------------------------------------------------------
# 5. Build ~20 REAL shot fixtures (real match coords + real goalmouth
#    coords -- never synthetic) with full input feature vectors + expected
#    xgot from predict() on the ORIGINAL booster, for the blog's float32
#    parity harness.
# ------------------------------------------------------------------------

cli_h2("Building real-shot fixtures")

fixture_league <- if (exists("xgot_fixture_league")) xgot_fixture_league else "ENG"
fixture_season <- if (exists("xgot_fixture_season")) xgot_fixture_season else "2024-2025"
shots <- load_opta_shot_events(fixture_league, season = fixture_season, source = "local")
ot <- shots$type_id %in% c(15L, 16L)
has_gm <- !is.na(shots$goalmouth_y) & !is.na(shots$goalmouth_z)
pool <- shots[ot & has_gm, ]
pool$is_penalty <- as.integer(grepl("penalty", tolower(pool$situation)))
cli_alert_info("Fixture pool: {nrow(pool)} on-target shots w/ goalmouth coords from {fixture_league} {fixture_season}")

pool_base <- .create_shot_features(
  x = pool$x, y = pool$y,
  bodypart = pool$body_part, situation = pool$situation,
  is_big_chance = as.integer(pool$big_chance)
)
pool_placement <- .create_placement_features(pool$goalmouth_y, pool$goalmouth_z)
pool_feats <- cbind(pool_base, pool_placement)
stopifnot(all(feature_names %in% names(pool_feats)))
pool_X <- as.matrix(pool_feats[, feature_names, drop = FALSE])
pool$xgot_pred <- as.numeric(predict(booster, pool_X))

np_idx <- which(pool$is_penalty == 0)
pen_idx <- which(pool$is_penalty == 1)

set.seed(42)
picks <- integer(0)
# Near-certain goals (tight angle, tucked placement)
picks <- c(picks, np_idx[order(-pool$xgot_pred[np_idx] * pool$is_goal[np_idx])[1:2]])
# "Great save" -- high predicted xgot but NOT a goal (keeper heroics)
saved_np <- np_idx[!pool$is_goal[np_idx]]
picks <- c(picks, saved_np[order(-pool$xgot_pred[saved_np])[1:2]])
# Headers (mix of goal/saved)
hdrs <- np_idx[pool$body_part[np_idx] == "Head"]
picks <- c(picks, sample(hdrs, min(4, length(hdrs))))
# Set-piece / corner situations
sp <- np_idx[pool$situation[np_idx] %in% c("SetPiece", "Corner")]
picks <- c(picks, sample(sp, min(3, length(sp))))
# Open play, right-foot, varied distance
op <- np_idx[pool$situation[np_idx] == "OpenPlay"]
picks <- c(picks, sample(op, min(3, length(op))))
# Near-post tucked finishes (small dist_to_near_post)
picks <- c(picks, np_idx[order(pool_placement$dist_to_near_post[np_idx])[1:2]])
# Central / far-from-frame placement (easy save territory)
picks <- c(picks, np_idx[order(-pool_placement$dist_to_near_post[np_idx])[1:2]])
# Random fill for diversity
picks <- c(picks, sample(np_idx, 4))
picks <- unique(picks[!is.na(picks)])
picks <- picks[seq_len(min(19, length(picks)))]
# One penalty edge case (model scores it despite exclude_penalties training)
if (length(pen_idx) > 0) picks <- c(picks, sample(pen_idx, 1))

sel <- pool[picks, ]
sel_X <- pool_X[picks, , drop = FALSE]
sel_expected <- pool$xgot_pred[picks]
cli_alert_success("Selected {nrow(sel)} real shots ({sum(sel$is_goal)} goals, {sum(sel$body_part == 'Head')} headers, {sum(sel$is_penalty)} penalty)")

fixtures <- lapply(seq_len(nrow(sel)), function(i) {
  feat_list <- as.list(sel_X[i, ])
  names(feat_list) <- feature_names
  list(
    match_id = sel$match_id[i],
    event_id = sel$event_id[i],
    player_name = sel$player_name[i],
    situation = sel$situation[i],
    body_part = sel$body_part[i],
    is_goal = isTRUE(sel$is_goal[i]),
    is_penalty = as.integer(sel$is_penalty[i]),
    x = sel$x[i], y = sel$y[i],
    goalmouth_y = sel$goalmouth_y[i], goalmouth_z = sel$goalmouth_z[i],
    features = feat_list,
    expected_xgot = sel_expected[i]
  )
})

fixtures_envelope <- list(
  model_type = "xgot_soccer",
  provenance = list(
    league = fixture_league, season = fixture_season,
    source = "load_opta_shot_events(source='local')",
    note = "Real Opta shot_events rows; expected_xgot from predict() on the original (pre-export) booster."
  ),
  feature_names = feature_names,
  n_fixtures = length(fixtures),
  shots = fixtures,
  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)

fixtures_path <- "data-raw/epv/xgot_model_fixtures.json"
write_json(fixtures_envelope, fixtures_path, auto_unbox = TRUE, digits = 17, pretty = TRUE)
cli_alert_success("Wrote {fixtures_path} ({round(file.info(fixtures_path)$size / 1024, 1)} KB)")

# ------------------------------------------------------------------------
# 6. Round-trip verification: score the fixtures through the RELOADED
#    booster (from the embedded JSON) and diff against expected_xgot
#    (predicted on the ORIGINAL booster before export).
# ------------------------------------------------------------------------

cli_h2("Round-trip verification")

fresh_pred <- as.numeric(predict(fresh_booster, sel_X))
orig_pred <- sel_expected
max_diff <- max(abs(fresh_pred - orig_pred))
cli_alert_info("Fixtures scored through reloaded (exported JSON) booster vs original booster:")
print(data.frame(player = sel$player_name, orig = orig_pred, reloaded = fresh_pred,
                  abs_diff = abs(fresh_pred - orig_pred)))
if (max_diff == 0) {
  cli_alert_success("Round-trip PASS: max abs diff = 0 (exact)")
} else {
  cli_alert_warning("Round-trip max abs diff = {format(max_diff, scientific = TRUE)}")
}

cli_h1("Complete")
cli_alert_success("Model JSON: {json_path}")
cli_alert_success("Fixtures JSON: {fixtures_path}")
cli_alert_info("Max abs diff (original vs reloaded booster on fixtures): {format(max_diff, scientific = TRUE)}")
