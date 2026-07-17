# 05b_export_wp_model_json.R
# Export the trained football WP XGBoost model to JSON for the live worker
# tree walker (Cloudflare Worker consuming from R2).
#
# NOTE (2026-04-24): This is an **export / validation only** script. It does NOT
# commit or upload anything. Pete will review the output JSON + this script's
# console report before any R2 upload happens.
#
# Model shape (important!): despite METRICS.md section 10 calling this a
# "3-class" model, R/wp_model.R::train_wp_model uses objective="reg:squarederror"
# on a continuous label in {0, 0.5, 1} → single scalar output (home expected
# points fraction). So the JSON dump has one tree per boosting round (not
# nrounds * num_class), and the worker consumes it with a sigmoid-free scalar
# sum (no softmax) clamped to [0, 1].
#
# Run from panna directory:
#   Rscript data-raw/epv/05b_export_wp_model_json.R

suppressPackageStartupMessages({
  library(cli)
  library(jsonlite)
  library(xgboost)
})
devtools::load_all(quiet = TRUE)

cli_h1("Export WP model to JSON")

# ------------------------------------------------------------------------------
# 1. Load model
# ------------------------------------------------------------------------------

# Default: published model. Set `wp_model_path` to export a candidate instead.
wp <- if (exists("wp_model_path")) readRDS(wp_model_path) else load_wp_model()

stopifnot(!is.null(wp$model), !is.null(wp$feature_names))

cli_alert_info("Loaded wp_model with features: {paste(wp$feature_names, collapse = ', ')}")
cli_alert_info("xgb.Booster class: {paste(class(wp$model), collapse = '/')}")

# Booster-level attrs
attrs <- xgboost::xgb.attributes(wp$model)
best_iter <- attrs$best_iteration
best_ntreelimit <- attrs$best_ntreelimit
cli_alert_info("best_iteration = {best_iter %||% 'unset'}; best_ntreelimit = {best_ntreelimit %||% 'unset'}")

# ------------------------------------------------------------------------------
# 2. Export trees as JSON
# ------------------------------------------------------------------------------

out_dir <- if (exists("wp_json_out_dir")) wp_json_out_dir else file.path(opta_data_dir(), "models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
json_path <- file.path(out_dir, "wp_model.json")

# xgb.save with dump_format="json" writes the booster's internal trees in the
# same nested {nodeid, split, split_condition, yes, no, missing, leaf, children}
# shape that worker/src/ep-model.js expects.
tmp_raw_json <- tempfile(fileext = ".json")
xgboost::xgb.dump(wp$model, fname = tmp_raw_json, dump_format = "json")

trees_nested <- jsonlite::fromJSON(tmp_raw_json, simplifyDataFrame = FALSE,
                                    simplifyVector = FALSE)

cli_alert_success("xgb.dump produced {length(trees_nested)} trees")

# Structural sanity
if (length(trees_nested) == 0) cli_abort("xgb.dump returned 0 trees")

first_tree <- trees_nested[[1]]
stopifnot(!is.null(first_tree$nodeid))

# Collect every split feature name across every tree to verify the model
# actually uses the feature_names we claim (vs silently training on something else).
collect_splits <- function(node, acc = character()) {
  if (!is.null(node$leaf)) return(acc)
  acc <- c(acc, node$split)
  if (!is.null(node$children)) {
    for (ch in node$children) acc <- collect_splits(ch, acc)
  }
  acc
}
all_splits <- unique(unlist(lapply(trees_nested, collect_splits)))
cli_alert_info("Split features found in trees: {paste(all_splits, collapse = ', ')}")

unknown <- setdiff(all_splits, wp$feature_names)
if (length(unknown) > 0) {
  cli_alert_warning("Tree uses features NOT in wp$feature_names: {paste(unknown, collapse = ', ')}")
}
unused <- setdiff(wp$feature_names, all_splits)
if (length(unused) > 0) {
  cli_alert_warning("feature_names declared but never split on: {paste(unused, collapse = ', ')}")
}

# ------------------------------------------------------------------------------
# 3. Wrap in worker-friendly envelope matching ep-model-live.json shape
# ------------------------------------------------------------------------------
# ep-model.js expects: { num_class: N, trees: [...] }
# This is a regression booster, so num_class = 1. Worker consumer does a simple
# scalar sum over trees (no softmax, no class-indexed accumulation) then clamps
# to [0,1] for the WP output.

# Pull objective + base_score from booster config so the envelope always
# matches what was actually trained (was previously hardcoded to
# "reg:squarederror"; caused JS tree walkers to skip the sigmoid transform
# after we switched panna to binary:logistic).
cfg <- xgboost::xgb.config(wp$model)
if (is.character(cfg)) cfg <- jsonlite::fromJSON(cfg, simplifyDataFrame = FALSE, simplifyVector = FALSE)
objective <- cfg$objective$name %||% cfg$learner$objective$name %||% "reg:squarederror"
bs_raw <- cfg$learner$learner_model_param$base_score
base_score <- as.numeric(gsub("[\\[\\]]", "", bs_raw, perl = TRUE))
cli_alert_info("Objective from booster config: {objective}; base_score: {round(base_score, 5)}")

envelope <- list(
  model_type = "wp_soccer",
  objective = objective,
  base_score = base_score,
  num_class = 1L,
  feature_names = wp$feature_names,
  nrounds = length(trees_nested),
  trees = trees_nested,
  # Training-time scaling context for the worker's feature engineering.
  # time_remaining now uses a PER-MATCH denominator: regulation_seconds for
  # matches that ended in 90 min, extra_time_seconds for matches that reached
  # ET (2026-05-31 ET/shootout WPA fix). The worker must pick the denominator
  # per match the same way create_wp_features() does — a single fixed cap is no
  # longer correct for ET matches. is_extra_time is now also a model feature.
  time_scaling = list(regulation_seconds = REGULATION_SECONDS,
                      extra_time_seconds = EXTRA_TIME_SECONDS),
  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)

jsonlite::write_json(
  envelope,
  json_path,
  auto_unbox = TRUE,
  digits = NA,      # keep full floating-point precision
  pretty = FALSE    # worker downloads, no need to pretty-print
)

cli_alert_success("Wrote {json_path}")
cli_alert_info("Size: {round(file.info(json_path)$size / 1024, 1)} KB")

# ------------------------------------------------------------------------------
# 4. Reload + validate structure
# ------------------------------------------------------------------------------

reloaded <- jsonlite::fromJSON(json_path, simplifyDataFrame = FALSE,
                                simplifyVector = FALSE)

stopifnot(
  identical(reloaded$num_class, 1L) || reloaded$num_class == 1,
  length(reloaded$trees) == length(trees_nested),
  !is.null(reloaded$trees[[1]]$nodeid)
)

# Assert the required node schema matches ep-model.js expectations
t1 <- reloaded$trees[[1]]
keys_root <- names(t1)
required_branch_keys <- c("nodeid", "split", "split_condition", "yes", "no", "missing", "children")
have_branch <- all(required_branch_keys %in% keys_root) || !is.null(t1$leaf)
if (!have_branch) {
  cli_abort("First tree root missing required keys: {paste(setdiff(required_branch_keys, keys_root), collapse = ', ')}")
}

# Walk one tree manually and compare to xgb predict on a hand-made feature vector
# at a "middle of match, level game" state — home WP should be close to 0.55.
# Columns MUST match train_wp_model()'s feature_names exactly (the model is
# scored via scenarios[, wp$feature_names]): time_remaining, xmargin, epv,
# xg_diff, red_card_diff, is_home, is_second_half, is_extra_time. `xmargin` is
# the score+EPV composite the model trains on (NOT score_diff — that was an
# older name); `is_extra_time` was added 2026-05-31 with the ET/shootout WPA
# fix; `epv` is the standalone in-possession threat feature added 2026-06-18
# (#92 — surfaced separately from xmargin so the trees can split the sub-1.0
# threat band).
# Emits a SUPERSET of features (base + the depth-2 time-interacted forms
# xmargin_x_time/epv_x_time) so `scenarios[, wp$feature_names]` works for either
# the base model OR the time-interacted one. time_elapsed_frac defaults to the
# complement of time_remaining (good enough for these sanity scenarios).
mk_feat <- function(time_remaining = 0.5, xmargin = 0, epv = 0, xg_diff = 0,
                    red_card_diff = 0L, is_home = 1L, is_second_half = 0L,
                    is_extra_time = 0L, time_elapsed_frac = NULL) {
  if (is.null(time_elapsed_frac)) time_elapsed_frac <- 1 - time_remaining
  data.frame(time_remaining = time_remaining, xmargin = xmargin, epv = epv,
             time_elapsed_frac = time_elapsed_frac,
             xmargin_x_time = xmargin * time_elapsed_frac,
             epv_x_time = epv * time_elapsed_frac,
             xg_diff = xg_diff, red_card_diff = red_card_diff,
             is_home = is_home, is_second_half = is_second_half,
             is_extra_time = is_extra_time)
}

neutral <- mk_feat()
home_lead_late <- mk_feat(time_remaining = 0.1, xmargin = 1,
                          is_second_half = 1L)
away_lead_late <- mk_feat(time_remaining = 0.1, xmargin = -1,
                          is_second_half = 1L)
away_lead_early <- mk_feat(time_remaining = 0.8, xmargin = -1)
# An ET scenario: level game deep into extra time, little time left.
et_level_late <- mk_feat(time_remaining = 0.05, xmargin = 0,
                         is_second_half = 1L, is_extra_time = 1L)
# #92 sanity: losing team late with a live in-possession chance (high epv) —
# the standalone epv feature should lift WP above the flat-threat baseline.
losing_late_chance <- mk_feat(time_remaining = 0.1, xmargin = -1, epv = 0.45,
                              is_second_half = 1L)

scenarios <- rbind(neutral, home_lead_late, away_lead_late, away_lead_early,
                   et_level_late, losing_late_chance)
scenarios$label <- c("neutral", "home_lead_late", "away_lead_late",
                     "away_lead_early", "et_level_late", "losing_late_chance")

pred_scalar <- predict(wp$model, as.matrix(scenarios[, wp$feature_names]))
cli_h2("In-R predict() scenario sanity")
print(cbind(scenarios, wp = round(pred_scalar, 3)))

# ------------------------------------------------------------------------------
# 5. Manual tree-walker emulation — mirrors worker/src/ep-model.js walkTree
# ------------------------------------------------------------------------------

walk_tree <- function(node, feats) {
  depth <- 0
  while (depth < 200) {
    if (!is.null(node$leaf)) return(as.numeric(node$leaf))
    split_feat <- node$split
    threshold <- node$split_condition
    val <- feats[[split_feat]]
    # Match xgboost default_left / missing handling
    go <- if (is.null(val) || is.na(val)) node$missing else if (val < threshold) node$yes else node$no
    # Find child by nodeid
    child <- NULL
    for (ch in node$children) {
      if (identical(ch$nodeid, go) || ch$nodeid == go) { child <- ch; break }
    }
    if (is.null(child)) return(0)
    node <- child
    depth <- depth + 1
  }
  0
}

predict_manual <- function(trees, feats) {
  s <- 0
  for (tr in trees) s <- s + walk_tree(tr, as.list(feats))
  s
}

# base_score is NOT re-derived here — reuse the value already extracted from
# the booster config in step 3 (same `cfg`, same `base_score` variable). A
# separate empirical re-derivation used to live in this section (predict a
# zero-feature row, subtract the manual raw sum from R's predict()), but for
# a binary:logistic booster predict() applies a sigmoid while the manual sum
# is raw logit-space, so "zpred - zwalk" mixed probability-space and
# logit-space and produced a bogus value (0.61295) that silently disagreed
# with step 3's correct 0.5157 read off the same booster config's
# learner_model_param$base_score. One extraction, reused everywhere, so the
# two sections can't drift apart again.
cli_alert_info("Booster base_score (reusing step 3 extraction) = {round(base_score, 5)}")

logit <- function(p) log(p / (1 - p))

for (i in seq_len(nrow(scenarios))) {
  feats <- as.list(scenarios[i, wp$feature_names])
  raw_sum <- predict_manual(reloaded$trees, feats)
  # binary:logistic booster: predict() = sigmoid(sum(leaves) + logit(base_score)).
  # reg:squarederror (and anything else): predict() = sum(leaves) + base_score,
  # no sigmoid. Comparing raw-sum-vs-predict() unconditionally (the old code)
  # is correct only for the latter — every binary:logistic scenario mismatched
  # by construction, not because the export was wrong.
  manual <- if (identical(objective, "binary:logistic")) {
    1 / (1 + exp(-(raw_sum + logit(base_score))))
  } else {
    raw_sum + base_score
  }
  rpred  <- pred_scalar[i]
  ok <- abs(manual - rpred) < 1e-5
  cli_alert_info("{scenarios$label[i]}: R = {round(rpred, 5)}, manual = {round(manual, 5)}, diff = {signif(abs(manual - rpred), 3)} {ifelse(ok, '[OK]', '[MISMATCH]')}")
}

# ------------------------------------------------------------------------------
# 6. Real-match WP-variation sanity — does WP wiggle between non-scoring events?
# ------------------------------------------------------------------------------

cli_h2("Per-event WP variation check on one real match")

tryCatch({
  events <- load_opta_match_events("ENG", season = "2023-2024", source = "local")
  lineups <- load_opta_lineups("ENG", season = "2023-2024", source = "local")
  one_mid <- unique(events$match_id)[1]
  ev1 <- events[events$match_id == one_mid, ]
  ln1 <- lineups[lineups$match_id == one_mid, ]
  spadl <- convert_opta_to_spadl(ev1)
  spadl_ch <- create_possession_chains(spadl)
  # Build a minimal match_results frame (no goals needed for prediction)
  ht <- unique(ln1[tolower(ln1$team_position) == "home", c("match_id", "team_id")])
  names(ht)[2] <- "home_team_id"
  ht$away_team_id <- NA_integer_
  ht$home_goals <- 0L; ht$away_goals <- 0L
  wp_feat <- create_wp_features(spadl_ch, ht)
  wp_feat <- add_wp_vars(wp_feat, wp)
  deltas <- abs(diff(wp_feat$wp))
  cli_alert_info("Match {one_mid}: {nrow(wp_feat)} actions; abs delta quantiles:")
  print(round(quantile(deltas, c(0.5, 0.75, 0.9, 0.95, 0.99, 1), na.rm = TRUE), 4))
  cli_alert_info("fraction of events shifting WP by > 0.001: {round(mean(deltas > 0.001, na.rm = TRUE), 3)}")
  cli_alert_info("fraction > 0.01: {round(mean(deltas > 0.01, na.rm = TRUE), 3)}")
  cli_alert_info("fraction > 0.05 (likely scoring events): {round(mean(deltas > 0.05, na.rm = TRUE), 3)}")
}, error = function(e) {
  cli_alert_warning("Real-match sanity check failed: {e$message}")
  cli_alert_info("(skip — not blocking the JSON export)")
})

cli_h1("Complete")
cli_alert_success("JSON: {json_path}")
