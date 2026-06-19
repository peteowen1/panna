#!/usr/bin/env Rscript
# Export EPV model (panna/R/epv_model.R trained model) to JSON for the
# Cloudflare Worker's xmargin computation. Mirrors 05b_export_wp_model_json.R.
#
# Target consumer: worker/src/wp-model.js's tree walker. Model is single-
# output regressor (reg:squarederror) with base_score offset — same shape
# as the WP model.

library(cli)
library(xgboost)
library(jsonlite)
devtools::load_all()

cli_h1("Export EPV model to JSON")

# Default: published model. Set `epv_model_path` / `epv_json_out_dir` to stage a candidate.
out_dir <- if (exists("epv_json_out_dir")) epv_json_out_dir else file.path(opta_data_dir(), "models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
rds_path <- if (exists("epv_model_path")) epv_model_path else file.path(opta_data_dir(), "models", "epv_model.rds")
json_path <- file.path(out_dir, "epv_model.json")

if (!file.exists(rds_path)) cli_abort("Missing RDS: {rds_path}")
obj <- readRDS(rds_path)
booster <- obj$model
feature_names <- obj$panna_metadata$feature_cols
method <- obj$method
cli_alert_info("Loaded EPV model ({method} variant) with {length(feature_names)} features")

# Use xgb.dump to a tempfile — same pattern as 05b (works cleanly with
# jsonlite and produces the nested tree shape the worker expects).
tmp_json <- tempfile(fileext = ".json")
xgb.dump(booster, fname = tmp_json, dump_format = "json")
trees_nested <- fromJSON(tmp_json, simplifyDataFrame = FALSE, simplifyVector = FALSE)
cli_alert_success("xgb.dump produced {length(trees_nested)} trees")

# Verify split-feature names match declared feature_names (quick sanity)
collect_splits <- function(node, acc = character()) {
  if (!is.null(node$leaf)) return(acc)
  acc <- c(acc, node$split)
  if (!is.null(node$children)) for (ch in node$children) acc <- collect_splits(ch, acc)
  acc
}
all_splits <- unique(unlist(lapply(trees_nested, collect_splits)))
cli_alert_info("Split features found: {paste(all_splits, collapse=', ')}")
unused <- setdiff(feature_names, all_splits)
if (length(unused) > 0) cli_alert_warning("Declared but unused in splits: {paste(unused, collapse=', ')}")
unknown <- setdiff(all_splits, feature_names)
if (length(unknown) > 0) cli_abort("Tree uses unknown features: {paste(unknown, collapse=', ')}")

# Extract base_score from booster config. xgb.config returns either a
# list (newer xgboost) or a JSON string. Handle both.
base_score <- tryCatch({
  cfg <- xgb.config(booster)
  if (is.character(cfg)) cfg <- fromJSON(cfg, simplifyDataFrame = FALSE, simplifyVector = FALSE)
  bs_raw <- cfg$learner$learner_model_param$base_score
  # Value is like "[5.5080795E-1]" — strip brackets, parse float
  as.numeric(gsub("[\\[\\]]", "", bs_raw, perl = TRUE))
}, error = function(e) {
  cli_alert_warning("Could not extract base_score from booster config: {e$message}")
  0.0
})
cli_alert_info("base_score: {round(base_score, 5)}")

envelope <- list(
  model_type = "epv_soccer",
  method = method,
  objective = "reg:squarederror",
  num_class = 1L,
  feature_names = feature_names,
  nrounds = length(trees_nested),
  base_score = base_score,
  # Panna-trained action_cat uses this exact integer mapping — worker must
  # produce action_cat values matching these codes per Opta event type.
  action_map = list(
    pass = 1L, cross = 2L, shot = 3L, take_on = 4L,
    tackle = 5L, interception = 6L, clearance = 7L,
    aerial = 8L, foul = 9L, ball_recovery = 10L
  ),
  # League encoding — 0 = unknown (worker can use this as fallback)
  league_map = as.list(EPV_LEAGUE_MAP),
  trees = trees_nested,
  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)

write_json(envelope, json_path, auto_unbox = TRUE, digits = NA, pretty = FALSE)
cli_alert_success("Wrote {json_path} ({round(file.info(json_path)$size / 1024, 1)} KB)")

# Sanity reload
reloaded <- fromJSON(json_path, simplifyDataFrame = FALSE, simplifyVector = FALSE)
stopifnot(length(reloaded$trees) == length(trees_nested))
stopifnot(reloaded$base_score == base_score)
cli_alert_success("Reload verification passed.")
