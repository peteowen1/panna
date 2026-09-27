#!/usr/bin/env Rscript
# Export the xG model (panna/R/xg_model.R, data-raw/cache/epv/xg_model.rds) to
# JSON for the Cloudflare Worker's live shot-xG scoring. Mirrors
# 05c_export_epv_model_json.R. Binary:logistic goal classifier; the worker's
# tree walker scores it as sigmoid(sum + logit(base_score)).
suppressMessages({ library(xgboost); library(jsonlite) })

# Usage: Rscript 06_export_xg_model_json.R [out.json] [model.rds]
# model.rds defaults to the production cache; xG v5 is
# data-raw/cache/epv/xg-vnext/xg_model_v5.rds.
args <- commandArgs(trailingOnly = TRUE)
out <- if (length(args) >= 1) args[1] else "xg-model.json"
rds <- if (length(args) >= 2) args[2] else "data-raw/cache/epv/xg_model.rds"
if (!file.exists(rds)) stop("Missing xG model: ", rds)

obj <- readRDS(rds)
booster <- obj$model
feature_names <- obj$panna_metadata$feature_cols
penalty_xg <- obj$panna_metadata$penalty_xg
if (is.null(penalty_xg)) {
  stop("xg_model.rds panna_metadata lacks penalty_xg (pre-panna#91 artifact) — ",
       "retrain via fit_xg_model() or patch the rds before exporting")
}
cat("features (", length(feature_names), "):", paste(feature_names, collapse = ", "), "\n")

# Exact format (xgb.save.raw, raw_format = "json"), spliced in verbatim, the
# same way 06b_export_xgot_model_json.R does it. xgb.dump() -- what this used to
# write -- rounds split points to ~7 digits, and xG v5's split points sit ON
# training values (shots share Opta's 0.1 grid), so the rounded ones sent 79 of
# 278 fixture shots down the other branch (max error 0.09). The worker's
# flattenRawTree() float32-rounds these split points and the scorer rounds the
# features, as XGBoost does. The worker reads both envelope shapes.
raw_json <- rawToChar(xgb.save.raw(booster, raw_format = "json"))
meta <- fromJSON(raw_json, simplifyDataFrame = FALSE, simplifyVector = FALSE)
obj_name <- meta$learner$objective$name
base_score <- as.numeric(gsub("[][]", "", meta$learner$learner_model_param$base_score))
n_trees <- length(meta$learner$gradient_booster$model$trees)
if (as.integer(meta$learner$learner_model_param$num_feature) != length(feature_names)) {
  stop("booster num_feature != panna_metadata$feature_cols length")
}
cat("objective:", obj_name, "| base_score:", round(base_score, 6), "| trees:", n_trees, "
")

envelope <- list(
  model_type = "xg_soccer",
  objective = obj_name,
  num_class = 1L,
  feature_names = feature_names,
  nrounds = n_trees,
  base_score = base_score,
  # Canonical penalty-override value (== panna::PENALTY_XG via panna_metadata).
  # The worker reads this instead of hardcoding it (panna#91).
  penalty_xg = penalty_xg,
  # xG v5 and later: the penalty rate by season end year (earlier seasons only,
  # shrunk; XG-VNEXT-2026-09.md), which the worker prefers over penalty_xg, and
  # whether a missing input means "missing" (the models learned an NA branch for
  # "no assist" / "too few earlier foot shots") rather than 0.
  penalty_xg_by_season = if (!is.null(obj$panna_metadata$penalty_xg_by_season))
    as.list(obj$panna_metadata$penalty_xg_by_season) else NULL,
  na_is_missing = isTRUE(obj$panna_metadata$na_is_missing),
  version = obj$panna_metadata$version %||% NA,
  exported_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")
)
meta_json <- as.character(toJSON(envelope, auto_unbox = TRUE, digits = 17, pretty = FALSE, null = "null", na = "null"))
stopifnot(endsWith(meta_json, "}"))
writeLines(paste0(substr(meta_json, 1, nchar(meta_json) - 1), ',"booster_json":', raw_json, "}"), out, useBytes = TRUE)
cat("wrote", out, "size:", file.info(out)$size, "bytes
")
