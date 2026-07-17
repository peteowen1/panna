#!/usr/bin/env Rscript
# Export the xG model (panna/R/xg_model.R, data-raw/cache/epv/xg_model.rds) to
# JSON for the Cloudflare Worker's live shot-xG scoring. Mirrors
# 05c_export_epv_model_json.R. Binary:logistic goal classifier; the worker's
# tree walker scores it as sigmoid(sum + logit(base_score)).
suppressMessages({ library(xgboost); library(jsonlite) })

rds <- "data-raw/cache/epv/xg_model.rds"
out <- if (length(commandArgs(trailingOnly = TRUE)) >= 1) commandArgs(trailingOnly = TRUE)[1] else "xg-model.json"
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

tmp <- tempfile(fileext = ".json")
xgb.dump(booster, fname = tmp, dump_format = "json")
trees <- fromJSON(tmp, simplifyDataFrame = FALSE, simplifyVector = FALSE)
cat("trees:", length(trees), "\n")

cfg <- xgb.config(booster)
if (is.character(cfg)) cfg <- fromJSON(cfg, simplifyDataFrame = FALSE, simplifyVector = FALSE)
obj_name <- cfg$learner$objective$name
bs_raw <- cfg$learner$learner_model_param$base_score
base_score <- as.numeric(gsub("[][]", "", bs_raw))  # strip [ ] brackets
cat("objective:", obj_name, "| base_score:", round(base_score, 6), "\n")

envelope <- list(
  model_type = "xg_soccer",
  objective = obj_name,
  num_class = 1L,
  feature_names = feature_names,
  nrounds = length(trees),
  base_score = base_score,
  # Canonical penalty-override value (== panna::PENALTY_XG via panna_metadata).
  # The worker reads this instead of hardcoding it (panna#91).
  penalty_xg = penalty_xg,
  trees = trees
)
write_json(envelope, out, auto_unbox = TRUE, digits = 10)
cat("wrote", out, "size:", file.info(out)$size, "bytes\n")
