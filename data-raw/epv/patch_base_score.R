library(xgboost)
library(jsonlite)

# Patch base_score into wp_model.json from the RDS booster config.
# The 05b export script produces the JSON envelope but doesn't include
# base_score — this small utility reads the RDS, pulls base_score from
# the booster config, and writes it into the JSON envelope.

# Resolve the model dir via panna rather than a hardcoded absolute path (was
# "C:/dev/pannaverse/pannadata/data/opta/models/...", which only existed on one
# machine). opta_data_dir() does the pannadata cwd-walk, so this works from the
# package root on any clone or CI runner.
devtools::load_all(quiet = TRUE)
model_dir <- file.path(opta_data_dir(), "models")
rds_path <- file.path(model_dir, "wp_model.rds")
json_path <- file.path(model_dir, "wp_model.json")
for (p in c(rds_path, json_path)) {
  if (!file.exists(p)) stop("Missing ", p, " -- run the WP train/export steps first.")
}

wp <- readRDS(rds_path)
bs_raw <- xgb.config(wp$model)$learner$learner_model_param$base_score
bs <- as.numeric(gsub("[\\[\\]]", "", bs_raw, perl = TRUE))
cat("base_score extracted:", bs, "\n")

j <- fromJSON(json_path, simplifyVector = FALSE)
j[["base_score"]] <- bs
write(toJSON(j, auto_unbox = TRUE, digits = NA), json_path)
cat("Patched JSON at", json_path, "\n")
