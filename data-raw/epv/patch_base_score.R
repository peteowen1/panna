library(xgboost)
library(jsonlite)

# Patch base_score into wp_model.json from the RDS booster config.
# The 05b export script produces the JSON envelope but doesn't include
# base_score — this small utility reads the RDS, pulls base_score from
# the booster config, and writes it into the JSON envelope.

rds_path <- "C:/Users/peteo/OneDrive/Documents/pannaverse/pannadata/data/opta/models/wp_model.rds"
json_path <- "C:/Users/peteo/OneDrive/Documents/pannaverse/pannadata/data/opta/models/wp_model.json"

wp <- readRDS(rds_path)
bs_raw <- xgb.config(wp$model)$learner$learner_model_param$base_score
bs <- as.numeric(gsub("[\\[\\]]", "", bs_raw, perl = TRUE))
cat("base_score extracted:", bs, "\n")

j <- fromJSON(json_path, simplifyVector = FALSE)
j[["base_score"]] <- bs
write(toJSON(j, auto_unbox = TRUE, digits = NA), json_path)
cat("Patched JSON at", json_path, "\n")
