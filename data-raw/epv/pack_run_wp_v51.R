# xG v5.1 release: WP retrained on the v5.1 pack in the LIVE WP's own configuration
# (05_train_wp_model.R's defaults: depth 4, eta 0.1, min_child_weight 50,
# binary:logistic, raw xmargin / epv). Only the EPV (epv_model_v51v0) and the xG
# (v5.1) underneath the features change from the live WP. Writes to its own
# folder; touches nothing published. Gate: pack_gate_wp_v5.R with PACK_TAG = "v51".
suppressMessages(devtools::load_all(".", quiet = TRUE))
cat("=== WP v5.1 retrain start:", format(Sys.time()), "===\n")
epv_model_override <- readRDS("data-raw/cache/epv/pack-2026-09/epv_model_v51v0.rds")
xg_model_override  <- load_xg_model("data-raw/cache/epv/xg-vnext/xg_model_v5_1.rds")
model_dir <- "data-raw/cache/epv/pack-2026-09/wp_v51"
source("data-raw/epv/05_train_wp_model.R")
cat("=== WP v5.1 retrain done:", format(Sys.time()), "===\n")
