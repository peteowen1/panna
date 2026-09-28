# xG v5 release: WP retrained on the v5 pack in the LIVE WP's own configuration
# (05_train_wp_model.R's defaults: depth 4, eta 0.1, min_child_weight 50,
# binary:logistic, raw xmargin / epv). Only the EPV (epv_model_v5v0) and the xG
# (v5) underneath the features change. Writes to its own folder; touches nothing
# published. Gate: pack_gate_wp_v5.R (WP_V5_CANDIDATE).
if (!identical(Sys.getenv("ALLOW_V5"), "1")) stop("Superseded: xG v5 / xGOT v3 trained on goals-only feeds (2026-09-28). Use the v5.1 runner; set ALLOW_V5=1 only to reproduce v5 on purpose.")
suppressMessages(devtools::load_all(".", quiet = TRUE))
cat("=== WP v5 retrain start:", format(Sys.time()), "===\n")
epv_model_override <- readRDS("data-raw/cache/epv/pack-2026-09/epv_model_v5v0.rds")
xg_model_override  <- load_xg_model("data-raw/cache/epv/xg-vnext/xg_model_v5.rds")
model_dir <- "data-raw/cache/epv/pack-2026-09/wp_v5"
source("data-raw/epv/05_train_wp_model.R")
cat("=== WP v5 retrain done:", format(Sys.time()), "===\n")
