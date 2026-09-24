# Model-pack step 6: WP retrained on the new EPV (labels at the ledger's shot price).
# One axis against the canonical WP (_run_retrain_wp_final.R): same config, scope and
# seasons; only the EPV model underneath the `epv` feature changes. Writes to its own
# cache dir; does NOT touch the published or canonical model.
cat("=== WP pack retrain start:", format(Sys.time()), "===\n")
epv_model_override <- readRDS("data-raw/cache/epv/pack-2026-09/epv_model_pubv0.rds")
wp_max_depth <- 2L
wp_eta <- 0.05
wp_min_child_weight <- 100L
wp_objective <- "reg:squarederror"
wp_feature_names <- c("time_remaining", "xmargin_x_time", "epv_x_time", "xg_diff",
                      "red_card_diff", "is_home", "is_second_half", "is_extra_time")
model_dir <- "data-raw/cache/epv/pack-2026-09/wp_pubv0"
source("data-raw/epv/05_train_wp_model.R")
cat("=== WP pack retrain done:", format(Sys.time()), "===\n")
