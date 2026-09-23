# Model-pack retrain step 7 (2026-09-23): the ledger on the canonical EPV vs the
# EPV whose labels are priced by the published xG (pack-2026-09). One axis:
# the EPV model; shares, xG, xPass, xGOT unchanged. Scored by ng_shares_ab.R:
# within-position repeatability decides (not worse), position means reported.
# Run from panna/: powershell.exe -Command 'Rscript "data-raw/epv/net-goals/ng_epv_pack_ab.R"'
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
ARMS <- list(epv_canonical = ng_shares(),
             epv_pubxg_labels = list(shares = ng_shares(), epv = "data-raw/cache/epv/pack-2026-09/epv_model_pubxg.rds"))
source("data-raw/epv/net-goals/ng_shares_ab.R")
