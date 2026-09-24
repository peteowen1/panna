# Model-pack retrain step 7 (2026-09-24): the ledger on the canonical EPV vs
# EPV with labels priced by the published xG (pubxg) and by the ledger's own
# shot price, xG + aftermath (pubv0). One axis: the EPV model. Scored by
# ng_shares_ab.R: within-position repeatability decides (not worse).
# Run from panna/: powershell.exe -Command 'Rscript "data-raw/epv/net-goals/ng_epv_pack_ab.R"'
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
P <- "data-raw/cache/epv/pack-2026-09/"
ARMS <- list(epv_canonical = ng_shares(),
             epv_pubxg = list(shares = ng_shares(), epv = paste0(P, "epv_model_pubxg.rds")),
             epv_pubv0 = list(shares = ng_shares(), epv = paste0(P, "epv_model_pubv0.rds")))
source("data-raw/epv/net-goals/ng_shares_ab.R")
