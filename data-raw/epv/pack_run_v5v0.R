# Runner: model-pack EPV with labels at the ledger's shot price on xG v5
# (xG v5 + aftermath). Smoke test: set PACK_SMOKE <- TRUE first (ENG 2024-25 only,
# separate output folder, so nothing here touches the full run's files).
LABEL_XG <- "v5_v0"
if (exists("PACK_SMOKE") && isTRUE(PACK_SMOKE)) {
  PACK_OUT <- "data-raw/cache/epv/pack-v5-smoke"
  PACK_UNITS <- quote(league == "ENG" & season == "2024-2025")
}
source("data-raw/epv/pack_2026_09_retrain.R")
