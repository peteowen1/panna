# Runner: model-pack EPV with labels at the ledger's shot price on xG v5
# (xG v5 + aftermath). Smoke test: set PACK_SMOKE <- TRUE first (ENG 2024-25 only,
# separate output folder, so nothing here touches the full run's files).
if (!identical(Sys.getenv("ALLOW_V5"), "1")) stop("Superseded: xG v5 / xGOT v3 trained on goals-only feeds (2026-09-28). Use the v5.1 runner; set ALLOW_V5=1 only to reproduce v5 on purpose.")
LABEL_XG <- "v5_v0"
if (exists("PACK_SMOKE") && isTRUE(PACK_SMOKE)) {
  PACK_OUT <- "data-raw/cache/epv/pack-v5-smoke"
  PACK_UNITS <- quote(league == "ENG" & season == "2024-2025")
}
source("data-raw/epv/pack_2026_09_retrain.R")
