# Duel loser A/B (2026-09-23): name the player who lost an aerial or tackle on
# the row that delivered the ball into it, taking loser_share of the losing
# side's pool there. Arms: 0 (shipped, nobody named), 0.35, 0.70 (mirrors the
# winner's named_share). Scored by ng_shares_ab.R: within-position
# repeatability decides; position means are reported, not targeted.
# Run from panna/: Rscript data-raw/epv/net-goals/ng_duel_loser_ab.R
suppressPackageStartupMessages(library(data.table)); devtools::load_all(quiet = TRUE)
ARMS <- list(loser_0 = ng_shares(), loser_0.35 = ng_shares(loser_share = 0.35),
             loser_0.70 = ng_shares(loser_share = 0.70))
source("data-raw/epv/net-goals/ng_shares_ab.R")
