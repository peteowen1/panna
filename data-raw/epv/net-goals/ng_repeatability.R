# ng_repeatability.R -- choose the free shares by year-over-year repeatability.
#
# The four shares in ng_shares(), plus dacts_share, are NOT identifiable from
# conservation: the identity holds for every value, which is the design working
# correctly and also why the goal difference cannot choose them. Torp's D17
# settles them on repeatability instead -- skill persists between seasons, noise
# does not, so the setting that best predicts a player's next season from his
# last is the one carrying signal.
#
# This is deliberately NOT "the setting that makes positions look even". The
# position spread is not a validated target and optimising it would be
# metric-forcing.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_repeatability.R
#
# Takes roughly 15-25 minutes: it rebuilds the action-level EPV for each season
# once, then re-allocates cheaply per share setting.

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE   <- "ENG"
SEASONS  <- c("2022-2023", "2023-2024", "2024-2025")
MIN_MINS <- 900          # per season, both seasons of a pair
CACHE    <- "data-raw/cache/epv/net-goals"
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

xg_model    <- readRDS("data-raw/cache/epv/xg_model.rds")
xpass_model <- readRDS("data-raw/cache/epv/xpass_model.rds")
epv_model   <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")
fx_all <- as.data.table(load_opta_fixtures(LEAGUE, source = "local"))[
  , .(match_id, home_team_id, away_team_id, home_score, away_score)]

# Per-season inputs are cached: rebuilding action-level EPV three times over is
# the whole runtime, and the share sweep below re-reads them dozens of times.
season_inputs <- function(season) {
  f <- file.path(CACHE, paste0("inputs_", LEAGUE, "_", season, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  cli::cli_alert_info("Building {LEAGUE} {season}...")
  events  <- load_opta_match_events(LEAGUE, season = season, source = "local")
  lineups <- load_opta_lineups(LEAGUE, season = season, source = "local")
  shot_lk <- panna:::.epv_shot_lookup(LEAGUE, season)
  spadl <- convert_opta_to_spadl(events)
  ch  <- create_possession_chains(spadl)
  lab <- label_actions_with_outcomes(ch, add_next_chain_outcome(classify_chain_outcomes(ch)))
  lab <- create_next_goal_labels(lab)
  if (epv_model$method == "xg") lab <- create_next_xg_labels(lab)
  ep  <- calculate_action_epv(lab, create_epv_features(lab, n_prev = 3), epv_model,
                              xg_model = xg_model, league = LEAGUE, season = season,
                              shot_lookup = shot_lk)
  ep  <- as.data.table(add_xpass_to_spadl(ep, xpass_model))
  out <- list(ep = ep, lu = as.data.table(lineups),
              adj = ng_build_adjacency(events, verbose = FALSE))
  saveRDS(out, f)
  out
}

inp <- lapply(SEASONS, season_inputs)
names(inp) <- SEASONS

# Per-player season totals under one share setting.
season_totals <- function(s, sh, ds) {
  i <- inp[[s]]
  pay <- ng_build_ledger(i$ep, adj = i$adj, fixtures = fx_all, shares = sh,
                         verbose = FALSE)
  pay <- ng_spread_pools(pay, i$ep, i$lu, dacts_share = ds, verbose = FALSE)
  mins <- unique(i$lu[, .(match_id, player_id, m = as.numeric(minutes_played))])[m > 0]
  tot <- pay[!is.na(player_id), .(ng = sum(value_own, na.rm = TRUE)), by = player_id]
  merge(tot, mins[, .(mins = sum(m)), by = player_id], by = "player_id")
}

# Repeatability: correlate a player's per-90 in season n with season n+1, over
# every consecutive pair, for players clearing MIN_MINS in BOTH. Per-90 rather
# than season totals, so the correlation is not driven by who played most.
repeatability <- function(sh, ds) {
  tots <- lapply(SEASONS, season_totals, sh = sh, ds = ds)
  names(tots) <- SEASONS
  pairs <- rbindlist(lapply(seq_len(length(SEASONS) - 1L), function(k) {
    a <- tots[[SEASONS[k]]][mins >= MIN_MINS]
    b <- tots[[SEASONS[k + 1L]]][mins >= MIN_MINS]
    m <- merge(a[, .(player_id, p90_1 = ng / mins * 90)],
               b[, .(player_id, p90_2 = ng / mins * 90)], by = "player_id")
    m[, pair := paste(SEASONS[k], SEASONS[k + 1L], sep = " -> ")][]
  }))
  if (nrow(pairs) < 30) return(list(r = NA_real_, n = nrow(pairs), pairs = pairs))
  list(r = stats::cor(pairs$p90_1, pairs$p90_2), n = nrow(pairs), pairs = pairs)
}

grid <- rbind(
  data.table(exec_blame = 0.30, named_share = 0.70, off_pool = 0.10, dacts = 0.50),
  data.table(exec_blame = 0.15, named_share = 0.70, off_pool = 0.10, dacts = 0.50),
  data.table(exec_blame = 0.50, named_share = 0.70, off_pool = 0.10, dacts = 0.50),
  data.table(exec_blame = 0.30, named_share = 0.40, off_pool = 0.10, dacts = 0.50),
  data.table(exec_blame = 0.30, named_share = 1.00, off_pool = 0.10, dacts = 0.50),
  data.table(exec_blame = 0.30, named_share = 0.70, off_pool = 0.00, dacts = 0.50),
  data.table(exec_blame = 0.30, named_share = 0.70, off_pool = 0.30, dacts = 0.50),
  data.table(exec_blame = 0.30, named_share = 0.70, off_pool = 0.10, dacts = 0.00),
  data.table(exec_blame = 0.30, named_share = 0.70, off_pool = 0.10, dacts = 0.25),
  data.table(exec_blame = 0.30, named_share = 0.70, off_pool = 0.10, dacts = 1.00)
)

res <- rbindlist(lapply(seq_len(nrow(grid)), function(i) {
  g <- grid[i]
  cat("setting", i, "of", nrow(grid), "\n"); flush.console()
  r <- repeatability(ng_shares(exec_blame = g$exec_blame,
                               named_share = g$named_share,
                               off_pool = g$off_pool), g$dacts)
  cbind(g, data.table(r = round(r$r, 4), n_pairs = r$n))
}))

cat("\n==== YEAR-OVER-YEAR REPEATABILITY ====\n")
cat("r = correlation of a player's net goals per 90 between consecutive seasons,\n")
cat("for players clearing", MIN_MINS, "minutes in both. Higher is better: skill\n")
cat("persists, noise does not. Seasons:", paste(SEASONS, collapse = ", "), "\n\n")
print(res[order(-r)], row.names = FALSE)

best <- res[which.max(r)]
cat("\nbest:", sprintf("exec_blame %.2f, named_share %.2f, off_pool %.2f, dacts_share %.2f (r = %.4f)",
                       best$exec_blame, best$named_share, best$off_pool, best$dacts, best$r), "\n")
cat("shipped default is exec_blame 0.30, named_share 0.70, off_pool 0.10, dacts_share 0.50\n")
cat("\nA spread of a few thousandths between settings is not a decision. Prefer the\n")
cat("shipped default unless a setting wins by a margin the pair count can support.\n")
fwrite(res, file.path(CACHE, "repeatability.csv"))
