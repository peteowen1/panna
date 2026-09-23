# ng_epr_gate.R -- does EPR built on net goals beat EPR built on production EPV?
#
# The decision point. Net goals is a more correct accounting object than the
# existing credit layer, and that does not entitle it to replace a better
# predictor. This scores both, out of sample, on the only thing a rating is for.
#
# THE METRIC IS LOCKED BEFORE THE RUN, deliberately:
#   primary   -- mean absolute error of predicted match margin, held-out season
#   secondary -- correlation of predicted with actual margin
#   decision  -- net goals ships only if its MAE is LOWER. A tie ships nothing.
#
# WHAT IS HELD CONSTANT. Both arms start from the same SPADL actions and the
# same EPV model, run through the same `calculate_epr_regression()` with the
# same decay and prior, and are scored by the same one-feature margin model
# fitted on the same training matches. The only thing that differs is how an
# action's value is allocated to players. Anything else moving would make the
# comparison meaningless.
#
# WHY A ONE-FEATURE MARGIN MODEL. The production match model carries Elo, team
# RAPM and a dozen other terms, any of which could absorb or mask a change in
# the rating. A single `team EPR difference` feature is a worse predictor in
# absolute terms and a much better instrument for this question, because the
# rating has nowhere to hide.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_epr_gate.R
# Reuses the per-season cache that ng_repeatability.R writes; ~15-25 min cold.

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUE   <- "ENG"
TRAIN    <- c("2022-2023", "2023-2024")
TEST     <- "2024-2025"
MIN_MINS <- 450
DECAY    <- 900L
PRIOR    <- 5
CACHE    <- "data-raw/cache/epv/net-goals"
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

say <- function(...) { cat(..., "\n", sep = ""); flush.console() }

xg_model    <- readRDS("data-raw/cache/epv/xg_model.rds")
xpass_model <- readRDS("data-raw/cache/epv/xpass_model.rds")
epv_model   <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")
fx_all <- as.data.table(load_opta_fixtures(LEAGUE, source = "local"))[
  , .(match_id, match_date, home_team_id, away_team_id, home_score, away_score)]
fx_all[, `:=`(home_score = as.numeric(home_score),
              away_score = as.numeric(away_score),
              match_date = as.Date(substr(match_date, 1, 10)))]

season_inputs <- function(season) {
  f <- file.path(CACHE, paste0("inputs_", LEAGUE, "_", season, ".rds"))
  if (file.exists(f)) return(readRDS(f))
  say("building ", LEAGUE, " ", season, " ...")
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
  saveRDS(out, f); out
}

# ---- the two arms ----------------------------------------------------------
# Arm A: the production credit layer, position-centred the way build_epr_weekly
# receives it (epv_*_adj renamed to the raw names).
arm_production <- function(i, season) {
  cr <- assign_epv_credit(as.data.frame(i$ep), xpass_model)
  pg <- as.data.table(aggregate_player_game_epv(cr, i$lu))
  keep <- intersect(c("player_id", "player_name", "match_id", "team_id",
                      "epv_offensive", "epv_defensive", "total_minutes",
                      "minutes_played"), names(pg))
  pg <- pg[, ..keep]
  if (!"minutes_played" %in% names(pg) && "total_minutes" %in% names(pg)) {
    data.table::setnames(pg, "total_minutes", "minutes_played")
  }
  pg[, season := season]
  posmap <- as.data.table(get_player_positions(i$lu, i$ep))
  ng_adjust_for_rating(pg, posmap, verbose = FALSE)
}

# Arm B: net goals, same centring.
arm_net_goals <- function(i, season) {
  pay <- ng_build_ledger(i$ep, adj = i$adj, fixtures = fx_all, verbose = FALSE)
  pay <- ng_spread_pools(pay, i$ep, i$lu, verbose = FALSE)
  pg  <- ng_player_game(pay, i$lu, verbose = FALSE)
  posmap <- as.data.table(get_player_positions(i$lu, i$ep))
  ng_adjust_for_rating(pg, posmap, verbose = FALSE)
}

seasons <- c(TRAIN, TEST)
inp <- lapply(seasons, season_inputs); names(inp) <- seasons

build_arm <- function(fn, label) {
  say("arm: ", label)
  out <- rbindlist(lapply(seasons, function(s) {
    d <- fn(inp[[s]], s)
    keep <- intersect(c("player_id", "player_name", "match_id", "team_id",
                        "minutes_played", "epv_offensive", "epv_defensive",
                        "season"), names(d))
    d[, ..keep]
  }), fill = TRUE)
  out <- merge(out, fx_all[, .(match_id, match_date)], by = "match_id", all.x = TRUE)
  out[, season_end_year := as.integer(substr(season, 6, 9))]
  out[, league := LEAGUE]
  out[!is.na(match_date) & !is.na(minutes_played)]
}

A <- build_arm(arm_production, "production credit layer")
B <- build_arm(arm_net_goals,  "net goals")
say("rows: production ", format(nrow(A), big.mark = ","),
    ", net goals ", format(nrow(B), big.mark = ","))

# ---- one rating per player as of the test season, then a margin model ------
# Fitted strictly on matches BEFORE the test season starts, so no test-season
# information reaches either the rating or the margin coefficients.
test_start <- min(fx_all[match_id %in% inp[[TEST]]$ep$match_id]$match_date, na.rm = TRUE)
say("test season starts ", test_start)

score_arm <- function(d, label) {
  train_pg <- d[match_date < test_start]
  epr <- as.data.table(calculate_epr_regression(
    train_pg, ref_date = test_start, decay = DECAY, prior_strength = PRIOR,
    alpha = 0, tier_interaction = FALSE, league_offsets = NULL, verbose = FALSE))
  mins <- d[, .(mins = sum(minutes_played, na.rm = TRUE)), by = player_id]
  epr <- merge(epr, mins, by = "player_id", all.x = TRUE)
  epr <- epr[!is.na(epr) & mins >= MIN_MINS]

  # Team strength for a match = mean EPR of the players who actually played,
  # weighted by their minutes in that match. Same construction both arms.
  lineup <- d[, .(match_id, team_id, player_id, minutes_played)]
  lineup <- merge(lineup, epr[, .(player_id, epr)], by = "player_id")
  team <- lineup[, .(s = sum(epr * minutes_played) / sum(minutes_played)),
                 by = .(match_id, team_id)]
  m <- merge(team, fx_all, by = "match_id")
  m <- m[team_id == home_team_id | team_id == away_team_id]
  m[, side := fifelse(team_id == home_team_id, "home", "away")]
  w <- dcast(m, match_id + match_date + home_score + away_score ~ side, value.var = "s")
  w <- w[!is.na(home) & !is.na(away)]
  w[, `:=`(margin = home_score - away_score, epr_diff = home - away)]

  tr <- w[match_date <  test_start]
  te <- w[match_date >= test_start]
  if (nrow(tr) < 50 || nrow(te) < 50) {
    say("  too few matches: train ", nrow(tr), ", test ", nrow(te)); return(NULL)
  }
  fit <- lm(margin ~ epr_diff, tr)
  te[, pred := predict(fit, te)]
  # Keep the per-match errors: the two arms predict the SAME matches, so the
  # comparison is paired and an unpaired look at two MAEs would understate the
  # evidence either way.
  assign(paste0("err_", make.names(label)),
         te[, .(match_id, abs_err = abs(margin - pred))], envir = .GlobalEnv)
  data.table(arm = label, n_train = nrow(tr), n_test = nrow(te),
             mae = mean(abs(te$margin - te$pred)),
             rmse = sqrt(mean((te$margin - te$pred)^2)),
             cor = stats::cor(te$pred, te$margin),
             slope = unname(coef(lm(margin ~ pred, te))[2]),
             n_rated = nrow(epr))
}

# A rating that cannot beat "predict the average margin" is not a rating, and
# both arms should be read against that floor rather than against each other
# alone. The one-feature model is deliberately weak; the floor says how weak.
baseline <- local({
  te <- fx_all[match_id %in% inp[[TEST]]$ep$match_id & !is.na(home_score)]
  tr <- fx_all[match_date < test_start & !is.na(home_score)]
  mu <- mean(tr$home_score - tr$away_score, na.rm = TRUE)
  m <- te$home_score - te$away_score
  data.table(arm = "baseline (train mean margin)", n_train = nrow(tr),
             n_test = length(m), mae = mean(abs(m - mu)),
             rmse = sqrt(mean((m - mu)^2)), cor = NA_real_, slope = NA_real_,
             n_rated = NA_integer_)
})


res <- rbindlist(list(score_arm(A, "production EPV"),
                      score_arm(B, "net goals")), fill = TRUE)

say("\n==== EPR GATE: predicting ", TEST, " match margin out of sample ====")
say("Trained on ", paste(TRAIN, collapse = " + "), ". Lower MAE and RMSE are")
say("better; correlation nearer 1 is better. The decision metric is MAE.\n")
print(rbind(baseline, res, fill = TRUE)[, .(arm, n_test, mae = round(mae, 4),
              rmse = round(rmse, 4), cor = round(cor, 4), slope = round(slope, 3),
              players_rated = n_rated)], row.names = FALSE)

if (nrow(res) == 2) {
  d_mae <- res[arm == "net goals"]$mae - res[arm == "production EPV"]$mae
  say("\nMAE difference (net goals minus production): ", round(d_mae, 4), " goals")
  say(if (d_mae < 0) "NET GOALS WINS on the locked metric."
      else "NET GOALS DOES NOT WIN. On the rule set before the run, it does not ship.")
  # ---- paired, because both arms predict the SAME matches ------------------
  # An unpaired look at two MAEs throws the pairing away and understates the
  # evidence in either direction.
  ea <- get(paste0("err_", make.names("production EPV")), envir = .GlobalEnv)
  eb <- get(paste0("err_", make.names("net goals")), envir = .GlobalEnv)
  pr <- merge(ea, eb, by = "match_id", suffixes = c("_prod", "_ng"))
  pr[, d := abs_err_ng - abs_err_prod]          # negative = net goals closer
  tt <- stats::t.test(pr$d)
  wins <- sum(pr$d < 0); n <- nrow(pr)
  bt <- stats::binom.test(wins, n, 0.5)
  say("")
  say("---- paired over the same ", n, " matches ----")
  say("net goals closer on ", wins, " of ", n,
      sprintf(" (%.1f%%); sign test p = %.4g", 100 * wins / n, bt$p.value))
  say(sprintf("mean paired difference %.4f goals, 95%% CI [%.4f, %.4f], p = %.4g",
              mean(pr$d), tt$conf.int[1], tt$conf.int[2], tt$p.value))
  say(if (tt$conf.int[2] < 0)
        "The interval excludes zero: the improvement is not noise."
      else
        "The interval INCLUDES zero. Not distinguishable from noise -- do not ship on it.")
  fwrite(pr, file.path(CACHE, "epr_gate_paired.csv"))
}
fwrite(res, file.path(CACHE, "epr_gate.csv"))
