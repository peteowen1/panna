# ng_epr_gate_wide.R -- the EPR gate across five leagues, walk-forward.
#
# `ng_epr_gate.R` answered the question on one league and one held-out season
# and came back thin: net goals better by 0.0558 goals of MAE, 95% CI
# [-0.1089, -0.0028], sign test p = 0.099. That clears the locked bar and is
# not enough to call settled. This widens it in the two ways that matter --
# more leagues, and every season tested in turn against everything before it --
# so the answer stops depending on which season happened to be held out.
#
# THE METRIC IS THE SAME ONE, LOCKED BEFORE THE FIRST RUN, and is not revisited
# here: out-of-sample MAE on match margin; net goals ships only if lower.
# Widening the evidence is legitimate; widening it and then choosing a new
# metric because the first one disappointed is not.
#
# MEMORY. A season of actions is large and twenty of them will not sit in
# memory together. Each (league, season) is built, reduced to the two small
# per-game frames, and the heavy objects dropped before the next one starts;
# only the frames are cached.
#
# Run from panna/:  Rscript data-raw/epv/net-goals/ng_epr_gate_wide.R
# Roughly 45-75 minutes cold, a few minutes once the per-game cache exists.

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

LEAGUES  <- c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS  <- c("2021-2022", "2022-2023", "2023-2024", "2024-2025")
MIN_MINS <- 450
DECAY    <- 900L
PRIOR    <- 5
CACHE    <- "data-raw/cache/epv/net-goals"
dir.create(CACHE, recursive = TRUE, showWarnings = FALSE)

say <- function(...) { cat(..., "\n", sep = ""); flush.console() }

xg_model    <- readRDS("data-raw/cache/epv/xg_model.rds")
xpass_model <- readRDS("data-raw/cache/epv/xpass_model.rds")
epv_model   <- readRDS("data-raw/cache/epv/epv_model_xg_clean_full.rds")

fixtures_for <- function(league) {
  f <- as.data.table(load_opta_fixtures(league, source = "local"))[
    , .(match_id, match_date, home_team_id, away_team_id, home_score, away_score)]
  f[, `:=`(home_score = as.numeric(home_score), away_score = as.numeric(away_score),
           match_date = as.Date(substr(match_date, 1, 10)))]
  f[!is.na(home_score)]
}

# ---- one league-season, reduced to two small frames ------------------------
pergame <- function(league, season) {
  f <- file.path(CACHE, sprintf("pg_%s_%s.rds", league, season))
  if (file.exists(f)) return(readRDS(f))
  say("  building ", league, " ", season, " ...")
  out <- tryCatch({
    events  <- load_opta_match_events(league, season = season, source = "local")
    lineups <- as.data.table(load_opta_lineups(league, season = season, source = "local"))
    if (nrow(events) == 0 || nrow(lineups) == 0) stop("no data")
    shot_lk <- panna:::.epv_shot_lookup(league, season)
    spadl <- convert_opta_to_spadl(events)
    ch  <- create_possession_chains(spadl)
    lab <- label_actions_with_outcomes(ch, add_next_chain_outcome(classify_chain_outcomes(ch)))
    lab <- create_next_goal_labels(lab)
    if (epv_model$method == "xg") lab <- create_next_xg_labels(lab)
    ep  <- as.data.table(calculate_action_epv(
      lab, create_epv_features(lab, n_prev = 3), epv_model, xg_model = xg_model,
      league = league, season = season, shot_lookup = shot_lk))
    ep  <- as.data.table(add_xpass_to_spadl(ep, xpass_model))
    adj <- ng_build_adjacency(events, verbose = FALSE)
    fx  <- fixtures_for(league)
    posmap <- as.data.table(get_player_positions(lineups, ep))

    slim <- function(d) {
      d <- as.data.table(d)
      k <- intersect(c("player_id", "player_name", "match_id", "team_id",
                       "minutes_played", "total_minutes",
                       "epv_offensive", "epv_defensive"), names(d))
      d <- d[, ..k]
      if (!"minutes_played" %in% names(d) && "total_minutes" %in% names(d)) {
        setnames(d, "total_minutes", "minutes_played")
      }
      d[, `:=`(league = league, season = season)]
      d[]
    }

    prod_pg <- slim(aggregate_player_game_epv(
      assign_epv_credit(as.data.frame(ep), xpass_model), lineups))
    prod_pg <- slim(ng_adjust_for_rating(prod_pg, posmap, verbose = FALSE))

    ng_pay <- ng_spread_pools(
      ng_build_ledger(ep, adj = adj, fixtures = fx, verbose = FALSE),
      ep, lineups, verbose = FALSE)
    ng_pg <- slim(ng_adjust_for_rating(
      ng_player_game(ng_pay, lineups, verbose = FALSE), posmap, verbose = FALSE))

    list(prod = prod_pg, ng = ng_pg, fx = fx[match_id %in% unique(ep$match_id)])
  }, error = function(e) { say("    skipped: ", conditionMessage(e)); NULL })
  saveRDS(out, f)
  out
}

all <- list()
for (lg in LEAGUES) for (ss in SEASONS) {
  r <- pergame(lg, ss)
  if (!is.null(r)) all[[paste(lg, ss)]] <- r
  gc(verbose = FALSE)
}
say("built ", length(all), " league-seasons")

prod_all <- rbindlist(lapply(all, `[[`, "prod"), fill = TRUE)
ng_all   <- rbindlist(lapply(all, `[[`, "ng"),   fill = TRUE)
fx_all   <- unique(rbindlist(lapply(all, `[[`, "fx"), fill = TRUE), by = "match_id")
for (d in list(prod_all, ng_all)) {
  d[, season_end_year := as.integer(substr(season, 6, 9))]
}
prod_all <- merge(prod_all, fx_all[, .(match_id, match_date)], by = "match_id")
ng_all   <- merge(ng_all,   fx_all[, .(match_id, match_date)], by = "match_id")
say("rows: production ", format(nrow(prod_all), big.mark = ","),
    ", net goals ", format(nrow(ng_all), big.mark = ","),
    " over ", uniqueN(fx_all$match_id), " matches")

# ---- walk-forward ----------------------------------------------------------
# Every season after the first is tested against a rating and a margin model
# fitted only on what came before it. No season is special.
score_fold <- function(d, test_season, label) {
  starts <- d[season == test_season, min(match_date, na.rm = TRUE)]
  tr_pg <- d[match_date < starts]
  if (nrow(tr_pg) < 5000) return(NULL)
  epr <- as.data.table(calculate_epr_regression(
    tr_pg, ref_date = starts, decay = DECAY, prior_strength = PRIOR, alpha = 0,
    tier_interaction = FALSE, league_offsets = NULL, verbose = FALSE))
  mins <- d[, .(mins = sum(minutes_played, na.rm = TRUE)), by = player_id]
  epr <- merge(epr, mins, by = "player_id")[!is.na(epr) & mins >= MIN_MINS]
  if (nrow(epr) < 100) return(NULL)

  lu <- merge(d[, .(match_id, team_id, player_id, minutes_played)],
              epr[, .(player_id, epr)], by = "player_id")
  team <- lu[, .(s = sum(epr * minutes_played) / sum(minutes_played)),
             by = .(match_id, team_id)]
  m <- merge(team, fx_all, by = "match_id")[team_id == home_team_id | team_id == away_team_id]
  m[, side := fifelse(team_id == home_team_id, "home", "away")]
  w <- dcast(m, match_id + match_date + home_score + away_score ~ side, value.var = "s")
  w <- w[!is.na(home) & !is.na(away)]
  w[, `:=`(margin = home_score - away_score, epr_diff = home - away)]

  tr <- w[match_date < starts]; te <- w[match_date >= starts &
                                        match_id %in% d[season == test_season]$match_id]
  if (nrow(tr) < 200 || nrow(te) < 100) return(NULL)
  fit <- lm(margin ~ epr_diff, tr)
  te[, `:=`(pred = predict(fit, te), arm = label, fold = test_season)]
  te[, .(match_id, fold, arm, margin, pred, abs_err = abs(margin - pred))]
}

folds <- SEASONS[-1]
pr <- rbindlist(lapply(folds, function(s) score_fold(prod_all, s, "production EPV")), fill = TRUE)
ng <- rbindlist(lapply(folds, function(s) score_fold(ng_all,   s, "net goals")),      fill = TRUE)

base <- rbindlist(lapply(folds, function(s) {
  starts <- ng_all[season == s, min(match_date, na.rm = TRUE)]
  tr <- fx_all[match_date < starts]; te <- fx_all[match_id %in% pr[fold == s]$match_id]
  if (!nrow(tr) || !nrow(te)) return(NULL)
  mu <- mean(tr$home_score - tr$away_score)
  te[, .(match_id, fold = s, arm = "baseline", margin = home_score - away_score,
         pred = mu, abs_err = abs((home_score - away_score) - mu))]
}), fill = TRUE)

summ <- rbindlist(list(base, pr, ng))[
  , .(n = .N, mae = mean(abs_err), rmse = sqrt(mean((margin - pred)^2)),
      cor = if (stats::sd(pred) > 0) stats::cor(pred, margin) else NA_real_),
  by = .(arm)]

say("\n==== EPR GATE, WIDE: ", length(LEAGUES), " leagues, walk-forward ====")
say("Each season tested against a rating and a margin model fitted only on")
say("earlier seasons. Lower MAE and RMSE are better; cor nearer 1 is better.\n")
print(summ[, .(arm, matches = n, mae = round(mae, 4), rmse = round(rmse, 4),
               cor = round(cor, 4))], row.names = FALSE)

say("\n---- by fold (MAE) ----")
byf <- rbindlist(list(base, pr, ng))[, .(mae = mean(abs_err), n = .N), by = .(fold, arm)]
print(dcast(byf, fold ~ arm, value.var = "mae")[, lapply(.SD, function(x)
  if (is.numeric(x)) round(x, 4) else x)], row.names = FALSE)

p2 <- merge(pr[, .(match_id, fold, e_prod = abs_err)],
            ng[, .(match_id, e_ng = abs_err)], by = "match_id")
p2[, d := e_ng - e_prod]
tt <- stats::t.test(p2$d); bt <- stats::binom.test(sum(p2$d < 0), nrow(p2), 0.5)
say("\n---- paired over the same ", nrow(p2), " matches ----")
say("net goals closer on ", sum(p2$d < 0), " of ", nrow(p2),
    sprintf(" (%.1f%%); sign test p = %.4g", 100 * mean(p2$d < 0), bt$p.value))
say(sprintf("mean paired difference %.4f goals, 95%% CI [%.4f, %.4f], p = %.4g",
            mean(p2$d), tt$conf.int[1], tt$conf.int[2], tt$p.value))
say(if (tt$conf.int[2] < 0) "\nThe interval excludes zero across every league and fold."
    else "\nThe interval INCLUDES zero. Not distinguishable from noise -- do not ship on it.")
fwrite(summ, file.path(CACHE, "epr_gate_wide.csv"))
fwrite(p2, file.path(CACHE, "epr_gate_wide_paired.csv"))
