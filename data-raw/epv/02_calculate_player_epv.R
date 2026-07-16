# 02_calculate_player_epv.R
# Calculate player-level EPV metrics using trained models
#
# DELIBERATELY OPT-IN / MANUAL: feeds the multi-target RAPM cache path in
# 03_splint_creation.R (player_game_epv), which never fires in CI -- the
# per-game value caches this step produces don't exist there (decision
# 2026-07-14).
#
# Run from panna directory: Rscript data-raw/epv/02_calculate_player_epv.R
#
# Requires:
#   - data-raw/cache/epv/xg_model.rds
#   - data-raw/cache/epv/xpass_model.rds
#   - data-raw/cache/epv/epv_model.rds
#
# Outputs:
#   - data-raw/cache/epv/player_epv_{league}_{season}.rds
#   - data-raw/cache/epv/players/player_game_epv_{league}_{season}.rds  (per-game)

library(cli)
devtools::load_all()

# 1. Configuration ----

# Leagues and seasons to process
LEAGUES <- c("ENG", "ESP", "GER", "ITA", "FRA")
SEASONS <- c("2023-2024")

# Minimum minutes for player output
MIN_MINUTES <- 450

# Input/output directories
MODEL_DIR <- "data-raw/cache/epv"
OUTPUT_DIR <- "data-raw/cache/epv/players"
dir.create(OUTPUT_DIR, recursive = TRUE, showWarnings = FALSE)

cli_h1("Calculate Player EPV Values")

# 2. Load Models ----

cli_h2("Step 1: Load Trained Models")

xg_model <- readRDS(file.path(MODEL_DIR, "xg_model.rds"))
xpass_model <- readRDS(file.path(MODEL_DIR, "xpass_model.rds"))
epv_model <- readRDS(file.path(MODEL_DIR, "epv_model.rds"))

cli_alert_success("Models loaded (method: {epv_model$method})")

# 2b. Helper: slim per-action EPV credit stream ----
#
# Feeds the multi-target RAPM per-splint attribution path
# (FABLE-PRIOR-FIX-PLAN.md D1/Step 2). assign_epv_credit() emits
# player_credit/receiver_credit/opponent_credit as three columns on ONE row
# per SPADL action, but receiver_credit and opponent_credit are earned by a
# DIFFERENT player (and usually a different team) than that row's own
# player_id/team_id -- e.g. a completed pass's receiver_credit belongs to
# receiver_player_id, not the passer. This unpivots to one row per (action,
# credited player) so that summing `credit` by (player_id, match_id)
# reproduces aggregate_player_game_epv()'s epv_total EXACTLY -- it mirrors
# that function's own actor + receiver + duel-blame join
# (R/epv_model.R:1389-1455), including its `!is.na(...)` inclusion filters.
#
# F1 (FABLE-PRIOR-FIX-PLAN.md review): the receiver/opponent team_id lookup
# is built per (match_id, player_id) from actor rows -- NOT season-globally
# -- and joined on both keys. A season-global unique(player_id, team_id)
# lookup silently keeps whichever row survives last when a player transfers
# mid-season within the same league, stamping the WRONG (new-club) team_id
# on receiver/opponent credit earned at the OLD club. For a (match, player)
# pair that still has no lookup row (the player never acted -- i.e. never
# had a player_credit row -- in that specific match), fall back to: receiver
# rows inherit the ACTOR's own team_id from that action row (a pass
# receiver is a teammate); opponent rows get the match's OTHER team_id,
# derived from the two distinct team_ids seen among that match's rows (left
# NA if a match somehow doesn't have exactly two).
build_action_epv_credit <- function(spadl_credit) {
  # C5: subset to the columns this function actually reads (~6 of 30+) in
  # the SAME copy as the data.table conversion, instead of as.data.table()
  # dragging every column from spadl_credit along for the ride.
  needed_cols <- intersect(
    c("match_id", "period_id", "time_seconds", "team_id", "player_id",
      "player_credit", "receiver_player_id", "receiver_credit",
      "opponent_player_id", "opponent_credit"),
    names(spadl_credit)
  )
  dt <- data.table::setDT(spadl_credit[needed_cols])
  key_cols <- c("match_id", "period_id", "time_seconds")
  out_cols <- c(key_cols, "team_id", "player_id", "credit")

  actor <- dt[!is.na(player_credit),
              c(key_cols, "team_id", "player_id", "player_credit"), with = FALSE]
  data.table::setnames(actor, "player_credit", "credit")

  # F1: per-(match_id, player_id) team_id lookup, built from actor rows only
  # (the only rows where a player's team_id for THAT match is directly
  # observed).
  pid_team_match <- unique(dt[!is.na(player_id) & !is.na(team_id),
                               .(match_id, player_id, team_id)])

  # F1: the two team_ids per match (from ALL rows' acting team_id, always
  # populated), for the opponent never-acted-this-match fallback. Matches
  # without exactly 2 distinct team_ids are simply absent from team_pair, so
  # the fallback below leaves those rows NA rather than guessing.
  match_teams <- unique(dt[!is.na(team_id), .(match_id, team_id)])
  two_team_matches <- match_teams[, .N, by = match_id][N == 2, match_id]
  match_teams_2 <- match_teams[match_id %in% two_team_matches]
  data.table::setorder(match_teams_2, match_id, team_id)
  match_teams_2[, side := seq_len(.N), by = match_id]
  team_pair <- merge(
    match_teams_2[side == 1, .(match_id, team_1 = team_id)],
    match_teams_2[side == 2, .(match_id, team_2 = team_id)],
    by = "match_id"
  )

  if (all(c("receiver_player_id", "receiver_credit") %in% names(dt))) {
    receiver <- dt[!is.na(receiver_player_id) & !is.na(receiver_credit),
                    c(key_cols, "team_id", "receiver_player_id", "receiver_credit"), with = FALSE]
    data.table::setnames(receiver, "team_id", "actor_team_id")
    data.table::setnames(receiver, c("receiver_player_id", "receiver_credit"),
                          c("player_id", "credit"))
    receiver[pid_team_match, team_id := i.team_id, on = c("match_id", "player_id")]
    # Fallback: receiver never acted in THIS match -> inherits the passer's
    # (actor's) team_id -- a pass receiver is a teammate.
    receiver[is.na(team_id), team_id := actor_team_id]
    receiver[, actor_team_id := NULL]
    receiver <- receiver[, out_cols, with = FALSE]
  } else {
    receiver <- actor[0]
  }

  if (all(c("opponent_player_id", "opponent_credit") %in% names(dt))) {
    opponent <- dt[!is.na(opponent_player_id) & !is.na(opponent_credit),
                    c(key_cols, "team_id", "opponent_player_id", "opponent_credit"), with = FALSE]
    data.table::setnames(opponent, "team_id", "actor_team_id")
    data.table::setnames(opponent, c("opponent_player_id", "opponent_credit"),
                          c("player_id", "credit"))
    opponent[pid_team_match, team_id := i.team_id, on = c("match_id", "player_id")]
    # Fallback: opponent never acted in THIS match -> the match's OTHER
    # team_id relative to the actor (opponent = the opposing team).
    opponent[team_pair, `:=`(.t1 = i.team_1, .t2 = i.team_2), on = "match_id"]
    opponent[is.na(team_id),
             team_id := data.table::fifelse(actor_team_id == .t1, .t2, .t1)]
    opponent[, c("actor_team_id", ".t1", ".t2") := NULL]
    opponent <- opponent[, out_cols, with = FALSE]
  } else {
    opponent <- actor[0]
  }

  out <- data.table::rbindlist(list(actor, receiver, opponent), use.names = TRUE)
  data.table::setorder(out, match_id, period_id, time_seconds)
  out[]
}

# 3. Calculate Player EPV ----

cli_h2("Step 2: Calculate Player EPV")

all_player_epv <- list()

for (league in LEAGUES) {
  for (season in SEASONS) {
    cli_alert_info("Processing {league} {season}...")

    tryCatch({
      # Load data
      events <- load_opta_match_events(league, season = season, source = "local")
      lineups <- load_opta_lineups(league, season = season, source = "local")

      # Convert to SPADL
      spadl <- convert_opta_to_spadl(events)

      # Create chains and labels
      spadl_chains <- create_possession_chains(spadl)
      chain_outcomes <- classify_chain_outcomes(spadl_chains)
      chain_outcomes <- add_next_chain_outcome(chain_outcomes)
      spadl_labeled <- label_actions_with_outcomes(spadl_chains, chain_outcomes)
      spadl_labeled <- create_next_goal_labels(spadl_labeled)

      if (epv_model$method == "xg") {
        spadl_labeled <- create_next_xg_labels(spadl_labeled)
      }

      # Create features and calculate EPV
      epv_features <- create_epv_features(spadl_labeled, n_prev = 3)
      spadl_epv <- calculate_action_epv(spadl_labeled, epv_features, epv_model,
                                        league = league)

      # Assign credit
      spadl_credit <- assign_epv_credit(spadl_epv, xpass_model)

      # Persist slim per-action EPV credit stream (see build_action_epv_credit()
      # above) for the multi-target RAPM per-splint attribution path.
      action_epv_credit <- build_action_epv_credit(spadl_credit)
      action_epv_file <- file.path(OUTPUT_DIR,
                                    sprintf("player_action_epv_%s_%s.parquet", league, season))
      arrow::write_parquet(action_epv_credit, action_epv_file)
      cli_alert_info("  {nrow(action_epv_credit)} action-credit rows -> {action_epv_file}")

      # Aggregate to player level (season totals)
      player_epv <- aggregate_player_epv(spadl_credit, lineups, min_minutes = MIN_MINUTES)
      player_epv$league <- league
      player_epv$season <- season

      # Aggregate to player-game level (one row per player per match)
      player_game_epv <- aggregate_player_game_epv(spadl_credit, lineups)
      player_game_epv$league <- league
      player_game_epv$season <- season

      # Save both
      output_file <- file.path(OUTPUT_DIR, sprintf("player_epv_%s_%s.rds", league, season))
      saveRDS(player_epv, output_file)

      game_output_file <- file.path(OUTPUT_DIR, sprintf("player_game_epv_%s_%s.rds", league, season))
      saveRDS(player_game_epv, game_output_file)

      all_player_epv[[paste(league, season)]] <- player_epv
      cli_alert_success("  {nrow(player_epv)} players, {nrow(player_game_epv)} player-games saved")

    }, error = function(e) {
      cli_alert_warning("  Skipping: {e$message}")
    })
  }
}

# 4. Combine Results ----

cli_h2("Step 3: Combine Results")

combined <- do.call(rbind, all_player_epv)
saveRDS(combined, file.path(OUTPUT_DIR, "player_epv_all.rds"))

cli_alert_success("Combined: {nrow(combined)} player-seasons")

# 5. Summary ----

cli_h1("Complete!")

cat("\nTop 20 Players by EPV per 90:\n")
top_p90 <- head(combined[order(-combined$epv_total_p90), ], 20)
print(top_p90[, c("player_name", "league", "season", "total_minutes",
                   "epv_total_p90", "epv_passing", "epv_shooting", "epv_defending")])
