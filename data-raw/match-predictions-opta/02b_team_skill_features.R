# 02b_team_skill_features.R
# Compute team-level skill features for the match dataset
#
# For each played match, computes skill estimates for the starting XI at
# the match date, then aggregates to team-level attacking/defensive skill
# features. For fixtures, uses the most recent skill estimates.
#
# These features supplement the panna/Elo/rolling features with granular
# skill information (e.g., team average shooting skill, tackling skill).

# 1. Setup ----

# 2. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
skill_cache_dir <- file.path("data-raw", "cache-skills")
output_path <- file.path(cache_dir, "02b_team_skill_features.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 02b_team_skill_features.rds")
  team_skill_features <- readRDS(output_path)
  message(sprintf("  %d matches with team skill features", nrow(team_skill_features)))
  return(invisible(NULL))
}

# 4. Check Prerequisites ----

match_stats_path <- file.path(skill_cache_dir, "01_match_stats.rds")
decay_params_path <- file.path(skill_cache_dir, "02b_decay_params.rds")

if (!file.exists(match_stats_path)) {
  warning("Skill match stats not found - model will train WITHOUT skill features. ",
          "Run the estimated skills pipeline first.", call. = FALSE)
  team_skill_features <- NULL
  saveRDS(team_skill_features, output_path)
  return(invisible(NULL))
}

message("\n=== Computing Team-Level Skill Features ===\n")

# 5. Load Data ----

fixture_results <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
match_stats <- readRDS(match_stats_path)
decay_params <- if (file.exists(decay_params_path)) readRDS(decay_params_path) else NULL

played <- fixture_results[fixture_results$match_status == "Played", ]
upcoming <- fixture_results[fixture_results$match_status != "Played", ]

# Load lineups
rapm_cache <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
if (file.exists(rapm_cache)) {
  message("  Loading lineups from RAPM cache...")
  raw_data <- readRDS(rapm_cache)
  lineups <- raw_data$lineups
  rm(raw_data); gc(verbose = FALSE)
} else {
  # GHA mode: load from consolidated Opta data
  message("  Loading lineups from consolidated Opta data...")
  leagues <- unique(played$league)
  all_lineups <- list()
  for (league in leagues) {
    available_seasons <- tryCatch(list_opta_seasons(league, source = "local"), error = function(e) character(0))
    for (season in available_seasons) {
      tryCatch({
        lu <- load_opta_lineups(league, season = season, source = "local")
        if (!is.null(lu) && nrow(lu) > 0) {
          lu$league <- league
          lu$season <- season
          all_lineups[[paste(league, season)]] <- lu
        }
      }, error = function(e) {
        message(sprintf("  Warning: failed to load lineups for %s %s: %s", league, season, e$message))
        NULL
      })
    }
  }
  lineups <- bind_rows(all_lineups)
  if (nrow(lineups) == 0) {
    warning("No lineups loaded - model will train WITHOUT skill features.", call. = FALSE)
    team_skill_features <- NULL
    saveRDS(team_skill_features, output_path)
    return(invisible(NULL))
  }
}

message(sprintf("  Played matches: %d", nrow(played)))
message(sprintf("  Match stats rows: %d", nrow(match_stats)))

# panna#128 pattern (second site): the per-SEY loop below hands match_stats to
# estimate_player_skills(), whose date bracket-filter copies a nearly
# full-width table every iteration — and 01_match_stats.rds carries 400+
# box-score/metadata columns while skill estimation only reads identity +
# stat + denominator columns. After the #127 full-sync fix grew the cache
# ~1.2M -> ~1.9M rows, those wide per-season copies OOM-killed the 16GB GHA
# predict job (runs 2026-07-05/06). Narrow ONCE here via [[-extraction (a
# bracket-select would itself copy the wide table); the fixtures path further
# down inherits the narrow table. Column references are shared, not copied,
# so this is near-free; the dropped columns free at the next gc().
stat_cols_02b <- .detect_skill_stat_cols(match_stats)
needed_cols_02b <- .compute_snapshot_loop_columns(
  available_cols = names(match_stats),
  stat_cols = stat_cols_02b,
  extra_cols = c("player_id", "player_name", "match_date", "position",
                 "total_minutes")
)
match_stats <- data.table::setDT(stats::setNames(
  lapply(needed_cols_02b, function(cc) match_stats[[cc]]),
  needed_cols_02b
))
gc(verbose = FALSE)
message(sprintf("  Narrowed match_stats to %d columns for the skill loops",
                ncol(match_stats)))

# 6. Compute Seasonal Skill Estimates ----

# Instead of computing per-match-date (very expensive), compute per season end
# and use those for all matches in that season
sey_values <- sort(unique(played$season_end_year))
all_skill_features <- list()
n_failed <- 0L

for (sey in sey_values) {
  # Use season START (previous July) to avoid data leakage: skills estimated
  # before the season began, so no future information enters the training set
  target_date <- as.Date(paste0(sey - 1, "-07-01"))

  tryCatch({
    skills <- estimate_player_skills(
      match_stats = match_stats,
      decay_params = decay_params,
      target_date = target_date
    )

    if (is.null(skills) || nrow(skills) == 0) next

    # Get lineups for this season's matches
    matches_sey <- played[played$season_end_year == sey, ]
    match_ids <- unique(matches_sey$match_id)
    lu_sey <- lineups[lineups$match_id %in% match_ids, ]

    if (nrow(lu_sey) == 0) next

    # Aggregate skills to team level
    tr <- aggregate_lineup_skills(lu_sey, skills)
    if (!is.null(tr) && nrow(tr) > 0) {
      all_skill_features[[as.character(sey)]] <- tr
      # Report per-season NA rate on a representative column so we can see
      # at a glance when skill coverage was thin (early seasons have less
      # because the underlying match_stats cache starts mid-2014).
      rep_col <- intersect("home_sk_att_goals", names(tr))
      na_rate <- if (length(rep_col) > 0L) {
        sprintf("%.0f%% NA", 100 * mean(is.na(tr[[rep_col]])))
      } else "n/a"
      message(sprintf("    SEY %d: %d matches with skill features (%s on home_sk_att_goals)",
                      sey, nrow(tr), na_rate))
    }
  }, error = function(e) {
    n_failed <<- n_failed + 1L
    message(sprintf("    SEY %d ERROR: %s", sey, e$message))
  })
  gc(verbose = FALSE)
}

if (n_failed > 0) {
  warning(sprintf("%d/%d season-end-years failed skill estimation.",
                  n_failed, length(sey_values)), call. = FALSE)
}

# 7. Handle Fixtures ----

# Compute latest lineups before freeing the full lineups object
latest_lineups <- NULL
if (nrow(upcoming) > 0) {
  latest_lineups <- lineups %>%
    filter(is_starter) %>%
    group_by(team_id) %>%
    filter(match_date == max(match_date)) %>%
    ungroup()
}
rm(lineups); gc(verbose = FALSE)

if (nrow(upcoming) > 0 && !is.null(latest_lineups)) {
  # Filter match_stats to only players in upcoming lineups (memory optimization)
  upcoming_player_ids <- unique(latest_lineups$player_id)
  if (length(upcoming_player_ids) > 0 && "player_id" %in% names(match_stats)) {
    n_before <- nrow(match_stats)
    ms_fixture <- match_stats[match_stats$player_id %in% upcoming_player_ids, ]
    message(sprintf("  Filtered match_stats: %d -> %d rows (%d players)",
                    n_before, nrow(ms_fixture), length(upcoming_player_ids)))
  } else {
    ms_fixture <- match_stats
  }
  rm(match_stats); gc(verbose = FALSE)

  tryCatch({
    # Use current date skills for fixtures
    live_skills <- estimate_player_skills(
      match_stats = ms_fixture,
      decay_params = decay_params,
      target_date = Sys.Date()
    )
    rm(ms_fixture); gc(verbose = FALSE)

    if (!is.null(live_skills) && nrow(live_skills) > 0) {

      # make_dummy_lineup() is defined in R/match_prediction.R

      # WC2026 announced-squad override (same parquet as step 02). Built by
      # data-raw/match-predictions-opta/announced_squads.R. All 26 squad
      # members flow through with lineup_weight = expected_minutes_norm/90
      # so the team-skill aggregation in R/match_prediction.R is
      # minute-weighted instead of equal-weighting a synthetic XI.
      ann_squads_path <- file.path(cache_dir, "wc2026_announced_squads.parquet")
      ann_squads <- if (file.exists(ann_squads_path) &&
                        requireNamespace("arrow", quietly = TRUE)) {
        s <- as.data.frame(arrow::read_parquet(ann_squads_path))
        s <- s[!is.na(s$player_id), , drop = FALSE]
        s$lineup_weight <- pmax(0, s$expected_minutes_norm) / 90
        s
      } else NULL

      # Apply the same min-resolved threshold as step 02 — keeps the two
      # paths' override eligibility identical so team_skill_features and
      # team_ratings never disagree about which WC2026 teams used the
      # override.
      wc2026_eligible_ids <- character(0)
      if (!is.null(ann_squads)) {
        per_team_resolved <- as.data.frame(table(ann_squads$team_id))
        names(per_team_resolved) <- c("team_id", "n_resolved")
        wc2026_eligible_ids <- as.character(
          per_team_resolved$team_id[per_team_resolved$n_resolved >= WC2026_OVERRIDE_MIN_RESOLVED]
        )
      }

      n_wc_override <- 0L
      lu_or_override <- function(team_id, team_name, team_pos, m) {
        use_override <- !is.null(ann_squads) && !is.na(team_id) &&
          isTRUE(!is.na(m$league) && m$league == WC2026_LEAGUE &&
                 !is.na(m$season) && m$season == WC2026_SEASON_LABEL) &&
          team_id %in% wc2026_eligible_ids
        if (use_override) {
          ann <- ann_squads[ann_squads$team_id == team_id, , drop = FALSE]
          n_wc_override <<- n_wc_override + 1L
          return(data.frame(
            match_id        = m$match_id,
            team_id         = team_id,
            team_name       = team_name,
            team_position   = team_pos,
            player_id       = ann$player_id,
            player_name     = ann$player_name,
            position        = ann$position,
            is_starter      = TRUE,
            is_starter_pred = if ("is_starter_pred" %in% names(ann)) ann$is_starter_pred else FALSE,
            lineup_weight   = ann$lineup_weight,
            stringsAsFactors = FALSE
          ))
        }
        latest_lineups %>%
          filter(team_id == !!team_id) %>%
          mutate(match_id = m$match_id, team_position = team_pos,
                 team_name = !!team_name)
      }

      # Build fixture lineups
      fixture_lu_list <- list()
      for (i in seq_len(nrow(upcoming))) {
        m <- upcoming[i, ]
        htid <- m$home_team_id
        atid <- m$away_team_id
        if (is.na(htid) || htid == "" || is.na(atid) || atid == "") next

        home_lu <- lu_or_override(htid, m$home_team, "home", m)
        away_lu <- lu_or_override(atid, m$away_team, "away", m)

        if (nrow(home_lu) == 0) {
          home_lu <- make_dummy_lineup(m$match_id, htid, m$home_team, "home")
        }
        if (nrow(away_lu) == 0) {
          away_lu <- make_dummy_lineup(m$match_id, atid, m$away_team, "away")
        }

        fixture_lu_list[[i]] <- bind_rows(home_lu, away_lu)
      }

      if (length(fixture_lu_list) > 0) {
        fixture_lu <- bind_rows(fixture_lu_list)
        fixture_tr <- aggregate_lineup_skills(fixture_lu, live_skills)
        if (!is.null(fixture_tr) && nrow(fixture_tr) > 0) {
          all_skill_features[["fixtures"]] <- fixture_tr
          message(sprintf("  Added %d fixture skill features", nrow(fixture_tr)))
        }
      }
      if (n_wc_override > 0) {
        message(sprintf("  Applied WC2026 announced-squad override to %d team-fixtures (skill features)",
                        n_wc_override))
      }
    }
  }, error = function(e) {
    warning(sprintf("Fixture skills failed: %s (using seasonal only)", e$message),
            call. = FALSE)
  })
}

# 8. Combine and Save ----

if (length(all_skill_features) > 0) {
  team_skill_features <- bind_rows(all_skill_features)
  message(sprintf("\nTotal: %d matches with team skill features (%d columns)",
                  nrow(team_skill_features), ncol(team_skill_features)))
} else {
  warning("No team skill features computed - model will train WITHOUT skill features.",
          call. = FALSE)
  team_skill_features <- NULL
}

saveRDS(team_skill_features, output_path)
message(sprintf("Saved to: %s", output_path))
