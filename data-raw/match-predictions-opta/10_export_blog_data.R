# 10_export_blog_data.R
# Export player ratings and match predictions for the blog
#
# Produces two parquet files and uploads them to the blog-latest
# release on peteowen1/pannadata for the blog to consume directly.

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
if (!exists("use_skill_ratings")) use_skill_ratings <- TRUE
repo <- "peteowen1/pannadata"
tag <- "blog-latest"

ratings_output <- file.path(cache_dir, "panna_ratings.parquet")
predictions_output <- file.path(cache_dir, "match_predictions.parquet")

# 3. Build Ratings Parquet ----

message("\n=== Building Blog Ratings ===\n")

# Load seasonal ratings: prefer skill-based (same logic as step 02)
skill_ratings_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
raw_ratings_path <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")

if (isTRUE(use_skill_ratings) && file.exists(skill_ratings_path)) {
  ratings_path <- skill_ratings_path
  message("  USING: SKILL-BASED ratings (cache-skills/06_seasonal_ratings.rds)")
} else if (file.exists(raw_ratings_path)) {
  ratings_path <- raw_ratings_path
  if (isTRUE(use_skill_ratings)) {
    warning("Skill ratings not found. Falling back to raw-stat ratings.", call. = FALSE, immediate. = TRUE)
    message("  USING: RAW-STAT ratings (FALLBACK - skill ratings not found)")
  } else {
    message("  USING: RAW-STAT ratings (use_skill_ratings = FALSE)")
  }
} else {
  stop("No ratings cache found. Run the Opta RAPM pipeline first.")
}

seasonal_results <- load_cache_with_meta(ratings_path, max_age_hours = 336)
if (!is.list(seasonal_results) ||
    is.null(seasonal_results$seasonal_xrapm) ||
    is.null(seasonal_results$seasonal_spm)) {
  stop(sprintf("Ratings cache at '%s' has unexpected structure. Rebuild the ratings pipeline.", ratings_path))
}
latest_season <- max(seasonal_results$seasonal_xrapm$season_end_year)

message(sprintf("  Latest season end year: %d", latest_season))

# Determine dedup/join key based on available columns
dedup_key <- if ("player_id" %in% names(seasonal_results$seasonal_xrapm)) "player_id" else "player_name"

# Filter xRAPM to latest season, drop the synthetic replacement-pool row,
# and deduplicate to one row per player.
# Why drop "replacement": rapm_matrix.R deliberately creates a synthetic
# player_id == "replacement" representing all <200-min players pooled. It's
# a model artifact (picks up uncontrolled game-state variance), not a
# coherent player rating, so it should never appear on the blog leaderboard.
seasonal_xrapm <- seasonal_results$seasonal_xrapm %>%
  filter(season_end_year == latest_season,
         !player_id %in% c("replacement"),
         !player_name %in% c("Replacement Level")) %>%
  group_by(.data[[dedup_key]]) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup()

# Filter SPM to latest season, drop replacement-pool row, deduplicate.
seasonal_spm <- seasonal_results$seasonal_spm %>%
  filter(season_end_year == latest_season,
         !player_id %in% c("replacement"),
         !player_name %in% c("Replacement Level")) %>%
  group_by(.data[[dedup_key]]) %>%
  slice_max(total_minutes, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(all_of(dedup_key), spm_overall = spm)

message(sprintf("  xRAPM players: %d (joined by %s)", nrow(seasonal_xrapm), dedup_key))
message(sprintf("  SPM players: %d", nrow(seasonal_spm)))

if (nrow(seasonal_xrapm) == 0) {
  stop(sprintf("No xRAPM data for season_end_year = %d. Check the ratings cache.", latest_season))
}

# OVERRIDE total_minutes from the authoritative source (opta_player_stats).
# The xRAPM/skills caches have an upstream bug (see panna issue for Salah
# 2026 showing 511 min in release cache 06_seasonal_ratings.rds when the
# truth is 3058 min across 43 matches). total_minutes is a simple sum, not
# a model output — derive it from per-match data directly rather than
# trusting cache provenance.
ps_path <- file.path(opta_data_dir(), "opta_player_stats.parquet")
if (!file.exists(ps_path)) {
  stop(sprintf("opta_player_stats.parquet missing at %s — required to derive total_minutes",
               ps_path), call. = FALSE)
}
ps <- as.data.frame(arrow::read_parquet(ps_path,
  col_select = c("player_id","season","minsPlayed")))
latest_season_str <- sprintf("%d-%d", latest_season - 1, latest_season)
ps_minutes <- ps %>%
  filter(season == latest_season_str, !is.na(player_id)) %>%
  group_by(player_id) %>%
  summarise(total_minutes_real = sum(minsPlayed, na.rm = TRUE),
            .groups = "drop")
message(sprintf("  Derived total_minutes for %d players from %s (season %s)",
                nrow(ps_minutes), basename(ps_path), latest_season_str))

# Join + compute ranks. Sign convention for the published file: POSITIVE =
# good for both offense and defense (matches torpverse, NBA RAPM, and
# consumer intuition). Internally the model treats `defense` as "additive
# contribution to opponent xG" where negative = good defender; we flip the
# sign here so the blog shows `defense` as "defensive value added (xG
# suppression per 90)" — positive = good. `panna` is unchanged because
# panna = offense - defense_internal = offense + defense_published.
#
# Minimum-minutes threshold for the panna_rank leaderboard. Without this,
# low-sample players (e.g., Salah at 500 cache-minutes-bug with xrapm=0.26)
# top the leaderboard from a handful of hot games. 900 min ≈ 10 full games
# = enough to estimate xrapm with reasonable confidence. Players below the
# threshold still appear in the parquet but with NA panna_rank.
MIN_MINUTES_FOR_RANK <- 900L

panna_ratings <- seasonal_xrapm %>%
  left_join(seasonal_spm, by = dedup_key) %>%
  left_join(ps_minutes, by = "player_id") %>%
  # Use the derived total_minutes; keep cache total_minutes as fallback
  # for players with no opta_player_stats row this season (rare; intl-only
  # players whose 2025-26 friendlies got filtered).
  mutate(total_minutes = coalesce(total_minutes_real, total_minutes)) %>%
  select(-total_minutes_real)

# Rank within the qualified subset; assign NA to sub-threshold players
qualified <- panna_ratings$total_minutes >= MIN_MINUTES_FOR_RANK
panna_ratings$panna_rank <- NA_integer_
panna_ratings$panna_percentile <- NA_real_
if (sum(qualified) > 0) {
  q_xrapm <- panna_ratings$xrapm[qualified]
  panna_ratings$panna_rank[qualified] <-
    as.integer(rank(-q_xrapm, ties.method = "min"))
  panna_ratings$panna_percentile[qualified] <-
    round(100 * rank(q_xrapm, ties.method = "min") / sum(qualified), 1)
}
message(sprintf("  Ranked %d players with total_minutes >= %d (of %d total)",
                sum(qualified), MIN_MINUTES_FOR_RANK, nrow(panna_ratings)))

panna_ratings <- panna_ratings %>%
  select(
    panna_rank,
    any_of("player_id"),
    player_name,
    panna = xrapm,
    offense,
    defense,
    spm_overall,
    total_minutes,
    panna_percentile
  ) %>%
  mutate(defense = -defense) %>%
  mutate(across(c(panna, offense, defense, spm_overall), ~round(.x, 4))) %>%
  arrange(panna_rank)

# Validation guard: a top-50 ranked player with implausibly low minutes is
# a strong signal the upstream cache is corrupted (skills/06_seasonal
# repeatedly ships wrong total_minutes for some players). After the
# total_minutes override above this should never trigger — keep the check
# as belt-and-braces for future cache schema changes.
top50 <- panna_ratings %>% filter(!is.na(panna_rank), panna_rank <= 50L)
top50_bad <- top50 %>% filter(total_minutes < MIN_MINUTES_FOR_RANK)
if (nrow(top50_bad) > 0L) {
  stop(sprintf(
    "%d top-50 panna_rank player(s) have total_minutes < %d after override — refusing to publish a leaderboard that surfaces low-sample players.\n  %s",
    nrow(top50_bad), MIN_MINUTES_FOR_RANK,
    paste(sprintf("  #%d %s (%.0f min)",
                  top50_bad$panna_rank, top50_bad$player_name,
                  top50_bad$total_minutes),
          collapse = "\n")
  ), call. = FALSE)
}

na_spm <- sum(is.na(panna_ratings$spm_overall))
if (na_spm > 0) message(sprintf("  Note: %d players have no SPM rating (NA)", na_spm))
message(sprintf("  Final ratings: %d players (%d ranked)", nrow(panna_ratings),
                sum(!is.na(panna_ratings$panna_rank))))
top1 <- panna_ratings %>% filter(panna_rank == 1L)
if (nrow(top1) > 0) {
  message(sprintf("  Top player: %s (panna=%.3f, %.0f min)",
                  top1$player_name, top1$panna, top1$total_minutes))
}

arrow::write_parquet(panna_ratings, ratings_output)
message(sprintf("  Written: %s", ratings_output))

# 4. Build Predictions Parquet ----

message("\n=== Building Blog Predictions ===\n")

predictions_input <- file.path(cache_dir, "predictions.parquet")
if (!file.exists(predictions_input)) {
  stop("Missing cache-predictions-opta/predictions.parquet. Run the prediction pipeline (07_predict_fixtures) first.")
}

predictions <- arrow::read_parquet(predictions_input)

# Required columns the blog cannot function without.
# `status` ("played"/"fixture") is a required contract for sim consumers:
# predictions.parquet covers every historical match plus upcoming fixtures
# (the blog's Results view shows past predictions alongside projections), so
# sims must filter to status == "fixture" to avoid re-simulating completed
# matches. Hard-fail here rather than fall through to date-based guessing.
required_cols <- c("match_id", "match_date", "league", "home_team", "away_team",
                   "prob_H", "prob_D", "prob_A", "status")
missing_required <- setdiff(required_cols, names(predictions))
if (length(missing_required) > 0) {
  stop(sprintf("Missing required prediction columns: %s\nRebuild predictions with the current pipeline.",
               paste(missing_required, collapse = ", ")))
}

# Optional columns — include if available.
optional_cols <- c("season", "pred_home_goals", "pred_away_goals", "predicted_result")
missing_optional <- setdiff(optional_cols, names(predictions))
if (length(missing_optional) > 0) {
  message(sprintf("  Note: Optional columns missing (excluded): %s",
                  paste(missing_optional, collapse = ", ")))
}
pred_cols <- intersect(c(required_cols, optional_cols), names(predictions))

match_predictions <- predictions %>% select(all_of(pred_cols))

if (nrow(match_predictions) == 0) {
  stop("No predictions to export. Run the prediction pipeline (step 07) first.")
}

message(sprintf("  %d predictions across %d leagues",
                nrow(match_predictions), length(unique(match_predictions$league))))
message(sprintf("  Date range: %s to %s",
                min(match_predictions$match_date), max(match_predictions$match_date)))

arrow::write_parquet(match_predictions, predictions_output)
message(sprintf("  Written: %s", predictions_output))

# 4b. Build Season Standings Parquet ----

message("\n=== Building Season Standings ===\n")

standings_output <- file.path(cache_dir, "season_standings.parquet")

# Load fixture results from step 01 cache
fixture_results_path <- file.path(cache_dir, "01_fixture_results.rds")
if (!file.exists(fixture_results_path)) {
  warning("01_fixture_results.rds not found — skipping standings export.", call. = FALSE)
  standings_ok <- FALSE
} else {
  fixture_results <- readRDS(fixture_results_path)

  # Leagues included in standings (domestic + UEFA cups for simulation scripts)
  standings_leagues <- c("ENG", "ENG2", "ESP", "FRA", "GER", "ITA", "NED", "POR", "SCO", "TUR",
                         "UCL", "UEL", "UECL")

  played <- fixture_results %>%
    filter(match_status == "Played", league %in% standings_leagues,
           !is.na(home_goals), !is.na(away_goals))

  # Current season only (same as predictions)
  if ("season" %in% names(played)) {
    latest <- max(played$season, na.rm = TRUE)
    played <- played %>% filter(season == latest)
  } else if ("season_end_year" %in% names(played)) {
    latest <- max(played$season_end_year, na.rm = TRUE)
    played <- played %>% filter(season_end_year == latest)
  }

  message(sprintf("  Played matches (current season): %d", nrow(played)))

  # Compute standings from home and away perspectives
  home_stats <- played %>%
    group_by(league, team = home_team) %>%
    summarise(gp = n(),
              pts = sum(ifelse(home_goals > away_goals, 3L,
                               ifelse(home_goals == away_goals, 1L, 0L))),
              gf = sum(home_goals), ga = sum(away_goals), .groups = "drop")

  away_stats <- played %>%
    group_by(league, team = away_team) %>%
    summarise(gp = n(),
              pts = sum(ifelse(away_goals > home_goals, 3L,
                               ifelse(away_goals == home_goals, 1L, 0L))),
              gf = sum(away_goals), ga = sum(home_goals), .groups = "drop")

  season_standings <- bind_rows(home_stats, away_stats) %>%
    group_by(league, team) %>%
    summarise(games_played = sum(gp), points = sum(pts),
              gf = sum(gf), ga = sum(ga), .groups = "drop") %>%
    mutate(gd = gf - ga) %>%
    select(league, team, games_played, points, gd) %>%
    arrange(league, desc(points), desc(gd))

  message(sprintf("  Teams with standings: %d across %d leagues",
                  nrow(season_standings), length(unique(season_standings$league))))

  # Self-consistency invariants on the aggregation. Pure arithmetic on the
  # same inputs step 10 already filtered; a violation means step 10's grouping
  # or summation has a bug. Provider drift (stale Opta, FD.org disagreement)
  # cannot trip these — only panna's own code can.
  #
  # - sum(games_played) == 2 * n_matches per league (each match adds to two gp)
  # - sum(gd) == 0 per league (zero-sum across the league)
  # - sum(points) == 2*n_draws + 3*n_decisives per league (2 pts distributed on
  #   a draw, 3 on a decisive). Assumes the standard 3-1-0 with no modeled
  #   administrative point deductions — currently true across all our leagues.
  per_league_stats <- played %>%
    group_by(league) %>%
    summarise(n_matches = n(),
              n_draw = sum(home_goals == away_goals),
              n_decisive = sum(home_goals != away_goals),
              .groups = "drop")

  totals <- season_standings %>%
    group_by(league) %>%
    summarise(sum_gp = sum(games_played), sum_pts = sum(points),
              sum_gd = sum(gd), n_teams = n(), .groups = "drop") %>%
    left_join(per_league_stats, by = "league") %>%
    mutate(
      expected_gp = 2L * n_matches,
      expected_pts = 2L * n_draw + 3L * n_decisive,
      gp_ok = sum_gp == expected_gp,
      pts_ok = sum_pts == expected_pts,
      gd_ok = sum_gd == 0
    )

  failed <- totals %>% filter(!gp_ok | !pts_ok | !gd_ok)
  if (nrow(failed) > 0) {
    message("  STANDINGS INVARIANT VIOLATION — aggregation bug in step 10:")
    for (i in seq_len(nrow(failed))) {
      r <- failed[i, ]
      message(sprintf(
        "    %s: sum_gp=%d (expected %d%s), sum_pts=%d (expected %d%s), sum_gd=%d (expected 0%s)",
        r$league, r$sum_gp, r$expected_gp, if (r$gp_ok) "" else " [FAIL]",
        r$sum_pts, r$expected_pts, if (r$pts_ok) "" else " [FAIL]",
        r$sum_gd, if (r$gd_ok) "" else " [FAIL]"))
    }
    # Hard-fail rather than warn: if the invariants trip, panna's aggregation
    # is broken and publishing the file would ship known-wrong standings to
    # the blog. Safer to block the upload and surface the bug.
    stop(sprintf("Standings invariants failed for %d league(s). Fix aggregation logic before republishing.",
                 nrow(failed)), call. = FALSE)
  } else if (nrow(totals) > 0) {
    message(sprintf("  Invariants OK across all %d leagues (gp, pts, gd sums consistent)",
                    nrow(totals)))
  }

  # Per-team schedule completeness — catches upstream Opta-scraper gaps that
  # the prior invariants (which only validate aggregation arithmetic) can't
  # see. A team with 37 fixtures in a 38-match league means the scraper lost
  # a match somewhere (e.g., Opta API pagination cap — see pannadata scraper
  # date-window logic). The sim then projects over the wrong number of
  # remaining games, producing "current + projected" totals that can't match
  # a fully-played season.
  #
  # Only applied to current-season domestic leagues with a fixed per-team
  # match count. UEFA cups (UCL/UEL/UECL) have variable per-team totals
  # (Swiss league phase + knockout path), so we skip them here.
  # Expected matches per team = 2 × (n_teams − 1) for a double round-robin,
  # with special cases for leagues that use a split format.
  #   SCO: 12 teams × 3 round-robins (33) + 5 post-split matches = 38
  LEAGUE_EXPECTED_MATCHES_PER_TEAM <- list(
    ENG = 38L, ENG2 = 46L, ESP = 38L, FRA = 34L, GER = 34L,
    ITA = 38L, NED = 34L, POR = 34L, SCO = 38L, TUR = 34L
  )

  current_fixtures_all <- fixture_results
  if ("season" %in% names(current_fixtures_all)) {
    current_fixtures_all <- current_fixtures_all %>% filter(season == latest)
  } else if ("season_end_year" %in% names(current_fixtures_all)) {
    current_fixtures_all <- current_fixtures_all %>% filter(season_end_year == latest)
  }

  team_totals <- bind_rows(
    current_fixtures_all %>%
      filter(league %in% names(LEAGUE_EXPECTED_MATCHES_PER_TEAM)) %>%
      select(league, team = home_team),
    current_fixtures_all %>%
      filter(league %in% names(LEAGUE_EXPECTED_MATCHES_PER_TEAM)) %>%
      select(league, team = away_team)
  ) %>%
    filter(!is.na(team), team != "") %>%
    count(league, team, name = "total_fixtures") %>%
    mutate(expected = vapply(league,
                             function(l) LEAGUE_EXPECTED_MATCHES_PER_TEAM[[l]],
                             integer(1)),
           diff = total_fixtures - expected)

  # Split into two cases:
  #   diff <  0 → real signal (team missing fixtures, probable Opta scraper gap)
  #   diff >  0 → benign overcount (playoff fixtures included under the league
  #               code). ENG2 has EFL playoffs (semi legs + Wembley final =
  #               up to +3 for the finalists, +2 for semi losers); NED has
  #               European-spot playoffs (up to +2 for finalists, +1 for semi
  #               losers). Audited 2026-05-29 via debug/keep/schedule_gap_audit.R
  #               — all 8 ENG2/NED overcounts that day matched the playoff
  #               bracket exactly, only the 2 FRA Nantes/Toulouse undercounts
  #               were genuine scraper misses.
  missing_fixtures <- team_totals %>% filter(diff < 0L)
  extra_fixtures   <- team_totals %>% filter(diff > 0L)

  if (nrow(missing_fixtures) > 0) {
    # Real upstream gap. Warn loudly — but don't block: the sim still runs
    # against what fixtures exist, just over the wrong remaining-games count
    # for affected teams. Hard-fail would dark all domestic leagues over a
    # transient one-league scraper issue.
    message(sprintf("  SCHEDULE GAP — %d team(s) MISSING fixtures (likely Opta scraper gap):",
                    nrow(missing_fixtures)))
    for (i in seq_len(nrow(missing_fixtures))) {
      r <- missing_fixtures[i, ]
      message(sprintf("    %s %s: %d fixtures (expected %d, diff %+d)",
                      r$league, r$team, r$total_fixtures, r$expected, r$diff))
    }
    warning(sprintf("%d team(s) MISSING fixtures vs expected — likely upstream Opta scraper gap. See pannadata scripts/opta/scrape_opta.py date windows.",
                    nrow(missing_fixtures)), call. = FALSE, immediate. = TRUE)
  }
  if (nrow(extra_fixtures) > 0) {
    # Calm informational message only — these are almost always playoffs.
    # Log the per-team detail so the playoff fingerprint is visible
    # (ENG2: 4 teams +2/+3 = semis+final; NED: similar pattern), but don't
    # raise a warning that triggers GHA's red annotation.
    message(sprintf("  EXTRA FIXTURES — %d team(s) have MORE than expected (playoff bracket, harmless):",
                    nrow(extra_fixtures)))
    for (i in seq_len(nrow(extra_fixtures))) {
      r <- extra_fixtures[i, ]
      message(sprintf("    %s %s: %d fixtures (base %d, diff %+d)",
                      r$league, r$team, r$total_fixtures, r$expected, r$diff))
    }
  }
  if (nrow(missing_fixtures) == 0L && nrow(extra_fixtures) == 0L &&
      nrow(team_totals) > 0) {
    message(sprintf("  Schedule completeness OK across %d teams in %d domestic leagues (all equal expected)",
                    nrow(team_totals), length(unique(team_totals$league))))
  }

  # Max-played-date check — single strongest "is this sim stale" signal.
  # If the latest played match in the current season is more than 5 days
  # old, the Opta scraper likely missed recent matches (daily cron failed,
  # pagination cap dropped overflow, etc.) and the whole sim is projecting
  # off an old snapshot of reality. Catches failure modes that the
  # arithmetic + per-team invariants can't see — they validate internal
  # consistency, not freshness.
  STALE_MATCH_DAYS <- 5L
  parsed_dates <- suppressWarnings(as.Date(substr(played$match_date, 1, 10)))
  if (nrow(played) > 0 && any(!is.na(parsed_dates))) {
    latest_played <- max(parsed_dates, na.rm = TRUE)
    days_stale <- as.integer(Sys.Date() - latest_played)
    if (days_stale > STALE_MATCH_DAYS) {
      warning(sprintf(
        "Sim freshness FAIL: latest played match in current season is %s (%d days ago, threshold %d). ",
        latest_played, days_stale, STALE_MATCH_DAYS),
        "Opta scraper may have missed recent matches — the sim is projecting off a stale snapshot. ",
        "Check pannadata daily-opta-scrape run history before trusting this publication.",
        call. = FALSE, immediate. = TRUE)
    } else {
      message(sprintf("  Sim freshness OK: latest played match %s (%d days ago, threshold %d)",
                      latest_played, days_stale, STALE_MATCH_DAYS))
    }
  } else if (nrow(played) > 0) {
    # All dates unparseable — schema drift / format regression upstream.
    warning("Freshness check cannot run: all played$match_date values failed to parse as Date. ",
            "Schema drift likely — check opta_fixtures parquet for match_date format.",
            call. = FALSE, immediate. = TRUE)
  }

  # Cross-provider sanity check: compare sim's EPL current_points/gp to the
  # live football-data.org standings from the blog's fixtures.json on R2
  # (same feed the Current tab reads). Catches Opta-vs-reality drift that
  # internal checks can't — specifically the "one played match silently
  # missing" case that bit us 2026-04-22 (Burnley 0-1 MCI not in the sim
  # until the manual backfill). Only runs for PL to keep scope narrow and
  # to avoid team-name mapping headaches for other leagues. Soft-fails on
  # any network/parse error since football-data.org availability is out of
  # our control and we don't want a third-party outage to block the blog.
  #
  # Wrapped in a function so `return(NULL)` short-circuits just the check,
  # not the enclosing source() call. The whole step-10 script is sourced
  # with local = TRUE, so a bare return() inside tryCatch({...}) would exit
  # the whole export — the exact bug pattern in feedback_r_tryCatch_return.md.
  fixtures_url <- "https://pub-ee4bf5b599a047f9ac2b9facc1587008.r2.dev/football/fixtures.json"
  compute_live_standings <- function() {
    # httr-based fetch with an explicit timeout; jsonlite::fromJSON has no
    # timeout option and can hang for minutes on a stuck connection.
    resp <- tryCatch(
      httr::GET(fixtures_url, httr::timeout(15)),
      error = function(e) {
        warning(sprintf("Cross-provider check SKIPPED (network): %s", conditionMessage(e)),
                call. = FALSE, immediate. = TRUE)
        NULL
      },
      warning = function(w) {
        warning(sprintf("Cross-provider check SKIPPED (network warning): %s", conditionMessage(w)),
                call. = FALSE, immediate. = TRUE)
        NULL
      }
    )
    if (is.null(resp) || httr::status_code(resp) >= 400) return(NULL)

    live_json <- tryCatch(
      jsonlite::fromJSON(rawToChar(httr::content(resp, "raw")),
                         simplifyVector = FALSE),
      error = function(e) {
        warning(sprintf("Cross-provider check SKIPPED (parse): %s", conditionMessage(e)),
                call. = FALSE, immediate. = TRUE)
        NULL
      }
    )
    if (is.null(live_json)) return(NULL)

    epl_finished <- Filter(function(m) identical(m$league, "ENG") &&
                                       identical(m$status, "FINISHED"),
                           live_json$matches)
    if (length(epl_finished) == 0) return(NULL)

    null_na <- function(x) if (is.null(x)) NA else x
    live_df <- do.call(rbind, lapply(epl_finished, function(m) {
      data.frame(
        home = sub(" AFC$", "", sub(" FC$", "", null_na(m$homeTeam))),
        away = sub(" AFC$", "", sub(" FC$", "", null_na(m$awayTeam))),
        hg = as.integer(null_na(m$homeScore)),
        ag = as.integer(null_na(m$awayScore)),
        stringsAsFactors = FALSE
      )
    }))
    live_df <- live_df[!is.na(live_df$hg) & !is.na(live_df$ag), , drop = FALSE]

    live_standings <- rbind(
      data.frame(team = live_df$home,
                 pts = ifelse(live_df$hg > live_df$ag, 3L,
                              ifelse(live_df$hg == live_df$ag, 1L, 0L)),
                 stringsAsFactors = FALSE),
      data.frame(team = live_df$away,
                 pts = ifelse(live_df$ag > live_df$hg, 3L,
                              ifelse(live_df$ag == live_df$hg, 1L, 0L)),
                 stringsAsFactors = FALSE)
    )
    live_standings %>%
      group_by(team) %>%
      summarise(live_gp = n(), live_pts = sum(pts), .groups = "drop")
  }
  live_check <- compute_live_standings()

  if (!is.null(live_check) && nrow(live_check) > 0) {
    sim_eng <- season_standings[season_standings$league == "ENG", ]
    cmp <- merge(
      data.frame(team = sim_eng$team, sim_gp = sim_eng$games_played,
                 sim_pts = sim_eng$points, stringsAsFactors = FALSE),
      live_check, by = "team"
    )
    # Coverage guard: if team-name normalization ever drifts (Opta renames a
    # team, or the " FC"/" AFC" strip stops matching), the merge silently
    # loses rows and the whole check quietly becomes no-op. A PL season has
    # 20 teams — refuse to accept more than 2 unmatched before warning.
    if (nrow(cmp) < 18L) {
      warning(sprintf(
        "Cross-provider coverage FAIL: only %d / 20 PL teams matched by name. ",
        nrow(cmp)),
        "Team-name normalization likely drifted — review the ` FC$`/` AFC$` strip.",
        call. = FALSE, immediate. = TRUE)
    }
    if (nrow(cmp) > 0) {
      cmp$dgp  <- cmp$sim_gp - cmp$live_gp
      cmp$dpts <- cmp$sim_pts - cmp$live_pts
      # Two-part predicate:
      # (a) |Δpts| > 3·|Δgp| + 3 — physical bound; each extra live-tracked
      #     game can swing at most 3 pts, slack of +3 covers minor timing.
      # (b) |Δgp| > 2 — catches sim-behind-live by multiple games even if
      #     the pts don't happen to violate (a). Both branches matter;
      #     removing either would blind a real failure mode.
      cmp$violates <- abs(cmp$dpts) > 3L * abs(cmp$dgp) + 3L |
                      abs(cmp$dgp)  > 2L
      bad <- cmp[cmp$violates, , drop = FALSE]
      if (nrow(bad) > 0) {
        message("  CROSS-PROVIDER DRIFT — sim vs football-data.org disagreement:")
        for (i in seq_len(nrow(bad))) {
          r <- bad[i, ]
          message(sprintf("    %-24s sim=%2d gp/%3d pts  live=%2d gp/%3d pts  (Δgp=%+d, Δpts=%+d)",
                          r$team, r$sim_gp, r$sim_pts, r$live_gp, r$live_pts,
                          r$dgp, r$dpts))
        }
        warning(sprintf("%d PL team(s) disagree with football-data.org by more than 3·|Δgp|+3 pts or |Δgp|>2. Sim is likely stale or has a data bug — investigate before relying on projections.",
                        nrow(bad)), call. = FALSE, immediate. = TRUE)
      } else {
        message(sprintf("  Cross-provider check OK: %d PL teams, max |Δgp|=%d, max |Δpts|=%d",
                        nrow(cmp), max(abs(cmp$dgp)), max(abs(cmp$dpts))))
      }
    }
  }

  arrow::write_parquet(season_standings, standings_output)
  message(sprintf("  Written: %s", standings_output))
  standings_ok <- TRUE
}

# 5. Upload to GitHub Releases ----

message("\n=== Uploading to GitHub ===\n")

# Check gh CLI is available
gh_check <- tryCatch(
  system2("gh", "--version", stdout = TRUE, stderr = TRUE),
  error = function(e) NULL
)
if (is.null(gh_check)) {
  stop("'gh' CLI is not installed or not on PATH. Install from https://cli.github.com/")
}

# Ensure release exists
message(sprintf("  Checking release '%s' on %s...", tag, repo))

release_check <- system2(
  "gh", c("release", "view", tag, "--repo", repo),
  stdout = TRUE, stderr = TRUE
)
release_status <- attr(release_check, "status")

if (!is.null(release_status) && release_status != 0) {
  stderr_text <- paste(release_check, collapse = "\n")

  # Match "not found" specifically; treat everything else as an unexpected error
  if (grepl("release not found|not found", stderr_text, ignore.case = TRUE)) {
    message("  Release not found. Creating...")
    create_result <- system2(
      "gh", c("release", "create", tag,
              "--repo", repo,
              "--title", shQuote("Blog Data (Latest)"),
              "--notes", shQuote("Player ratings and match predictions for the blog.")),
      stdout = TRUE, stderr = TRUE
    )
    create_status <- attr(create_result, "status")
    if (!is.null(create_status) && create_status != 0) {
      stop(sprintf("Failed to create release '%s': %s",
                   tag, paste(create_result, collapse = "\n")))
    }
  } else {
    stop(sprintf("Failed to check release '%s': %s\nCheck network, auth (gh auth login), and repo name.",
                 tag, stderr_text))
  }
}

# Upload files
upload_files <- c(ratings_output, predictions_output)
if (exists("standings_ok") && isTRUE(standings_ok)) {
  upload_files <- c(upload_files, standings_output)
}
for (fpath in upload_files) {
  fname <- basename(fpath)
  size_mb <- round(file.size(fpath) / (1024 * 1024), 2)
  message(sprintf("  Uploading %s (%.2f MB)...", fname, size_mb))
  result <- system2(
    "gh", c("release", "upload", tag, shQuote(fpath),
            "--repo", repo, "--clobber"),
    stdout = TRUE, stderr = TRUE
  )
  if (!is.null(attr(result, "status")) && attr(result, "status") != 0) {
    stop(sprintf("Failed to upload %s: %s", fname, paste(result, collapse = "\n")))
  }
}

# 6. Summary ----

message("\n========================================")
message("Blog data exported successfully!")
message("========================================")
message(sprintf("  Release: https://github.com/%s/releases/tag/%s", repo, tag))
message(sprintf("  Ratings: %d players (season %d)", nrow(panna_ratings), latest_season))
message(sprintf("  Predictions: %d matches", nrow(match_predictions)))
if (exists("standings_ok") && isTRUE(standings_ok)) {
  message(sprintf("  Standings: %d teams", nrow(season_standings)))
}
message("\nBlog URLs:")
message(sprintf("  https://github.com/%s/releases/download/%s/panna_ratings.parquet", repo, tag))
message(sprintf("  https://github.com/%s/releases/download/%s/match_predictions.parquet", repo, tag))
if (exists("standings_ok") && isTRUE(standings_ok)) {
  message(sprintf("  https://github.com/%s/releases/download/%s/season_standings.parquet", repo, tag))
}
