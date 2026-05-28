# 01_build_fixture_results.R
# Build historical results and future fixtures from Opta data
#
# Loads lineups and events to construct match results with goals and xG.
# Also loads fixture data for upcoming matches. Reuses cached RAPM pipeline
# data when available.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)

leagues <- if (exists("leagues")) leagues else c(
  "ENG", "ESP", "GER", "ITA", "FRA",
  "NED", "POR", "TUR", "ENG2", "SCO",
  "UCL", "UEL", "UECL",
  "WC", "EURO"
)
seasons <- if (exists("seasons")) seasons else NULL
min_season <- if (exists("min_season")) min_season else "2013-2014"

# Tournament leagues (neutral venue flag)
TOURNAMENT_LEAGUES <- c("WC", "EURO")

# extract_season_end_year() is defined in R/utils.R

output_path <- file.path(cache_dir, "01_fixture_results.rds")

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 01_fixture_results.rds")
  fixture_results <- readRDS(output_path)
  message(sprintf("  %d historical + %d fixtures",
                  sum(fixture_results$match_status == "Played"),
                  sum(fixture_results$match_status != "Played")))
  return(invisible(NULL))
}

# 4. Try Loading from RAPM Cache ----

rapm_cache <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
use_rapm_cache <- file.exists(rapm_cache)

if (use_rapm_cache) {
  message("Loading results from RAPM pipeline cache...")
  raw_data <- readRDS(rapm_cache)
  results <- raw_data$results
  # Filter to requested leagues
  results <- results[results$league %in% leagues, ]
  message(sprintf("  %d matches from RAPM cache (covers %d / %d requested leagues)",
                  nrow(results),
                  length(unique(results$league)), length(leagues)))
  results$score_source <- "rapm_cache"  # will be overridden below where fixtures has scores
  cached_leagues <- unique(results$league)
} else {
  message("No RAPM cache found - loading from Opta data directly...")
  results <- NULL
  cached_leagues <- character(0)
}

# 5. Load Results from Opta for any leagues not in the cache ----
#
# Previously this block was only entered when the RAPM cache was absent
# entirely (`if (is.null(results))`). When the cache existed but didn't
# cover all requested leagues (e.g., 14 newly-added intl competitions
# whose RAPM hasn't been run), the missing leagues silently produced
# zero played matches — fixtures iterated them but historical results
# were stuck on the cache's pre-existing scope. Result: Norway's entire
# UEFA WC qualifying campaign was invisible to the Elo iteration despite
# the data being on disk.
#
# Fix: compute `missing_leagues` and direct-load just those. The cache
# fast-path still applies to leagues it already covers.

missing_leagues <- setdiff(leagues, cached_leagues)
if (length(missing_leagues) > 0) {
  message(sprintf("\n=== Loading %d league%s directly from Opta (not in RAPM cache): %s ===\n",
                  length(missing_leagues),
                  if (length(missing_leagues) == 1L) "" else "s",
                  paste(missing_leagues, collapse = ", ")))

  all_results <- list()

  for (league in missing_leagues) {
    opta_league <- to_opta_league(league)
    available_seasons <- tryCatch(list_opta_seasons(league, source = "local"), error = function(e) character(0))
    if (length(available_seasons) == 0) next

    if (!is.null(seasons)) available_seasons <- intersect(available_seasons, seasons)
    if (!is.null(min_season)) available_seasons <- available_seasons[available_seasons >= min_season]

    for (season in available_seasons) {
      label <- paste(league, season)
      tryCatch({
        lineups <- load_opta_lineups(league, season = season, source = "local")
        events <- load_opta_events(league, season = season, source = "local")
        fixtures_all <- load_opta_fixtures(league, season = season, source = "local")

        if (is.null(lineups) || nrow(lineups) == 0) next

        # Build match info from lineups (team_name here is the lineup-feed variant,
        # which is canonical downstream).
        match_info <- lineups %>%
          filter(is_starter) %>%
          group_by(match_id) %>%
          summarise(
            home_team = first(team_name[tolower(team_position) == "home"]),
            away_team = first(team_name[tolower(team_position) == "away"]),
            match_date = first(match_date),
            home_team_id = first(team_id[tolower(team_position) == "home"]),
            away_team_id = first(team_id[tolower(team_position) == "away"]),
            .groups = "drop"
          )

        # PRIMARY SOURCE for scores: opta_fixtures.parquet (matchstats endpoint).
        # These are the authoritative match scores and crucially attribute own
        # goals to the correct side. Deriving from goal events silently miscodes
        # own goals because `events.parquet` tags `team_id` with the scoring
        # player's team (= opponent of the credited side). See panna#59.
        fixtures_scores <- if (!is.null(fixtures_all) && nrow(fixtures_all) > 0) {
          fixtures_all %>%
            mutate(home_goals_fx = suppressWarnings(as.integer(home_score)),
                   away_goals_fx = suppressWarnings(as.integer(away_score))) %>%
            select(match_id, home_goals_fx, away_goals_fx)
        } else {
          tibble::tibble(match_id = character(0),
                         home_goals_fx = integer(0),
                         away_goals_fx = integer(0))
        }

        # FALLBACK: events-derived goal counts, used only when fixtures has no
        # score for a match (rare — usually only very recent matches that
        # matchstats hasn't landed yet). Correct for non-own-goal matches;
        # own goals get miscoded, so we warn on any fallback use.
        goal_counts <- events %>%
          filter(event_type == "goal") %>%
          count(match_id, team_id, name = "goals")

        home_goals_ev <- match_info %>%
          select(match_id, home_team_id) %>%
          left_join(goal_counts, by = c("match_id", "home_team_id" = "team_id")) %>%
          select(match_id, home_goals_ev = goals)

        away_goals_ev <- match_info %>%
          select(match_id, away_team_id) %>%
          left_join(goal_counts, by = c("match_id", "away_team_id" = "team_id")) %>%
          select(match_id, away_goals_ev = goals)

        match_results <- match_info %>%
          left_join(fixtures_scores, by = "match_id") %>%
          left_join(home_goals_ev, by = "match_id") %>%
          left_join(away_goals_ev, by = "match_id") %>%
          mutate(
            # NA preserved when neither source has a score, so section 5b's
            # drop-filter can see missing data rather than fabricating 0-0.
            home_goals = coalesce(home_goals_fx, home_goals_ev),
            away_goals = coalesce(away_goals_fx, away_goals_ev),
            score_source = case_when(
              !is.na(home_goals_fx) & !is.na(away_goals_fx) ~ "fixtures",
              !is.na(home_goals_ev) & !is.na(away_goals_ev) ~ "events",
              TRUE ~ NA_character_
            ),
            league = !!league, season = !!season
          )

        # Cross-check: where BOTH sources produced a score, they should agree.
        # A disagreement is the own-goal attribution signature — useful
        # regression signal if the events-derivation logic ever changes.
        both_scored <- match_results %>%
          filter(!is.na(home_goals_fx) & !is.na(home_goals_ev) &
                 !is.na(away_goals_fx) & !is.na(away_goals_ev))
        mismatches <- both_scored %>%
          filter(home_goals_fx != home_goals_ev | away_goals_fx != away_goals_ev)
        if (nrow(mismatches) > 0) {
          message(sprintf("  %s %s: %d/%d matches where events-derived scores disagree with fixtures (using fixtures; own goals the likely cause)",
                          league, season, nrow(mismatches), nrow(both_scored)))
        }

        # Drop matches with no score from either source. This is the only
        # legitimate drop reason — the previous "matches without events" filter
        # conflated "no events" (common, fine if fixtures has score) with
        # "no score anywhere" (the actual bad state).
        no_score <- match_results %>% filter(is.na(score_source))
        if (nrow(no_score) > 0) {
          message(sprintf("  WARNING: %s %s: dropping %d matches with no fixture score AND no events (scraper gap)",
                          league, season, nrow(no_score)))
          match_results <- match_results %>% filter(!is.na(score_source))
        }

        # Drop the _fx/_ev intermediates but keep score_source so the
        # aggregate summary below can surface fixtures-vs-events provenance
        # across all leagues. Section 8 filters to keep_cols before saving, so
        # score_source is excluded from the output schema.
        match_results <- match_results %>%
          select(-any_of(c("home_goals_fx", "away_goals_fx",
                           "home_goals_ev", "away_goals_ev")))

        all_results[[label]] <- match_results
      }, error = function(e) {
        message(sprintf("  ERROR %s: %s", label, e$message))
      })
    }
  }

  direct_results <- bind_rows(all_results)
  message(sprintf("  Loaded %d matches from Opta direct (across %d leagues)",
                  nrow(direct_results), length(unique(direct_results$league))))

  # Combine cache results (if any) with direct-load results.
  # Use bind_rows with fill semantics — direct-load may have columns the
  # cache lacks (e.g., score_source = "fixtures" vs "rapm_cache").
  if (!is.null(results) && nrow(results) > 0) {
    results <- bind_rows(results, direct_results)
  } else {
    results <- direct_results
  }
  message(sprintf("  TOTAL matches available (cache + direct): %d across %d leagues",
                  nrow(results), length(unique(results$league))))

  # Provenance summary. Any non-zero "events" count points at Opta's matchstats
  # endpoint lagging for recent matches — we're falling back to goal events
  # which silently miscredits own goals (panna#59). Persistent high counts
  # here mean fixtures scrape needs attention upstream.
  if ("score_source" %in% names(results)) {
    src_summary <- table(results$score_source, useNA = "always")
    message("  Score provenance: ",
            paste(names(src_summary), src_summary, sep = "=", collapse = ", "))
    n_events_fallback <- sum(results$score_source == "events", na.rm = TRUE)
    if (n_events_fallback > 0) {
      # Isolate the recent-matches case (the healthy case) from systemic gaps.
      # A handful of "today's matches" using the events fallback is normal.
      recent <- results %>%
        filter(score_source == "events") %>%
        arrange(desc(match_date))
      n_stale <- sum(as.Date(substr(recent$match_date, 1, 10)) <
                     Sys.Date() - 7, na.rm = TRUE)
      if (n_stale > 0) {
        message(sprintf("  WARNING: %d matches older than 7 days still using events fallback — opta_fixtures.parquet may be stale", n_stale))
      }
    }
  }
}

# 5b. Override Scores from opta_fixtures (Source of Truth) ----
#
# RAPM-cache path: `results` carries goals from prior pipeline runs, which may
# pre-date the own-goal fix and therefore disagree with fixtures.
# Direct-load path: belt-and-braces for the overlap case; the in-loop logic
# already prefers fixtures, so this is a no-op for matches we scored there.
#
# The fixtures endpoint provides authoritative match scores that correctly
# attribute own goals — events-derived goal counts do not (see section 5
# comment for the mechanism).

message("\n=== Overriding scores from opta_fixtures (authoritative) ===\n")

lg_seasons <- unique(results[, c("league", "season")])
n_overridden <- 0L
n_corrected <- 0L  # disagreements between prior value and fixtures

for (i in seq_len(nrow(lg_seasons))) {
  lg <- lg_seasons$league[i]
  sn <- lg_seasons$season[i]
  fx <- tryCatch(
    load_opta_fixtures(lg, season = sn, source = "local"),
    error = function(e) {
      message(sprintf("  ERROR loading fixtures for %s %s: %s (skipping override; matches will keep prior-path goals)",
                      lg, sn, conditionMessage(e)))
      NULL
    }
  )
  if (is.null(fx) || nrow(fx) == 0) next

  fx_clean <- fx %>%
    mutate(home_goals_fx = suppressWarnings(as.integer(home_score)),
           away_goals_fx = suppressWarnings(as.integer(away_score))) %>%
    filter(!is.na(home_goals_fx), !is.na(away_goals_fx)) %>%
    select(match_id, home_goals_fx, away_goals_fx)

  idx <- results$league == lg & results$season == sn
  slice <- results[idx, , drop = FALSE]
  merged <- slice %>%
    left_join(fx_clean, by = "match_id")

  has_fx <- !is.na(merged$home_goals_fx) & !is.na(merged$away_goals_fx)
  had_prior <- !is.na(merged$home_goals) & !is.na(merged$away_goals)
  differs <- has_fx & had_prior &
             (merged$home_goals_fx != merged$home_goals |
              merged$away_goals_fx != merged$away_goals)

  if (any(differs)) {
    n_corrected <- n_corrected + sum(differs)
    message(sprintf("  %s %s: %d matches had prior scores disagreeing with fixtures (corrected — likely own-goal miscount)",
                    lg, sn, sum(differs)))
  }
  if (any(has_fx)) {
    merged$home_goals[has_fx] <- merged$home_goals_fx[has_fx]
    merged$away_goals[has_fx] <- merged$away_goals_fx[has_fx]
    if ("score_source" %in% names(merged)) merged$score_source[has_fx] <- "fixtures"
    n_overridden <- n_overridden + sum(has_fx)
  }

  results[idx, ] <- merged[, names(results), drop = FALSE]
}

message(sprintf("  Scores overridden from fixtures: %d matches", n_overridden))
if (n_corrected > 0) {
  message(sprintf("  Of those, %d had prior scores corrected (see panna#59 for the original symptom report)",
                  n_corrected))
}

missing_scores <- sum(is.na(results$home_goals) | is.na(results$away_goals))
if (missing_scores > 0) {
  message(sprintf("  WARNING: %d matches still have NA scores after fixtures override — scraper gap, will drop below",
                  missing_scores))
  results <- results[!is.na(results$home_goals) & !is.na(results$away_goals), ]
}

# 5c. Drop matches with missing team identification ----
#
# These are matches where ONE side has both team_name AND team_id missing
# from the Opta feed (typically minnow international friendlies where the
# scraper lost the opponent). They literally can't be processed downstream:
#   - compute_match_elos can't update Elo without knowing both teams
#   - the prior step 03 re-iteration was POISONING every other team's Elo
#     through NA-name lookups in the elos named vector, cascading to
#     France/Germany/Brazil all getting NA Elo and then 0 via step 04's
#     NA-fill
# Dropping at source surfaces the upstream scraper gap loudly (so it can
# be reported to pannadata), and prevents the cascade. We don't impute
# the missing team — there's nothing meaningful to impute.
missing_team <- is.na(results$home_team) | is.na(results$away_team)
if (any(missing_team)) {
  bad <- results[missing_team, , drop = FALSE]
  by_league <- table(bad$league)
  message(sprintf("  WARNING: %d matches have NA home_team or away_team — DROPPING (Opta scraper gap, file a pannadata issue):",
                  nrow(bad)))
  for (lg in names(by_league)) {
    message(sprintf("    %s: %d matches", lg, by_league[lg]))
  }
  # Show up to 5 examples so the user can see what's broken
  example_n <- min(5, nrow(bad))
  for (i in seq_len(example_n)) {
    r <- bad[i, ]
    message(sprintf("    e.g., %s [%s] '%s' vs '%s'",
                    r$match_date, r$league,
                    if (is.na(r$home_team)) "<NA>" else r$home_team,
                    if (is.na(r$away_team)) "<NA>" else r$away_team))
  }
  results <- results[!missing_team, , drop = FALSE]
}

# 6. Ensure Required Columns ----

if (!"season_end_year" %in% names(results)) {
  results$season_end_year <- sapply(results$season, extract_season_end_year)
}

# 6b. Backfill missing home/away team_ids from opta_lineups.parquet ----
#
# The RAPM cache (01_raw_data.rds) provides historical matches with NA
# home_team_id / away_team_id columns — when it was built, team_ids weren't
# preserved. With cached matches making up most of the dataset, ~99% of
# `results` rows have NA team_ids. Downstream:
#   - the xG join (next block) is keyed by (match_id, team_id) so it
#     misses every cache-loaded match → 0% join rate session-wide
#   - rolling xG features in step 03 become all-NA → step 04 NA-fill (now
#     narrowed) propagates NaN → the model trains on NaN xG for every
#     match historically
# Backfill from opta_lineups.parquet, which has team_ids for every match
# we have a lineup for. This recovers ~99% coverage and lifts the xG join
# from 0% to ~86% (the remaining 14% are matches in competitions the xG
# model hasn't been run on yet — genuine NA, not a join failure).
lineups_path <- "../pannadata/data/opta/opta_lineups.parquet"
n_before_backfill <- sum(is.na(results$home_team_id) | is.na(results$away_team_id))
if (n_before_backfill > 0L &&
    file.exists(lineups_path) &&
    requireNamespace("arrow", quietly = TRUE)) {
  lu_all <- as.data.frame(arrow::read_parquet(
    lineups_path, col_select = c("match_id", "team_id", "team_position")))
  lu_dt <- data.table::as.data.table(lu_all)
  # One team_id per (match_id, side). first() is safe because all lineup
  # rows for a side of a match share the same team_id.
  lu_dt <- lu_dt[, .(team_id = team_id[1L]),
                  by = .(match_id, side = tolower(team_position))]
  lu_home <- lu_dt[side == "home", .(match_id, home_tid = team_id)]
  lu_away <- lu_dt[side == "away", .(match_id, away_tid = team_id)]
  rm(lu_all, lu_dt); invisible(gc(verbose = FALSE))

  rdt <- data.table::as.data.table(results)
  rdt <- merge(rdt, lu_home, by = "match_id", all.x = TRUE)
  rdt <- merge(rdt, lu_away, by = "match_id", all.x = TRUE)
  rdt[is.na(home_team_id) & !is.na(home_tid), home_team_id := home_tid]
  rdt[is.na(away_team_id) & !is.na(away_tid), away_team_id := away_tid]
  rdt[, c("home_tid", "away_tid") := NULL]
  results <- as.data.frame(rdt)

  n_after_backfill <- sum(is.na(results$home_team_id) | is.na(results$away_team_id))
  n_recovered <- n_before_backfill - n_after_backfill
  message(sprintf("  Backfilled team_ids from opta_lineups: %d / %d previously-NA matches resolved (%.0f%%), %d still NA",
                  n_recovered, n_before_backfill,
                  100 * n_recovered / n_before_backfill, n_after_backfill))
}

# Match-level team xG from panna's own xG model (per-shot xG summed by team;
# built by debug/keep/build_match_team_xg.R -> opta_match_xg.parquet). Opta's
# feed carries no usable match xG, so without this the xG-rolling features in
# step 03 collapse to a constant.
results$home_xg <- NA_real_
results$away_xg <- NA_real_
mxg_path <- "../pannadata/data/opta/opta_match_xg.parquet"
if (file.exists(mxg_path) && requireNamespace("arrow", quietly = TRUE)) {
  mxg <- as.data.frame(arrow::read_parquet(mxg_path))
  xg_lookup <- stats::setNames(mxg$xg, paste(mxg$match_id, mxg$team_id))
  results$home_xg <- unname(xg_lookup[paste(results$match_id, results$home_team_id)])
  results$away_xg <- unname(xg_lookup[paste(results$match_id, results$away_team_id)])
  n_xg <- sum(!is.na(results$home_xg) & !is.na(results$away_xg))
  message(sprintf("  Match xG (panna model) joined: %d/%d matches (%.0f%%)",
                  n_xg, nrow(results), 100 * n_xg / nrow(results)))
  # Sanity gate: if the join rate is implausibly low (signalling a key-mismatch
  # bug rather than just missing-coverage), surface it loudly. Healthy state
  # after the team_id backfill above is ~86% (club matches covered; intl
  # qualifiers not, which is fine). Anything <50% is structurally wrong.
  pct <- 100 * n_xg / nrow(results)
  if (pct < 50) {
    warning(sprintf(
      "Match xG join rate is %.0f%% — implausibly low. Either the xG parquet is empty, the team_id backfill failed, or the encoding has drifted between sources. Rolling xG features will be mostly NA, hollowing out a major predictor.",
      pct), call. = FALSE, immediate. = TRUE)
  }
}

# Compute result label
results$result <- ifelse(results$home_goals > results$away_goals, "H",
                  ifelse(results$home_goals == results$away_goals, "D", "A"))
results$match_status <- "Played"

# Tournament/neutral venue flag
results$is_neutral_venue <- as.integer(results$league %in% TOURNAMENT_LEAGUES)

# 7. Load Fixtures (Upcoming Matches) ----

message("\n=== Loading Fixtures ===\n")

all_fixtures <- list()
for (league in leagues) {
  available_seasons <- tryCatch(list_opta_seasons(league), error = function(e) character(0))
  if (length(available_seasons) == 0) next

  # Only load current/recent season for fixtures
  current_season <- available_seasons[1]

  tryCatch({
    fixtures <- load_opta_fixtures(league, season = current_season,
                                   status = c("Fixture", "Postponed", "Awarded"),
                                   source = "local")
    if (!is.null(fixtures) && nrow(fixtures) > 0) {
      fixtures$league <- league
      fixtures$season_end_year <- extract_season_end_year(current_season)
      fixtures$home_goals <- NA_integer_
      fixtures$away_goals <- NA_integer_
      fixtures$home_xg <- NA_real_
      fixtures$away_xg <- NA_real_
      fixtures$result <- NA_character_

      # Awarded = walkover/forfeit with final scores. Promote to Played so the
      # standings absorb the outcome instead of re-simulating a decided match.
      # Gate on non-NA scores; any unresolved Awarded row stays as-is and flows
      # through as an upcoming fixture.
      is_awarded <- fixtures$match_status == "Awarded" &
        !is.na(fixtures$home_score) & !is.na(fixtures$away_score)
      if (any(is_awarded)) {
        fixtures$home_goals[is_awarded] <- as.integer(fixtures$home_score[is_awarded])
        fixtures$away_goals[is_awarded] <- as.integer(fixtures$away_score[is_awarded])
        hg <- fixtures$home_goals[is_awarded]
        ag <- fixtures$away_goals[is_awarded]
        fixtures$result[is_awarded] <- ifelse(hg > ag, "H",
                                       ifelse(hg == ag, "D", "A"))
        fixtures$match_status[is_awarded] <- "Played"
      }

      fixtures$is_neutral_venue <- as.integer(league %in% TOURNAMENT_LEAGUES)
      all_fixtures[[league]] <- fixtures
      n_fix <- sum(fixtures$match_status == "Fixture")
      n_ppd <- sum(fixtures$match_status == "Postponed")
      n_awd <- sum(is_awarded)
      message(sprintf("  %s %s: %d Fixture + %d Postponed + %d Awarded(->Played)",
                      league, current_season, n_fix, n_ppd, n_awd))
    }
  }, error = function(e) {
    message(sprintf("  No fixtures for %s: %s", league, e$message))
  })
}

fixtures_df <- if (length(all_fixtures) > 0) bind_rows(all_fixtures) else NULL

# 8. Combine ----

# Ensure consistent columns
keep_cols <- c("match_id", "match_date", "match_status", "league", "season",
               "season_end_year", "home_team", "away_team", "home_team_id",
               "away_team_id", "home_goals", "away_goals", "home_xg", "away_xg",
               "result", "is_neutral_venue")

# Add missing columns
for (col in keep_cols) {
  if (!col %in% names(results)) results[[col]] <- NA
}
results_clean <- results[, intersect(keep_cols, names(results))]

if (!is.null(fixtures_df)) {
  for (col in keep_cols) {
    if (!col %in% names(fixtures_df)) fixtures_df[[col]] <- NA
  }
  fixtures_clean <- fixtures_df[, intersect(keep_cols, names(fixtures_df))]

  # Normalize fixture team names to the variant Opta uses in its lineup feed.
  # Opta's fixtures endpoint sometimes serves full legal names ("AFC Ajax",
  # "BV Borussia 09 Dortmund", "Çaykur Rize Spor Kulübü") while lineups use a
  # different spelling ("Ajax", "Borussia Dortmund", "Rizespor"). That split
  # caused sim scripts to treat them as different teams and inflate team counts.
  # Use team_id (stable across both feeds) to rewrite fixture names to match.
  team_variants <- results_clean %>%
    filter(!is.na(home_team_id), !is.na(home_team)) %>%
    select(team_id = home_team_id, team_name = home_team) %>%
    bind_rows(
      results_clean %>%
        filter(!is.na(away_team_id), !is.na(away_team)) %>%
        select(team_id = away_team_id, team_name = away_team)
    ) %>%
    count(team_id, team_name, name = "n")

  # Deterministic tie-break: prefer the most-frequent name; on ties, prefer the
  # shortest (lineup variants tend to be shorter than fixture-feed legal names)
  # then alphabetically. Without this, the canonical name for a mid-season
  # renamed team could flip between pipeline runs based on row ordering, and
  # downstream name-keyed consumers (blog standings, PSR rollups) would see
  # phantom "team renamed" events.
  team_name_map <- team_variants %>%
    group_by(team_id) %>%
    arrange(desc(n), nchar(team_name), team_name, .by_group = TRUE) %>%
    slice(1) %>%
    ungroup() %>%
    select(team_id, team_name)

  # Report team_ids with multiple name variants — these are the genuine
  # split-identity cases and the debug info someone will want next time
  # the standings view shows a split team.
  collisions <- team_variants %>% count(team_id, name = "variants") %>% filter(variants > 1)
  if (nrow(collisions) > 0) {
    message(sprintf("  %d team_ids have multiple name variants in lineups (canonical = most frequent, tie-break = shortest)",
                    nrow(collisions)))
  }

  n_home_renamed <- 0L
  n_away_renamed <- 0L
  unresolved_home <- 0L
  unresolved_away <- 0L
  if (nrow(team_name_map) > 0) {
    home_lookup <- setNames(team_name_map$team_name, team_name_map$team_id)
    new_home <- home_lookup[as.character(fixtures_clean$home_team_id)]
    n_home_renamed <- sum(!is.na(new_home) & (is.na(fixtures_clean$home_team) | new_home != fixtures_clean$home_team))
    unresolved_home <- sum(!is.na(fixtures_clean$home_team_id) & is.na(new_home))
    fixtures_clean$home_team <- ifelse(is.na(new_home), fixtures_clean$home_team, unname(new_home))

    new_away <- home_lookup[as.character(fixtures_clean$away_team_id)]
    n_away_renamed <- sum(!is.na(new_away) & (is.na(fixtures_clean$away_team) | new_away != fixtures_clean$away_team))
    unresolved_away <- sum(!is.na(fixtures_clean$away_team_id) & is.na(new_away))
    fixtures_clean$away_team <- ifelse(is.na(new_away), fixtures_clean$away_team, unname(new_away))
  }

  # Always-on diagnostic: distinguishes "rename block ran correctly" from
  # "rename block silently skipped due to upstream issue" (empty lineup map,
  # zero fixture rows, etc.). Even the zero cases should be visible in logs.
  message(sprintf("  Fixture team-name normalization: %d lineup variants, %d fixtures, %d renamed",
                  nrow(team_name_map), nrow(fixtures_clean),
                  n_home_renamed + n_away_renamed))

  # Unresolved team_ids (fixture has a team_id absent from the lineup map) are
  # the silent split-identity risk: a newly-promoted team with zero played
  # matches yet keeps its Opta-fixtures name, and the moment they play their
  # first match the same team_id will start producing the lineup variant —
  # reintroducing the exact bug this block is meant to prevent. Warn loudly.
  if (unresolved_home + unresolved_away > 0) {
    message(sprintf("  WARNING: %d fixture team_ids have no lineup match (home: %d, away: %d) — names kept as-is, may cause split-identity after first played match",
                    unresolved_home + unresolved_away, unresolved_home, unresolved_away))
  }

  fixture_results <- bind_rows(results_clean, fixtures_clean)
} else {
  fixture_results <- results_clean
}

fixture_results <- fixture_results[order(fixture_results$match_date), ]

# 9. Save ----

saveRDS(fixture_results, output_path)

# 10. Summary ----

message("\n========================================")
message("Fixture results complete!")
message("========================================")
message(sprintf("Historical: %d matches", sum(fixture_results$match_status == "Played")))
message(sprintf("Upcoming: %d fixtures", sum(fixture_results$match_status != "Played", na.rm = TRUE)))
message(sprintf("Leagues: %d", length(unique(fixture_results$league))))
message(sprintf("Date range: %s to %s", min(fixture_results$match_date, na.rm = TRUE),
                max(fixture_results$match_date, na.rm = TRUE)))
message(sprintf("\nSaved to: %s", output_path))
