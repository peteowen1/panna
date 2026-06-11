# 02_player_ratings_to_team.R
# Aggregate player ratings (xRAPM/SPM/RAPM) to team-level features
#
# For each match, gets the starting XI from lineups, joins to seasonal
# ratings by player_name + season_end_year, and computes team-level
# summary statistics (sum, mean, max, positional group averages, etc.)

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-predictions-opta")
output_path <- file.path(cache_dir, "02_team_ratings.rds")

# Use skill-based ratings if available (from estimated skills pipeline)
if (!exists("use_skill_ratings")) use_skill_ratings <- TRUE

# 3. Check Cache ----

if (file.exists(output_path) && !isTRUE(force_rebuild)) {
  message("Cache exists - loading 02_team_ratings.rds")
  team_ratings <- readRDS(output_path)
  message(sprintf("  %d matches with team ratings", nrow(team_ratings)))
  return(invisible(NULL))
}

# 4. Load Required Data ----

message("\n=== Aggregating Player Ratings to Team Level ===\n")

# Load fixture results from step 01
fixture_results <- readRDS(file.path(cache_dir, "01_fixture_results.rds"))
played <- fixture_results[fixture_results$match_status == "Played", ]

# Load seasonal ratings: prefer skill-based, fall back to raw-stat
skill_ratings_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
raw_ratings_path <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")

if (isTRUE(use_skill_ratings) && file.exists(skill_ratings_path)) {
  message("========================================")
  message("  USING: SKILL-BASED ratings (cache-skills/06_seasonal_ratings.rds)")
  message("========================================")
  seasonal_data <- load_cache_with_meta(skill_ratings_path, max_age_hours = 336,
                                        expected_pipeline = "skills")
} else if (file.exists(raw_ratings_path)) {
  message("========================================")
  if (isTRUE(use_skill_ratings)) {
    warning("Skill ratings not found at ", skill_ratings_path,
            ". Falling back to raw-stat ratings.", call. = FALSE, immediate. = TRUE)
    message("  USING: RAW-STAT ratings (FALLBACK - skill ratings not found)")
    message("  Expected at: ", skill_ratings_path)
    message("  Run the skills pipeline first for skill-based predictions.")
  } else {
    message("  USING: RAW-STAT ratings (use_skill_ratings = FALSE)")
  }
  message("========================================")
  seasonal_data <- load_cache_with_meta(raw_ratings_path, max_age_hours = 336,
                                        expected_pipeline = "opta-rapm")
} else {
  stop("No seasonal ratings found. Run the Opta RAPM pipeline first:\n  ",
       "source('data-raw/player-ratings-opta/run_pipeline_opta.R')")
}

# Extract and combine rating tables
# The RDS contains separate tables: seasonal_xrapm, seasonal_spm, seasonal_rapm
if (is.data.frame(seasonal_data)) {
  ratings <- seasonal_data
} else if ("seasonal_xrapm" %in% names(seasonal_data)) {
  # Standard structure from Opta RAPM pipeline
  xrapm_cols <- c("player_id", "player_name", "season_end_year",
                   "xrapm", "offense", "defense", "total_minutes")
  xrapm_cols <- intersect(xrapm_cols, names(seasonal_data$seasonal_xrapm))
  xrapm <- seasonal_data$seasonal_xrapm[, xrapm_cols]
  names(xrapm)[names(xrapm) == "xrapm"] <- "panna"

  spm_cols <- c("player_id", "player_name", "season_end_year", "spm")
  spm_cols <- intersect(spm_cols, names(seasonal_data$seasonal_spm))
  spm <- seasonal_data$seasonal_spm[, spm_cols]

  # Choose merge key: player_id if available in both, else player_name
  merge_key <- if ("player_id" %in% names(xrapm) && "player_id" %in% names(spm)) {
    c("player_id", "season_end_year")
  } else {
    c("player_name", "season_end_year")
  }
  spm <- spm[!duplicated(spm[, merge_key]), ]

  ratings <- merge(xrapm, spm[, c(merge_key, "spm")],
                   by = merge_key,
                   all.x = TRUE)
  ratings$spm[is.na(ratings$spm)] <- 0

  # Merge PSR/OSR/DSR if available in seasonal data
  if ("seasonal_psr" %in% names(seasonal_data) &&
      !is.null(seasonal_data$seasonal_psr) &&
      nrow(seasonal_data$seasonal_psr) > 0) {
    psr_data <- as.data.frame(seasonal_data$seasonal_psr)
    psr_cols <- intersect(c("player_id", "season_end_year", "psr", "osr", "dsr"), names(psr_data))
    if (length(psr_cols) >= 3) {
      psr_data <- psr_data[, psr_cols]
      psr_data <- psr_data[!duplicated(psr_data[, c("player_id", "season_end_year")]), ]
      ratings <- merge(ratings, psr_data, by = c("player_id", "season_end_year"), all.x = TRUE)
      ratings$psr[is.na(ratings$psr)] <- 0
      if ("osr" %in% names(ratings)) ratings$osr[is.na(ratings$osr)] <- 0
      if ("dsr" %in% names(ratings)) ratings$dsr[is.na(ratings$dsr)] <- 0
      message(sprintf("  Merged PSR/OSR/DSR for %d player-seasons",
                      sum(ratings$psr != 0)))
    }
  }

  # Merge EPR/EPR_offensive/EPR_defensive from weekly snapshots.
  # We take the LATEST snapshot whose snapshot_date falls within (or just
  # before) each season_end_year (June 30 of that season). This treats EPR
  # as a season-level rating for the prediction model — same staleness
  # contract as PSR.
  #
  # Path resolution via opta_data_dir() — works locally (resolves to
  # ../pannadata/data/opta) AND on GHA (resolves to PANNADATA_DIR/opta).
  # Previous hardcoded "../pannadata/data/opta/..." silently returned FALSE
  # on GHA where pannadata is staged at $RUNNER_TEMP/pannadata/opta without
  # the "data/" segment, so the EPR merge was skipped and downstream
  # wc2026_team_strength.parquet shipped with epr=NA for all 48 WC teams.
  # 2026-05-29 incident.
  epr_path <- file.path(opta_data_dir(), "opta_epr_weekly.parquet")
  if (!file.exists(epr_path)) {
    stop(sprintf(
      "EPR weekly snapshots not found at %s — required for fixture-side EPR. ",
      epr_path),
      "On GHA: add opta_epr_weekly.parquet to the predictions-pipeline.yml ",
      "download list. Locally: run the EPR weekly pipeline first.",
      call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("arrow package not available — required to read opta_epr_weekly.parquet",
         call. = FALSE)
  }
  epr_w <- as.data.frame(arrow::read_parquet(epr_path))
  epr_w$snapshot_date <- as.Date(epr_w$snapshot_date)
  epr_w$snapshot_season_end_year <- ifelse(
    as.integer(format(epr_w$snapshot_date, "%m")) >= 7L,
    as.integer(format(epr_w$snapshot_date, "%Y")) + 1L,
    as.integer(format(epr_w$snapshot_date, "%Y"))
  )
  # Latest snapshot per (player_id, season)
  epr_dt <- data.table::as.data.table(epr_w)
  data.table::setorder(epr_dt, player_id, snapshot_season_end_year, -snapshot_date)
  epr_seasonal <- epr_dt[, .SD[1L],
                          by = .(player_id, season_end_year = snapshot_season_end_year),
                          .SDcols = c("epr", "epr_offensive", "epr_defensive")]
  ratings <- merge(ratings, as.data.frame(epr_seasonal),
                    by = c("player_id", "season_end_year"), all.x = TRUE)
  # TODO(panna#74): NA→0 imputation here violates the no-silent-imputation
  # rule but preserves downstream aggregate_lineup_ratings behaviour for
  # this PR. Proper fix is a meaningful per-tier prior (or just letting NA
  # propagate if the aggregator handles it). Surface count loudly for now
  # so the imputation is visible.
  n_na_epr <- sum(is.na(ratings$epr))
  for (c in c("epr", "epr_offensive", "epr_defensive")) {
    ratings[[c]][is.na(ratings[[c]])] <- 0
  }
  message(sprintf("  Merged EPR for %d player-seasons (%d had NA → imputed to 0; see panna#74)",
                  sum(ratings$epr != 0), n_na_epr))

  # Merge centrality scores if available
  centrality_path <- file.path("data-raw", "cache-opta", "07b_centrality.rds")
  if (file.exists(centrality_path)) {
    centrality_data <- readRDS(centrality_path)
    if (!is.null(centrality_data) && nrow(centrality_data) > 0 &&
        "player_id" %in% names(centrality_data)) {
      centrality_data <- centrality_data[, c("player_id", "centrality")]
      centrality_data <- centrality_data[!duplicated(centrality_data$player_id), ]
      ratings <- merge(ratings, centrality_data, by = "player_id", all.x = TRUE)
      ratings$centrality[is.na(ratings$centrality)] <- 0
      message(sprintf("  Merged centrality for %d players",
                      sum(ratings$centrality > 0)))
    }
  }
} else if ("combined" %in% names(seasonal_data)) {
  ratings <- seasonal_data$combined
} else {
  ratings <- seasonal_data[[1]]
}

rm(seasonal_data); gc(verbose = FALSE)
message(sprintf("  Ratings: %d player-seasons", nrow(ratings)))

# Validate required columns
required_rating_cols <- c("season_end_year", "panna", "offense", "defense", "spm")
missing <- setdiff(required_rating_cols, names(ratings))
if (length(missing) > 0) {
  stop("Missing required columns in ratings: ", paste(missing, collapse = ", "))
}
if (!any(c("player_id", "player_name") %in% names(ratings))) {
  stop("Ratings must have at least one of 'player_id' or 'player_name' for player matching.")
}

# 5. Load Lineups ----

# Try RAPM cache first
rapm_cache <- file.path("data-raw", "cache-opta", "01_raw_data.rds")
if (file.exists(rapm_cache)) {
  message("  Loading lineups from RAPM cache...")
  raw_data <- readRDS(rapm_cache)
  lineups <- raw_data$lineups
  rm(raw_data); gc(verbose = FALSE)
} else {
  message("  Loading lineups from Opta data...")
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
}

message(sprintf("  Lineups: %d rows", nrow(lineups)))
if (nrow(lineups) == 0) {
  stop("No lineups loaded. Cannot compute team-level ratings. Check data availability and error messages above.")
}

# 6. Aggregate Ratings to Team Level ----

# Get unique season_end_years
sey_values <- unique(played$season_end_year)
message(sprintf("  Processing %d season-years...", length(sey_values)))

all_team_ratings <- list()
n_failed <- 0L

for (sey in sey_values) {
  matches_sey <- played[played$season_end_year == sey, ]
  match_ids <- unique(matches_sey$match_id)

  # Filter lineups to these matches
  lu_sey <- lineups[lineups$match_id %in% match_ids, ]
  if (nrow(lu_sey) == 0) next

  tryCatch({
    tr <- aggregate_lineup_ratings(lu_sey, ratings, season_end_year = sey)
    all_team_ratings[[as.character(sey)]] <- tr
    message(sprintf("    SEY %d: %d matches", sey, nrow(tr)))
  }, error = function(e) {
    n_failed <<- n_failed + 1L
    message(sprintf("    SEY %d ERROR: %s", sey, e$message))
  })
}

if (n_failed > 0) {
  warning(sprintf("%d/%d season-end-years failed team rating aggregation.",
                  n_failed, length(sey_values)), call. = FALSE)
}

team_ratings <- bind_rows(all_team_ratings)
rm(all_team_ratings); gc(verbose = FALSE)

# 7. Handle Future Fixtures ----

# For upcoming fixtures, compute date-specific skill ratings if available
upcoming <- fixture_results[fixture_results$match_status != "Played", ]
if (nrow(upcoming) > 0) {
  message(sprintf("\n  Processing %d upcoming fixtures...", nrow(upcoming)))
  latest_sey <- max(sey_values, na.rm = TRUE)

  # Get most recent lineup per team (last played match).
  #
  # Why supplement: the RAPM cache only contains domestic-league lineups +
  # WC/EURO tournament rosters from 2014/2018, so many international
  # team_ids would fall back to make_dummy_lineup() (= all zeros). Even
  # teams that DID appear in WC 2014/2018 get stuck with decade-old squads
  # (USA's "latest" lineup was the 2014 Tim Howard roster).
  #
  # opta_lineups.parquet has ~7.6M rows across all comps (qualifiers, AFCON,
  # friendlies, Gold Cup, Nations League, CONMEBOL/UEFA/CAF WC quals, etc.).
  # After supplementation, effectively every WC2026 team gets a real recent
  # lineup. The ratings join (player_id × season_end_year) still works —
  # same Opta IDs throughout.
  # Path via opta_data_dir() — works local + GHA. Previous hardcoded
  # "../pannadata/data/opta/..." silently fell through to the RAPM-cache
  # fallback on GHA (since the file isn't at that relative path under the
  # runner's wd), giving most WC2026 teams either decade-old WC 2014/2018
  # squads or all-zero make_dummy_lineup() rows. That fed zero/empty
  # player_ids into the EPR merge → epr=NA for all 48 WC teams.
  # Hard-fail rather than fall through: the RAPM-cache fallback is OK for
  # club leagues but actively broken for WC fixtures, so allowing it to
  # silently kick in is the bug.
  opta_lineups_path <- file.path(opta_data_dir(), "opta_lineups.parquet")
  if (!file.exists(opta_lineups_path)) {
    stop(sprintf(
      "opta_lineups.parquet not found at %s — required for fixture lineup ",
      opta_lineups_path),
      "supplementation. Without it WC2026 teams get all-zero dummy lineups ",
      "and downstream EPR/PSR/Elo aggregations are nonsense for intl fixtures. ",
      "On GHA: confirm opta_lineups.parquet is in predictions-pipeline.yml ",
      "download list.",
      call. = FALSE)
  }
  if (!requireNamespace("arrow", quietly = TRUE)) {
    stop("arrow package not available — required to read opta_lineups.parquet",
         call. = FALSE)
  }
  message(sprintf("  Supplementing fixture lineups from %s", opta_lineups_path))
  opta_all <- as.data.frame(arrow::read_parquet(opta_lineups_path))
  opta_all$match_date <- as.Date(sub("Z$", "", opta_all$match_date))
  # Same is_starter filter; harmonise columns to match RAPM cache schema
  opta_all <- opta_all[opta_all$is_starter == TRUE & !is.na(opta_all$team_id), ]
  # Normalize RAPM-cache match_date to Date (cache stores as character)
  lineups$match_date <- as.Date(sub("Z$", "", as.character(lineups$match_date)))
  # Combine: prefer opta_all (broader coverage), fall back to RAPM cache rows
  common_cols <- intersect(names(opta_all), names(lineups))
  combined_lu <- dplyr::bind_rows(
    opta_all[, common_cols, drop = FALSE],
    lineups[lineups$is_starter == TRUE, common_cols, drop = FALSE]
  )
  rm(opta_all); gc(verbose = FALSE)
  latest_lineups <- combined_lu %>%
    filter(!is.na(team_id)) %>%
    group_by(team_id) %>%
    filter(match_date == max(match_date)) %>%
    ungroup()
  rm(combined_lu)
  rm(lineups); gc(verbose = FALSE)

  # make_dummy_lineup() is defined in R/match_prediction.R

  # Load WC2026 announced-squad override (built by
  # data-raw/match-predictions-opta/announced_squads.R). For WC2026 group
  # games we prefer the officially-announced 26-man squad over the
  # most-recent-played-XI proxy: many federations make 4–10
  # cuts/inclusions vs the last friendly, and our last-played-XI snapshot
  # is sometimes a "look at fringe players" friendly, not a WC-relevant XI.
  #
  # All 26 squad members flow through with lineup_weight =
  # expected_minutes_norm / 90 (sums to ~11 across the squad — same scale
  # as the non-override path of 11 starters at weight 1). The aggregator
  # in R/match_prediction.R consumes lineup_weight for the weighted sums
  # and means; max/min stay unweighted ("best player in the squad").
  ann_squads_path <- file.path(cache_dir, "wc2026_announced_squads.parquet")
  ann_squads <- if (file.exists(ann_squads_path) &&
                    requireNamespace("arrow", quietly = TRUE)) {
    s <- as.data.frame(arrow::read_parquet(ann_squads_path))
    s <- s[!is.na(s$player_id), , drop = FALSE]
    s$lineup_weight <- pmax(0, s$expected_minutes_norm) / 90
    message(sprintf("  WC2026 announced-squad override loaded: %d squad rows across %d teams (mean wt %.2f)",
                    nrow(s), length(unique(s$team_id)),
                    mean(s$lineup_weight)))
    s
  } else NULL
  if (is.null(ann_squads)) {
    # This silent skip shipped last-XI-weighted WC2026 ratings from every
    # GHA run before 2026-06-11 — never let it happen quietly again.
    warning(sprintf(paste(
      "WC2026 announced-squad override NOT loaded (%s) — any WC2026",
      "fixtures in this run will use last-played-XI weighting"),
      if (!file.exists(ann_squads_path)) paste0("missing ", ann_squads_path)
      else "arrow package unavailable"),
      call. = FALSE, immediate. = TRUE)
  }

  # Decide which WC2026 teams have an override eligible. A team is eligible
  # iff it appears in ann_squads AND at least WC2026_OVERRIDE_MIN_RESOLVED
  # of its players resolved to opta player_ids. Below that threshold the
  # EM-weighted aggregation collapses (e.g., 1 resolved player at weight
  # ~1/26 produces a near-zero sum_panna that masquerades as a real team
  # rating). Fall back to latest_lineups in that case with a loud warning.
  wc2026_eligible_ids <- character(0)
  wc2026_too_thin <- character(0)
  if (!is.null(ann_squads)) {
    per_team_resolved <- as.data.frame(table(ann_squads$team_id))
    names(per_team_resolved) <- c("team_id", "n_resolved")
    wc2026_eligible_ids <- as.character(
      per_team_resolved$team_id[per_team_resolved$n_resolved >= WC2026_OVERRIDE_MIN_RESOLVED]
    )
    wc2026_too_thin <- as.character(
      per_team_resolved$team_id[per_team_resolved$n_resolved < WC2026_OVERRIDE_MIN_RESOLVED]
    )
    if (length(wc2026_too_thin) > 0) {
      warning(sprintf(
        "WC2026 override refused for %d team(s) with <%d resolved players (falling back to latest XI): %s",
        length(wc2026_too_thin), WC2026_OVERRIDE_MIN_RESOLVED,
        paste(wc2026_too_thin, collapse = ", ")
      ), call. = FALSE, immediate. = TRUE)
    }
  }

  build_team_synthetic_lineup <- function(team_id, team_name, team_pos, m) {
    use_override <- !is.null(ann_squads) && !is.na(team_id) &&
      isTRUE(!is.na(m$league) && m$league == WC2026_LEAGUE &&
             !is.na(m$season) && m$season == WC2026_SEASON_LABEL) &&
      team_id %in% wc2026_eligible_ids
    if (use_override) {
      ann <- ann_squads[ann_squads$team_id == team_id, , drop = FALSE]
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
             # Force team_name to the fixture-side value (avoids split-identity
             # when latest_lineups carries an Opta name variant).
             team_name = !!team_name)
  }

  # For each upcoming match, construct synthetic lineup rows
  upcoming_lineups <- list()
  n_dummy <- 0L
  n_wc_override <- 0L
  for (i in seq_len(nrow(upcoming))) {
    m <- upcoming[i, ]
    htid <- m$home_team_id
    atid <- m$away_team_id

    # Skip TBD matches (empty team_id from unresolved knockouts)
    if (is.na(htid) || htid == "" || is.na(atid) || atid == "") next

    home_lu <- build_team_synthetic_lineup(htid, m$home_team, "home", m)
    away_lu <- build_team_synthetic_lineup(atid, m$away_team, "away", m)

    # Count override applications using the same eligibility condition as
    # build_team_synthetic_lineup (was: any WC match, any season, no
    # eligibility gate — overcounted WC2014/2018/2022 historical rows).
    wc26_match <- isTRUE(!is.na(m$league) && m$league == WC2026_LEAGUE &&
                          !is.na(m$season) && m$season == WC2026_SEASON_LABEL)
    if (wc26_match) {
      if (htid %in% wc2026_eligible_ids) n_wc_override <- n_wc_override + 1L
      if (atid %in% wc2026_eligible_ids) n_wc_override <- n_wc_override + 1L
    }

    # Fallback to dummy lineup (replacement-level ratings) for unknown teams
    if (nrow(home_lu) == 0) {
      home_lu <- make_dummy_lineup(m$match_id, htid, m$home_team, "home")
      n_dummy <- n_dummy + 1L
    }
    if (nrow(away_lu) == 0) {
      away_lu <- make_dummy_lineup(m$match_id, atid, m$away_team, "away")
      n_dummy <- n_dummy + 1L
    }

    upcoming_lineups[[i]] <- bind_rows(home_lu, away_lu)
  }
  if (n_dummy > 0) message(sprintf("  Used replacement-level ratings for %d team-fixtures with no lineup history", n_dummy))
  if (n_wc_override > 0) message(sprintf("  Applied WC2026 announced-squad override to %d team-fixtures", n_wc_override))

  # Try date-specific skill ratings for fixtures
  fixture_ratings <- NULL
  skill_cache_dir <- file.path("data-raw", "cache-skills")
  match_stats_path <- file.path(skill_cache_dir, "01_match_stats.rds")
  decay_params_path <- file.path(skill_cache_dir, "02b_decay_params.rds")
  skill_spm_path <- file.path(skill_cache_dir, "03_skill_spm.rds")

  if (isTRUE(use_skill_ratings) && file.exists(match_stats_path) &&
      file.exists(skill_spm_path)) {
    tryCatch({
      message("  Computing date-specific skill estimates for fixtures...")

      # Only load match history for players in upcoming lineups (memory optimization)
      upcoming_player_ids <- unique(unlist(lapply(upcoming_lineups, function(x) x$player_id)))
      match_stats <- readRDS(match_stats_path)
      if (length(upcoming_player_ids) > 0 && "player_id" %in% names(match_stats)) {
        match_stats <- match_stats[match_stats$player_id %in% upcoming_player_ids, ]
        message(sprintf("  Filtered match_stats to %d players (%d rows)",
                        length(upcoming_player_ids), nrow(match_stats)))
      }

      decay_params <- if (file.exists(decay_params_path)) readRDS(decay_params_path) else NULL
      skill_spm <- readRDS(skill_spm_path)

      # Compute skills at the earliest fixture date (close enough for all)
      fixture_date <- min(as.Date(upcoming$match_date), na.rm = TRUE)
      live_skills <- estimate_player_skills_at_date(
        match_stats = match_stats,
        decay_params = decay_params,
        date = fixture_date
      )
      rm(match_stats, decay_params); gc(verbose = FALSE)

      if (!is.null(live_skills) && nrow(live_skills) > 0) {
        # estimate_player_skills() now outputs "primary_position" directly

        # Add position dummies (SPM models use these as predictors)
        if ("primary_position" %in% names(live_skills)) {
          pos <- live_skills$primary_position
          live_skills$is_gk <- as.integer(grepl("GK|Goalkeeper", pos, ignore.case = TRUE))
          live_skills$is_df <- as.integer(grepl("DEF|Defender", pos, ignore.case = TRUE))
          live_skills$is_mf <- as.integer(grepl("MID|Midfielder", pos, ignore.case = TRUE))
          live_skills$is_fw <- as.integer(grepl("FWD|FW|Forward|Striker", pos, ignore.case = TRUE))
        }

        # Add total_minutes and n_matches (SPM models may use these)
        if (!"total_minutes" %in% names(live_skills)) {
          live_skills$total_minutes <- live_skills$weighted_90s * 90
        }
        if (!"n_matches" %in% names(live_skills)) {
          live_skills$n_matches <- 0L
        }

        # Add mins_per_90 for SPM model compatibility
        if (!"mins_per_90" %in% names(live_skills)) {
          live_skills$mins_per_90 <- live_skills$weighted_90s
        }

        # Generate SPM predictions from live skills
        off_blend <- calculate_spm_blend(
          live_skills, skill_spm$offense_spm_glmnet, skill_spm$offense_spm_xgb
        )
        def_blend <- calculate_spm_blend(
          live_skills, skill_spm$defense_spm_glmnet, skill_spm$defense_spm_xgb
        )

        # Build fixture-specific ratings table.
        # CRITICAL: keep player_id from off_blend/def_blend. The previous code
        # selected only (player_name, total_minutes, offense_spm), dropping
        # player_id silently. The PSR + EPR joins below use `by = "player_id"`
        # so they failed (caught by their tryCatch, warned once), leaving
        # fixture_ratings with no PSR/EPR columns at all — which then
        # propagated as 0 home_sum_psr / home_sum_epr for every fixture in
        # step 04's NA-fill, hollowing out two model features for ALL
        # upcoming-fixture predictions including WC2026. Also: joining
        # off_blend × def_blend on player_name produced many-to-many
        # duplicates (a single name shared across player_ids); join on
        # player_id instead, dedupe, and pin relationship = "one-to-one" so
        # a future schema drift can't silently reintroduce the row-inflation.
        off_join_cols <- intersect(c("player_id", "player_name"), names(off_blend))
        def_join_cols <- intersect(c("player_id", "player_name"), names(def_blend))
        join_keys <- intersect(off_join_cols, def_join_cols)
        fixture_ratings <- off_blend %>%
          select(any_of(c("player_id", "player_name", "total_minutes")),
                 offense_spm = spm) %>%
          distinct(across(any_of(off_join_cols)), .keep_all = TRUE) %>%
          inner_join(
            def_blend %>%
              select(any_of(c("player_id", "player_name")), defense_spm = spm) %>%
              distinct(across(any_of(def_join_cols)), .keep_all = TRUE),
            by = join_keys,
            relationship = "one-to-one"
          ) %>%
          mutate(
            panna = offense_spm - defense_spm,
            offense = offense_spm,
            defense = defense_spm,
            spm = panna,
            season_end_year = latest_sey
          )

        # Add PSR/OSR/DSR from live skills.
        #
        # Pre-check preconditions explicitly rather than wrap the whole block
        # in a catch-all tryCatch — the previous catch-all turned a real
        # schema bug (player_id missing from fixture_ratings, see above) into
        # a single one-line warning that scrolled by in 20+ other pipeline
        # warnings, hollowing PSR for ALL fixtures for who-knows-how-long.
        # Skip silently only when PSR data is genuinely absent; let any
        # other failure mode propagate so it's caught in the next run.
        live_psr <- compute_player_psr(live_skills, center = TRUE)
        if (is.null(live_psr) || nrow(live_psr) == 0) {
          message("  PSR skipped: compute_player_psr returned no rows")
        } else if (!"player_id" %in% names(fixture_ratings)) {
          warning("Fixture PSR skipped: fixture_ratings has no player_id ",
                  "column (upstream select likely dropped it). Investigate ",
                  "step 02's fixture-ratings construction.",
                  call. = FALSE, immediate. = TRUE)
        } else if (!"player_id" %in% names(live_psr)) {
          warning("Fixture PSR skipped: live_psr has no player_id column. ",
                  "Investigate compute_player_psr output.",
                  call. = FALSE, immediate. = TRUE)
        } else {
          live_psr <- as.data.frame(live_psr)
          psr_cols <- intersect(c("player_id", "psr", "osr", "dsr"), names(live_psr))
          fixture_ratings <- fixture_ratings %>%
            left_join(live_psr[, psr_cols], by = "player_id",
                      relationship = "many-to-one")
          fixture_ratings$psr[is.na(fixture_ratings$psr)] <- 0
          if ("osr" %in% names(fixture_ratings)) fixture_ratings$osr[is.na(fixture_ratings$osr)] <- 0
          if ("dsr" %in% names(fixture_ratings)) fixture_ratings$dsr[is.na(fixture_ratings$dsr)] <- 0
          message(sprintf("  Added PSR for %d fixture players", sum(fixture_ratings$psr != 0)))
        }

        # Add EPR from the most recent weekly snapshot.
        # Path via opta_data_dir() — works local + GHA. Previously used a
        # hardcoded "../pannadata/data/opta/..." that silently returned FALSE
        # on GHA and skipped the entire merge, shipping wc2026 fixtures with
        # NA EPR. This is the FIXTURE-side fix; the played-side at line ~120
        # was fixed in a00f679. Don't trust silent skip patterns on required
        # inputs — EPR is required for WC2026 fixtures.
        epr_path <- file.path(opta_data_dir(), "opta_epr_weekly.parquet")
        if (!file.exists(epr_path)) {
          stop(sprintf(
            "Fixture EPR file not found at %s — required for WC2026 fixtures. ",
            epr_path),
            "On GHA: check that opta_epr_weekly.parquet is in the predictions-",
            "pipeline.yml download list. Locally: run the EPR weekly pipeline.",
            call. = FALSE)
        } else if (!requireNamespace("arrow", quietly = TRUE)) {
          stop("arrow package not available — required to read opta_epr_weekly.parquet",
               call. = FALSE)
        } else if (!"player_id" %in% names(fixture_ratings)) {
          warning("Fixture EPR skipped: fixture_ratings has no player_id ",
                  "column. Investigate step 02's fixture-ratings construction.",
                  call. = FALSE, immediate. = TRUE)
        } else {
          epr_w <- as.data.frame(arrow::read_parquet(epr_path))
          epr_w$snapshot_date <- as.Date(epr_w$snapshot_date)
          latest <- max(epr_w$snapshot_date)
          live_epr <- epr_w[epr_w$snapshot_date == latest,
                              c("player_id", "epr", "epr_offensive", "epr_defensive")]
          fixture_ratings <- fixture_ratings %>%
            left_join(live_epr, by = "player_id",
                      relationship = "many-to-one")
          for (c in c("epr", "epr_offensive", "epr_defensive")) {
            fixture_ratings[[c]][is.na(fixture_ratings[[c]])] <- 0
          }
          message(sprintf("  Added EPR for %d fixture players (snapshot %s)",
                          sum(fixture_ratings$epr != 0), latest))
        }

        message(sprintf("  Live skill ratings for %d players at %s",
                        nrow(fixture_ratings), fixture_date))
      }
    }, error = function(e) {
      warning(sprintf("Date-specific skills failed: %s (using seasonal fallback)", e$message),
              call. = FALSE)
    })
  }

  if (length(upcoming_lineups) > 0) {
    upcoming_lu <- bind_rows(upcoming_lineups)
    # Use fixture-specific ratings if available, else seasonal fallback
    fixture_rat <- if (!is.null(fixture_ratings)) fixture_ratings else ratings
    tryCatch({
      upcoming_tr <- aggregate_lineup_ratings(upcoming_lu, fixture_rat,
                                               season_end_year = latest_sey)
      team_ratings <- bind_rows(team_ratings, upcoming_tr)
      message(sprintf("  Added %d fixture team ratings", nrow(upcoming_tr)))
    }, error = function(e) {
      warning(sprintf("Fixture team ratings failed: %s", e$message), call. = FALSE)
    })
  }
}

message(sprintf("\nTotal: %d matches with team ratings", nrow(team_ratings)))

# 7b. End-of-step assertions ----
#
# Output validation that would have caught the EPR/PSR all-zero bug
# (2026-05-28) the first time it ran. Pattern: when you fix a bug, ALSO
# add the assertion that would have caught it. The assertion ships as
# the falsifiable claim "this is what this step's output should look like."
assert_step_output(
  data.table::as.data.table(team_ratings),
  "step 02",
  list(
    "row count plausible (>10k matches across all leagues+fixtures)" =
      function(d) nrow(d) > 10000L,
    "EPR + PSR columns populated (not all NA from a join failure)" =
      function(d) {
        cols <- intersect(c("home_sum_epr", "home_sum_psr",
                             "away_sum_epr", "away_sum_psr"),
                          names(d))
        if (length(cols) == 0L) return(TRUE)
        all(vapply(cols, function(c) sum(!is.na(d[[c]])) > 0.5 * nrow(d),
                   logical(1)))
      },
    "WC2026 fixtures have nonzero sum_epr (catches the player_id-drop bug)" =
      function(d) {
        wc <- d[league == WC2026_LEAGUE & season == WC2026_SEASON_LABEL]
        if (nrow(wc) == 0L) return(TRUE)  # no WC2026 fixtures yet
        # At least 80% of WC2026 fixtures should have non-zero home_sum_epr
        # (a few teams legitimately have no EPR coverage; total zero is the bug)
        mean(wc$home_sum_epr != 0, na.rm = TRUE) > 0.8
      },
    "WC2026 fixtures have nonzero sum_psr" =
      function(d) {
        wc <- d[league == WC2026_LEAGUE & season == WC2026_SEASON_LABEL]
        if (nrow(wc) == 0L) return(TRUE)
        mean(wc$home_sum_psr != 0, na.rm = TRUE) > 0.8
      }
  )
)

# 8. Save ----

saveRDS(team_ratings, output_path)
message(sprintf("Saved to: %s", output_path))
