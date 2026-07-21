# 06_seasonal_skill_ratings.R
# Fit seasonal RAPM with skill-based SPM prior → seasonal skill-based xRAPM
#
# For each season, computes skill-based SPM predictions using the pre-trained
# skill SPM models (from step 03), then uses those as Bayesian priors for
# seasonal RAPM fitting. Produces output matching cache-opta/07_seasonal_ratings.rds
# so match predictions can swap in skill-based ratings.

# 1. Setup ----

library(dplyr)
devtools::load_all()

# 2. Configuration ----

cache_dir <- file.path("data-raw", "cache-skills")
opta_cache_dir <- file.path("data-raw", "cache-opta")
seasonal_lambda <- "min"

cat(sprintf("Using lambda = %s for seasonal ratings\n", seasonal_lambda))

# Within-position normalization artifact (per-role skill means). Loaded once;
# passed to compute_player_psr so PSR values a player vs their role. Set
# position_normalize <- FALSE before sourcing to disable.
.psr_position_means <- if (exists("position_normalize") && !isTRUE(position_normalize)) {
  NULL
} else load_position_role_means()

# extract_season_end_year() is defined in R/utils.R

# 3. Load Data ----

cat("\n=== Loading Data ===\n")

# Splints for seasonal RAPM fitting
splint_data <- readRDS(file.path(opta_cache_dir, "03_splints.rds"))

# Skill features (per player-season from step 02)
skill_features <- readRDS(file.path(cache_dir, "02_skill_features.rds"))

# Skill SPM models (from step 03)
spm_results <- readRDS(file.path(cache_dir, "03_skill_spm.rds"))

cat("Splints:", nrow(splint_data$splints), "\n")
cat("Skill feature rows:", nrow(skill_features), "\n")

# Primary league per player-season -- used in section 6c to ATTACH each player's
# league to the seasonal PSR table so the network league offsets can be joined
# (the offsets themselves are estimated from per-game game logs, not this).
# Seasonal PSR is one row per player-season with no per-game league, so we take
# the league where the player logged the most minutes. Kept as a standalone
# lookup so it survives the skill_features rm() before section 6c.
psr_primary_league <- local({
  ms <- data.table::as.data.table(readRDS(file.path(cache_dir, "01_match_stats.rds")))
  lg_col <- if ("competition" %in% names(ms)) "competition" else
            if ("league" %in% names(ms)) "league" else NULL
  m_col  <- if ("total_minutes" %in% names(ms)) "total_minutes" else
            if ("minutes_played" %in% names(ms)) "minutes_played" else NULL
  if (is.null(lg_col) || is.null(m_col)) {
    warning("01_match_stats lacks competition/minutes; PSR league offsets disabled.",
            call. = FALSE)
    return(NULL)
  }
  if (!"season_end_year" %in% names(ms)) {
    if ("season" %in% names(ms)) {
      ms[, season_end_year := as.integer(vapply(season, extract_season_end_year, numeric(1)))]
    } else {
      d <- as.Date(ms$match_date)
      ms[, season_end_year := data.table::fifelse(
        data.table::month(d) >= 7L, data.table::year(d) + 1L, data.table::year(d))]
    }
  }
  pl <- ms[!is.na(get(lg_col)) & !is.na(season_end_year),
           .(mins = sum(as.numeric(get(m_col)), na.rm = TRUE)),
           by = c("player_id", "season_end_year", lg_col)]
  data.table::setnames(pl, lg_col, "league")
  data.table::setorder(pl, player_id, season_end_year, -mins)
  pl <- pl[, .(league = league[1L]), by = .(player_id, season_end_year)]
  cat(sprintf("Primary league lookup: %d player-seasons, %d leagues\n",
              nrow(pl), data.table::uniqueN(pl$league)))
  pl
})

# Filter bad xG data (same threshold as Opta pipeline)
filter_result <- filter_bad_xg_data(splint_data, zero_xg_threshold = ZERO_XG_THRESHOLD_OPTA, verbose = TRUE)
splint_data <- filter_result$splint_data

seasons <- sort(unique(splint_data$splints$season_end_year))
cat("\nAvailable seasons:", paste(seasons, collapse = ", "), "\n")

# 4. Load SPM Models ----

cat("\n=== Loading Skill SPM Models ===\n")
cat("Using 50/50 Elastic Net + XGBoost blend\n")

offense_spm_glmnet <- spm_results$offense_spm_glmnet
offense_spm_xgb <- spm_results$offense_spm_xgb
defense_spm_glmnet <- spm_results$defense_spm_glmnet
defense_spm_xgb <- spm_results$defense_spm_xgb

# Free memory
rm(spm_results); gc(verbose = FALSE)

required_models <- list(
  offense_spm_glmnet = offense_spm_glmnet,
  offense_spm_xgb = offense_spm_xgb,
  defense_spm_glmnet = defense_spm_glmnet,
  defense_spm_xgb = defense_spm_xgb
)

missing_models <- names(required_models)[sapply(required_models, is.null)]
if (length(missing_models) > 0) {
  stop(sprintf(
    "Missing required skill SPM models: %s\nRe-run 03_skill_spm.R to generate O/D models.",
    paste(missing_models, collapse = ", ")
  ))
}

# 5. Define Season Processing Function ----

fit_season_skill_ratings <- function(splint_data, skill_features, season,
                                      offense_spm_glmnet, offense_spm_xgb,
                                      defense_spm_glmnet, defense_spm_xgb,
                                      min_minutes_spm = 200, min_minutes_rapm = 200) {
  cat(sprintf("\n--- Season %d ---\n", season))

  # Filter splints to this season
  season_splints <- splint_data$splints[splint_data$splints$season_end_year == season, ]
  season_splint_ids <- season_splints$splint_id
  season_players <- splint_data$players[splint_data$players$splint_id %in% season_splint_ids, ]

  season_splint_data <- list(
    splints = season_splints,
    players = season_players,
    match_info = splint_data$match_info
  )

  cat(sprintf("  Splints: %d, Player-splint records: %d\n",
              nrow(season_splints), nrow(season_players)))

  if (nrow(season_splints) < 100) {
    warning(sprintf("Season %d has only %d splints, skipping", season, nrow(season_splints)))
    return(NULL)
  }

  # Get skill features for this season
  season_skills <- skill_features[skill_features$season_end_year == season, ]

  cat(sprintf("  Players with skill features: %d\n", nrow(season_skills)))

  if (nrow(season_skills) == 0) {
    warning(sprintf("Season %d has no skill features, skipping", season))
    return(NULL)
  }

  # Ensure required columns for SPM prediction
  if (!"mins_per_90" %in% names(season_skills)) {
    season_skills$mins_per_90 <- season_skills$total_minutes / 90
  }

  # Ensure player_name and total_minutes exist for SPM model compatibility
  if (!"player_name" %in% names(season_skills)) {
    season_skills$player_name <- season_skills$player_id
  }
  if (!"total_minutes" %in% names(season_skills)) {
    season_skills$total_minutes <- 0
  }

  # Calculate season-specific SPM predictions using skill SPM models (blended)
  off_glmnet <- calculate_spm_ratings(season_skills, offense_spm_glmnet)
  off_xgb <- calculate_spm_ratings_xgb(season_skills, offense_spm_xgb)

  offense_spm_season <- off_glmnet %>%
    rename(off_glmnet = spm) %>%
    inner_join(off_xgb %>% select(player_id, off_xgb = spm), by = "player_id") %>%
    mutate(offense_spm = 0.5 * off_glmnet + 0.5 * off_xgb)

  def_glmnet <- calculate_spm_ratings(season_skills, defense_spm_glmnet)
  def_xgb <- calculate_spm_ratings_xgb(season_skills, defense_spm_xgb)

  defense_spm_season <- def_glmnet %>%
    rename(def_glmnet = spm) %>%
    inner_join(def_xgb %>% select(player_id, def_xgb = spm), by = "player_id") %>%
    mutate(defense_spm = 0.5 * def_glmnet + 0.5 * def_xgb)

  cat(sprintf("  Season SPM predictions: %d offense, %d defense\n",
              nrow(offense_spm_season), nrow(defense_spm_season)))

  # Create seasonal SPM ratings table
  seasonal_spm <- offense_spm_season %>%
    select(player_id, player_name, total_minutes, offense_spm) %>%
    inner_join(
      defense_spm_season %>% select(player_id, defense_spm),
      by = "player_id"
    ) %>%
    mutate(
      spm = offense_spm - defense_spm,
      season_end_year = season
    ) %>%
    arrange(desc(spm))

  # Prepare RAPM data for this season
  rapm_data <- prepare_rapm_data(
    season_splint_data,
    min_minutes = min_minutes_rapm,
    include_covariates = TRUE
  )

  cat(sprintf("  Players meeting RAPM min_minutes (%d): %d\n",
              min_minutes_rapm, rapm_data$n_players))

  if (rapm_data$n_players < 50) {
    warning(sprintf("Season %d has only %d players, skipping", season, rapm_data$n_players))
    return(NULL)
  }

  n_folds <- min(10, floor(nrow(rapm_data$X) / 20))
  n_folds <- max(n_folds, 3)

  # Fit base RAPM
  rapm_model <- fit_rapm(rapm_data, alpha = 0, nfolds = n_folds, use_weights = TRUE)
  seasonal_rapm <- extract_rapm_ratings(rapm_model, lambda = seasonal_lambda)
  seasonal_rapm$season_end_year <- season

  cat(sprintf("  Seasonal RAPM ratings: %d players\n", nrow(seasonal_rapm)))

  # Build prior vectors for xRAPM
  player_mapping <- rapm_data$player_mapping

  offense_prior <- build_prior_vector(
    spm_data = offense_spm_season,
    spm_col = "offense_spm",
    player_mapping = player_mapping
  )

  defense_prior <- build_prior_vector(
    spm_data = defense_spm_season,
    spm_col = "defense_spm",
    player_mapping = player_mapping
  )

  cat(sprintf("  Matched skill SPM priors: %d offense, %d defense\n",
              sum(offense_prior != 0), sum(defense_prior != 0)))

  # Fit xRAPM with skill-based SPM prior
  xrapm_model <- fit_rapm_with_prior(
    rapm_data,
    offense_prior = offense_prior,
    defense_prior = defense_prior,
    alpha = 0,
    nfolds = n_folds,
    use_weights = TRUE,
    penalize_covariates = FALSE
  )

  seasonal_xrapm <- extract_xrapm_ratings(xrapm_model, lambda = seasonal_lambda)
  seasonal_xrapm$season_end_year <- season

  cat(sprintf("  Seasonal xRAPM ratings: %d players\n", nrow(seasonal_xrapm)))

  # Compute PSR/OSR/DSR from season skills using bundled coefficients.
  # Cross-league calibration (transfer-graph offsets) is applied AFTER the loop,
  # once the full multi-season panel exists -- see "Cross-league PSR offsets".
  seasonal_psr <- tryCatch({
    # Within-position normalization (BPM-style): value a player vs their role,
    # not vs all outfielders. Display-only (the RAPM psvf90 target is untouched).
    psr_result <- compute_player_psr(season_skills, center = TRUE,
                                     position_means = .psr_position_means)
    if (!is.null(psr_result) && nrow(psr_result) > 0) {
      psr_result$season_end_year <- season
      cat(sprintf("  Seasonal PSR ratings: %d players\n", nrow(psr_result)))
      psr_result
    } else {
      NULL
    }
  }, error = function(e) {
    warning(sprintf("PSR computation failed: %s", e$message), call. = FALSE)
    NULL
  })

  list(
    spm = seasonal_spm,
    rapm = seasonal_rapm,
    xrapm = seasonal_xrapm,
    psr = seasonal_psr
  )
}

# 6. Process All Seasons ----

cat("\n=== Processing All Seasons ===\n")
cat(sprintf("Processing %d seasons: %s\n",
            length(seasons), paste(seasons, collapse = ", ")))

error_messages <- character(0)
seasonal_ratings_list <- lapply(seasons, function(season) {
  tryCatch({
    fit_season_skill_ratings(
      splint_data = splint_data,
      skill_features = skill_features,
      season = season,
      offense_spm_glmnet = offense_spm_glmnet,
      offense_spm_xgb = offense_spm_xgb,
      defense_spm_glmnet = defense_spm_glmnet,
      defense_spm_xgb = defense_spm_xgb,
      min_minutes_spm = 200,
      min_minutes_rapm = 200
    )
  }, error = function(e) {
    error_messages[as.character(season)] <<- e$message
    warning(sprintf("Failed to process season %d: %s", season, e$message))
    NULL
  })
})

# Free memory
rm(splint_data, skill_features); gc(verbose = FALSE)

n_total <- length(seasons)
n_failed <- sum(vapply(seasonal_ratings_list, is.null, logical(1)))
seasonal_ratings_list <- Filter(Negate(is.null), seasonal_ratings_list)

if (length(seasonal_ratings_list) == 0) {
  unique_errors <- unique(error_messages)
  if (length(unique_errors) == 1) {
    stop(sprintf("All %d seasons failed with the same error: %s", n_total, unique_errors))
  }
  stop("All seasons failed to process. Cannot generate seasonal skill ratings.")
}

if (n_failed > 0) {
  pct_failed <- round(100 * n_failed / n_total, 1)
  unique_errors <- unique(error_messages)
  if (length(unique_errors) == 1) {
    warning(sprintf(
      "%d/%d (%.1f%%) seasons failed with same error: %s",
      n_failed, n_total, pct_failed, unique_errors
    ), call. = FALSE)
  } else {
    warning(sprintf(
      "%d/%d (%.1f%%) seasons failed to process. Results may be incomplete.",
      n_failed, n_total, pct_failed
    ), call. = FALSE)
  }
  if (pct_failed > 50) {
    stop(sprintf(
      "%d/%d (%.1f%%) seasons failed. This suggests a systematic issue. Aborting.",
      n_failed, n_total, pct_failed
    ))
  }
}

seasonal_spm <- bind_rows(lapply(seasonal_ratings_list, `[[`, "spm"))
seasonal_rapm <- bind_rows(lapply(seasonal_ratings_list, `[[`, "rapm"))
seasonal_xrapm <- bind_rows(lapply(seasonal_ratings_list, `[[`, "xrapm"))
seasonal_psr <- bind_rows(Filter(Negate(is.null), lapply(seasonal_ratings_list, `[[`, "psr")))

# 6b. Override total_minutes with box-score minutes (panna#74) ----
#
# The `total_minutes` carried through the seasonal tables above is SPLINT-derived
# (sum of RAPM splint `duration`), so it tracks splint-cache completeness, not
# minutes actually played — e.g. Salah surfaced 511.5 / 3110.65 instead of the
# true ~3058. Override it with box-score minutes (sum of `minsPlayed`) from the
# consolidated opta_player_stats.parquet, keeping the splint value only as a
# fallback for players absent from player_stats (never silently zero anyone).

cat("\n=== Overriding total_minutes with box-score minutes (panna#74) ===\n")

# Read the consolidated opta_player_stats.parquet (the same file the loaders use,
# resolved via the package's own accessor). Pull only the 3 needed columns so we
# don't materialise the ~200-column box-score table.
ps_path <- download_opta_release_file("opta_player_stats.parquet", source = "local")
ps_conn <- DBI::dbConnect(duckdb::duckdb())
opta_player_stats <- tryCatch(
  DBI::dbGetQuery(ps_conn, sprintf(
    "SELECT player_id, season, minsPlayed FROM '%s'",
    normalizePath(ps_path, winslash = "/", mustWork = TRUE))),
  finally = DBI::dbDisconnect(ps_conn, shutdown = TRUE)
)

# Season-label trap guard: NEVER match exact "YYYY-YYYY" strings. Three label
# formats share one end year ("2025-2026" European, "2026" calendar leagues,
# "2026 Canada-Mexico-USA" tournaments); exact-string matching silently drops
# the calendar-league + tournament rows. Always derive end year via the helper.
box_minutes <- opta_player_stats %>%
  mutate(season_end_year = vapply(season, extract_season_end_year, numeric(1))) %>%
  filter(!is.na(season_end_year), !is.na(player_id)) %>%
  group_by(player_id, season_end_year) %>%
  summarise(box_minutes = sum(minsPlayed, na.rm = TRUE), .groups = "drop")

cat(sprintf("Box-minutes table: %d player-seasons from %d player_stats rows\n",
            nrow(box_minutes), nrow(opta_player_stats)))

rm(opta_player_stats); gc(verbose = FALSE)

# Left-join box minutes and prefer them; splint-derived value remains as fallback.
# na_if(box_minutes, 0): some players have anomalous zero-minute box rows in old
# seasons (e.g. Alaba 2014 — 55 rows summing to 0 min while splints have his real
# minutes). coalesce only falls back on NA, so a 0 would silently override a valid
# splint value and zero out a top-50 player. Treat box_minutes == 0 as missing so
# the splint fallback applies. Preserves the panna#74 fix (box preferred when real).
.override_minutes <- function(tbl) {
  if (is.null(tbl) || nrow(tbl) == 0) return(tbl)
  tbl %>%
    left_join(box_minutes, by = c("player_id", "season_end_year")) %>%
    mutate(total_minutes = coalesce(dplyr::na_if(box_minutes, 0), total_minutes)) %>%
    select(-box_minutes)
}

seasonal_xrapm <- .override_minutes(seasonal_xrapm)
seasonal_spm   <- .override_minutes(seasonal_spm)
seasonal_rapm  <- .override_minutes(seasonal_rapm)

n_matched <- seasonal_xrapm %>%
  inner_join(box_minutes, by = c("player_id", "season_end_year")) %>%
  nrow()
cat(sprintf("xRAPM player-seasons with box-minute override: %d / %d (rest keep splint fallback)\n",
            n_matched, nrow(seasonal_xrapm)))

# Hard-stop sanity check: catch a regression like the 511.5 bug. Any top-50
# (by xRAPM) player with < 900 total minutes after the override is a red flag —
# top-rated regulars play far more than 10 full matches in a season.
top50_low_minutes <- seasonal_xrapm %>%
  group_by(season_end_year) %>%
  arrange(desc(xrapm)) %>%
  slice_head(n = 50) %>%
  ungroup() %>%
  filter(total_minutes < 900) %>%
  arrange(season_end_year, total_minutes)

# Scope the HARD-STOP to recent seasons. The check exists to catch a *regression*
# (sudden minutes corruption), which would manifest in current data. Old seasons
# (pre-2016ish) have chronically incomplete minutes — both box (anomalous 0-min
# rows) and splint (only 1-3 matches captured) — so legit stars show 90-270 min
# there; that's a known data limitation, not a regression. Warn on old, stop on
# recent.
current_season <- suppressWarnings(max(seasonal_xrapm$season_end_year, na.rm = TRUE))
recent_floor <- current_season - 3
# The 900-min "full season" expectation only holds for COMPLETED non-tournament
# seasons. Exempt: (a) old seasons (chronically incomplete minutes), and (b) the
# CURRENT/in-progress season — which in a World Cup year (e.g. 2026) is also a
# tournament where top performers legitimately have ~270-630 min (a few matches).
# A real splint-minutes regression corrupts MANY established players in COMPLETED
# recent seasons; that's the only case that hard-stops.
old_bad     <- top50_low_minutes %>% filter(season_end_year <  recent_floor)
current_bad <- top50_low_minutes %>% filter(season_end_year == current_season)
recent_bad  <- top50_low_minutes %>%
  filter(season_end_year >= recent_floor & season_end_year < current_season)

if (nrow(old_bad) > 0 || nrow(current_bad) > 0) {
  warning(sprintf(paste0(
    "%d old-season + %d current-season (%g) top-50-by-xRAPM player-season(s) ",
    "have < 900 min — known incomplete (old data gaps / in-progress or tournament ",
    "season), not a regression. Not blocking."),
    nrow(old_bad), nrow(current_bad), current_season), call. = FALSE)
}
if (nrow(recent_bad) > 0) {
  print(recent_bad %>%
          select(season_end_year, player_id, player_name, xrapm, total_minutes) %>%
          head(20))
  stop(sprintf(
    paste0("total_minutes sanity check FAILED: %d RECENT (>= %g) top-50-by-xRAPM ",
           "player-season(s) have < 900 minutes after box-minute override. This ",
           "looks like the splint-minutes regression (panna#74). Investigate."),
    nrow(recent_bad), recent_floor
  ))
}
cat(sprintf("total_minutes sanity check passed (recent >= %g clean; %d old-season gaps warned)\n",
            recent_floor, nrow(old_bad)))

cat(sprintf("\n=== Combined Results ===\n"))
cat(sprintf("Seasons processed: %d\n", length(seasonal_ratings_list)))
cat(sprintf("Seasonal SPM:  %d player-seasons, %d unique players\n",
            nrow(seasonal_spm), n_distinct(seasonal_spm$player_name)))
cat(sprintf("Seasonal RAPM: %d player-seasons, %d unique players\n",
            nrow(seasonal_rapm), n_distinct(seasonal_rapm$player_name)))
cat(sprintf("Seasonal xRAPM: %d player-seasons, %d unique players\n",
            nrow(seasonal_xrapm), n_distinct(seasonal_xrapm$player_name)))
if (nrow(seasonal_psr) > 0) {
  cat(sprintf("Seasonal PSR:  %d player-seasons, %d unique players\n",
              nrow(seasonal_psr), n_distinct(seasonal_psr$player_id)))
}

# 6c. Cross-league PSR offsets (transfer-graph calibration) ----
#
# PSR is built from box-score rates that barely vary by league, so strong
# players in weakly-connected leagues (A-League, Brazil, MLS, ...) post inflated
# PSR. Those leagues can't be reached by Elo / opponent controls (no shared
# matches) -- but each metric's own per-game value, pooled over the full
# same-season co-occurrence network, does. compute_psr_league_offsets() runs
# build_league_network() on per-game PSV (PSR's own analogue, so no rescaling)
# and we ADD the offset so PSR is on a Big-5-equivalent scale. The offset table
# is saved so step 08b (weekly snapshots) applies the identical offsets.
# Game logs come from the predictions pipeline (cache-predictions-opta); if
# absent (fresh clone, predictions not yet run) we skip — offsets are stable, so
# a one-cycle lag is fine.
if (nrow(seasonal_psr) > 0 && !is.null(psr_primary_league)) {
  cat("\n=== Cross-league PSR offsets (PSV 2-year-bucket network) ===\n")
  gl_dir <- file.path("data-raw", "cache-predictions-opta")
  gl_files <- list.files(gl_dir, pattern = "^game_logs_20.*\\.parquet$", full.names = TRUE)
  psr_offsets <- if (length(gl_files) == 0) {
    warning("No game_logs_*.parquet found; PSR league offsets skipped ",
            "(run the predictions pipeline first).", call. = FALSE); NULL
  } else tryCatch({
    gl <- data.table::rbindlist(lapply(gl_files, function(f) {
      d <- arrow::read_parquet(f)
      d[, intersect(c("player_id","season","league","total_minutes","psv","psv_league_offset"), names(d)), with = FALSE]
    }), use.names = TRUE, fill = TRUE)
    # 10b end-adds psv_league_offset INTO psv in these parquets (#162). The
    # calibration must see the offset-free signal: feeding the adjusted psv
    # back into build_league_network() makes each cycle estimate the residual
    # of its own previous output (offset_N ~= true - offset_{N-1}), so the
    # offsets oscillate/decay instead of converging. Parquets predating #162
    # lack the column (rbindlist fills NA -> treated as 0).
    if ("psv_league_offset" %in% names(gl)) {
      gl[, psv := psv - data.table::fcoalesce(as.numeric(psv_league_offset), 0)]
      gl[, psv_league_offset := NULL]
    }
    # bucket_years=2: bridge leagues a player straddles across adjacent seasons,
    # not only within one season. Fixes the same-season network's connectivity
    # starvation for leagues with no UCL co-occurrence (Argentina/Saudi/MLS),
    # widening their offsets toward the mover/EPV evidence; well-connected leagues
    # are ~unchanged (overall spread ~constant, bridges +50%).
    compute_psr_league_offsets(gl, bucket_years = 2L, verbose = TRUE)
  }, error = function(e) {
    warning("PSR offset estimation failed: ", e$message, call. = FALSE); NULL
  })

  if (!is.null(psr_offsets) && nrow(psr_offsets) > 0) {
    off_path <- file.path(cache_dir, "psr_league_offsets.parquet")
    arrow::write_parquet(psr_offsets, off_path)
    write.csv(psr_offsets, sub("\\.parquet$", ".csv", off_path), row.names = FALSE)
    cat(sprintf("  Saved offsets: %s (%d leagues)\n",
                basename(off_path), nrow(psr_offsets)))
    # Attach each player-season's primary league, then add the offset (full).
    psr_dt <- data.table::as.data.table(seasonal_psr)
    if ("league" %in% names(psr_dt)) psr_dt[, league := NULL]
    psr_dt <- merge(psr_dt, psr_primary_league,
                    by = c("player_id", "season_end_year"), all.x = TRUE)
    psr_dt <- apply_psr_league_offsets(psr_dt, psr_offsets, verbose = TRUE)
    psr_dt[, league := NULL]  # don't leak league into the saved seasonal table
    seasonal_psr <- as.data.frame(psr_dt)
  }
}

# 7. Summary Statistics ----

cat("\n=== Top Players by Season (Skill xRAPM) ===\n")

for (s in sort(unique(seasonal_xrapm$season_end_year))) {
  cat(sprintf("\nTop 10 xRAPM - Season %d:\n", s))
  print(
    seasonal_xrapm %>%
      filter(season_end_year == s) %>%
      arrange(desc(xrapm)) %>%
      head(10) %>%
      select(player_name, xrapm, offense, defense, off_prior, def_prior, total_minutes)
  )
}

# 8. Compare with Raw Seasonal Ratings ----

cat("\n=== Comparison with Raw-Stat Seasonal Ratings ===\n")

raw_seasonal_path <- file.path(opta_cache_dir, "07_seasonal_ratings.rds")
if (file.exists(raw_seasonal_path)) {
  raw_seasonal <- readRDS(raw_seasonal_path)
  raw_xrapm <- raw_seasonal$seasonal_xrapm

  comp <- seasonal_xrapm %>%
    select(player_id, player_name, season_end_year, skill_xrapm = xrapm) %>%
    inner_join(
      raw_xrapm %>% select(player_id, season_end_year, raw_xrapm = xrapm),
      by = c("player_id", "season_end_year")
    )

  if (nrow(comp) > 0) {
    cat(sprintf("Overlapping player-seasons: %d\n", nrow(comp)))
    cat(sprintf("Skill vs Raw seasonal xRAPM: r = %.3f\n",
                cor(comp$skill_xrapm, comp$raw_xrapm)))

    # Per-season correlations
    for (s in sort(unique(comp$season_end_year))) {
      sc <- comp[comp$season_end_year == s, ]
      if (nrow(sc) > 10) {
        cat(sprintf("  Season %d: r = %.3f (n=%d)\n", s, cor(sc$skill_xrapm, sc$raw_xrapm), nrow(sc)))
      }
    }
  } else {
    cat("  WARNING: 0 player-seasons matched between skill and raw seasonal xRAPM\n")
  }
}

# 9. Player Consistency ----

cat("\n=== Player Consistency Across Seasons (Skill xRAPM) ===\n")

player_season_counts <- seasonal_xrapm %>%
  group_by(player_name) %>%
  summarise(
    n_seasons = n(),
    seasons = paste(season_end_year, collapse = ", "),
    mean_xrapm = mean(xrapm),
    sd_xrapm = sd(xrapm),
    total_minutes = sum(total_minutes),
    .groups = "drop"
  ) %>%
  arrange(desc(n_seasons), desc(mean_xrapm))

cat("\nPlayers with most seasons:\n")
print(head(player_season_counts, 20))

# 10. Save Results ----

cat("\n=== Saving Results ===\n")

seasonal_results <- list(
  seasonal_spm = seasonal_spm,
  seasonal_rapm = seasonal_rapm,
  seasonal_xrapm = seasonal_xrapm,
  seasonal_psr = seasonal_psr,
  player_season_counts = player_season_counts,
  seasons = seasons,
  metadata = list(
    source = "skill_based",
    min_minutes_spm = 200,
    min_minutes_rapm = 200,
    lambda = seasonal_lambda,
    n_seasons = length(seasonal_ratings_list),
    spm_player_seasons = nrow(seasonal_spm),
    rapm_player_seasons = nrow(seasonal_rapm),
    xrapm_player_seasons = nrow(seasonal_xrapm),
    # See the matching note in player-ratings-opta/07_seasonal_ratings.R --
    # step 03's skill-SPM (used here) is likewise fit through the present.
    # career_panna_asof.parquet (09b) is the point-in-time equivalent.
    weights_vintage = "retrospective",
    created = Sys.time()
  )
)

save_cache_with_meta(seasonal_results, file.path(cache_dir, "06_seasonal_ratings.rds"),
                     pipeline = "skills")
cat("Saved to cache-skills/06_seasonal_ratings.rds\n")

# Export CSVs
write.csv(
  seasonal_xrapm %>%
    select(season_end_year, player_name, xrapm, offense, defense,
           any_of(c("off_deviation", "def_deviation")), total_minutes) %>%
    mutate(across(where(is.numeric) & !matches("season|minutes"), ~round(.x, 4))) %>%
    arrange(season_end_year, desc(xrapm)),
  file.path(cache_dir, "seasonal_skill_xrapm.csv"),
  row.names = FALSE
)

cat("\n=== COMPLETE ===\n")
