# RAPM matrix construction functions for panna package
#
# Builds the design matrix for Regularized Adjusted Plus-Minus models.
# Each row represents one team's perspective during a splint.


# ============================================================================
# Value metric helpers
# ============================================================================

#' Add value metric columns to splints
#'
#' Joins per-game player value metrics (EPV, WPA, PSV) to splint data,
#' aggregating to team-level totals within each splint. Values are prorated
#' by splint duration relative to total match minutes.
#'
#' This allows RAPM to be trained on EPV, WPA, or PSV as response variables
#' alongside the default xG target.
#'
#' @param splint_data List with \code{splints} and \code{players} data.frames
#'   (from \code{create_all_splints()}).
#' @param player_game_epv Per-game EPV from \code{aggregate_player_game_epv()}.
#'   If NULL, EPV columns are not added.
#' @param player_game_wpa Per-game WPA from \code{aggregate_player_game_wpa()}.
#'   If NULL, WPA columns are not added.
#' @param player_game_psv Per-game PSV from \code{calculate_psv()}.
#'   If NULL, PSV columns are not added.
#'
#' @return The \code{splint_data} list with additional columns on the
#'   \code{splints} data.frame: \code{epv_home/epv_away},
#'   \code{wpa_home/wpa_away}, \code{psv_home/psv_away}.
#'
#' @export
add_value_metrics_to_splints <- function(splint_data, player_game_epv = NULL,
                                          player_game_wpa = NULL,
                                          player_game_psv = NULL) {
  splints <- data.table::as.data.table(splint_data$splints)
  players <- data.table::as.data.table(splint_data$players)

  # Total match duration per match (for prorating)
  match_dur <- splints[, .(match_duration = sum(duration, na.rm = TRUE)),
                        by = match_id]
  splints[match_dur, match_duration := i.match_duration, on = "match_id"]
  splints[, prorate := data.table::fifelse(
    match_duration > 0, duration / match_duration, 0)]

  # Helper: join player values to splint players, sum per team, prorate
  .add_metric <- function(splints, players, pgd, value_col, suffix) {
    if (is.null(pgd)) return(splints)
    pgd <- data.table::as.data.table(pgd)

    if (!value_col %in% names(pgd)) {
      cli::cli_warn("Column {.val {value_col}} not found in player game data")
      return(splints)
    }

    # Join player game values to splint players
    # Splint players may use "team" or "team_id" for team column
    team_col <- if ("team_id" %in% names(players)) "team_id" else "team"
    keep_cols <- intersect(c("splint_id", "match_id", "player_id", team_col, "is_home"),
                            names(players))
    player_vals <- merge(
      players[, ..keep_cols],
      pgd[, c("player_id", "match_id", value_col), with = FALSE],
      by = c("player_id", "match_id"),
      all.x = TRUE
    )
    player_vals[is.na(get(value_col)), (value_col) := 0]

    # Sum per splint per team (is_home determines home/away)
    if ("is_home" %in% names(player_vals)) {
      team_totals <- player_vals[, .(
        val_home = sum(get(value_col)[is_home == 1L], na.rm = TRUE),
        val_away = sum(get(value_col)[is_home == 0L], na.rm = TRUE)
      ), by = splint_id]
    } else {
      # Fallback: use home_team_id from splints
      splint_home <- splints[, .(splint_id, home_team_id)]
      player_vals[splint_home, home_team_id := i.home_team_id, on = "splint_id"]
      team_totals <- player_vals[, .(
        val_home = sum(get(value_col)[team_id == home_team_id], na.rm = TRUE),
        val_away = sum(get(value_col)[team_id != home_team_id], na.rm = TRUE)
      ), by = splint_id]
    }

    home_col <- paste0(suffix, "_home")
    away_col <- paste0(suffix, "_away")
    data.table::setnames(team_totals, c("val_home", "val_away"),
                          c(home_col, away_col))

    # Prorate by splint duration fraction
    splints[team_totals, (home_col) := get(paste0("i.", home_col)) * prorate,
            on = "splint_id"]
    splints[team_totals, (away_col) := get(paste0("i.", away_col)) * prorate,
            on = "splint_id"]
    splints[is.na(get(home_col)), (home_col) := 0]
    splints[is.na(get(away_col)), (away_col) := 0]

    splints
  }

  splints <- .add_metric(splints, players, player_game_epv, "epv_total", "epv")
  # Position-adjusted EPV (if available from adjust_epv_for_position)
  if (!is.null(player_game_epv) && "epv_total_adj" %in% names(player_game_epv)) {
    splints <- .add_metric(splints, players, player_game_epv, "epv_total_adj", "epv_adj")
  }
  splints <- .add_metric(splints, players, player_game_wpa, "wpa_total", "wpa")
  splints <- .add_metric(splints, players, player_game_psv, "psv", "psv")

  # Clean up
  splints[, c("match_duration", "prorate") := NULL]

  splint_data$splints <- as.data.frame(splints)
  splint_data
}


# ============================================================================
# Internal helpers for create_rapm_design_matrix
# ============================================================================

#' Aggregate player minutes and split into regular/replacement pools
#'
#' @param players Data frame of player appearances per splint
#' @param splints Data frame of splints with duration
#' @param min_minutes Minimum total minutes for inclusion as regular player
#'
#' @return List with player_minutes, replacement_player_ids, player_ids,
#'   n_players, all_player_minutes
#' @keywords internal
.aggregate_player_minutes <- function(players, splints, min_minutes) {
  players_dt <- data.table::as.data.table(players)
  splints_dt <- data.table::as.data.table(splints[, c("splint_id", "duration")])

  data.table::setkey(players_dt, splint_id)
  data.table::setkey(splints_dt, splint_id)

  players_with_duration <- splints_dt[players_dt, on = "splint_id"]

  all_player_minutes <- players_with_duration[, .(
    total_minutes = sum(duration, na.rm = TRUE),
    player_name = {
      tbl <- table(player_name)
      stringi::stri_trans_totitle(tolower(names(tbl)[which.max(tbl)]))
    }
  ), by = player_id]

  all_player_minutes <- as.data.frame(all_player_minutes)

  player_minutes <- all_player_minutes[all_player_minutes$total_minutes >= min_minutes, ]
  replacement_player_ids <- all_player_minutes$player_id[all_player_minutes$total_minutes < min_minutes]

  player_ids <- player_minutes$player_id
  n_players <- length(player_ids)

  if (n_players == 0) {
    cli::cli_abort(c(
      "No players meet minimum minutes requirement.",
      "i" = "Current threshold: {min_minutes} minutes",
      "i" = "Try lowering {.arg min_minutes} or adding more match data."
    ))
  }

  progress_msg(sprintf("Including %d players (>= %d minutes)", n_players, min_minutes))
  progress_msg(sprintf("Replacement pool: %d players (< %d minutes)",
                       length(replacement_player_ids), min_minutes))

  list(
    player_minutes = player_minutes,
    replacement_player_ids = replacement_player_ids,
    player_ids = player_ids,
    n_players = n_players,
    all_player_minutes = all_player_minutes
  )
}


#' Build RAPM row data from valid splints
#'
#' Creates 2 rows per splint (home attacking, away attacking) with game state
#' covariates and target variable.
#'
#' @param valid_splints Data frame of splints with duration > 0
#' @param target_type One of "xg", "goals", "epv", "wpa", "psv", or "custom"
#'
#' @return List with row_data data.frame and target_per90_name string
#' @keywords internal
.build_rapm_row_data <- function(valid_splints, target_type) {
  n_splints <- nrow(valid_splints)

  # Pre-compute game state columns with defaults
  gf_home <- if ("gf_home" %in% names(valid_splints)) valid_splints$gf_home else rep(0, n_splints)
  ga_home <- if ("ga_home" %in% names(valid_splints)) valid_splints$ga_home else rep(0, n_splints)
  avg_min_val <- if ("avg_min" %in% names(valid_splints)) {
    valid_splints$avg_min
  } else {
    (valid_splints$start_minute + valid_splints$end_minute) / 2
  }

  n_players_home <- if ("n_players_home" %in% names(valid_splints)) {
    valid_splints$n_players_home
  } else {
    rep(11, n_splints)
  }
  n_players_away <- if ("n_players_away" %in% names(valid_splints)) {
    valid_splints$n_players_away
  } else {
    rep(11, n_splints)
  }

  duration <- valid_splints$duration

  target_map <- list(
    xg   = list(home = "npxg_home",   away = "npxg_away",   name = "xgf90"),
    goals = list(home = "goals_home", away = "goals_away",  name = "gf90"),
    epv  = list(home = "epv_home",    away = "epv_away",    name = "epvf90"),
    wpa  = list(home = "wpa_home",    away = "wpa_away",    name = "wpaf90"),
    psv  = list(home = "psv_home",    away = "psv_away",    name = "psvf90")
  )

  if (target_type %in% names(target_map)) {
    tm <- target_map[[target_type]]
    home_col <- tm$home
    away_col <- tm$away
    target_per90_name <- tm$name

    if (home_col %in% names(valid_splints)) {
      target_home <- ifelse(is.na(valid_splints[[home_col]]), 0, valid_splints[[home_col]])
    } else {
      cli::cli_abort("Splints missing column {.val {home_col}} for target_type={.val {target_type}}")
    }
    if (away_col %in% names(valid_splints)) {
      target_away <- ifelse(is.na(valid_splints[[away_col]]), 0, valid_splints[[away_col]])
    } else {
      cli::cli_abort("Splints missing column {.val {away_col}} for target_type={.val {target_type}}")
    }
  } else {
    cli::cli_abort("Unknown target_type: {.val {target_type}}")
  }

  n_rows <- n_splints * 2

  row_data <- data.frame(
    row_id = seq_len(n_rows),
    splint_id = rep(valid_splints$splint_id, each = 2),
    match_id = rep(valid_splints$match_id, each = 2),
    target = as.vector(rbind(target_home, target_away)),
    minutes = rep(duration, each = 2),
    target_per_90 = as.vector(rbind(
      ifelse(duration > 0, target_home * 90 / duration, 0),
      ifelse(duration > 0, target_away * 90 / duration, 0)
    )),
    gd = as.vector(rbind(gf_home - ga_home, ga_home - gf_home)),
    gf = as.vector(rbind(gf_home, ga_home)),
    ga = as.vector(rbind(ga_home, gf_home)),
    avg_min = rep(avg_min_val, each = 2),
    home_away = rep(c("home", "away"), n_splints),
    n_offense = as.vector(rbind(n_players_home, n_players_away)),
    n_defense = as.vector(rbind(n_players_away, n_players_home))
  )

  row_data$net_players <- row_data$n_offense - row_data$n_defense

  list(row_data = row_data, target_per90_name = target_per90_name)
}


#' Build sparse player matrix from triplets
#'
#' Constructs the sparse matrix encoding which players are on offense/defense
#' in each row, including replacement-level columns.
#'
#' @param players Data frame of player appearances
#' @param valid_splints Data frame of valid splints
#' @param player_ids Character vector of regular player IDs
#' @param replacement_player_ids Character vector of replacement player IDs
#' @param n_rows Total rows in design matrix
#'
#' @return List with X_players (sparse matrix), col_names, n_player_cols,
#'   replacement_off_appearances, replacement_def_appearances
#' @keywords internal
.build_rapm_sparse_matrix <- function(players, valid_splints, player_ids,
                                       replacement_player_ids, n_rows) {
  n_players <- length(player_ids)
  player_idx <- stats::setNames(seq_along(player_ids), player_ids)

  replacement_off_col <- n_players + 1
  n_player_cols <- (n_players + 1) * 2
  replacement_def_col <- n_player_cols

  col_names <- c(paste0(player_ids, "_off"), "replacement_off",
                 paste0(player_ids, "_def"), "replacement_def")

  valid_splint_ids <- valid_splints$splint_id
  splint_to_idx <- stats::setNames(seq_along(valid_splint_ids), valid_splint_ids)

  # panna#87 (attempt-4 flight recorder): the old implementation subset the
  # FULL ~17M-row appearances table into four data.frame copies — dragging an
  # unused 17M-element player_name character column through each — plus
  # stats::aggregate on the replacement rows and a do.call(rbind) over eight
  # triplet data.frames; step 4 sat at 12.3GB before any fit. Same triplets,
  # same matrix, built from narrow mostly-integer vectors instead.
  splint_idx <- unname(splint_to_idx[players$splint_id])
  keep <- !is.na(splint_idx)
  splint_idx <- as.integer(splint_idx[keep])
  pid <- players$player_id[keep]
  is_home <- players$is_home[keep]
  has_share <- "share" %in% names(players)
  share <- if (has_share) as.numeric(players$share[keep]) else rep(1, length(pid))

  player_col <- unname(player_idx[pid])
  is_regular_name <- pid %in% player_ids
  # Validate: regular players must have valid column indices (legacy guard)
  regular_with_na <- is_regular_name & is.na(player_col)
  if (any(regular_with_na)) {
    bad_ids <- unique(pid[regular_with_na])
    cli::cli_warn("Found regular players with NA column indices: {paste(head(bad_ids, 5), collapse = ', ')}")
  }
  is_regular <- is_regular_name & !is.na(player_col)
  is_replacement <- pid %in% replacement_player_ids
  rm(pid, is_regular_name, regular_with_na)

  # -- Regular-player triplets: all four old blocks (home/away x off/def)
  #    from the same vectors. Offense row = 2s-1 for home (home attacking),
  #    2s for away; defense row is the opposite row of the pair. --
  r <- which(is_regular)
  rs <- splint_idx[r]
  rc <- as.integer(player_col[r])
  rx <- share[r]
  rh <- is_home[r]
  i_off <- 2L * rs - ifelse(rh, 1L, 0L)
  i_def <- 2L * rs - ifelse(rh, 0L, 1L)
  trip_i <- c(i_off, i_def)
  trip_j <- c(rc, rc + n_players + 1L)
  trip_x <- c(rx, rx)
  rm(r, rs, rc, rx, rh, i_off, i_def)

  # -- Replacement triplets: sum of replacement shares per (splint, side).
  #    share-less legacy data keeps the old binary fallback (1 per splint,
  #    NOT the count). rowsum() replaces stats::aggregate (same sums, a
  #    fraction of the memory). --
  repl_block <- function(side_mask) {
    m <- is_replacement & side_mask
    if (!any(m)) return(NULL)
    if (has_share) {
      s <- rowsum(share[m], group = splint_idx[m])
      list(sidx = as.integer(rownames(s)), x = as.numeric(s))
    } else {
      u <- sort(unique(splint_idx[m]))
      list(sidx = u, x = rep(1, length(u)))
    }
  }
  home_repl <- repl_block(is_home)
  away_repl <- repl_block(!is_home)
  if (!is.null(home_repl)) {
    # offense while home attacking (2s-1); defense on the away-attacking row (2s)
    trip_i <- c(trip_i, 2L * home_repl$sidx - 1L, 2L * home_repl$sidx)
    trip_j <- c(trip_j, rep(replacement_off_col, length(home_repl$sidx)),
                rep(replacement_def_col, length(home_repl$sidx)))
    trip_x <- c(trip_x, home_repl$x, home_repl$x)
  }
  if (!is.null(away_repl)) {
    # offense while away attacking (2s); defense on the home-attacking row (2s-1)
    trip_i <- c(trip_i, 2L * away_repl$sidx, 2L * away_repl$sidx - 1L)
    trip_j <- c(trip_j, rep(replacement_off_col, length(away_repl$sidx)),
                rep(replacement_def_col, length(away_repl$sidx)))
    trip_x <- c(trip_x, away_repl$x, away_repl$x)
  }

  replacement_off_appearances <- sum(
    if (!is.null(home_repl)) length(home_repl$sidx) else 0,
    if (!is.null(away_repl)) length(away_repl$sidx) else 0
  )
  replacement_def_appearances <- replacement_off_appearances

  progress_msg(sprintf("Replacement appearances: %d offense, %d defense",
                       replacement_off_appearances, replacement_def_appearances))

  X_players <- Matrix::sparseMatrix(
    i = trip_i,
    j = trip_j,
    x = trip_x,
    dims = c(n_rows, n_player_cols),
    dimnames = list(NULL, col_names)
  )
  rm(trip_i, trip_j, trip_x, splint_idx, share, is_home, player_col,
     is_regular, is_replacement)

  list(
    X_players = X_players,
    col_names = col_names,
    n_player_cols = n_player_cols,
    replacement_off_appearances = replacement_off_appearances,
    replacement_def_appearances = replacement_def_appearances
  )
}


# ============================================================================
# Main RAPM design matrix function
# ============================================================================

#' Create RAPM design matrix (new structure)
#'
#' Creates the design matrix with 2 rows per splint (one per team perspective):
#' - Target: xgf90 or gf90 (xG or goals FOR per 90 from each team's perspective)
#' - Covariates: gd, gf, ga, avg_min, home_away
#' - Player columns: playerX_off (attacking), playerX_def (defending)
#' - Replacement columns: replacement_off, replacement_def for low-minute players
#'
#' @param splint_data Combined splint data from create_all_splints
#' @param min_minutes Minimum total minutes for player inclusion
#' @param target_type Type of target variable: \code{"xg"} for non-penalty xG
#'   (default), \code{"goals"} for actual goals, \code{"epv"} for Expected
#'   Possession Value, \code{"wpa"} for Win Probability Added, \code{"psv"}
#'   for Player Stat Value. Requires corresponding home/away columns on
#'   splints (e.g., \code{epv_home}, \code{epv_away}).
#' @param min_duration Minimum splint duration in minutes (default 1.0).
#'   Splints shorter than this are dropped to avoid per-90 inflation
#'   artefacts on stoppage-time fragments. Set to 0 to keep all splints.
#'   Note: with chain-derived splint creation
#'   (\code{create_splint_boundaries_fast}, default \code{min_splint_duration = 5}),
#'   the upstream pipeline already enforces a 5-min minimum so this
#'   secondary filter rarely fires.
#'
#' @return List with design matrix components
#' @export
create_rapm_design_matrix <- function(splint_data, min_minutes = 90,
                                       target_type = c("xg", "goals", "epv",
                                                        "wpa", "psv"),
                                       min_duration = 1.0) {
  target_type <- match.arg(target_type)

  # Validate splint_data structure
  if (!is.list(splint_data)) {
    cli::cli_abort(c(
      "{.arg splint_data} must be a list.",
      "x" = "Got {.cls {class(splint_data)}} instead."
    ))
  }

  required_elements <- c("splints", "players")
  missing_elements <- setdiff(required_elements, names(splint_data))
  if (length(missing_elements) > 0) {
    cli::cli_abort(c(
      "{.arg splint_data} is missing required element{?s}.",
      "x" = "Missing: {.field {missing_elements}}",
      "i" = "Use {.fn create_all_splints} to generate valid splint data."
    ))
  }

  splints <- splint_data$splints
  players <- splint_data$players

  validate_dataframe(splints, required_cols = c("splint_id", "duration"), arg_name = "splint_data$splints")
  validate_dataframe(players, required_cols = c("splint_id", "player_id", "player_name"), arg_name = "splint_data$players")

  # Filter to valid splints. Drop splints under min_duration (default 1 min):
  # ultra-short fragments come almost entirely from stoppage time and produce
  # extreme per-90 targets (e.g. 0.5-min splint with one shot -> 18+ xG per 90).
  # See debug/measure_short_splints.R for the impact analysis.
  n_before <- sum(splints$duration > 0)
  valid_splints <- splints[splints$duration >= min_duration, ]
  n_splints <- nrow(valid_splints)
  n_dropped <- n_before - n_splints
  if (n_dropped > 0) {
    progress_msg(sprintf("Dropping %d splints with duration < %.2f min (%.2f%% of valid splints)",
                         n_dropped, min_duration, 100 * n_dropped / n_before))
  }
  progress_msg(sprintf("Processing %d splints...", n_splints))

  # Step 1: Aggregate player minutes and split into regular/replacement
  pm <- .aggregate_player_minutes(players, splints, min_minutes)

  # Step 2: Build row data (2 rows per splint with game state)
  progress_msg("Building row data (vectorized)...")
  rd <- .build_rapm_row_data(valid_splints, target_type)
  row_data <- rd$row_data
  n_rows <- nrow(row_data)

  # Step 3: Build sparse player matrix
  progress_msg("Building sparse matrix (vectorized)...")
  sm <- .build_rapm_sparse_matrix(
    players, valid_splints, pm$player_ids, pm$replacement_player_ids, n_rows
  )

  # Weights based on duration. The historical 0.01 floor (pmax(.../90, 0.01))
  # was a defensive guard against zero-weight rows when ultra-short stoppage
  # splints existed. Now that splint creation enforces min_splint_duration
  # (default 5 min, see create_splint_boundaries_fast) and per-90 inflation
  # on tiny splints is structurally prevented, the floor never activates
  # (5/90 ~= 0.056). Dropping it for clarity.
  weights <- row_data$minutes / 90

  progress_msg(sprintf("Design matrix: %d rows, %d player columns (+2 replacement), %d covariates",
                       n_rows, pm$n_players * 2, 5))

  # Build player mapping with replacement row
  replacement_minutes <- sum(pm$all_player_minutes$total_minutes[
    pm$all_player_minutes$player_id %in% pm$replacement_player_ids
  ])

  player_mapping <- rbind(
    pm$player_minutes,
    data.frame(
      player_id = "replacement",
      player_name = "Replacement Level",
      total_minutes = replacement_minutes
    )
  )

  list(
    X_players = sm$X_players,
    row_data = row_data,
    y = row_data$target_per_90,
    weights = weights,
    player_mapping = player_mapping,
    player_ids = c(pm$player_ids, "replacement"),
    n_players = pm$n_players,
    n_players_total = pm$n_players + 1,
    n_rows = n_rows,
    target_type = target_type,
    target_name = rd$target_per90_name,
    replacement_player_ids = pm$replacement_player_ids,
    replacement_stats = list(
      n_players = length(pm$replacement_player_ids),
      total_minutes = replacement_minutes,
      off_appearances = sm$replacement_off_appearances,
      def_appearances = sm$replacement_def_appearances
    )
  )
}


#' Prepare RAPM data for model fitting
#'
#' Creates design matrix with covariates for ridge regression.
#' This is the primary RAPM data preparation function.
#'
#' @param splint_data Combined splint data from create_all_splints
#' @param min_minutes Minimum minutes for player inclusion
#' @param target_type Type of target variable: "xg" for non-penalty xG (default),
#'   "goals" for actual goals scored, "epv" for EPV, "wpa" for WPA, "psv" for
#'   PSV. Use "goals" when shots data unavailable.
#' @param include_covariates Whether to include game state covariates
#' @param include_league Whether to include league dummies (for multi-league)
#' @param include_season Whether to include season dummies
#'
#' @return List with all model inputs
#' @keywords internal
prepare_rapm_data <- function(splint_data, min_minutes = 90,
                               target_type = c("xg", "goals", "epv",
                                                "wpa", "psv"),
                               include_covariates = TRUE,
                               include_league = NULL,
                               include_season = NULL) {
  target_type <- match.arg(target_type)

  # Validate required columns exist for target type
  if (target_type == "goals") {
    splint_cols <- names(splint_data$splints)
    if (!all(c("goals_home", "goals_away") %in% splint_cols)) {
      cli::cli_warn(c(
        "target_type='goals' requires 'goals_home' and 'goals_away' columns in splints.",
        "i" = "Falling back to xG-based target. Splints may need to be regenerated with {.fn create_all_splints}."
      ))
      target_type <- "xg"
    }
  }

  # Create base design matrix
  rapm_data <- create_rapm_design_matrix(splint_data, min_minutes, target_type)

  covariate_list <- list()

  if (include_covariates) {
    covariate_list$gd <- rapm_data$row_data$gd
    covariate_list$abs_goals <- rapm_data$row_data$gf + rapm_data$row_data$ga
    covariate_list$avg_min <- rapm_data$row_data$avg_min
    covariate_list$is_home <- as.numeric(rapm_data$row_data$home_away == "home")

    if ("n_offense" %in% names(rapm_data$row_data) &&
        "n_defense" %in% names(rapm_data$row_data)) {
      covariate_list$net_players <- rapm_data$row_data$n_offense -
                                    rapm_data$row_data$n_defense
      covariate_list$abs_reds <- 22 - rapm_data$row_data$n_offense -
                                 rapm_data$row_data$n_defense
    }
  }

  # Auto-detect league and season availability

  has_league <- "league" %in% names(splint_data$splints)
  if (is.null(include_league)) {
    include_league <- has_league
  }

  has_season <- "season_end_year" %in% names(splint_data$splints)
  if (is.null(include_season)) {
    include_season <- has_season
  }

  # Determine if we should use cell means (both league and season available)
  use_cell_means <- include_league && has_league && include_season && has_season

  # Only create league-only dummies when season is not available
  if (include_league && has_league && !use_cell_means) {
    splint_leagues <- splint_data$splints$league[
      match(rapm_data$row_data$splint_id, splint_data$splints$splint_id)
    ]

    unique_leagues <- sort(unique(splint_leagues[!is.na(splint_leagues)]))
    if (length(unique_leagues) > 1) {
      progress_msg(sprintf("Adding %d league dummies (ref: %s)",
                           length(unique_leagues) - 1, unique_leagues[1]))
      for (lg in unique_leagues[-1]) {
        col_name <- paste0("league_", gsub(" ", "_", lg))
        covariate_list[[col_name]] <- as.numeric(splint_leagues == lg)
      }
      rapm_data$leagues <- unique_leagues
    }
  }

  # Only create season-only dummies when league is not available
  if (include_season && has_season && !use_cell_means) {
    splint_seasons <- splint_data$splints$season_end_year[
      match(rapm_data$row_data$splint_id, splint_data$splints$splint_id)
    ]

    unique_seasons <- sort(unique(splint_seasons[!is.na(splint_seasons)]))
    if (length(unique_seasons) > 1) {
      progress_msg(sprintf("Adding %d season dummies (ref: %s)",
                           length(unique_seasons) - 1, unique_seasons[1]))
      for (sn in unique_seasons[-1]) {
        col_name <- paste0("season_", sn)
        covariate_list[[col_name]] <- as.numeric(splint_seasons == sn)
      }
      rapm_data$seasons <- unique_seasons
    }
  }

  # League-season cell means (when both available)
  ls_sparse <- NULL
  if (use_cell_means) {
    splint_leagues <- splint_data$splints$league[
      match(rapm_data$row_data$splint_id, splint_data$splints$splint_id)
    ]
    splint_seasons <- splint_data$splints$season_end_year[
      match(rapm_data$row_data$splint_id, splint_data$splints$splint_id)
    ]

    league_season <- paste0(splint_leagues, "_", splint_seasons)
    unique_ls <- sort(unique(league_season[!is.na(league_season)]))

    unique_leagues <- sort(unique(splint_leagues[!is.na(splint_leagues)]))
    unique_seasons <- sort(unique(splint_seasons[!is.na(splint_seasons)]))
    rapm_data$leagues <- unique_leagues
    rapm_data$seasons <- unique_seasons

    if (length(unique_ls) > 1) {
      progress_msg(sprintf("Adding %d league-season dummies (ref: %s)",
                           length(unique_ls) - 1, unique_ls[1]))
      # panna#87 (attempt-3 flight recorder): each dummy built as a dense
      # 1.23M-element double (~10MB) put ~3GB in covariate_list, and the dense
      # do.call(cbind) below doubled it (~6GB transient) — for columns that
      # are >99.5% zeros. Build ONE sparse matrix directly instead (~50MB):
      # identical columns, names, and order to the old per-level loop.
      keep <- !is.na(league_season) & league_season != unique_ls[1]
      ls_sparse <- Matrix::sparseMatrix(
        i = which(keep),
        j = match(league_season[keep], unique_ls[-1]),
        x = 1,
        dims = c(length(league_season), length(unique_ls) - 1L),
        dimnames = list(NULL, paste0("ls_", gsub(" ", "_", unique_ls[-1])))
      )
    }
  }

  # Combine into covariate matrix. Dense base covariates (a handful of
  # columns) are converted to sparse at cbind time; the league-season block
  # is already sparse. X_full stays a sparse Matrix exactly as before — only
  # the dense intermediates are gone.
  if (length(covariate_list) > 0 || !is.null(ls_sparse)) {
    X_dense <- if (length(covariate_list) > 0) {
      m <- do.call(cbind, covariate_list)
      colnames(m) <- names(covariate_list)
      m
    } else NULL

    parts <- list(rapm_data$X_players)
    if (!is.null(X_dense)) parts <- c(parts, list(Matrix::Matrix(X_dense, sparse = TRUE)))
    if (!is.null(ls_sparse)) parts <- c(parts, list(ls_sparse))
    rapm_data$X_full <- do.call(cbind, parts)
    rapm_data$covariate_names <- c(colnames(X_dense), colnames(ls_sparse))
    rm(parts, X_dense)
  } else {
    rapm_data$X_full <- rapm_data$X_players
    rapm_data$covariate_names <- character(0)
  }

  # Summary stats
  rapm_data$summary <- list(
    n_rows = rapm_data$n_rows,
    n_players = rapm_data$n_players,
    n_player_cols = rapm_data$n_players * 2,
    n_covariates = length(rapm_data$covariate_names),
    total_matrix_cols = ncol(rapm_data$X_full),
    target_type = rapm_data$target_type,
    response_range = range(rapm_data$y, na.rm = TRUE)
  )

  target_desc <- if (rapm_data$target_type == "xg") "xG-based" else "Goals-based"
  progress_msg(sprintf("RAPM data ready (%s): %d observations, %d players (%d columns), %d covariates",
                       target_desc, rapm_data$n_rows, rapm_data$n_players,
                       rapm_data$n_players * 2, length(rapm_data$covariate_names)))

  rapm_data
}
