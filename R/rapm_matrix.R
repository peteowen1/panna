# RAPM matrix construction functions for panna package
#
# Builds the design matrix for Regularized Adjusted Plus-Minus models.
# Each row represents one team's perspective during a splint.


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
      tools::toTitleCase(tolower(names(tbl)[which.max(tbl)]))
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
#' @param target_type "xg" or "goals"
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

  if (target_type == "xg") {
    target_home <- ifelse(is.na(valid_splints$npxg_home), 0, valid_splints$npxg_home)
    target_away <- ifelse(is.na(valid_splints$npxg_away), 0, valid_splints$npxg_away)
    target_per90_name <- "xgf90"
  } else {
    target_home <- ifelse(is.na(valid_splints$goals_home), 0, valid_splints$goals_home)
    target_away <- ifelse(is.na(valid_splints$goals_away), 0, valid_splints$goals_away)
    target_per90_name <- "gf90"
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

  players_valid <- players[players$splint_id %in% valid_splint_ids, ]
  players_valid$splint_idx <- splint_to_idx[players_valid$splint_id]

  players_valid$is_regular <- players_valid$player_id %in% player_ids
  players_valid$is_replacement <- players_valid$player_id %in% replacement_player_ids
  players_valid$player_col <- player_idx[players_valid$player_id]

  # Validate: regular players must have valid column indices
  regular_with_na <- players_valid$is_regular & is.na(players_valid$player_col)
  if (any(regular_with_na)) {
    bad_ids <- unique(players_valid$player_id[regular_with_na])
    cli::cli_warn("Found regular players with NA column indices: {paste(head(bad_ids, 5), collapse = ', ')}")
    players_valid$is_regular[regular_with_na] <- FALSE
  }

  home_regular <- players_valid[players_valid$is_home & players_valid$is_regular, ]
  away_regular <- players_valid[!players_valid$is_home & players_valid$is_regular, ]
  home_replacement <- players_valid[players_valid$is_home & players_valid$is_replacement, ]
  away_replacement <- players_valid[!players_valid$is_home & players_valid$is_replacement, ]

  triplets <- list()

  # Home players on offense (home attacking)
  if (nrow(home_regular) > 0) {
    triplets$home_off <- data.frame(
      i = 2 * home_regular$splint_idx - 1,
      j = home_regular$player_col,
      x = 1
    )
  }

  # Home players on defense (away attacking)
  if (nrow(home_regular) > 0) {
    triplets$home_def <- data.frame(
      i = 2 * home_regular$splint_idx,
      j = home_regular$player_col + n_players + 1,
      x = 1
    )
  }

  # Away players on offense (away attacking)
  if (nrow(away_regular) > 0) {
    triplets$away_off <- data.frame(
      i = 2 * away_regular$splint_idx,
      j = away_regular$player_col,
      x = 1
    )
  }

  # Away players on defense (home attacking)
  if (nrow(away_regular) > 0) {
    triplets$away_def <- data.frame(
      i = 2 * away_regular$splint_idx - 1,
      j = away_regular$player_col + n_players + 1,
      x = 1
    )
  }

  # Replacement on offense (home attacking with home replacement)
  if (nrow(home_replacement) > 0) {
    home_repl_splints <- unique(home_replacement$splint_idx)
    triplets$home_repl_off <- data.frame(
      i = 2 * home_repl_splints - 1, j = replacement_off_col, x = 1
    )
  }

  # Replacement on offense (away attacking with away replacement)
  if (nrow(away_replacement) > 0) {
    away_repl_splints <- unique(away_replacement$splint_idx)
    triplets$away_repl_off <- data.frame(
      i = 2 * away_repl_splints, j = replacement_off_col, x = 1
    )
  }

  # Replacement on defense (home attacking with away replacement)
  if (nrow(away_replacement) > 0) {
    away_repl_splints <- unique(away_replacement$splint_idx)
    triplets$away_repl_def <- data.frame(
      i = 2 * away_repl_splints - 1, j = replacement_def_col, x = 1
    )
  }

  # Replacement on defense (away attacking with home replacement)
  if (nrow(home_replacement) > 0) {
    home_repl_splints <- unique(home_replacement$splint_idx)
    triplets$home_repl_def <- data.frame(
      i = 2 * home_repl_splints, j = replacement_def_col, x = 1
    )
  }

  all_triplets <- do.call(rbind, triplets)

  replacement_off_appearances <- sum(
    if (nrow(home_replacement) > 0) length(unique(home_replacement$splint_idx)) else 0,
    if (nrow(away_replacement) > 0) length(unique(away_replacement$splint_idx)) else 0
  )
  replacement_def_appearances <- replacement_off_appearances

  progress_msg(sprintf("Replacement appearances: %d offense, %d defense",
                       replacement_off_appearances, replacement_def_appearances))

  X_players <- Matrix::sparseMatrix(
    i = all_triplets$i,
    j = all_triplets$j,
    x = all_triplets$x,
    dims = c(n_rows, n_player_cols),
    dimnames = list(NULL, col_names)
  )

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
#' @param target_type Type of target variable: "xg" for non-penalty xG (default),
#'   "goals" for actual goals scored
#'
#' @return List with design matrix components
#' @export
create_rapm_design_matrix <- function(splint_data, min_minutes = 90,
                                       target_type = c("xg", "goals")) {
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

  # Filter to valid splints
  valid_splints <- splints[splints$duration > 0, ]
  n_splints <- nrow(valid_splints)
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

  # Weights based on duration
  weights <- pmax(row_data$minutes / 90, 0.01)

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
#'   "goals" for actual goals scored. Use "goals" when shots data unavailable.
#' @param include_covariates Whether to include game state covariates
#' @param include_league Whether to include league dummies (for multi-league)
#' @param include_season Whether to include season dummies
#'
#' @return List with all model inputs
#' @keywords internal
prepare_rapm_data <- function(splint_data, min_minutes = 90,
                               target_type = c("xg", "goals"),
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
      for (ls in unique_ls[-1]) {
        col_name <- paste0("ls_", gsub(" ", "_", ls))
        covariate_list[[col_name]] <- as.numeric(league_season == ls)
      }
    }
  }

  # Combine into covariate matrix
  if (length(covariate_list) > 0) {
    X_covariates <- do.call(cbind, covariate_list)
    colnames(X_covariates) <- names(covariate_list)

    rapm_data$X_full <- cbind(rapm_data$X_players, X_covariates)
    rapm_data$covariate_names <- colnames(X_covariates)
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


#' Filter RAPM data to specific seasons
#'
#' Subsets RAPM data to include only splints from specified seasons.
#'
#' @param rapm_data List from prepare_rapm_data
#' @param seasons Character vector of seasons to include
#' @param match_info Match info data frame with season column
#'
#' @return Filtered rapm_data
#' @keywords internal
filter_rapm_by_season <- function(rapm_data, seasons, match_info) {
  # Get match_ids for specified seasons
  valid_matches <- match_info$match_id[match_info$season %in% seasons]

  # Get splint indices
  splint_info <- rapm_data$splint_info
  splint_info$row_num <- seq_len(nrow(splint_info))
  idx <- splint_info$row_num[splint_info$match_id %in% valid_matches]

  # Subset all components
  rapm_data$X <- rapm_data$X[idx, , drop = FALSE]
  rapm_data$y <- rapm_data$y[idx]
  rapm_data$weights <- rapm_data$weights[idx]
  rapm_data$splint_info <- rapm_data$splint_info[idx, ]

  if (!is.null(rapm_data$X_weighted)) {
    rapm_data$X_weighted <- rapm_data$X_weighted[idx, , drop = FALSE]
    rapm_data$y_weighted <- rapm_data$y_weighted[idx]
  }

  if (!is.null(rapm_data$X_od)) {
    rapm_data$X_od <- rapm_data$X_od[idx, , drop = FALSE]
  }

  rapm_data
}
