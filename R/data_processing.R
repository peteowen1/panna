# Data processing and cleaning functions for panna package


#' Detect and filter bad xG data from splints
#'
#' Identifies league-seasons with high rates of missing/zero xG data
#' and filters them out. Reports what was filtered.
#'
#' @param splint_data Splint data list from create_all_splints
#' @param zero_xg_threshold Percentage threshold for flagging bad data (default 20%)
#' @param verbose Whether to print filtering report
#'
#' @return List with filtered splint_data and filtering report
#' @keywords internal
filter_bad_xg_data <- function(splint_data, zero_xg_threshold = 20, verbose = TRUE) {
  splints <- splint_data$splints

  # Check if we have league/season columns
 has_league <- "league" %in% names(splints)
  has_season <- "season_end_year" %in% names(splints)

  if (!has_league && !has_season) {
    if (verbose) progress_msg("No league/season columns - skipping xG quality filter")
    return(list(
      splint_data = splint_data,
      report = NULL,
      filtered_groups = NULL
    ))
  }

  # Calculate zero xG percentage by league-season (or just season if no league)
  group_cols <- c()
  if (has_league) group_cols <- c(group_cols, "league")
  if (has_season) group_cols <- c(group_cols, "season_end_year")

  dt <- data.table::as.data.table(splints)
  xg_quality <- dt[, .(
    n_splints = .N,
    n_matches = data.table::uniqueN(match_id),
    zero_xg_count = sum(npxg_home == 0 & npxg_away == 0, na.rm = TRUE),
    avg_npxg = mean(npxg_home + npxg_away, na.rm = TRUE)
  ), by = group_cols]
  xg_quality[, zero_xg_pct := zero_xg_count / n_splints * 100]
  xg_quality[, is_bad := zero_xg_pct >= zero_xg_threshold]
  xg_quality <- as.data.frame(xg_quality)

  # Identify bad groups
  bad_groups <- xg_quality[xg_quality$is_bad, ]

  good_groups <- xg_quality[!xg_quality$is_bad, ]

  if (verbose) {
    cat("\n=== xG Data Quality Check ===\n")
    cat(sprintf("Threshold: %.0f%% zero xG splints\n\n", zero_xg_threshold))

    if (nrow(bad_groups) > 0) {
      cat("FILTERED OUT (bad data):\n")
      for (i in seq_len(nrow(bad_groups))) {
        row <- bad_groups[i, ]
        label <- if (has_league && has_season) {
          sprintf("  %s %d", row$league, row$season_end_year)
        } else if (has_league) {
          sprintf("  %s", row$league)
        } else {
          sprintf("  Season %d", row$season_end_year)
        }
        cat(sprintf("%s: %.1f%% zero xG (%d splints, %d matches)\n",
                    label, row$zero_xg_pct, row$n_splints, row$n_matches))
      }
      cat("\n")
    }

    cat("KEPT (good data):\n")
    for (i in seq_len(nrow(good_groups))) {
      row <- good_groups[i, ]
      label <- if (has_league && has_season) {
        sprintf("  %s %d", row$league, row$season_end_year)
      } else if (has_league) {
        sprintf("  %s", row$league)
      } else {
        sprintf("  Season %d", row$season_end_year)
      }
      cat(sprintf("%s: %.1f%% zero xG (%d splints, avg xG=%.2f)\n",
                  label, row$zero_xg_pct, row$n_splints, row$avg_npxg))
    }
  }

  # Filter splints
  if (nrow(bad_groups) > 0) {
    # Build filter condition dynamically
    if (has_league && has_season) {
      bad_keys <- paste(bad_groups$league, bad_groups$season_end_year, sep = "_")
      splint_keys <- paste(splints$league, splints$season_end_year, sep = "_")
      keep_mask <- !(splint_keys %in% bad_keys)
    } else if (has_league) {
      keep_mask <- !(splints$league %in% bad_groups$league)
    } else {
      keep_mask <- !(splints$season_end_year %in% bad_groups$season_end_year)
    }

    valid_splints <- splints[keep_mask, ]
    valid_splint_ids <- valid_splints$splint_id

    splint_data$splints <- valid_splints
    splint_data$players <- splint_data$players[splint_data$players$splint_id %in% valid_splint_ids, ]
    splint_data$match_info <- splint_data$match_info[splint_data$match_info$match_id %in% unique(valid_splints$match_id), ]

    if (verbose) {
      cat(sprintf("\nFiltered: %d -> %d splints (removed %d)\n",
                  nrow(splints), nrow(valid_splints),
                  nrow(splints) - nrow(valid_splints)))
    }
  } else {
    if (verbose) cat("\nNo bad data detected - keeping all splints\n")
  }

  list(
    splint_data = splint_data,
    report = xg_quality,
    filtered_groups = bad_groups
  )
}
