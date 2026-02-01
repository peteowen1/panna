# SPADL Conversion Functions for EPV Model
#
# Converts Opta events to a standardized SPADL-like (Soccer Player Action
# Description Language) format for EPV modeling. This normalized format
# enables consistent feature engineering across different data sources.
#
# Reference: https://arxiv.org/abs/1802.07127 (SPADL paper)

#' @importFrom cli cli_alert_info cli_alert_success cli_abort cli_warn
#' @importFrom data.table data.table setDT setorder .SD .N .I := fifelse as.data.table
NULL

# Opta type_id to SPADL action type mapping
OPTA_ACTION_MAP <- list(
  # Passes (type_id = 1)
  pass = 1L,
  # Take-ons/dribbles (type_id = 3)
  take_on = 3L,
  # Fouls (type_id = 4)
  foul = 4L,
  # Tackles (type_id = 7)
  tackle = 7L,
  # Interceptions (type_id = 8)

  interception = 8L,
  # Shots (type_id = 13-16)
  shot_saved = 13L,
  shot_post = 14L,
  shot_miss = 15L,
  goal = 16L,
  # Aerial duels (type_id = 44)
  aerial = 44L,
  # Ball touch (type_id = 61)
  ball_touch = 61L,
  # Clearance (type_id = 12)
  clearance = 12L,
  # Offside pass (type_id = 2)
  offside_pass = 2L,
  # Ball recovery (type_id = 49)
  ball_recovery = 49L,
  # Dispossessed (type_id = 50)
  dispossessed = 50L,
  # Keeper events
  keeper_pick_up = 52L,
  keeper_save = 10L,
  keeper_claim = 53L,
  keeper_punch = 41L
)

# Qualifier IDs for pass subtypes
OPTA_PASS_QUALIFIERS <- list(
  cross = 2L,
  throw_in = 107L,
  freekick = 5L,
  corner = 6L,
  goal_kick = 124L,
  through_ball = 4L,
  long_ball = 1L,
  chipped = 155L
)

# Qualifier IDs for body parts
OPTA_BODYPART_QUALIFIERS <- list(
  head = 15L,
  right_foot = 72L,
  left_foot = 36L
)


#' Convert Opta Match Events to SPADL Format
#'
#' Transforms Opta event data into a standardized SPADL-like format suitable
#' for EPV modeling. Normalizes coordinates so teams always attack left-to-right
#' and standardizes action types across different event categories.
#'
#' @param opta_events Data frame from load_opta_match_events()
#' @param normalize_direction Whether to flip coordinates so attacking team
#'   always goes left-to-right (default TRUE)
#'
#' @return Data frame in SPADL format with columns:
#'   \itemize{
#'     \item match_id: Match identifier
#'     \item action_id: Sequential action number within match
#'     \item period_id: 1=first half, 2=second half
#'     \item time_seconds: Time in seconds from period start
#'     \item team_id: Team performing action
#'     \item player_id: Player performing action
#'     \item player_name: Player name
#'     \item start_x, start_y: Starting coordinates (0-100)
#'     \item end_x, end_y: Ending coordinates (0-100)
#'     \item action_type: Standardized action type
#'     \item action_type_id: Numeric action type code
#'     \item result: "success" or "fail"
#'     \item bodypart: "foot", "head", or "other"
#'     \item receiver_player_id: Player who receives/intercepts (from next action)
#'     \item receiver_player_name: Name of receiver/interceptor
#'     \item receiver_team_id: Team of receiver (for detecting possession change)
#'     \item possession_change: TRUE if next action is by opponent team
#'   }
#'
#' @export
#' @examples
#' \dontrun{
#' events <- load_opta_match_events("ENG", "2024-2025")
#' spadl <- convert_opta_to_spadl(events)
#' }
convert_opta_to_spadl <- function(opta_events, normalize_direction = TRUE) {
  if (is.null(opta_events) || nrow(opta_events) == 0) {
    cli::cli_abort("No events provided for SPADL conversion")
  }

  cli::cli_alert_info("Converting {format(nrow(opta_events), big.mark=',')} Opta events to SPADL format...")

  # Ensure required columns exist
  required_cols <- c("match_id", "type_id", "team_id", "player_id", "player_name",
                     "minute", "second", "x", "y", "outcome", "period_id")
  missing_cols <- setdiff(required_cols, names(opta_events))
  if (length(missing_cols) > 0) {
    cli::cli_abort("Missing required columns: {paste(missing_cols, collapse=', ')}")
  }

  # Parse qualifiers if present
  if ("qualifier_json" %in% names(opta_events)) {
    opta_events <- parse_opta_qualifiers(opta_events)
  }

  # Convert to data.table for efficient processing
  dt <- data.table::as.data.table(opta_events)

  # Filter out non-player events (match start, period start, etc.)
  # These have empty player_name and are administrative events
  n_before <- nrow(dt)
  dt <- dt[!is.na(player_name) & player_name != ""]
  n_filtered <- n_before - nrow(dt)
  if (n_filtered > 0) {
    cli::cli_alert_info("Filtered {n_filtered} non-player events (match/period markers)")
  }

  # Create SPADL columns (vectorized)
  dt[, `:=`(
    original_event_id = if ("event_id" %in% names(dt)) event_id else .I,
    time_seconds = minute * 60 + second,
    start_x = x,
    start_y = y,
    end_x = if ("end_x" %in% names(dt)) end_x else x,
    end_y = if ("end_y" %in% names(dt)) end_y else y,
    opta_type_id = type_id
  )]

  # Map Opta type_id to SPADL action types (vectorized)
  dt[, action_type := map_opta_action_type(
    type_id = opta_type_id,
    qualifiers = if ("qualifiers_parsed" %in% names(dt)) qualifiers_parsed else NULL
  )]

  # Map result (success/fail) - vectorized
  # For shots: success = goal (type_id = 16), fail = saved/missed/post (type_id = 13-15)
  # For other actions: use Opta outcome (1 = success)
  dt[, result := fifelse(
    opta_type_id %in% c(13L, 14L, 15L, 16L),
    fifelse(opta_type_id == 16L, "success", "fail"),
    fifelse(outcome == 1, "success", "fail")
  )]

  # Determine body part (vectorized)
  dt[, bodypart := map_opta_bodypart(
    type_id = opta_type_id,
    qualifiers = if ("qualifiers_parsed" %in% names(dt)) qualifiers_parsed else NULL
  )]

  # Sort and create sequential action_id per match
  data.table::setorder(dt, match_id, period_id, time_seconds)
  dt[, action_id := seq_len(.N), by = match_id]

  # Normalize coordinates if requested
  if (normalize_direction) {
    dt <- data.table::as.data.table(normalize_spadl_coordinates(as.data.frame(dt)))
  }

  # Create numeric action type ID
  dt[, action_type_id := as.integer(factor(action_type))]

  # Add receiver information from next action
  # The player who does the next action is effectively the "receiver" of this action
  dt[, `:=`(
    receiver_player_id = data.table::shift(player_id, 1, type = "lead"),
    receiver_player_name = data.table::shift(player_name, 1, type = "lead"),
    receiver_team_id = data.table::shift(team_id, 1, type = "lead")
  ), by = match_id]

  # Detect possession change (next action by different team)
  dt[, possession_change := !is.na(receiver_team_id) & receiver_team_id != team_id]

  # Select final columns
  base_cols <- c("match_id", "action_id", "period_id", "time_seconds",
                  "team_id", "player_id", "player_name",
                  "start_x", "start_y", "end_x", "end_y",
                  "action_type", "action_type_id", "result", "bodypart",
                  "receiver_player_id", "receiver_player_name",
                  "receiver_team_id", "possession_change")

  # Include is_own_goal if qualifiers were parsed
  if ("is_own_goal" %in% names(dt)) {
    result_cols <- c(base_cols, "is_own_goal")
  } else {
    result_cols <- base_cols
  }
  result <- dt[, ..result_cols]

  cli::cli_alert_success("Converted to {format(nrow(result), big.mark=',')} SPADL actions")

  # Merge duplicate duel rows (aerial, tackle) into single rows with opponent info
  result <- merge_duel_rows(result)

  as.data.frame(result)
}


#' Parse Opta Qualifiers JSON
#'
#' Extracts qualifier information from the qualifier_json column.
#' Opta JSON format is a dictionary where keys are qualifier IDs:
#' \code{{"108":null,"55":"145","28":null,...}}
#'
#' @param events Data frame with qualifier_json column
#'
#' @return Data frame with parsed qualifier columns added
#' @keywords internal
parse_opta_qualifiers <- function(events) {
  if (!"qualifier_json" %in% names(events)) {
    return(events)
  }

  dt <- data.table::as.data.table(events)
  n <- nrow(dt)

  # Initialize qualifier columns (vectorized)
  dt[, `:=`(
    is_cross = FALSE,
    is_through_ball = FALSE,
    is_long_ball = FALSE,
    is_corner = FALSE,
    is_freekick = FALSE,
    is_throw_in = FALSE,
    is_goal_kick = FALSE,
    is_headed = FALSE,
    is_right_foot = FALSE,
    is_left_foot = FALSE,
    is_big_chance = FALSE,
    is_own_goal = FALSE
  )]

  # Get indices of rows with valid JSON
  valid_idx <- which(!is.na(dt$qualifier_json) &
                      dt$qualifier_json != "" &
                      dt$qualifier_json != "[]" &
                      dt$qualifier_json != "{}")

  if (length(valid_idx) > 0) {
    # Parse JSON in batch - Opta format is {"qualId": value, ...}
    # Keys are qualifier IDs as strings
    qual_ids_list <- lapply(dt$qualifier_json[valid_idx], function(qjson) {
      tryCatch({
        parsed <- jsonlite::fromJSON(qjson, simplifyVector = FALSE)
        if (is.list(parsed)) {
          as.integer(names(parsed))
        } else {
          integer(0)
        }
      }, error = function(e) integer(0))
    })

    # Vectorized qualifier checks using sapply
    dt$is_cross[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$cross %in% q)
    dt$is_through_ball[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$through_ball %in% q)
    dt$is_long_ball[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$long_ball %in% q)
    dt$is_corner[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$corner %in% q)
    dt$is_freekick[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$freekick %in% q)
    dt$is_throw_in[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$throw_in %in% q)
    dt$is_goal_kick[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_PASS_QUALIFIERS$goal_kick %in% q)
    dt$is_headed[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_BODYPART_QUALIFIERS$head %in% q)
    dt$is_right_foot[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_BODYPART_QUALIFIERS$right_foot %in% q)
    dt$is_left_foot[valid_idx] <- sapply(qual_ids_list, function(q) OPTA_BODYPART_QUALIFIERS$left_foot %in% q)
    dt$is_big_chance[valid_idx] <- sapply(qual_ids_list, function(q) 214L %in% q)
    dt$is_own_goal[valid_idx] <- sapply(qual_ids_list, function(q) 28L %in% q)  # Qualifier 28 = own goal
  }

  dt[, qualifiers_parsed := TRUE]

  as.data.frame(dt)
}


#' Map Opta Type ID to SPADL Action Type
#'
#' @param type_id Vector of Opta type_id values
#' @param qualifiers Parsed qualifier information (optional)
#'
#' @return Character vector of SPADL action types
#' @keywords internal
map_opta_action_type <- function(type_id, qualifiers = NULL) {
  n <- length(type_id)
  action_type <- rep("other", n)

  # Basic mappings
  action_type[type_id == 1L] <- "pass"
  action_type[type_id == 3L] <- "take_on"
  action_type[type_id == 4L] <- "foul"
  action_type[type_id == 7L] <- "tackle"
  action_type[type_id == 8L] <- "interception"
  action_type[type_id == 12L] <- "clearance"
  action_type[type_id == 13L] <- "shot"
  action_type[type_id == 14L] <- "shot"
  action_type[type_id == 15L] <- "shot"
  action_type[type_id == 16L] <- "shot"
  action_type[type_id == 44L] <- "aerial"
  action_type[type_id == 49L] <- "ball_recovery"
  action_type[type_id == 50L] <- "dispossessed"
  action_type[type_id == 61L] <- "ball_touch"

  # Keeper actions
  action_type[type_id == 10L] <- "keeper_save"
  action_type[type_id == 41L] <- "keeper_punch"
  action_type[type_id == 52L] <- "keeper_pick_up"
  action_type[type_id == 53L] <- "keeper_claim"

  # Refine pass subtypes using qualifiers if available
  if (!is.null(qualifiers) && is.list(qualifiers)) {
    # This would need the parsed qualifier flags
  }

  action_type
}


#' Map Opta Events to Body Part
#'
#' @param type_id Vector of Opta type_id values
#' @param qualifiers Parsed qualifier information (optional)
#'
#' @return Character vector of body parts: "foot", "head", or "other"
#' @keywords internal
map_opta_bodypart <- function(type_id, qualifiers = NULL) {
  n <- length(type_id)
  bodypart <- rep("foot", n)

  # Default non-foot actions
  bodypart[type_id == 44L] <- "head"  # Aerials typically headed
  bodypart[type_id %in% c(10L, 52L, 53L)] <- "other"  # Keeper actions

  # Would refine based on qualifiers if available

  bodypart
}


#' Normalize SPADL Coordinates
#'
#' Flips coordinates so the attacking team always attacks left-to-right
#' (toward x = 100). Uses period and team position to determine direction.
#' Optimized with data.table.
#'
#' @param spadl Data frame in SPADL format
#'
#' @return SPADL data frame with normalized coordinates
#' @keywords internal
normalize_spadl_coordinates <- function(spadl) {
  # Opta coordinates are already 0-100 with teams attacking opposite directions
  # We need to flip for one team per period

  dt <- data.table::as.data.table(spadl)

  # Determine which team attacks which direction per match/period
  # Heuristic: team with more events in x > 50 is attacking right
  team_direction <- dt[, .(attacks_right = mean(start_x, na.rm = TRUE) > 50),
                        by = .(match_id, period_id, team_id)]

  # Merge direction info
  dt <- merge(dt, team_direction, by = c("match_id", "period_id", "team_id"),
               all.x = TRUE, sort = FALSE)

  # Flip coordinates for teams attacking left (vectorized)
  dt[attacks_right == FALSE & !is.na(attacks_right), `:=`(
    start_x = 100 - start_x,
    end_x = 100 - end_x,
    start_y = 100 - start_y,
    end_y = 100 - end_y
  )]

  # Remove helper column and restore order
  dt[, attacks_right := NULL]
  data.table::setorder(dt, match_id, action_id)

  as.data.frame(dt)
}


#' Calculate Distance to Goal
#'
#' Calculates Euclidean distance from coordinates to center of goal.
#' Assumes goal is at x=100, y=50.
#'
#' @param x X coordinate (0-100 scale)
#' @param y Y coordinate (0-100 scale)
#' @param goal_x X coordinate of goal center (default 100)
#' @param goal_y Y coordinate of goal center (default 50)
#'
#' @return Distance to goal in coordinate units
#' @export
#' @examples
#' calculate_distance_to_goal(85, 50)  # Close, central
#' calculate_distance_to_goal(50, 50)  # Halfway line
calculate_distance_to_goal <- function(x, y, goal_x = 100, goal_y = 50) {
  sqrt((goal_x - x)^2 + (goal_y - y)^2)
}


#' Calculate Angle to Goal
#'
#' Calculates the visible angle (in radians) to the goal from given coordinates.
#' The angle represents how much of the goal is visible from the player's position.
#' Useful for shot xG models.
#'
#' @param x X coordinate (0-100 scale, attacking toward x=100)
#' @param y Y coordinate (0-100 scale, y=50 is center of pitch)
#' @param goal_width Goal width in coordinate units (default ~12 for standard goal)
#'
#' @return Angle to goal in radians (always positive)
#' @export
#' @examples
#' calculate_angle_to_goal(90, 50)  # Central, close - large angle
#' calculate_angle_to_goal(90, 80)  # Wide position - smaller angle
#' calculate_angle_to_goal(50, 50)  # Halfway line - small angle
calculate_angle_to_goal <- function(x, y, goal_width = 12) {
  # Goal posts at y = 50 +/- goal_width/2 on x = 100
  goal_y_left <- 50 - goal_width / 2   # y = 44 with default width

  goal_y_right <- 50 + goal_width / 2  # y = 56 with default width

  # Distance to goal line (avoid division by zero for positions at/behind goal)
  dist_to_goal <- pmax(100 - x, 0.1)

  # Calculate angles to each post using atan2(dy, dx)
  # dy = difference in y from player to post
  # dx = distance to goal line
  angle_left <- atan2(goal_y_left - y, dist_to_goal)
  angle_right <- atan2(goal_y_right - y, dist_to_goal)

  # Visible angle is the absolute difference between angles to each post
  abs(angle_right - angle_left)
}


#' Determine Pitch Zone
#'
#' Assigns a zone ID based on pitch location. Uses a 3x6 grid (18 zones).
#'
#' @param x X coordinate (0-100)
#' @param y Y coordinate (0-100)
#'
#' @return Integer zone ID (1-18)
#' @export
#' @examples
#' get_pitch_zone(10, 50)   # Defensive third, center
#' get_pitch_zone(90, 20)   # Attacking third, left
get_pitch_zone <- function(x, y) {
  # X zones: 0-33 (def), 33-67 (mid), 67-100 (att)
  x_zone <- cut(x, breaks = c(-Inf, 33, 67, Inf), labels = FALSE)

  # Y zones: 0-33 (left), 33-67 (center), 67-100 (right)
  y_zone <- cut(y, breaks = c(-Inf, 33, 67, Inf), labels = FALSE)

  # Combine to single zone ID
  (x_zone - 1) * 3 + y_zone
}


#' Check if Coordinates are in Penalty Area
#'
#' @param x X coordinate (0-100)
#' @param y Y coordinate (0-100)
#' @param attacking If TRUE, checks attacking penalty area (x > 83)
#'
#' @return Logical indicating if in penalty area
#' @export
is_in_penalty_area <- function(x, y, attacking = TRUE) {
  # Penalty area: roughly x > 83, y between 21 and 79 (18-yard box)
  if (attacking) {
    x > 83 & y > 21 & y < 79
  } else {
    x < 17 & y > 21 & y < 79
  }
}


#' Check if Coordinates are in Final Third
#'
#' @param x X coordinate (0-100)
#' @param attacking If TRUE, checks attacking final third (x > 67)
#'
#' @return Logical indicating if in final third
#' @export
is_in_final_third <- function(x, attacking = TRUE) {
  if (attacking) {
    x > 67
  } else {
    x < 33
  }
}


#' Merge Duplicate Duel Rows
#'
#' Opta records both participants of a duel as separate rows (winner and loser
#' perspectives). This function merges these into single rows, keeping the
#' winner's row and adding opponent information from the loser's row.
#'
#' @param spadl_dt Data.table in SPADL format with duel actions
#'
#' @return Data.table with merged duel rows and new columns:
#'   \itemize{
#'     \item opponent_player_id - Player who lost the duel (NA for non-duels)
#'     \item opponent_player_name - Name of opponent
#'   }
#'
#' @keywords internal
merge_duel_rows <- function(spadl_dt) {
  dt <- data.table::as.data.table(spadl_dt)

  # Initialize opponent columns
  dt[, `:=`(
    opponent_player_id = NA_character_,
    opponent_player_name = NA_character_
  )]

  # Identify duel actions (aerial and tackle)
  duel_types <- c("aerial", "tackle")

  # Get indices of duel actions
  duel_idx <- which(dt$action_type %in% duel_types)

  if (length(duel_idx) < 2) {
    return(as.data.frame(dt))
  }

  # Track rows to remove (loser's duplicate rows)
  rows_to_remove <- integer(0)

  # Process consecutive duel pairs
  i <- 1
  while (i < length(duel_idx)) {
    idx1 <- duel_idx[i]
    idx2 <- duel_idx[i + 1]

    # Check if these are a matching duel pair:
    # - Consecutive rows (idx2 == idx1 + 1)
    # - Same match, same period, same time
    # - Same action type
    # - Different teams
    if (idx2 == idx1 + 1 &&
        dt$match_id[idx1] == dt$match_id[idx2] &&
        dt$period_id[idx1] == dt$period_id[idx2] &&
        dt$time_seconds[idx1] == dt$time_seconds[idx2] &&
        dt$action_type[idx1] == dt$action_type[idx2] &&
        dt$team_id[idx1] != dt$team_id[idx2]) {

      # Determine winner (success) vs loser (fail)
      result1 <- dt$result[idx1]
      result2 <- dt$result[idx2]

      if (result1 == "success" && result2 == "fail") {
        # Row 1 is winner, row 2 is loser
        winner_idx <- idx1
        loser_idx <- idx2
      } else if (result1 == "fail" && result2 == "success") {
        # Row 2 is winner, row 1 is loser
        winner_idx <- idx2
        loser_idx <- idx1
      } else {
        # Edge case: both success or both fail - keep first, log warning
        if (result1 == result2) {
          cli::cli_warn(paste0(
            "Duel at match ", dt$match_id[idx1], " time ", dt$time_seconds[idx1],
            "s has same result for both players (", result1, "). Keeping first row."
          ))
        }
        winner_idx <- idx1
        loser_idx <- idx2
      }

      # Add opponent info to winner's row
      dt$opponent_player_id[winner_idx] <- dt$player_id[loser_idx]
      dt$opponent_player_name[winner_idx] <- dt$player_name[loser_idx]

      # Mark loser's row for removal
      rows_to_remove <- c(rows_to_remove, loser_idx)

      # Skip the paired row in next iteration
      i <- i + 2
    } else {
      # Not a matching pair, move to next
      i <- i + 1
    }
  }

  # Remove loser's duplicate rows
  if (length(rows_to_remove) > 0) {
    dt <- dt[-rows_to_remove]
    cli::cli_alert_info("Merged {length(rows_to_remove)} duplicate duel rows")

    # Re-sequence action_id after deletions
    data.table::setorder(dt, match_id, period_id, time_seconds)
    dt[, action_id := seq_len(.N), by = match_id]
  }

  as.data.frame(dt)
}
