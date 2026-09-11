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
  # Shots (type_id = 13-16) — per Opta: 13=Miss, 14=Post, 15=Attempt Saved
  shot_miss = 13L,
  shot_post = 14L,
  shot_saved = 15L,
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
  keeper_claim = 11L,     # Claim (GK catches cross)
  keeper_punch = 41L,
  # 1v1 challenges and skill moves
  one_on_one = 83L,       # Att One on One
  good_skill = 42L        # Good Skill (skillful play)
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

# Opta F24 event type_id -> human-readable name. SINGLE source of truth for the
# descriptive `opta_type_name` column. Reconciled 2026-06-18 against the public
# Opta F24 spec (statsperform.com/opta-event-definitions; tomh05/football-scores;
# znstrider/opta_event_ids_and_names) AND validated against our own feed's event
# distribution + median pitch location (x: 0 = own goal line, 100 = opp goal).
# The SPADL action mapping (map_opta_action_type) keys off type_id, NOT these
# names, so corrections here are display-only and safe.
# "(verify)" = our feed conflicts with the public spec (e.g. non-located where the
# spec implies a located action) — confirm vs the official Opta doc before relying
# on it. "Unknown" = absent from all our spec sources.
#
# FLAGGED follow-ups (NOT changed here — model-affecting):
#   * 74 "Blocked Pass" (a DEFENDER blocking a pass) is dropped via
#     OPTA_NON_GAMEPLAY_TYPES (was mislabelled "Injury Clearance"). It is the
#     DEFENSIVE companion of the opponent's failed pass (qualifier 233 links them),
#     so dropping it avoids double-counting the ball event — BUT it means the
#     blocker gets no defensive credit. ~77k/league. Decide vs panna#104.
#   * 67 "50/50" (was "Offside") is likewise a contest that's dropped — same
#     defensive-credit consideration.
#   * 54 "Smother" is a keeper action (own goalmouth, x~9) but
#     map_opta_action_type() maps it to `ball_touch`; really a possession-settling
#     keeper action (cf. keeper_claim). See panna#104.
#   * non-located admin types 22/24/25/39/71/75/79/81 are NOT in
#     OPTA_NON_GAMEPLAY_TYPES, so they fall through to SPADL as `other`.
OPTA_TYPE_NAMES <- c(
  "1" = "Pass",
  "2" = "Offside Pass",
  "3" = "Take On",
  "4" = "Foul",
  "5" = "Ball Out",                       # Opta "Out"
  "6" = "Corner Awarded",
  "7" = "Tackle",
  "8" = "Interception",
  "9" = "Turnover",
  "10" = "Save",
  "11" = "Claim",
  "12" = "Clearance",
  "13" = "Miss",
  "14" = "Post",
  "15" = "Attempt Saved",                 # incl. defender-blocked shots (qualifier 82)
  "16" = "Goal",
  "17" = "Card",
  "18" = "Player Off",
  "19" = "Player On",
  "20" = "Player Retired",
  "21" = "Player Returns",
  "22" = "Player Becomes Goalkeeper",
  "23" = "Goalkeeper Becomes Player",
  "24" = "Condition Change",
  "25" = "Official Change",
  "27" = "Start Delay",                   # was "Start" (WRONG)
  "28" = "End Delay",                     # was "End" (WRONG)
  "30" = "End",                           # was "End 1H"; period end (read by extract_period_end_times)
  "32" = "Start",                         # was "Start 2H"; period start
  "34" = "Team Set Up",
  "35" = "Position Change",
  "36" = "Jersey Change",
  "37" = "Collection End",
  "38" = "Temp Goal",
  "39" = "Temp Attempt",
  "40" = "Formation Change",
  "41" = "Punch",
  "42" = "Good Skill",
  "43" = "Deleted Event",
  "44" = "Aerial",
  "45" = "Challenge",
  "47" = "Rescinded Card",
  "49" = "Ball Recovery",
  "50" = "Dispossessed",                  # was "Blocked Pass" (WRONG); maps to action `dispossessed`, x~64
  "51" = "Error",                         # was "Delay of Play" (WRONG); defensive mistake, x~18
  "52" = "Keeper Pick-up",
  "53" = "Cross Not Claimed",             # was "Chance Missed" (WRONG); keeper, own goalmouth x~5
  "54" = "Smother",                       # was "Ball Touch" (WRONG); keeper, own goalmouth x~9 (see FLAG)
  "55" = "Offside Provoked",              # was "Temp Goal" (WRONG)
  "56" = "Shield Ball Opp",               # was "Resume Play" (WRONG) (our feed: non-located)
  "57" = "Foul Throw-in",                 # was "Contentious Decision" (WRONG) (our feed: non-located)
  "58" = "Penalty Faced",                 # keeper; carries penalty qualifier 9
  "59" = "Keeper Sweeper",                # (the name was wrongly at 77)
  "60" = "Chance Missed",                 # (a copy was wrongly at 80)
  "61" = "Ball Touch",
  "63" = "Temp Save",
  "64" = "Resume",
  "65" = "Contentious Referee Decision",
  "66" = "Possession Data",
  "67" = "50/50",                         # was "Offside" (WRONG); midfield contests, x~50, 55k
  "68" = "Referee Drop Ball",             # was "Offside Provoked" (WRONG) (verify - our feed mostly non-located)
  "69" = "Failed To Block",
  "70" = "Injury Time Announcement",      # was "Shield Ball" (WRONG); non-located marker
  "71" = "Coach Setup",                   # was "Unknown"
  "72" = "Caught Offside",
  "73" = "Other Ball Contact",
  "74" = "Blocked Pass",                  # was "Injury Clearance" (WRONG); defender blocks a pass, x~44 (see FLAG)
  # ids 75+ are NOT in the public F24 references; named from our feed where the
  # evidence is clear, else left Unknown. Confirm vs the official Opta spec.
  "75" = "Unknown",                       # n~508 corpus-wide; non-located
  "76" = "Unknown",                       # n~56; NOT in any public ref nor prior dict
  "79" = "Unknown",                       # n~10.3k; non-located
  "80" = "Unknown",                       # n~89k; was "Chance Missed" (that is 60); located x~12 (verify)
  "81" = "Unknown",                       # n~15.7k; located x~64 (verify)
  "83" = "Att One on One",                # n~798k; maps to take_on; located x~39 (verify - not in public sources)
  "84" = "Unknown"                        # n~10.8k; non-located; carries rich qualifiers
)

# Non-gameplay event type_ids to filter out
# These don't contribute to EPV and shouldn't be in SPADL
OPTA_NON_GAMEPLAY_TYPES <- c(
  2L,   # Offside Pass - play is dead
  5L,   # Ball Out - ball is dead
  6L,   # Corner Awarded - just the award, not the kick
  17L,  # Card - booking/sending off
  18L,  # Player Off - substitution
  19L,  # Player On - substitution
  27L,  # Start - period start
  28L,  # End - period end
  30L,  # End 1H - first half end
  32L,  # Start 2H - second half start
  34L,  # Team Set Up - lineup info
  35L,  # Position Change - tactical
  36L,  # Jersey Change - admin
  37L,  # Collection End - data marker
  40L,  # Formation Change - tactical
  43L,  # Deleted Event - removed from feed!
  45L,  # Challenge - 50/50 contest, no clear possession
  51L,  # Delay of Play - time wasting
  55L,  # Temp Goal - temporary marker
  56L,  # Resume Play - marker
  57L,  # Contentious Decision - ref decision
  67L,  # 50/50 - contest (was mislabelled "Offside"); dropped like Challenge (45) - see FLAG
  68L,  # Referee Drop Ball (was mislabelled "Offside Provoked")
  74L,  # Blocked Pass - defensive mirror of the opponent's failed pass (was mislabelled "Injury Clearance") - see FLAG
  80L,  # Chance Missed - redundant with shot data
  53L,  # Chance Missed (alternate) - redundant with shot data
  # Unknown/rare types with no clear gameplay contribution
  20L,  # Unknown
  58L,  # Unknown
  59L,  # Unknown
  60L,  # Unknown
  65L,  # Unknown
  84L   # Unknown
)


#' Get or Build SPADL (with disk cache)
#'
#' Thin wrapper over \code{convert_opta_to_spadl()} that caches the result
#' on disk, keyed by \code{league} and \code{season}. SPADL conversion is the
#' single most expensive step in downstream EPV pipelines (~25% of total
#' runtime per league-season) and is fully deterministic given raw events, so
#' caching yields a large win on repeat runs and backfills.
#'
#' @param events Raw Opta events from \code{load_opta_match_events()}.
#' @param league League code (e.g. "ENG"). Used only to build the cache key.
#' @param season Season string (e.g. "2024-2025"). Used only to build the cache key.
#' @param cache_dir Directory to read/write the cached \code{.rds}. Defaults to
#'   \code{SPADL_CACHE_DIR} so all pipelines share one cache.
#' @param force_rebuild If \code{TRUE}, rebuild and overwrite the cache.
#'
#' @return Data frame in SPADL format, identical to \code{convert_opta_to_spadl()}.
#' @family epv
#' @export
get_or_build_spadl <- function(events, league, season,
                                cache_dir = SPADL_CACHE_DIR,
                                force_rebuild = FALSE) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_path <- file.path(cache_dir,
                           sprintf("spadl_%s_%s.rds", league, season))

  if (!force_rebuild && file.exists(cache_path)) {
    spadl <- readRDS(cache_path)
    # Schema sanity -- downstream pipelines (equity export) assume certain
    # columns exist. If the cache predates a schema addition, rebuild rather
    # than silently returning a stale snapshot.
    required_cols <- c("match_id", "action_id", "original_event_id",
                       "player_id", "action_type", "start_x", "start_y",
                       "result")
    missing_cols <- setdiff(required_cols, names(spadl))
    if (length(missing_cols) == 0) {
      # Coverage sanity -- cache must cover all requested match_ids.
      wanted <- unique(events$match_id)
      if (length(setdiff(wanted, unique(spadl$match_id))) == 0) {
        return(spadl)
      }
    }
    # Stale, partial, or schema-outdated -- rebuild.
  }

  spadl <- convert_opta_to_spadl(events)
  saveRDS(spadl, cache_path)
  spadl
}


#' Convert Opta Match Events to SPADL Format
#'
#' Transforms Opta event data into a standardized SPADL-like format suitable
#' for EPV modeling. Coordinates are preserved in Opta's native team-relative
#' format where each team's actions are from their own perspective (x=0 is own
#' goal, x=100 is opponent's goal). This is the correct format for EPV models
#' where each action is evaluated from the ball-carrier's perspective.
#'
#' @param opta_events Data frame from load_opta_match_events()
#' @param normalize_direction Whether to attempt coordinate normalization. Set
#'   to FALSE (default) to preserve Opta's team-relative coordinates, which is
#'   correct for EPV. Set to TRUE only for visualizations requiring unified
#'   pitch coordinates (note: the normalization heuristic may not work well
#'   with team-relative data).
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
#'     \item opta_type_id: Original Opta type_id (e.g., 13=miss, 15=saved, 16=goal)
#'     \item result: "success" or "fail"
#'     \item bodypart: "foot", "head", or "other"
#'     \item receiver_player_id: Player who receives/intercepts (from next action)
#'     \item receiver_player_name: Name of receiver/interceptor
#'     \item receiver_team_id: Team of receiver (for detecting possession change)
#'     \item possession_change: TRUE if next action is by opponent team
#'   }
#'
#' @family epv
#' @export
#' @examples
#' \dontrun{
#' events <- load_opta_match_events("ENG", "2024-2025")
#' spadl <- convert_opta_to_spadl(events)
#' }
convert_opta_to_spadl <- function(opta_events, normalize_direction = FALSE) {
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

  # Convert to data.table once at the start - all operations in place
  dt <- data.table::as.data.table(opta_events)

  # Parse qualifiers if present (modifies dt in place)
  if ("qualifier_json" %in% names(dt)) {
    dt <- parse_opta_qualifiers(dt)
  }

  # Filter out non-player events (match start, period start, etc.)
  n_before <- nrow(dt)
  dt <- dt[!is.na(player_name) & player_name != ""]
  n_filtered <- n_before - nrow(dt)
  if (n_filtered > 0) {
    cli::cli_alert_info("Filtered {n_filtered} non-player events (match/period markers)")
  }

  # Check for end_x/end_y columns once
  has_end_x <- "end_x" %in% names(dt)
  has_end_y <- "end_y" %in% names(dt)
  has_event_id <- "event_id" %in% names(dt)

  # Create SPADL columns (vectorized, in place)
  if (has_event_id) {
    dt[, original_event_id := event_id]
  } else {
    dt[, original_event_id := .I]
  }

  dt[, `:=`(
    time_seconds = minute * 60L + second,
    start_x = x,
    start_y = y,
    opta_type_id = type_id,
    opta_type_name = OPTA_TYPE_NAMES[as.character(type_id)]
  )]

  # Filter out non-gameplay events (ball out, deleted, cards, subs, etc.)
  n_before_filter <- nrow(dt)
  dt <- dt[!opta_type_id %in% OPTA_NON_GAMEPLAY_TYPES]
  n_non_gameplay <- n_before_filter - nrow(dt)
  if (n_non_gameplay > 0) {
    cli::cli_alert_info("Filtered {format(n_non_gameplay, big.mark=',')} non-gameplay events (ball out, deleted, cards, subs, etc.)")
  }

  # Drop penalty-shootout events (period_id >= 5). Shootout kicks are recorded
  # as goals/shots at minute 120 but are not open play — leaving them in would
  # credit EPV/WPA for shootout conversions and (via the WP time feature, which
  # reads them as time_remaining == 0) massively inflate per-event win-prob
  # swings. A match decided on penalties is treated as a draw in open play.
  if ("period_id" %in% names(dt)) {
    n_before_so <- nrow(dt)
    dt <- dt[!is_shootout_period(period_id)]
    n_shootout <- n_before_so - nrow(dt)
    if (n_shootout > 0) {
      cli::cli_alert_info("Filtered {format(n_shootout, big.mark=',')} penalty-shootout events (period_id >= 5)")
    }
  }

  if (has_end_x) {
    dt[, end_x_new := end_x]
  } else {
    dt[, end_x_new := x]
  }
  if (has_end_y) {
    dt[, end_y_new := end_y]
  } else {
    dt[, end_y_new := y]
  }
  # Rename to avoid conflicts
  if (has_end_x) dt[, end_x := NULL]
  if (has_end_y) dt[, end_y := NULL]
  data.table::setnames(dt, c("end_x_new", "end_y_new"), c("end_x", "end_y"))

  # Map Opta type_id to SPADL action types (vectorized)
  dt[, action_type := map_opta_action_type(opta_type_id, NULL)]

  # ===========================================================================
  # Handle Opta end_x/end_y data issues
  # ===========================================================================
  #
  # ANALYSIS FINDINGS (see debug/analyze_spadl_continuity.R):
  # - Only passes (type_id=1) and clearances (type_id=12) have proper end_x/end_y
  # - All other action types have end_x=0, end_y=0 in Opta data
  # - This breaks EPV chain continuity where end should match next action's start
  #
  # SOLUTION:
  # For actions with end_x=0, set end = start (ball stays with player)
  # This includes duels (aerials, tackles) - winner keeps ball at their position
  # Passes and clearances already have correct coordinates - keep as-is
  # ===========================================================================

  # Actions where ball stays with the player (end = start when end_x is 0)
  # Includes duels: aerial winner and tackle winner keep the ball at their position
  # Note: clearance usually has end coords, but rare Opta data quality issues exist
  stationary_actions <- c(
    "tackle",        # Won tackle - ball stays with tackler
    "interception",  # Intercepted - ball stays with interceptor
    "ball_recovery", # Recovered - ball stays with recoverer
    "ball_touch",    # Simple touch - ball doesn't travel
    "take_on",       # Successful dribble - ball stays with dribbler
    "foul",          # Play stops at foul location
    "dispossessed",  # Lost possession at that spot
    "keeper_save",   # Save at keeper position
    "keeper_pick_up",# Pick up at keeper position
    "keeper_claim",  # Claim at keeper position
    "keeper_punch",  # Punch from keeper position
    "aerial",        # Aerial winner - ball stays near winner's position
    "clearance"      # Fallback for rare Opta data quality issues (usually has end coords)
  )

  # Only fix where end_x is 0 (Opta's missing value indicator)
  fix_idx <- which(dt$action_type %in% stationary_actions & dt$end_x == 0)
  if (length(fix_idx) > 0) {
    dt[fix_idx, `:=`(end_x = start_x, end_y = start_y)]
    cli::cli_alert_info("Fixed {length(fix_idx)} actions with end_x=0 (set end = start)")
  }

  # Note on remaining "other" type actions:
  # Actions not explicitly mapped (e.g., corners awarded, ball out) remain as "other"
  # and are filtered naturally downstream since they don't contribute to EPV

  # Map result (success/fail) - vectorized
  dt[, result := fifelse(
    opta_type_id %in% c(13L, 14L, 15L, 16L),
    fifelse(opta_type_id == 16L, "success", "fail"),
    fifelse(outcome == 1L, "success", "fail")
  )]

  # Determine body part (vectorized)
  dt[, bodypart := map_opta_bodypart(opta_type_id, NULL)]

  # Sort and create sequential action_id per match
  data.table::setorder(dt, match_id, period_id, time_seconds)
  dt[, action_id := seq_len(.N), by = match_id]

  # Normalize coordinates if requested (modifies dt in place)
  if (normalize_direction) {
    dt <- normalize_spadl_coordinates(dt)
  }


  # Fix ALL shot end coordinates to goal center AFTER normalization
  # Opta sometimes has (100, 100) for shots going wide - we normalize to (100, 50)
  # For EPV, shots are terminal actions so precise end position doesn't matter
  shot_idx <- which(dt$action_type == "shot")
  if (length(shot_idx) > 0) {
    dt[shot_idx, `:=`(end_x = 100, end_y = 50)]
    cli::cli_alert_info("Set {length(shot_idx)} shots to end at goal center (100, 50)")
  }

  # Create numeric action type ID
  dt[, action_type_id := as.integer(factor(action_type))]

  cli::cli_alert_success("Converted to {format(nrow(dt), big.mark=',')} SPADL actions")

  # Merge duplicate duel rows BEFORE calculating receiver info
  # (so receiver points to correct next action, not deleted duel loser)
  dt <- merge_duel_rows(dt)

  # Re-sequence action_id after duel merging
  data.table::setorder(dt, match_id, period_id, time_seconds)
  dt[, action_id := seq_len(.N), by = match_id]

  # Add receiver information from next action (AFTER duel merge)
  dt[, `:=`(
    receiver_player_id = shift(player_id, 1, type = "lead"),
    receiver_player_name = shift(player_name, 1, type = "lead"),
    receiver_team_id = shift(team_id, 1, type = "lead")
  ), by = match_id]

  # Detect possession change (next action by different team)
  dt[, possession_change := !is.na(receiver_team_id) & receiver_team_id != team_id]

  # Select final columns
  # Keep opta_type_id and opta_type_name for downstream use and debugging
  # Include opponent_player_id/name for duel credit assignment
  base_cols <- c("match_id", "action_id", "original_event_id",
                  "period_id", "time_seconds",
                  "team_id", "player_id", "player_name",
                  "start_x", "start_y", "end_x", "end_y",
                  "action_type", "action_type_id", "opta_type_id", "opta_type_name",
                  "result", "bodypart",
                  "receiver_player_id", "receiver_player_name",
                  "receiver_team_id", "possession_change",
                  "opponent_player_id", "opponent_player_name")

  # Include qualifier-derived columns if available
  optional_cols <- c("is_own_goal", "is_big_chance", "is_penalty")
  available_optional <- optional_cols[optional_cols %in% names(dt)]
  result_cols <- c(base_cols, available_optional)
  dt <- dt[, ..result_cols]

  as.data.frame(dt)
}


#' Parse Opta Qualifiers JSON
#'
#' Extracts qualifier information from the qualifier_json column.
#' Opta JSON format is a dictionary where keys are qualifier IDs:
#' \code{{"108":null,"55":"145","28":null,...}}
#'
#' Uses fast regex-based extraction instead of JSON parsing.
#'
#' @param dt Data.table with qualifier_json column (modified in place)
#'
#' @return Data.table with parsed qualifier columns added
#' @keywords internal
parse_opta_qualifiers <- function(dt) {
  if (!"qualifier_json" %in% names(dt)) {
    return(dt)
  }

  # Use regex to check for qualifier IDs - much faster than JSON parsing

  # Opta format: {"2":null,"15":"value",...} - keys are qualifier IDs
  qjson <- dt$qualifier_json

  # Pre-compute valid mask once

  valid <- !is.na(qjson) & nchar(qjson) > 2

  # Vectorized regex checks - pattern matches exact qualifier key in JSON
  # Anchor with [{,] to prevent "1": matching "10": or "15":
  dt[, `:=`(
    is_cross = valid & grepl('[{,]"2":', qjson),
    is_through_ball = valid & grepl('[{,]"4":', qjson),
    is_long_ball = valid & grepl('[{,]"1":', qjson),
    is_corner = valid & grepl('[{,]"6":', qjson),
    is_freekick = valid & grepl('[{,]"5":', qjson),
    is_throw_in = valid & grepl('[{,]"107":', qjson),
    is_goal_kick = valid & grepl('[{,]"124":', qjson),
    is_headed = valid & grepl('[{,]"15":', qjson),
    is_right_foot = valid & grepl('[{,]"72":', qjson),
    is_left_foot = valid & grepl('[{,]"36":', qjson),
    is_big_chance = valid & grepl('[{,]"214":', qjson),
    is_own_goal = valid & grepl('[{,]"28":', qjson),
    is_penalty = valid & grepl('[{,]"9":', qjson)   # qualifier 9 = penalty
  )]

  dt[, qualifiers_parsed := TRUE]

  dt
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
  action_type[type_id == 11L] <- "keeper_claim"  # Claim (GK catches cross)
  action_type[type_id == 41L] <- "keeper_punch"
  action_type[type_id == 52L] <- "keeper_pick_up"
  # Note: Type 53 is "Cross Not Claimed" (keeper FAILED to claim a cross), NOT a
  # successful keeper_claim (type 11) - don't map it.

  # 1v1 situations and skill moves (similar to take_on)
  action_type[type_id == 42L] <- "take_on"       # Good Skill
  action_type[type_id == 83L] <- "take_on"       # Att One on One

  # Additional ball touch type
  action_type[type_id == 54L] <- "ball_touch"    # Ball Touch (alternate)

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

  # STUB, and a consequential one. `qualifiers` is never passed by the only
  # caller (convert_opta_to_spadl passes NULL), so the refinement below was
  # never written and SHOTS never get a real body part: they are types
  # 13/14/15/16, none of which is 44, so every shot -- header included --
  # returns "foot". Measured on ENG 2015-2016: 9,782 of 9,782 shots labelled
  # "foot", against 15.7% headers in the shot events.
  #
  # That silently killed three xG features (is_header, is_right_foot,
  # is_left_foot) for as long as this has existed, worth +6.30% on total xG.
  # xG now joins Opta's own `body_part` from the shot events instead -- see
  # add_xg_to_spadl()'s shot_lookup. Fixing it HERE would be better, since
  # every other SPADL consumer is still reading a constant, but it means
  # parsing qualifiers and rebuilding ~2.8GB of cached SPADL, so it is tracked
  # as a follow-up rather than done inline.

  bodypart
}


#' Normalize SPADL Coordinates
#'
#' WARNING: This function uses a heuristic that doesn't work well with Opta's
#' team-relative coordinate system. For EPV modeling, use normalize_direction=FALSE
#' (the default) to preserve Opta's native coordinates where each team's actions
#' are from their own perspective.
#'
#' Attempts to flip coordinates so all teams attack toward x=100. Uses mean
#' position per team/period as a heuristic, but this fails when both teams have
#' similar mean positions or when coordinates are team-relative.
#'
#' @param dt Data.table in SPADL format (modified in place)
#'
#' @return Data.table with normalized coordinates (same object, modified)
#' @keywords internal
normalize_spadl_coordinates <- function(dt) {
 # WARNING: Opta uses team-relative coordinates where x=0 is each team's own goal
 # and x=100 is their attacking goal. This means the mean_x heuristic below
 # doesn't reliably identify which direction each team is attacking.

  # Determine which team attacks which direction per match/period
  # Heuristic: team with mean x > 50 is attacking right
  team_direction <- dt[, .(mean_x = mean(start_x, na.rm = TRUE)),
                        by = .(match_id, period_id, team_id)]
  team_direction[, attacks_right := mean_x > 50]

  # Use keyed join for speed
  data.table::setkeyv(team_direction, c("match_id", "period_id", "team_id"))
  data.table::setkeyv(dt, c("match_id", "period_id", "team_id"))

  # Add attacks_right column via join
 dt[team_direction, attacks_right := i.attacks_right]

  # Flip coordinates for teams attacking left (vectorized, in place)
  flip_idx <- which(dt$attacks_right == FALSE & !is.na(dt$attacks_right))
  if (length(flip_idx) > 0) {
    dt[flip_idx, `:=`(
      start_x = 100 - start_x,
      end_x = 100 - end_x,
      start_y = 100 - start_y,
      end_y = 100 - end_y
    )]
  }

  # Remove helper column and restore order
  dt[, attacks_right := NULL]
  data.table::setorder(dt, match_id, action_id)

  dt
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
#' @keywords internal
#' @examples
#' \dontrun{
#' calculate_distance_to_goal(50, 50)  # Halfway line
#' }
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
#' @keywords internal
#' @examples
#' \dontrun{
#' calculate_angle_to_goal(90, 80)  # Wide position - smaller angle
#' calculate_angle_to_goal(50, 50)  # Halfway line - small angle
#' }
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
#' @keywords internal
#' @examples
#' \dontrun{
#' get_pitch_zone(90, 20)   # Attacking third, left
#' }
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
#' @keywords internal
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
#' @keywords internal
is_in_final_third <- function(x, attacking = TRUE) {

  if (attacking) {
    x > 67
  } else {
    x < 33
  }
}


#' Calculate Physical Discontinuity
#'
#' Calculates spatial discontinuity between consecutive actions using a unified
#' pitch coordinate frame. This converts team-relative Opta coordinates to a
#' common physical frame, allowing measurement of actual ball movement rather
#' than perspective changes.
#'
#' Opta uses team-relative coordinates where each team sees x=100 as their
#' attacking goal. When possession changes, coordinates appear to "jump" due to
#' perspective change, not ball movement. This function removes that artifact
#' by converting all coordinates to a single reference frame.
#'
#' @param spadl_dt Data.table or data.frame in SPADL format from
#'   \code{convert_opta_to_spadl()}. Must have columns: match_id, team_id,
#'   start_x, start_y, end_x, end_y.
#'
#' @return Data.table with additional columns:
#'   \itemize{
#'     \item phys_disc: Physical discontinuity (Euclidean distance between
#'       action end and next action start in unified frame)
#'     \item phys_end_x, phys_end_y: End coordinates in physical frame
#'     \item phys_next_x, phys_next_y: Next action start in physical frame
#'   }
#'
#' @details
#' For validation purposes, physical discontinuity should be low for most

#' action types (median < 10-15 units). High values indicate data quality
#' issues or unusual game situations (e.g., set pieces).
#'
#' Expected median physical discontinuity by action type:
#' \itemize{
#'   \item pass: ~3 (pass lands where receiver starts)
#'   \item aerial: ~3 (winner catches at their position)
#'   \item tackle: ~6 (tackler wins ball nearby)
#'   \item shot: ~10 (shot to goal, next action at keeper/goal)
#' }
#'
#' @keywords internal
#' @examples
#' \dontrun{
#' events <- load_opta_match_events("ENG", "2024-2025")
#' spadl <- convert_opta_to_spadl(events)
#' spadl_with_disc <- calculate_physical_discontinuity(spadl)
#'
#' # Check median discontinuity by action type
#' spadl_with_disc[, .(median_disc = median(phys_disc, na.rm = TRUE)),
#'                 by = action_type]
#' }
calculate_physical_discontinuity <- function(spadl_dt) {
  # Convert to data.table if needed

  dt <- data.table::as.data.table(spadl_dt)

 # Pick first team in each match as the reference frame
  teams_per_match <- dt[, .(ref_team = team_id[1]), by = match_id]
  dt <- teams_per_match[dt, on = "match_id"]

  # Convert end coordinates to physical frame
  # Reference team: keep as-is; other team: flip (100 - coord)
  dt[, `:=`(
    phys_end_x = data.table::fifelse(team_id == ref_team, end_x, 100 - end_x),
    phys_end_y = data.table::fifelse(team_id == ref_team, end_y, 100 - end_y)
  )]

  # Get next action info
  dt[, `:=`(
    next_team = shift(team_id, 1, type = "lead"),
    next_start_x = shift(start_x, 1, type = "lead"),
    next_start_y = shift(start_y, 1, type = "lead")
  ), by = match_id]

  # Convert next action start to physical frame
  dt[, `:=`(
    phys_next_x = data.table::fifelse(next_team == ref_team, next_start_x, 100 - next_start_x),
    phys_next_y = data.table::fifelse(next_team == ref_team, next_start_y, 100 - next_start_y)
  )]

  # Calculate physical discontinuity
  dt[, phys_disc := sqrt((phys_end_x - phys_next_x)^2 + (phys_end_y - phys_next_y)^2)]

  # Clean up helper columns
  dt[, c("ref_team", "next_team", "next_start_x", "next_start_y") := NULL]

  dt
}


#' Merge Duplicate Duel Rows
#'
#' Opta records both participants of a duel as separate rows (winner and loser
#' perspectives). This function merges these into single rows, keeping the
#' winner's row and adding opponent information from the loser's row.
#' Fully vectorized with data.table for speed.
#'
#' @param dt Data.table in SPADL format with duel actions (modified in place)
#'
#' @return Data.table with merged duel rows and new columns:
#'   \itemize{
#'     \item opponent_player_id - Player who lost the duel (NA for non-duels)
#'     \item opponent_player_name - Name of opponent
#'   }
#'
#' @keywords internal
merge_duel_rows <- function(dt) {
  # Initialize opponent columns
  dt[, `:=`(
    opponent_player_id = NA_character_,
    opponent_player_name = NA_character_
  )]

  # Get next row info for duel detection (vectorized)
  dt[, `:=`(
    next_match_id = shift(match_id, 1, type = "lead"),
    next_period_id = shift(period_id, 1, type = "lead"),
    next_time = shift(time_seconds, 1, type = "lead"),
    next_action_type = shift(action_type, 1, type = "lead"),
    next_team_id_duel = shift(team_id, 1, type = "lead"),
    next_result = shift(result, 1, type = "lead"),
    next_player_id = shift(player_id, 1, type = "lead"),
    next_player_name = shift(player_name, 1, type = "lead"),
    next_start_x = shift(start_x, 1, type = "lead")
  )]

  # Calculate x_sum for location-based duel detection
  dt[, x_sum := start_x + next_start_x]

  # ===========================================================================
  # DUEL DETECTION: Same event recorded from both team perspectives
  #
  # Type 1: Same action type (Aerial vs Aerial)
  # Type 2: Cross-type duels where x_sum ~= 100:
  #   - Take On (fail) + Tackle (success) = defender won dribble duel
  #   - Dispossessed + Tackle = defender won possession duel
  #   - Foul + Foul = same foul from both perspectives
  # ===========================================================================

  # Same-type duels (Aerial vs Aerial, Tackle vs Tackle)
  same_type_duels <- c("aerial", "tackle")
  dt[, is_same_type_duel := action_type %in% same_type_duels &
       action_type == next_action_type &
       match_id == next_match_id &
       period_id == next_period_id &
       time_seconds == next_time &
       team_id != next_team_id_duel]

  # Cross-type duels (detected by x_sum ~= 100, same time +/-1sec, different teams)
  # Take On/Dispossessed + Tackle
  # Allow 1 second time tolerance because Opta sometimes records them at slightly different times
  dt[, is_cross_type_duel :=
       match_id == next_match_id &
       period_id == next_period_id &
       abs(time_seconds - next_time) <= 1 &  # Same time +/-1 second
       team_id != next_team_id_duel &
       abs(x_sum - 100) < 2 &  # Same location (opposite perspectives)
       ((action_type %in% c("take_on", "dispossessed") & next_action_type == "tackle") |
        (action_type == "foul" & next_action_type == "foul"))]

  # Combined duel detection
  dt[, is_duel_pair := is_same_type_duel | is_cross_type_duel]

  # Mark which row in pair is winner vs loser
  # Winner: result == "success", Loser: result == "fail"
  # For cross-type duels (Take On + Tackle): Tackle winner is always the "success" side
  dt[, `:=`(
    is_winner = is_duel_pair & (result == "success" | next_result != "success"),
    is_loser = FALSE  # Will mark losers separately
  )]

  # Mark loser rows (next row after a winner)
  loser_idx <- which(dt$is_duel_pair & dt$is_winner) + 1
  loser_idx <- loser_idx[loser_idx <= nrow(dt)]  # Bounds check
  if (length(loser_idx) > 0) {
    dt$is_loser[loser_idx] <- TRUE
  }

  # Handle case where first row is loser (result == "fail" & next is "success")
  first_is_loser <- which(dt$is_duel_pair & dt$result == "fail" & dt$next_result == "success")
  if (length(first_is_loser) > 0) {
    # Swap: mark first as loser, second as winner
    dt$is_loser[first_is_loser] <- TRUE
    dt$is_winner[first_is_loser] <- FALSE
    winner_after <- first_is_loser + 1
    winner_after <- winner_after[winner_after <= nrow(dt)]
    if (length(winner_after) > 0) {
      dt$is_winner[winner_after] <- TRUE
      dt$is_loser[winner_after] <- FALSE
    }
  }

  # For cross-type duels where both might have same outcome, prefer tackle
  # (Take On fail + Tackle fail -> keep Tackle as it's the defensive action)
  cross_same_outcome <- which(dt$is_cross_type_duel &
                               dt$action_type %in% c("take_on", "dispossessed") &
                               dt$next_action_type == "tackle" &
                               dt$result == dt$next_result)
  if (length(cross_same_outcome) > 0) {
    dt$is_loser[cross_same_outcome] <- TRUE
    dt$is_winner[cross_same_outcome] <- FALSE
    tackle_rows <- cross_same_outcome + 1
    tackle_rows <- tackle_rows[tackle_rows <= nrow(dt)]
    if (length(tackle_rows) > 0) {
      dt$is_winner[tackle_rows] <- TRUE
      dt$is_loser[tackle_rows] <- FALSE
    }
  }

  # Add opponent info to winners from losers
  # For winners where next row is the loser
  winner_with_loser_next <- which(dt$is_winner & c(dt$is_loser[-1], FALSE))
  if (length(winner_with_loser_next) > 0) {
    dt$opponent_player_id[winner_with_loser_next] <- dt$player_id[winner_with_loser_next + 1]
    dt$opponent_player_name[winner_with_loser_next] <- dt$player_name[winner_with_loser_next + 1]
  }

  # For winners where prev row is the loser (when first row was loser)
  winner_with_loser_prev <- which(dt$is_winner & c(FALSE, dt$is_loser[-nrow(dt)]))
  if (length(winner_with_loser_prev) > 0) {
    dt$opponent_player_id[winner_with_loser_prev] <- dt$player_id[winner_with_loser_prev - 1]
    dt$opponent_player_name[winner_with_loser_prev] <- dt$player_name[winner_with_loser_prev - 1]
  }

  # Count removals before filtering
  n_remove <- sum(dt$is_loser, na.rm = TRUE)

  # Remove loser rows
  dt <- dt[is_loser == FALSE | is.na(is_loser)]

  # Cleanup helper columns
  helper_cols <- c("next_match_id", "next_period_id", "next_time",
                   "next_action_type", "next_team_id_duel", "next_result",
                   "next_player_id", "next_player_name", "next_start_x",
                   "x_sum", "is_same_type_duel", "is_cross_type_duel",
                   "is_duel_pair", "is_winner", "is_loser")
  dt[, (helper_cols) := NULL]

  if (n_remove > 0) {
    cli::cli_alert_info("Merged {n_remove} duplicate duel rows")

    # Re-sequence action_id after deletions
    data.table::setorder(dt, match_id, period_id, time_seconds)
    dt[, action_id := seq_len(.N), by = match_id]
  }

  dt
}
