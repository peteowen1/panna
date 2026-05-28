# Constants for panna package
#
# Centralizes magic numbers and default values used throughout the package.
# Import these constants instead of using hard-coded values.

# =============================================================================
# Match Structure Constants
# =============================================================================

#' Minutes per regulation match
#'
#' Standard duration of a regulation football match (90 minutes).
#' Used for per-90 rate calculations.
#'
#' @format Integer value: 90
#' @export
#' @examples
#' MINUTES_PER_MATCH
MINUTES_PER_MATCH <- 90L

#' Players per team in standard lineup
#'
#' Number of players per team in a standard football lineup.
#'
#' @format Integer value: 11
#' @export
#' @examples
#' PLAYERS_PER_TEAM
PLAYERS_PER_TEAM <- 11L

#' Minute marking halftime (end of first half)
#'
#' The minute at which the first half ends. Used for detecting
#' first-half stoppage time events.
#'
#' @format Integer value: 45
#' @export
#' @examples
#' HALFTIME_MINUTE
HALFTIME_MINUTE <- 45L


# =============================================================================
# Model Default Parameters
# =============================================================================

#' Default minimum minutes for RAPM matrix inclusion
#'
#' Players must have at least this many total minutes to be included
#' in the RAPM design matrix as individual players. Players below this
#' threshold are grouped into a "replacement" pool.
#'
#' @format Integer value: 90
#' @export
#' @examples
#' MIN_MINUTES_RAPM
MIN_MINUTES_RAPM <- 90L

#' Default minimum minutes for SPM/player stats functions
#'
#' Minimum minutes threshold used by player stats aggregation functions
#' and SPM model training. Higher than RAPM threshold because box score
#' stats need more sample size for stability.
#'
#' @format Integer value: 450
#' @export
#' @examples
#' MIN_MINUTES_SPM
MIN_MINUTES_SPM <- 450L

#' Default minimum minutes for player feature matrix
#'
#' Minimum minutes for inclusion in player feature matrices used
#' for SPM prediction and Panna rating calculation.
#'
#' @format Integer value: 180
#' @export
#' @examples
#' MIN_MINUTES_FEATURES
MIN_MINUTES_FEATURES <- 180L

#' Default minimum games for Bayesian padding full weight
#'
#' Number of games required for a player's statistics to receive full weight
#' in Bayesian padding calculations. Players with fewer games are regressed
#' toward population mean.
#'
#' @format Integer value: 10
#' @export
#' @examples
#' MIN_GAMES_FOR_PADDING
MIN_GAMES_FOR_PADDING <- 10L

#' Default minimum shots for finishing modifier calculation
#'
#' Minimum shots required for a player to have a meaningful finishing
#' modifier calculated. Players with fewer shots get default modifier.
#'
#' @format Integer value: 20
#' @export
#' @examples
#' MIN_SHOTS_FOR_FINISHING
MIN_SHOTS_FOR_FINISHING <- 20L

#' Minimum weight threshold for duration-based weighting
#'
#' Floor value for weights to prevent division by very small numbers
#' in RAPM weighting. Splints are weighted by minutes/90, with this
#' as the minimum.
#'
#' @format Numeric value: 0.01
#' @export
#' @examples
#' MIN_WEIGHT_DURATION
MIN_WEIGHT_DURATION <- 0.01


# =============================================================================
# Statistical Defaults
# =============================================================================
# Used for Bayesian shrinkage and regularization

#' Beta prior alpha for finishing modifier (shrinkage toward 1.0)
#'
#' Pseudocount added to both goals and xG when calculating finishing modifier.
#' Formula: (goals + BETA_PRIOR_ALPHA) / (xG + BETA_PRIOR_ALPHA)
#' This shrinks extreme values toward 1.0.
#'
#' @format Integer value: 5
#' @export
#' @examples
#' BETA_PRIOR_ALPHA
BETA_PRIOR_ALPHA <- 5L

#' Default confidence level for statistical intervals
#'
#' Standard confidence level (95%) for confidence intervals and
#' hypothesis tests throughout the package.
#'
#' @format Numeric value: 0.95
#' @export
#' @examples
#' CONFIDENCE_LEVEL
CONFIDENCE_LEVEL <- 0.95


# =============================================================================
# Sequence Estimation Constants
# =============================================================================
# Used for per-100-sequences rate calculations

#' Average touches per possession sequence (approximation)
#'
#' Rough approximation used to estimate the number of possession sequences
#' from total touches. Used in per-100-sequences rate calculations.
#'
#' @format Integer value: 5
#' @export
#' @examples
#' TOUCHES_PER_SEQUENCE
TOUCHES_PER_SEQUENCE <- 5L

#' Minimum estimated sequences per team per match
#'
#' Lower bound for sequence estimation. A team will have at least
#' this many sequences per match regardless of touch count.
#'
#' @format Integer value: 20
#' @export
#' @examples
#' MIN_SEQUENCES_PER_MATCH
MIN_SEQUENCES_PER_MATCH <- 20L


# =============================================================================
# xG Model Bounds
# =============================================================================
# Bounds for xG predictions to prevent extreme values

#' Minimum xG value (prevents 0 in log calculations)
#'
#' Floor for xG predictions to prevent issues with log calculations
#' and overly confident predictions of 0 probability.
#'
#' @format Numeric value: 0.01
#' @export
#' @examples
#' XG_MIN
XG_MIN <- 0.01

#' Maximum xG value (caps extreme predictions)
#'
#' Ceiling for xG predictions to prevent overconfident predictions.
#' Even penalty kicks and open-net chances rarely exceed this threshold.
#'
#' @format Numeric value: 0.75
#' @export
#' @examples
#' XG_MAX
XG_MAX <- 0.75


# =============================================================================
# Possession Chain Constants
# =============================================================================

#' Time gap threshold for chain breaks (seconds)
#'
#' Maximum gap in seconds between consecutive actions before
#' a new possession chain is started.
#'
#' @format Integer value: 30
#' @export
#' @examples
#' CHAIN_TIME_GAP_SECONDS
CHAIN_TIME_GAP_SECONDS <- 30L


# =============================================================================
# Pitch Coordinate Constants (0-100 scale)
# =============================================================================
# Boundaries for pitch zones on a normalised 0-100 coordinate system

#' Six-yard box x threshold (attacking end)
#'
#' Minimum x coordinate for the attacking six-yard box on a 0-100 pitch.
#'
#' @format Numeric value: 94
#' @export
#' @examples
#' SIX_YARD_X_MIN
SIX_YARD_X_MIN <- 94

#' Six-yard box y lower bound
#'
#' Lower y boundary of the six-yard box on a 0-100 pitch.
#'
#' @format Numeric value: 37
#' @export
#' @examples
#' SIX_YARD_Y_MIN
SIX_YARD_Y_MIN <- 37

#' Six-yard box y upper bound
#'
#' Upper y boundary of the six-yard box on a 0-100 pitch.
#'
#' @format Numeric value: 63
#' @export
#' @examples
#' SIX_YARD_Y_MAX
SIX_YARD_Y_MAX <- 63

# =============================================================================
# EPV Credit Assignment Parameters
# =============================================================================
# Tuning parameters for how EPV deltas are split between actors.
# Used in assign_epv_credit().

#' Base share for passer in successful pass credit split
#'
#' When a pass gains EPV, this is the passer's base share.
#' Adjusted upward by difficulty: passer_share = base + adjustment * (1 - xpass).
#'
#' @format Numeric value: 0.5
#' @keywords internal
EPV_BASE_PASSER_SHARE <- 0.5

#' Difficulty adjustment for pass credit/blame
#'
#' Scales passer credit/blame by pass difficulty (1 - xpass for credit,
#' xpass for blame). Higher values give more credit for hard passes.
#'
#' @format Numeric value: 0.3
#' @keywords internal
EPV_PASS_DIFFICULTY_ADJUSTMENT <- 0.3

#' Blame share for turnovers, saved shots, and duels
#'
#' When a turnover or duel results in negative EPV delta, this fraction
#' of blame goes to the actor (the rest to the receiver who gains).
#'
#' @format Numeric value: 0.5
#' @keywords internal
EPV_TURNOVER_BLAME_SHARE <- 0.5

#' Defensive action credit boost multiplier
#'
#' Multiplier applied to defensive actions (clearance, interception,
#' tackle, ball_recovery) to better capture their value. EPV deltas
#' underestimate defensive contributions.
#'
#' @format Numeric value: 1.5
#' @keywords internal
EPV_DEFENSIVE_BOOST <- 1.5

#' Minimum position scale for successful passes
#'
#' At x=0 (own goal), successful pass credit is scaled by this factor.
#' Ramps linearly to 1.0 at EPV_POSITION_RAMP_X.
#'
#' @format Numeric value: 0.3
#' @keywords internal
EPV_POSITION_SCALE_MIN <- 0.3

#' X-coordinate where position scaling reaches 1.0
#'
#' Passes from beyond this x-coordinate get full credit.
#' Below this, credit is scaled down toward EPV_POSITION_SCALE_MIN.
#'
#' @format Numeric value: 40
#' @keywords internal
EPV_POSITION_RAMP_X <- 40


#' Opponent adjustment: exponential decay rate
#'
#' Controls how quickly past matches lose influence in opponent profiling.
#' lambda = 0.003 gives ~231-day half-life, suitable for within-season use.
#'
#' @format Numeric value: 0.003
#' @keywords internal
EPV_OPP_LAMBDA_DECAY <- 0.003

#' Opponent adjustment: prior games for shrinkage
#'
#' Number of pseudo-games at league average for Bayesian shrinkage of
#' opponent profiles. Lower = faster response, higher = more stable.
#' 2 is appropriate for within-season single-league data.
#'
#' @format Numeric value: 2
#' @keywords internal
EPV_OPP_PRIOR_GAMES <- 2


#' Default penalty kick xG value
#'
#' xG override for penalty kicks. Based on historical penalty
#' conversion rates (~76%). Used when xG model is trained without
#' penalties.
#'
#' @format Numeric value: 0.76
#' @export
#' @examples
#' PENALTY_XG
PENALTY_XG <- 0.76


# =============================================================================
# Chain Analytics Constants
# =============================================================================

#' Final third x-coordinate threshold
#'
#' Actions with start_x greater than this value are considered
#' to be in the attacking final third (0-100 pitch scale).
#'
#' @format Numeric value: 66
#' @keywords internal
CHAIN_FINAL_THIRD_X <- 66

#' Progressive chain distance threshold
#'
#' Minimum forward x-distance (in 0-100 units) a player must move
#' the ball within a chain to count as a "progressive" chain contribution.
#'
#' @format Numeric value: 25
#' @keywords internal
CHAIN_PROGRESSIVE_THRESHOLD <- 25


# =============================================================================
# Pipeline Data Quality Constants
# =============================================================================

#' Zero-xG threshold for Opta pipeline data quality filter
#'
#' Maximum percentage of zero-xG splints allowed before a match is flagged
#' as bad data. Opta data via SPADL conversion naturally has ~25% zero-xG
#' splints, so the threshold is set higher than FBref.
#' Used in \code{filter_bad_xg_data()}.
#' Raised from 30 → 50 on 2026-04-18: with second-precision splint creation
#' and 5-min boundary-merge minimum, the per-splint zero-xG baseline rose
#' (shorter splints naturally have fewer shots). 50% only catches genuine
#' bad-data league-seasons rather than penalising fine-grained splits.
#'
#' @format Integer value: 50
#' @keywords internal
ZERO_XG_THRESHOLD_OPTA <- 50L

#' Zero-xG threshold for FBref pipeline data quality filter (DEPRECATED)
#'
#' Maximum percentage of zero-xG splints allowed before a match is flagged.
#' FBref data has fewer zero-xG splints than Opta, so threshold is lower.
#' Kept for backward compatibility — FBref pipeline archived 2026-04-18,
#' Opta is the active data source.
#'
#' @format Integer value: 20
#' @keywords internal
ZERO_XG_THRESHOLD_FBREF <- 20L

#' Minimum minutes for RAPM model fitting
#'
#' Minimum total minutes a player needs across all matches to be included
#' in RAPM model fitting. Higher than \code{MIN_MINUTES_RAPM} (design matrix
#' inclusion), because the model fit needs more data per player.
#'
#' @format Integer value: 200
#' @keywords internal
MIN_MINUTES_RAPM_FIT <- 200L

#' SPM model blend weight (glmnet vs XGBoost)
#'
#' Weight given to the glmnet (elastic net) model in the SPM blend.
#' The remaining \code{1 - SPM_BLEND_WEIGHT_GLMNET} goes to XGBoost.
#'
#' @format Numeric value: 0.5
#' @keywords internal
SPM_BLEND_WEIGHT_GLMNET <- 0.5


# =============================================================================
# Cache Path Constants
# =============================================================================

#' Default SPADL cache directory
#'
#' Relative path (from panna/ root) to the SPADL conversion cache shared
#' between the EPV/xMetrics pipeline and the Opta RAPM pipeline.
#'
#' @format Character value
#' @keywords internal
SPADL_CACHE_DIR <- "data-raw/cache/epv/spadl"


# =============================================================================
# Panna Value Blend Constants
# =============================================================================

#' EPR weight in combined Panna Value rating
#'
#' Fraction of the combined rating attributed to EPR (play-by-play EPV-based).
#' The remaining \code{1 - PANNA_EPR_WEIGHT} goes to PSR (stat-based).
#' Analogous to torpverse's \code{TORP_EPR_WEIGHT = 0.5}.
#'
#' @format Numeric value: 0.5
#' @export
PANNA_EPR_WEIGHT <- 0.5

#' PSR weight in combined Panna Value rating
#'
#' @format Numeric value: 0.5
#' @export
PANNA_PSR_WEIGHT <- 0.5


# =============================================================================
# Win Probability / WPA Constants
# =============================================================================

#' WP draw value
#'
#' A draw is worth 0.5 in WP terms (1 of 3 league points).
#'
#' @format Numeric value: 0.5
#' @keywords internal
WP_DRAW_VALUE <- 0.5

#' WPA actor share
#'
#' Fraction of WPA credited to the acting player (remainder to receiver).
#'
#' @format Numeric value: 0.5
#' @keywords internal
WPA_ACTOR_SHARE <- 0.5


# =============================================================================
# EPR (Expected Points Rating) Constants
# =============================================================================

#' @keywords internal
EPR_DECAY_OFFENSIVE <- 400
#' @keywords internal
EPR_DECAY_DEFENSIVE <- 400
#' @keywords internal
EPR_PRIOR_GAMES <- 10.2
#' @keywords internal
EPR_PRIOR_RATE_OFF <- 0.20
#' @keywords internal
EPR_PRIOR_RATE_DEF <- 0.04
#' @keywords internal
EPR_LOADING <- 1.0


# =============================================================================
# Match-Prediction Model Segmentation
# =============================================================================

#' Club (domestic) competitions
#'
#' Competitions played between club teams. Used to split the match-prediction
#' models into a domestic (club) model and an international (national-team)
#' model — the two behave very differently (international prediction leans on
#' Elo + recent form; club prediction leans on squad player-ratings).
#' Any competition NOT in this list is treated as international.
#'
#' BUG-FIX 2026-05-28: previously this list contained "EPL" (the Opta-side
#' competition name) but the rest as panna short codes ("ESP", "ITA", ...).
#' Since the predictions pipeline passes SHORT CODES through (its `leagues`
#' vector is "ENG", "ESP", ...), match_is_international("ENG") was returning
#' TRUE — i.e., the entire English Premier League was being trained on the
#' international-specialist model and receiving the international prediction
#' blend. Replaced "EPL" with "ENG" and added BEL/BRA/AUS/TUN/CAFCL so any
#' future addition of those leagues to the default set classifies correctly.
#'
#' @format Character vector of panna short codes (matches what flows through
#'   fixture_results$league in step 01).
#' @keywords internal
MATCH_CLUB_LEAGUES <- c(
  # Big 5
  "ENG", "ESP", "ITA", "GER", "FRA",
  # Extended domestic
  "ENG2", "NED", "POR", "TUR", "SCO", "BEL", "BRA", "AUS", "TUN",
  # Continental club competitions
  "UCL", "UEL", "UECL", "CAFCL"
)

#' Classify competitions as international vs domestic
#'
#' @param league Character vector of competition codes.
#' @return Logical vector — \code{TRUE} for international (national-team)
#'   competitions, \code{FALSE} for domestic club competitions.
#' @export
match_is_international <- function(league) {
  !(league %in% MATCH_CLUB_LEAGUES)
}

#' International blend weight
#'
#' Weight on the international-specialist model when predicting international
#' (national-team) matches; the remainder is on the pooled (all-data) model.
#' The prediction is \code{w * international + (1 - w) * pooled}.
#'
#' A blend-weight sweep on held-out international games found accuracy improves
#' monotonically toward \code{w = 1} (pure specialist), but only by ~0.6\%.
#' The default 0.5 trades that small edge for robustness against the
#' smaller-sample specialist model misbehaving on out-of-distribution squads.
#'
#' @format Numeric value: 0.5
#' @keywords internal
MATCH_INTL_BLEND_WEIGHT <- 0.5


# =============================================================================
# WC 2026 Tournament Constants
# =============================================================================
# Centralised so a single Opta-side rename can't silently fan out into three
# files (step 02 / 02b / 04 / 11 / 12) each treating it as a separate string
# literal — which would turn off the WC override / empty the blog parquet
# without any warning.

#' League code for the WC 2026 tournament
#' @keywords internal
WC2026_LEAGUE <- "WC"

#' Season label for the WC 2026 tournament (as it appears in Opta fixtures)
#' @keywords internal
WC2026_SEASON_LABEL <- "2026 Canada-Mexico-USA"

#' Opta team_ids of the three WC 2026 hosts (USA / Canada / Mexico)
#'
#' Keyed by team_id rather than name because Opta has already served at least
#' one name variant for these teams ("USA" vs "United States" — see the
#' fixture-name normalisation block in 01_build_fixture_results.R). step 04
#' asserts all three IDs resolve in the WC2026 fixture set before flagging
#' host advantage.
#' @keywords internal
WC2026_HOST_TEAM_IDS <- c(
  USA    = "9vh2u1p4ppm597tjfahst2m3n",
  Canada = "eg7vduna0h3vis1wd47s41za7",
  Mexico = "4vofb84dzb5fyc81n2ssws6ah"
)

#' Minimum resolved announced-squad players required to apply the override
#'
#' If fewer than this many of a team's announced-squad names resolve to
#' Opta player_ids, the override is refused and the team falls back to the
#' most-recent intl XI. Prevents the silent "near-empty synthetic team"
#' failure mode where the override fires with 1-2 resolved players and the
#' EM-weighted aggregation collapses to ~zero sum_panna.
#' @keywords internal
WC2026_OVERRIDE_MIN_RESOLVED <- 11L
