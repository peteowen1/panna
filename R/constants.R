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
