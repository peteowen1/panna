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
#' @family constants
#' @export
#' @examples
#' MINUTES_PER_MATCH
MINUTES_PER_MATCH <- 90L

#' Players per team in standard lineup
#'
#' Number of players per team in a standard football lineup.
#'
#' @format Integer value: 11
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
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
#' @family constants
#' @export
#' @examples
#' SIX_YARD_X_MIN
SIX_YARD_X_MIN <- 94

#' Six-yard box y lower bound
#'
#' Lower y boundary of the six-yard box on a 0-100 pitch.
#'
#' @format Numeric value: 37
#' @family constants
#' @export
#' @examples
#' SIX_YARD_Y_MIN
SIX_YARD_Y_MIN <- 37

#' Six-yard box y upper bound
#'
#' Upper y boundary of the six-yard box on a 0-100 pitch.
#'
#' @format Numeric value: 63
#' @family constants
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
#' xG override for penalty kicks, applied in `add_xg_to_spadl()` to shots flagged
#' `is_penalty` (Opta qualifier 9). The xG model is trained with penalties
#' excluded (`exclude_penalties = TRUE`), so without this override a penalty
#' scores like a contested ~12m open-play shot (~0.23). Empirical: ENG 2021-24 =
#' 251/306 = 0.82 (thin, seasonal range 0.74-0.90); long-run top-flight ~0.78.
#' 0.80 is a robust central value.
#'
#' @format Numeric value: 0.80
#' @family constants
#' @export
#' @examples
#' PENALTY_XG
PENALTY_XG <- 0.80

#' Empirical penalty-shootout conversion rate
#'
#' Per-kick conversion probability in a penalty shootout, measured from local
#' Opta data: 900 goals / 1200 shootout kicks = 0.75 across 116 shootouts
#' (cross-validates the literature consensus of ~0.75-0.76). Distinct from
#' \code{PENALTY_XG} (in-run penalty xG): shootout kicks are a different,
#' higher-pressure context, even though the rates happen to be close. Default
#' conversion rate for \code{\link{shootout_win_prob}}.
#'
#' @format Numeric value: 0.75
#' @family constants
#' @export
#' @examples
#' PENALTY_SHOOTOUT_CONVERSION
PENALTY_SHOOTOUT_CONVERSION <- 0.75


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
#' Raised from 30 -> 50 on 2026-04-18: with second-precision splint creation
#' and 5-min boundary-merge minimum, the per-splint zero-xG baseline rose
#' (shorter splints naturally have fewer shots). 50% only catches genuine
#' bad-data league-seasons rather than penalising fine-grained splits.
#'
#' @format Integer value: 50
#' @keywords internal
ZERO_XG_THRESHOLD_OPTA <- 50L

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

#' Goalkeeper PSR scale correction (panna#202)
#'
#' Multiplier putting goalkeeper PSR on the same goals-per-90 footing as
#' outfield PSR. The two sub-models are trained on different targets --
#' outfield on xG difference, keepers always on goal difference
#' (\code{psr.R}, \code{compute_player_psr()}) -- so their outputs are not
#' directly comparable even though they share the name "PSR", get summed into
#' team totals, and are ranked in one leaderboard.
#'
#' Derived from a leak-free calibration: each player-match was assigned the
#' last weekly PSR snapshot STRICTLY BEFORE that match (rolling as-of join on
#' \code{opta_psr_weekly.parquet}), minute-weighted and summed per position
#' group, then regressed as own-minus-opponent differences on actual goal
#' difference. Goalkeepers came out at 0.70 of their own outfield mean slope
#' (GK 1.323 vs outfield 1.877, n = 54,328 team-matches) -- i.e. a unit of
#' keeper PSR buys ~30\% less goal difference than a unit of outfield PSR.
#'
#' \strong{CONDITIONAL on the outfield coefficient set.} The GK coefficients are
#' identical across sets, but this ratio is not, because the outfield regressors
#' it is measured against change. Same out-of-sample harness (prior-season
#' rating -> next season's matches, n = 34,304 team-matches):
#'
#' \tabular{lrrr}{
#'   outfield set \tab GK slope \tab outfield mean \tab ratio \cr
#'   xg    \tab 0.877 \tab 1.158 \tab 0.757 \cr
#'   blend \tab 0.641 \tab 1.271 \tab 0.504 \cr
#'   goals \tab 0.527 \tab 1.211 \tab 0.435 \cr
#' }
#'
#' 0.50 matches \code{compute_player_psr()}'s \code{"blend"} default (panna#214).
#' If that default changes again this constant MUST move with it -- carrying the
#' xG-era 0.72 into a blend build would over-credit keepers by ~1.4x.
#'
#' Robustness, measured on the xG set where both routes exist: the weekly
#' point-in-time harness gave 0.70 and this prior-season harness 0.757, so about
#' +/-0.06 of method variance. That xG estimate was tight to the minutes floor
#' (0.705-0.746 across floors 0/3/5/8) and unanimous in direction (every slice
#' below 1.0), but moved by era (2019-2022 0.828 vs 2023-2026 0.641) and league
#' tier (Big-5 0.639 vs non-Big-5 0.771). Treat as a central estimate, not a
#' precise constant, and re-derive when the GK coefficients are retrained
#' (\code{07_train_psr_model.R}) or the outfield target changes.
#'
#' NOTE the earlier seasonal-PSR version of this test gave the OPPOSITE sign
#' (keepers appeared under-weighted). That was leakage: a season rating
#' contains the matches being predicted, and keepers play every minute, so
#' their season figure is the most contaminated of any position. Only the
#' point-in-time result is trustworthy.
#'
#' \strong{Residual gap:} \code{\link{compute_player_psv}} has the IDENTICAL
#' goal-vs-xG target split for keepers (\code{.score(dt[is_gk], "goals", "gk")})
#' and is deliberately NOT corrected here. PSV and PSR are both consumed by
#' \code{piero_value} and the game-logs export, so until PSV is addressed the
#' two metrics disagree about keepers by roughly this factor. Tracked
#' separately -- do not assume both are fixed.
#'
#' @format Numeric value: 0.50
#' @keywords internal
GK_PSR_GOAL_SCALE <- 0.50

#' Minimum weighted 90s to qualify for a PSR leaderboard (panna#215)
#'
#' PSR rankings apply no minutes floor, so short partial seasons sit alongside
#' full campaigns: **23 of the outfield top 100 have fewer than 15 weighted
#' nineties and 11 fewer than 10** — F. Totti at 7.0, M. Pinilla at 5.5,
#' J. Pastore at 9.7. This was the dominant driver of leaderboard noise,
#' bigger than the cross-league offsets it is often blamed on (weak leagues are
#' actually *under*-represented in the top 100: 91 of 100 are Big-5).
#'
#' 15 removes every sub-threshold entry currently in the outfield top 100; 10
#' would remove 11 of 23. Chosen at 15 as roughly half a domestic season, the
#' conventional qualification bar.
#'
#' This is a DISPLAY threshold, not a rating filter — a low-minutes rating is
#' still the best estimate available for that player, it is just too noisy to
#' rank against a full season. Use \code{\link{psr_leaderboard_eligible}} and
#' keep the rows.
#'
#' @format Numeric value: 15
#' @family constants
#' @export
#' @examples
#' MIN_90S_PSR_LEADERBOARD
MIN_90S_PSR_LEADERBOARD <- 15


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
#' @family epr
#' @export
PANNA_EPR_WEIGHT <- 0.5

#' PSR weight in combined Panna Value rating
#'
#' @format Numeric value: 0.5
#' @family epr
#' @export
PANNA_PSR_WEIGHT <- 0.5

#' Reliability-shrunk PSV display scale ("expected GD contribution per 90")
#'
#' Multiplies the lambda-priced display PSV (\code{calculate_psv(reliability =
#' )}) so its units become "expected goal-difference contribution per 90".
#' Re-derived 2026-08-14 (was 5.134) by regressing match goal difference on
#' minutes-weighted team sums of lambda-priced player PSV: 13,548 matches,
#' R-squared = 0.156, t(c_outfield) = 40.2 -- the slope makes summed player
#' PSVs predict match GD with slope 1.
#'
#' The 0.31 quoted in 07d's header is a hardcoded reading from the 2026-07-20
#' run, and the coefficients were retrained THREE times after it while nothing
#' re-derived this constant. Measured by re-running 07d against each vintage
#' with everything else held identical (same matches, same
#' \code{psv_match_reliability.csv} -- unchanged since ffa549f -- same
#' SPLIT_DATE, same n = 13,548):
#'
#' \preformatted{
#'   coefficients                                c_outfield   R^2     t
#'   2026-07-20 (pre f9c7e31/bd34465)                 5.134   0.31   ~59
#'   bd34465 (last pre-join-fix retrain)              4.888   0.142  37.2
#'   7b34f51 (post join fix)                          2.717   0.156  40.2
#' }
#'
#' (The middle row was measured from 86d3e9e's tree, which is not itself a
#' retrain -- it carries bd34465's coefficients unchanged, since nothing
#' between them touched inst/extdata. The vintage is bd34465's.)
#'
#' So the join fix moved fit quality UP (0.142 -> 0.156, t 37.2 -> 40.2), which
#' is the expected direction for a data-bug fix; it cut the SLOPE by ~44%
#' (4.888 -> 2.717) because the corrected PSR effects are ~3x larger, so summed
#' PSVs need a smaller multiplier to reach the same GD. The 54% relative R-squared drop happened in
#' the 2026-07-21 retrains -- most plausibly f9c7e31, which removed the zonal
#' finishing features that had been supplying large and partly spurious
#' variance -- and went unnoticed for 3.5 weeks. That also means 5.134 was
#' stale from 2026-07-21 (it should have been ~4.888) independently of the
#' join bug: BOTH July retrains changed the coefficients this constant is fit
#' against, and neither re-ran 07d.
#'
#' ONE global constant for BOTH the outfield and GK populations: the
#' GK-specific GD coefficient (c = 0.305 in this fit) is again REJECTED as
#' pricing the #159 team-context leak in the GK reliability artifact, not
#' genuine keeper skill -- GKs use this same constant until #159 retrains.
#'
#' Re-derive via \code{data-raw/estimated-skills/07d_derive_psv_gd_scale.R}
#' after any retrain of \code{psv_match_reliability.csv} (07b) or the PSR/PSV
#' coefficients (07_train_psr_model.R), then update this value by hand -- the
#' script writes no file, it only prints the number. Note 07b reads neither the
#' coefficients nor this constant, so a coefficient-only retrain does not
#' require re-running it.
#'
#' @format Numeric value: 2.717
#' @family psr
#' @export
PSV_RELIABILITY_GD_SCALE <- 2.717


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

#' Regulation seconds (90 minutes)
#'
#' Duration of regulation time in seconds. Win-probability time features use
#' this denominator for matches that did NOT reach extra time.
#'
#' @format Numeric value: 5400
#' @keywords internal
REGULATION_SECONDS <- 5400

#' Extra-time seconds (120 minutes)
#'
#' Duration including two 15-minute extra-time periods, in seconds. WP time
#' features use this denominator only for matches that actually reached extra
#' time — a fixed 5400 cap clamps every ET action to time_remaining == 0,
#' telling the model the match is over for the full 30 min of ET and inflating
#' per-event WPA in knockout matches.
#'
#' @format Numeric value: 7200
#' @keywords internal
EXTRA_TIME_SECONDS <- 7200

#' Opta match period identifiers
#'
#' Opta F24 `period_id`: 1 = first half, 2 = second half (regulation);
#' 3 = first half extra time, 4 = second half extra time; >= 5 = penalty
#' shootout. Confirmed against UCL 2025-2026 PSG-Arsenal (match
#' 6sb5ga83yrll15624x1z0gwt0, 2026-05-30): the minute clock runs continuously
#' across periods (ET actions are minute 90-120, not reset), and shootout
#' kicks are stamped at minute 120 under period_id 5.
#'
#' @format Integer vectors
#' @keywords internal
OPTA_REGULATION_PERIODS <- c(1L, 2L)
#' @rdname OPTA_REGULATION_PERIODS
#' @keywords internal
OPTA_EXTRA_TIME_PERIODS <- c(3L, 4L)

#' Test whether period_id values are penalty-shootout periods
#'
#' Shootout kicks are recorded as goals (\code{type_id == 16}) at minute 120 but
#' are not open play: they must be excluded from match scores, SPADL, EPV and
#' WPA. A match decided on penalties is a draw in open play (its WP label is
#' 0.5). Any \code{period_id >= 5} is treated as shootout — no legitimate
#' open-play period exceeds 4 (covers the standard 5 and a stray 16 some feeds
#' emit).
#'
#' @param period_id Integer vector of Opta period identifiers.
#' @return Logical vector, \code{TRUE} where the period is a shootout period.
#' @family penalty shootouts
#' @export
is_shootout_period <- function(period_id) {
  !is.na(period_id) & period_id >= 5L
}


# =============================================================================
# EPR (Expected Possession Rating) Constants
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
#' model -- the two behave very differently (international prediction leans on
#' Elo + recent form; club prediction leans on squad player-ratings).
#' Any competition NOT in this list is treated as international.
#'
#' BUG-FIX 2026-05-28: previously this list contained "EPL" (the Opta-side
#' competition name) but the rest as panna short codes ("ESP", "ITA", ...).
#' Since the predictions pipeline passes SHORT CODES through (its `leagues`
#' vector is "ENG", "ESP", ...), match_is_international("ENG") was returning
#' TRUE -- i.e., the entire English Premier League was being trained on the
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
  # Americas / Asia domestic
  "MLS", "MEX", "ARG", "SAU",
  # Continental club competitions
  "UCL", "UEL", "UECL", "CAFCL",
  "LIB", "SUD", "CCC", "LGC", "ACLE", "CWC"
)

#' Classify competitions as international vs domestic
#'
#' @param league Character vector of competition codes.
#' @return Logical vector -- \code{TRUE} for international (national-team)
#'   competitions, \code{FALSE} for domestic club competitions.
#' @family world cup simulation
#' @export
match_is_international <- function(league) {
  !(league %in% MATCH_CLUB_LEAGUES)
}

#' Canonical "rating/display" league set (shared across pipelines)
#'
#' Single source of truth for the competitions the pipelines RATE and DISPLAY,
#' so the EPV/xMetrics (step 03), skills/PSR, RAPM/panna and blog (10b) pipelines
#' cannot silently drift apart (the 2026-06 audit found four different lists).
#' Grouped by season-label convention because 10b resolves labels per group
#' ("YYYY-YYYY" domestic vs calendar "YYYY" vs tournament "YYYY Country").
#' \code{PANNA_RATING_LEAGUES} is the flat union (25 comps). Bridge comps live
#' in \code{PANNA_BRIDGE_LEAGUES} (offset/RAPM connectivity only, never displayed)
#' and are added ON TOP in step 03 / RAPM.
#' @keywords internal
PANNA_LEAGUE_GROUPS <- list(
  domestic    = c("ENG", "ESP", "GER", "ITA", "FRA", "NED", "POR", "SCO",
                  "TUR", "ENG2", "BEL", "MEX", "SAU", "AUS"),
  calendar    = c("MLS", "ARG", "BRA"),
  continental = c("UCL", "UEL", "UECL", "CAFCL"),
  intl        = c("WC", "EURO", "AFCON", "Copa_America")
)

#' @rdname PANNA_LEAGUE_GROUPS
#' @keywords internal
PANNA_RATING_LEAGUES <- unlist(PANNA_LEAGUE_GROUPS, use.names = FALSE)

#' @rdname PANNA_LEAGUE_GROUPS
#' @keywords internal
PANNA_BRIDGE_LEAGUES <- c("LIB", "SUD", "CCC", "LGC", "ACLE", "CWC")

#' Domestic-only competitions (for league-offset attribution)
#'
#' The subset of \code{PANNA_RATING_LEAGUES} that is a domestic league. Used
#' when attributing a player-season to "the league he plays in", which must be
#' a domestic competition -- a cross-league cup is where leagues MEET, not a
#' league a player belongs to.
#'
#' Without this restriction the max-minutes rule assigns a continental
#' competition as a player's league whenever his domestic one is absent from
#' the rated set: measured at **19.1\% of player-seasons** (24,613 of 128,589)
#' -- UEL 9,062, Conference 6,770, CAF_CL 3,438, UCL 2,724. Those players
#' (typically from unrated leagues such as Norway, Czechia or Japan appearing
#' only in Europe) were priced with the UEL/UCL offset rather than anything
#' reflecting their actual domestic standard. Adding
#' \code{PANNA_BRIDGE_LEAGUES} to the skills pipeline would extend the same
#' problem to South American and Asian players.
#'
#' @format Character vector of domestic competition codes
#' @keywords internal
PANNA_DOMESTIC_LEAGUES <- c(PANNA_LEAGUE_GROUPS$domestic,
                             PANNA_LEAGUE_GROUPS$calendar)

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
# literal -- which would turn off the WC override / empty the blog parquet
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
#' one name variant for these teams ("USA" vs "United States" -- see the
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
