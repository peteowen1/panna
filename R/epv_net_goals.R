# EPV Net Goals ledger
#
# Allocates each action's `epv_delta` to players so that, summed over a match,
# the home total minus the away total equals the goal difference.
#
# Design and evidence: docs/plans/EPV-NET-GOALS.md (measurement) and
# docs/plans/EPV-NET-GOALS-RULES.md (the rules). Method deliberately mirrors
# torpverse's net points -- see torpverse/docs/plans/EPV-V4-CREDIT-RULES.md.
#
# This is a NEW entry point. It does not replace assign_epv_credit() or
# aggregate_player_game_epv(); production is untouched until a gate says
# otherwise.

#' @importFrom data.table data.table as.data.table setDT setorder shift .SD .N
#'   .I := fifelse setnames rbindlist %chin% copy setkey nafill
NULL


# =============================================================================
# CONSTANTS
# =============================================================================

#' Opta types that are dropped from SPADL but still name a player who caused a
#' possession change
#'
#' `convert_opta_to_spadl()` removes `OPTA_NON_GAMEPLAY_TYPES` before
#' `calculate_action_epv()` computes its `shift(..., type = "lead")`, so the
#' ledger's idea of "who was next" steps over every dropped row. Measured on ENG
#' 2024-2025: 92,808 of 639,507 events dropped, **60,535 of them sitting exactly
#' on a possession change** (160.6 a match). Torp paid for the same defect --
#' adjacency computed after filtering silently rewrote who did what while every
#' conservation assertion stayed green.
#'
#' These nine types are the ones worth seeing. Each names a player, and each
#' sits on a possession change often enough to matter. Everything else that is
#' filtered (deleted events, substitutions, cards, period markers, formation
#' changes) is a marker with no attribution content and stays invisible.
#'
#' Counts are ENG 2024-2025, "on change" = the surviving rows either side belong
#' to different teams. **The table below is the named-player subset**: its "on
#' change" column sums to 43,241 of the 60,535, the remaining 17,294 being
#' markers (deleted events, substitutions, cards, period boundaries) that also
#' sit on a possession change but name nobody worth paying.
#'
#' | id | name              |      n | on change |
#' |----|-------------------|--------|-----------|
#' |  5 | Ball Out          | 35,860 |    30,934 |
#' |  6 | Corner Awarded    |  7,766 |     6,328 |
#' | 74 | Blocked Pass      |  5,421 |     1,844 |
#' |  2 | Offside Pass      |  1,264 |     1,027 |
#' | 55 | Offside Provoked  |  1,264 |     1,020 |
#' | 45 | Challenge         |  5,908 |       869 |
#' | 56 | Shield Ball Opp   |    462 |       431 |
#' | 51 | Error             |    634 |       413 |
#' | 59 | Keeper Sweeper    |    463 |       375 |
#'
#' @keywords internal
NG_ATTRIBUTION_TYPES <- c(
  2L,   # Offside Pass     - the defensive line won the ball
  5L,   # Ball Out         - names who conceded the throw-in
  6L,   # Corner Awarded   - names who conceded the corner
  45L,  # Challenge        - a named contest
  51L,  # Error            - a named mistake causing a turnover
  55L,  # Offside Provoked - the mirror of 2
  56L,  # Shield Ball Opp  - shielding the ball out
  59L,  # Keeper Sweeper   - a keeper ball-win outside the box
  74L   # Blocked Pass     - a named defensive ball-win
)

# Why the type ids above are trusted by number, not by comment.
#
# OPTA_NON_GAMEPLAY_TYPES and OPTA_TYPE_NAMES disagree on several ids: 51 is
# commented "Delay of Play" but names "Error"; 55 is commented "Temp Goal" but
# names "Offside Provoked". Some of that mislabelling is already flagged in
# spadl_conversion.R, some is not. The ids above were chosen from measured
# behaviour -- how often each sits on a possession change -- not from either
# label, and ng_build_adjacency() reports the names it actually sees so a future
# reader can check rather than trust.


# =============================================================================
# STEP 1 -- FULL-STREAM ADJACENCY
# =============================================================================

#' Build a full-stream adjacency table
#'
#' Answers "what happened next" from the **complete** Opta event stream, before
#' any SPADL filtering, and returns it keyed on `event_id` so the ledger can join
#' it to SPADL actions via `original_event_id`.
#'
#' Nothing about the EPV model changes: `convert_opta_to_spadl()`, the chains,
#' the features and `calculate_action_epv()` all keep seeing exactly what they
#' see today. Only the ledger's view of "who was next" is corrected. This is
#' deliberate -- moving the filter itself would shift the model's inputs and
#' every measurement taken against them.
#'
#' @param events Raw Opta events, as returned by `load_opta_match_events()`.
#'   Needs `match_id`, `event_id`, `type_id`, `team_id`, `player_id`,
#'   `period_id`, `minute`, `second`.
#' @param verbose Print a summary of what the table found. Default `TRUE`.
#'
#' @return A data.table keyed on `match_id` + `event_id`, one row per event,
#'   with:
#'   \describe{
#'     \item{next_team_id}{team on the next *visible* event (attribution types
#'       and real actions; markers are skipped)}
#'     \item{next_player_id}{player on that event}
#'     \item{next_type_id}{its Opta type id}
#'     \item{gap_type_id}{if the next visible event is one of
#'       `NG_ATTRIBUTION_TYPES`, its id -- otherwise `NA`. This is the row SPADL
#'       drops and the ledger would otherwise step over.}
#'     \item{gap_player_id}{the player named on that dropped row}
#'     \item{true_possession_change}{next visible event belongs to the other
#'       team}
#'   }
#'
#' @family net_goals
#' @export
ng_build_adjacency <- function(events, verbose = TRUE) {
  dt <- data.table::as.data.table(events)

  req <- c("match_id", "event_id", "type_id", "team_id", "player_id",
           "period_id", "minute", "second")
  missing <- setdiff(req, names(dt))
  if (length(missing)) {
    cli::cli_abort("{.fn ng_build_adjacency} needs column{?s} {.val {missing}}.")
  }

  dt <- dt[, ..req]
  dt[, tsec := as.numeric(minute) * 60 + as.numeric(second)]

  # Opta emits events in feed order; event_id breaks ties within a second.
  data.table::setorder(dt, match_id, period_id, tsec, event_id)

  # A marker carries no attribution: it is neither a real action nor one of the
  # named-player drops worth seeing. Skipping markers is what makes "the next
  # event" mean "the next thing a player did".
  dt[, is_marker := !(type_id %in% NG_ATTRIBUTION_TYPES) &
       (type_id %in% OPTA_NON_GAMEPLAY_TYPES)]
  dt[, is_attrib := type_id %in% NG_ATTRIBUTION_TYPES]

  # Walk backwards filling each row with the next NON-marker row's fields.
  # data.table's `shift` cannot skip rows, so index the visible ones and map.
  dt[, vis_idx := cumsum(!is_marker)]
  vis <- dt[is_marker == FALSE,
            .(vis_idx, v_match = match_id, v_team = team_id,
              v_player = player_id, v_type = type_id, v_period = period_id)]

  # The next visible event is vis_idx + 1 for a marker, vis_idx + 1 for a
  # visible row too (its own entry is vis_idx, so the next is +1).
  dt[, nxt_vis := vis_idx + 1L]
  dt <- merge(dt, vis, by.x = "nxt_vis", by.y = "vis_idx",
              all.x = TRUE, sort = FALSE)

  # Never look across a match or a period boundary.
  same_scope <- !is.na(dt$v_match) & dt$v_match == dt$match_id &
    dt$v_period == dt$period_id
  dt[, `:=`(
    next_team_id   = fifelse(same_scope, v_team, NA_character_),
    next_player_id = fifelse(same_scope, v_player, NA_character_),
    next_type_id   = fifelse(same_scope, v_type, NA_integer_)
  )]

  dt[, true_possession_change :=
       !is.na(next_team_id) & !is.na(team_id) & next_team_id != team_id]

  # The gap: the next visible event is a row SPADL drops. That player caused the
  # possession change and today is paid nothing, while whoever follows the gap
  # reads as the ball winner.
  is_gap <- !is.na(dt$next_type_id) & dt$next_type_id %in% NG_ATTRIBUTION_TYPES
  dt[, `:=`(
    gap_type_id   = fifelse(is_gap, next_type_id, NA_integer_),
    gap_player_id = fifelse(is_gap, next_player_id, NA_character_)
  )]

  out <- dt[, .(match_id, event_id, type_id, team_id, player_id,
                period_id, tsec, is_marker, is_attrib,
                next_team_id, next_player_id, next_type_id,
                gap_type_id, gap_player_id, true_possession_change)]
  data.table::setkey(out, match_id, event_id)

  if (isTRUE(verbose)) .ng_report_adjacency(out)

  out[]
}


#' Summarise what the adjacency table found
#'
#' Reports the names it actually saw rather than the ones the constants claim,
#' so a reader can check the ids in `NG_ATTRIBUTION_TYPES` against reality.
#'
#' @param adj Output of `ng_build_adjacency()`
#' @keywords internal
.ng_report_adjacency <- function(adj) {
  n_ev <- nrow(adj)
  n_mk <- sum(adj$is_marker)
  n_at <- sum(adj$is_attrib)
  n_gap <- sum(!is.na(adj$gap_type_id))

  cli::cli_alert_info(paste0(
    "Adjacency built over {format(n_ev, big.mark = ',')} events: ",
    "{format(n_mk, big.mark = ',')} marker{?s} skipped, ",
    "{format(n_at, big.mark = ',')} attribution row{?s} kept visible."))

  if (n_gap > 0) {
    nm <- OPTA_TYPE_NAMES[as.character(adj$gap_type_id[!is.na(adj$gap_type_id)])]
    tab <- sort(table(nm), decreasing = TRUE)
    top <- paste(sprintf("%s (%s)", names(tab),
                         format(as.integer(tab), big.mark = ",")),
                 collapse = ", ")
    cli::cli_alert_info(paste0(
      "{format(n_gap, big.mark = ',')} action{?s} followed by a dropped ",
      "named-player row: {top}"))
  }

  invisible(adj)
}


# =============================================================================
# STEP 2 -- THE LEDGER
# =============================================================================

#' Build the net goals ledger
#'
#' Turns per-action `epv_delta` into one payment row per (action, recipient), in
#' the **home-margin frame**: an away team's action is negated so a single number
#' conserves. Summed over a match, the payments equal the goal difference.
#'
#' The allocation at this stage is deliberately crude -- every row pays its whole
#' `epv_delta` to the acting player. That is obviously wrong about *who* and
#' provably right about *how much*, which makes it the one clean moment to assert
#' the identity: there is no rule for an error to hide behind, and no reconciler
#' downstream absorbing it. Torp's suite passed 48 assertions against a ledger
#' with the away sign flipped because every test ran after its reconciler; this
#' function is the assertion point that avoids repeating that.
#'
#' Difficulty terms (the decision/surprise split, D-SHOT, defensive routing)
#' arrive in step 3 and only ever *subdivide* a row's `epv_delta`. They can never
#' change a match total.
#'
#' @param spadl_with_epv Output of `calculate_action_epv()`.
#' @param adj Output of `ng_build_adjacency()`. Optional: when supplied, the
#'   ledger uses the true next actor rather than SPADL's post-filter neighbour.
#'   12.01% of actions disagree, so leaving it out is a measurably different
#'   ledger, not a convenience.
#' @param fixtures Fixtures with `match_id`, `home_team_id`, `away_team_id`.
#'   Used only to pick the home frame, never to fit anything.
#' @param allocate Apply the credit and blame rules (`TRUE`, the default) or pay
#'   every row whole to its actor (`FALSE`). The crude mode exists so the
#'   identity can be asserted with no rule for an error to hide behind.
#' @param shares Output of `ng_shares()`. Ignored when `allocate = FALSE`.
#' @param convention `"team"` (the default, Pete 2026-09-21) allocates each
#'   action twice, once per side, so **each team** sums to its own goal
#'   difference -- +2 and -2 for a 3-1 win, zero across the match. That is the
#'   ESPN Net Points convention and what torp ships as of 1.7.0.
#'   `"margin"` allocates it once, split across both sides, so the **match**
#'   sums to the goal difference and team totals float. Its pools carry 17.4% of
#'   absolute ledger value against the team convention's 39.1%, because it never
#'   books the conceding half -- which is most of what nobody is named for. Kept
#'   for comparison.
#' @param verbose Print a summary. Default `TRUE`.
#'
#' @return A data.table of payments, one row per (action, recipient):
#'   `match_id`, `action_id`, `player_id`, `team_id`, `is_home`, `role`,
#'   `play_type`, `value_home` (home-margin frame), `value_own` (the player's
#'   own frame, so positive is always good for him).
#'
#' @family net_goals
#' @export
ng_build_ledger <- function(spadl_with_epv, adj = NULL, fixtures,
                            allocate = TRUE, shares = ng_shares(),
                            convention = c("team", "margin"),
                            verbose = TRUE) {
  convention <- match.arg(convention)
  dt <- data.table::as.data.table(spadl_with_epv)
  fx <- data.table::as.data.table(fixtures)[
    , .(match_id, home_team_id, away_team_id)]

  needed <- c("match_id", "action_id", "team_id", "player_id", "action_type",
              "epv_delta")
  # `result` is only needed once the rules run, but then it is essential: a
  # turnover requires a FAILED action, and defaulting it would quietly treat
  # every action as successful and pay no defender anything.
  if (isTRUE(allocate)) needed <- c(needed, "result")
  missing <- setdiff(needed, names(dt))
  if (length(missing)) {
    cli::cli_abort("{.fn ng_build_ledger} needs column{?s} {.val {missing}}.")
  }

  dt <- merge(dt, fx, by = "match_id", all.x = TRUE)
  n_nofx <- sum(is.na(dt$home_team_id))
  if (n_nofx > 0) {
    cli::cli_warn(paste0(
      "{format(n_nofx, big.mark = ',')} action{?s} in {uniqueN(dt[is.na(home_team_id)]$match_id)} ",
      "match{?es} have no fixture row and are dropped -- their match cannot be ",
      "framed or checked against a scoreline."))
    dt <- dt[!is.na(home_team_id)]
  }

  dt[, is_home := team_id == home_team_id]

  # Attach the true next actor where the caller supplied one. `original_event_id`
  # is SPADL's link back to the raw feed.
  if (!is.null(adj)) {
    if (!"original_event_id" %in% names(dt)) {
      cli::cli_abort(paste0(
        "{.arg adj} was supplied but {.field original_event_id} is missing from ",
        "{.arg spadl_with_epv}, so actions cannot be linked back to the raw feed."))
    }
    a <- data.table::as.data.table(adj)[
      , .(match_id, event_id, true_next_team = next_team_id,
          true_next_player = next_player_id, gap_type_id, gap_player_id,
          true_possession_change)]
    dt <- merge(dt, a, by.x = c("match_id", "original_event_id"),
                by.y = c("match_id", "event_id"), all.x = TRUE)
  }

  # The home-margin frame. This single negation is what makes one number
  # conserve: summed over a match the payments equal home minus away.
  dt[, value_home := fifelse(is_home, epv_delta, -epv_delta)]
  dt[is.na(value_home), value_home := 0]

  if (isTRUE(allocate) && convention == "team") {
    pay <- .ng_credit_terms_team(dt, shares)
    .ng_assert_double_entry(dt, pay)
    pay <- merge(pay, unique(dt[, .(match_id, home_team_id)]), by = "match_id",
                 all.x = TRUE)
    pay[, is_home := team_id == home_team_id]
    pay[, value_home := fifelse(is_home, value_own, -value_own)]
    pay[, home_team_id := NULL]
    pay <- .ng_pool_unnamed(pay)
    if (isTRUE(verbose)) {
      cli::cli_alert_success(paste0(
        "Ledger (team convention): {format(nrow(pay), big.mark = ',')} payment{?s} ",
        "over {format(nrow(dt), big.mark = ',')} action{?s} in ",
        "{uniqueN(dt$match_id)} match{?es}; each action booked once to each side."))
    }
    return(pay[])
  }

  if (isTRUE(allocate)) {
    pay <- .ng_credit_terms(dt, shares)
    # The identity that makes every rule safe: a row's terms sum to its value,
    # so no rule can move a match total. Checked here rather than trusted,
    # because this is the only place it can still be checked cheaply.
    .ng_assert_row_sums(dt, pay)
  } else {
    # Step 2 allocation: the whole row to the actor. Deliberately crude about
    # WHO and provably right about HOW MUCH, which is what makes it the honest
    # place to assert the identity.
    pay <- dt[, .(match_id, action_id, player_id, team_id,
                  role = "actor", play_type = action_type, value_home)]
  }

  pay <- .ng_pool_unnamed(pay)
  pay <- merge(pay, unique(dt[, .(match_id, home_team_id)]), by = "match_id",
               all.x = TRUE)
  pay[, is_home := team_id == home_team_id]
  pay[, value_own := fifelse(is_home, value_home, -value_home)]
  pay[, home_team_id := NULL]

  if (isTRUE(verbose)) {
    n_pool <- sum(pay$role %chin% c("pool_off", "pool_def"))
    cli::cli_alert_success(paste0(
      "Ledger: {format(nrow(pay), big.mark = ',')} payment{?s} over ",
      "{format(nrow(dt), big.mark = ',')} action{?s} in ",
      "{uniqueN(dt$match_id)} match{?es}",
      if (n_pool > 0) "; {format(n_pool, big.mark = ',')} go to a team pool awaiting a spread" else ""))
  }

  pay[]
}


#' Assert that each action's payments sum to that action's value
#'
#' The one property that makes the rules safe to change: a rule may move value
#' between recipients but can never create or destroy it, so a match total is
#' untouchable by definition. Checked against the action table, upstream of any
#' aggregation or reconciliation -- torp's suite passed 48 assertions against a
#' deliberately broken ledger because every one of them ran downstream of a
#' reconciler that forced the total.
#'
#' @param dt Prepared actions with `value_home`
#' @param pay Payments from `.ng_credit_terms()`
#' @param tol Absolute tolerance per action. Default 1e-9.
#' @keywords internal
.ng_assert_row_sums <- function(dt, pay, tol = 1e-9) {
  got <- pay[, .(paid = sum(value_home, na.rm = TRUE)),
             by = .(match_id, action_id)]
  want <- dt[, .(match_id, action_id, value_home)]
  cmp <- merge(want, got, by = c("match_id", "action_id"), all.x = TRUE)
  cmp[is.na(paid), paid := 0]
  bad <- cmp[abs(paid - value_home) > tol]
  if (nrow(bad) > 0) {
    worst <- bad[which.max(abs(bad$paid - bad$value_home))]
    cli::cli_abort(c(
      "Allocation does not conserve: {format(nrow(bad), big.mark = ',')} of {format(nrow(cmp), big.mark = ',')} action{?s} pay out something other than their own value.",
      "x" = "Worst: match {worst$match_id} action {worst$action_id} is worth {round(worst$value_home, 6)} and pays {round(worst$paid, 6)}.",
      "i" = "A rule must SPLIT a row's value, never add to it."
    ))
  }
  invisible(TRUE)
}


#' Check the ledger against the scoreline
#'
#' The identity is `sum(value_home) == home_score - away_score` per match. It is
#' **approximate** in football, unlike torp's: panna's EPV asks "who scores next
#' *this half*" and the shot override swaps model EPV for xG, so the telescoping
#' terms nearly but not quite cancel. Measured on ENG 2024-2025 the median error
#' is 0.198 goals and the worst match 1.339.
#'
#' This function is deliberately a *report*, not a gate: it prints what the gap
#' is rather than forcing it to zero. Anything that forces the total would make
#' every downstream assertion vacuous.
#'
#' @param pay Output of `ng_build_ledger()`
#' @param fixtures Fixtures with `match_id`, `home_score`, `away_score`
#' @param verbose Print the summary. Default `TRUE`.
#'
#' @return A data.table, one row per match: `ledger`, `gd`, `err`.
#'
#' @family net_goals
#' @export
ng_check_conservation <- function(pay, fixtures, verbose = TRUE) {
  fx <- data.table::as.data.table(fixtures)[
    , .(match_id, home_score = as.numeric(home_score),
        away_score = as.numeric(away_score))]
  m <- data.table::as.data.table(pay)[
    , .(ledger = sum(value_home, na.rm = TRUE)), by = match_id]
  m <- merge(m, fx, by = "match_id")
  m <- m[!is.na(home_score) & !is.na(away_score)]
  m[, `:=`(gd = home_score - away_score)]
  m[, err := ledger - gd]

  if (isTRUE(verbose) && nrow(m) > 0) {
    cli::cli_alert_info(paste0(
      "Conservation over {nrow(m)} match{?es}: ",
      "cor {round(stats::cor(m$ledger, m$gd), 4)}, ",
      "slope {round(stats::coef(stats::lm(gd ~ ledger, m))[2], 4)}, ",
      "median |error| {round(stats::median(abs(m$err)), 3)} goal{?s}, ",
      "max {round(max(abs(m$err)), 3)}"))
  }

  m[]
}


# =============================================================================
# STEP 3 -- CREDIT AND BLAME
# =============================================================================

#' Default shares for the net goals allocation
#'
#' None of these is identifiable from conservation -- the identity holds for
#' every value, which is the design working correctly and also why the goal
#' difference cannot choose them. They are defaults to start a year-over-year
#' repeatability search (torp D17), never results.
#'
#' @param exec_blame Share of a failed action's value the actor keeps as
#'   execution blame. The rest is the defence's credit. Torp D7's default.
#' @param named_share Share of the defence's credit going to the player the feed
#'   names, with the remainder to the defending team pool for the pressure and
#'   shape nobody is named for.
#' @param reb_named Share of a rebound going to the stopper rather than his
#'   team, per Pete 2026-09-21: the keeper is credited for the stop and charged
#'   for parrying it back into play.
#' @param off_pool Share of retained attacking value going to the attacking team
#'   pool, for the runs and structure that created the option. Torp D9.
#'
#' @return A named list of shares.
#' @family net_goals
#' @export
ng_shares <- function(exec_blame = 0.30, named_share = 0.70,
                      reb_named = 0.40, off_pool = 0.10) {
  s <- list(exec_blame = exec_blame, named_share = named_share,
            reb_named = reb_named, off_pool = off_pool)
  bad <- vapply(s, function(x) !is.numeric(x) || length(x) != 1L ||
                  is.na(x) || x < 0 || x > 1, logical(1))
  if (any(bad)) {
    cli::cli_abort("Share{?s} {.val {names(s)[bad]}} must each be one number in [0, 1].")
  }
  s
}

#' Actions treated as stopping a shot
#'
#' SPADL has no block type: 2,802 of 5,124 `keeper_save` rows (54.7%) are
#' outfield players, which are blocks. So one action type covers both, which is
#' why extending the rule from keepers to all shot-stoppers costs nothing.
#' `keeper_claim`, `keeper_punch` and `keeper_pick_up` are deliberately absent --
#' all three already read positive, and changing something already correct is a
#' correctness note, not a fix.
#'
#' @keywords internal
NG_STOP_ACTIONS <- c("keeper_save")


#' Prepare actions for allocation
#'
#' The work both conventions share: who won the ball, whether it was a turnover
#' at all, the opposing team, and whether a stop has a shot in front of it.
#' Factored out so the two conventions cannot drift apart on the definitions
#' they both depend on.
#'
#' @param dt Prepared actions from `ng_build_ledger()`
#' @keywords internal
.ng_prepare_terms <- function(dt) {
  d <- data.table::copy(dt)
  n <- nrow(d)
  NAc <- rep(NA_character_, n)

  # Who won the ball, and was it a turnover at all. The adjacency table answers
  # both from the full event stream; SPADL's own neighbour disagrees on 12.01%
  # of actions because the filter runs before the shift.
  if (!"true_possession_change" %in% names(d)) {
    if (!"possession_change" %in% names(d)) d[, possession_change := FALSE]
    d[, true_possession_change := possession_change %in% TRUE]
    d[, true_next_player := NA_character_]
    d[, gap_player_id := NA_character_]
  }
  d[, winner_id := fifelse(!is.na(gap_player_id), gap_player_id, true_next_player)]

  # A turnover needs BOTH a possession change and a failed action. Possession
  # alone is not enough: the next visible event after a successful pass is often
  # an opponent's challenge or blocked-pass row, which reads as a change while
  # the pass in fact found its target. Charging those as turnovers pays an
  # opponent for the other side's good play -- caught on Man City's third goal
  # against Brentford, where Ederson's 0.13-xPass ball released Haaland to score
  # and a Brentford centre-half was charged -0.178 as its ball winner.
  d[, is_turnover := true_possession_change %in% TRUE & !(result %in% "success")]

  # The opposing team, for paying defenders.
  d[, opp_team_id := fifelse(team_id == home_team_id, away_team_id, home_team_id)]

  if (!"xpass" %in% names(d)) d[, xpass := NA_real_]
  if (!"receiver_player_id" %in% names(d)) d[, receiver_player_id := NA_character_]

  d[, `:=`(
    is_shot = action_type == "shot",
    is_stop = action_type %chin% NG_STOP_ACTIONS,
    # A receiver share is a TEAMMATE's share, so the receiver must be on the
    # acting team. SPADL names a receiver on 26.4% of actions who is on the
    # OPPOSING side -- 11,905 of them on passes SPADL calls successful, carrying
    # 201.7 goals of absolute value. Paying those the teammate split credited an
    # opponent and booked it under his own team, i.e. on the wrong side of the
    # double entry, which is how 55% of player-matches ended up with payments
    # under two team ids. Such a pass falls through to the generic branch and
    # the actor keeps it.
    #
    # Whether a pass that reaches an opponent should be `result == "success"`
    # at all is an upstream question about SPADL, not one to answer by
    # redefining `result` here.
    has_receiver = !is.na(receiver_player_id) &
      nzchar(as.character(receiver_player_id))
  )]
  # Element-wise, deliberately: `receiver_team_id %in% team_id` would test
  # membership against the WHOLE column and be TRUE on every row.
  if ("receiver_team_id" %in% names(d)) {
    d[, has_receiver := has_receiver & (receiver_team_id == team_id) %in% TRUE]
  }
  # A stop is scored on its own row, never as a turnover: its value IS the
  # rebound, which the rebound rule already pays for. Scoring it both ways would
  # charge the stopper twice for one event.
  d[is_stop == TRUE, is_turnover := FALSE]

  # Does a stopper exist for this shot? The stop is the next row 98.9% of the
  # time; 0.9% of stops are orphaned from their shot and pay the pool instead.
  data.table::setorder(d, match_id, action_id)
  d[, nxt_act := shift(action_type, 1, type = "lead"), by = match_id]
  d[, nxt_player := shift(player_id, 1, type = "lead"), by = match_id]
  d[, nxt_team := shift(team_id, 1, type = "lead"), by = match_id]
  d[, stopper_id := NA_character_]
  d[is_shot == TRUE & nxt_act %chin% NG_STOP_ACTIONS & nxt_team != team_id,
    stopper_id := nxt_player]

  # An orphaned stop -- one whose shot the feed did not record as a shot -- must
  # not be charged for its rebound either. Crediting the stop depends on finding
  # the shot; charging the rebound does not. Applying only the second makes the
  # orphan case pure penalty, which is how Ederson took -0.194 for a save
  # Brentford's opening goal rebounded from, with nothing on the other side.
  # 0.9% of stops (47 of 5,124 on ENG 2024-2025). Their value goes to the pool.
  d[, prv_act := shift(action_type, 1), by = match_id]
  d[, prv_team := shift(team_id, 1), by = match_id]
  d[, stop_has_shot := is_stop & prv_act %in% "shot" & !is.na(prv_team) &
      prv_team != team_id]

  d[]
}


#' Split each action's value into credit and blame
#'
#' The **margin convention**. Subdivides every row's `value_home` among
#' recipients, allocating it ONCE and splitting it across both sides.
#' `.ng_credit_terms_team()` is the other convention, which books it twice.
#' Subdivides every row's `value_home` among recipients. The terms for a row
#' always sum to that row's `value_home`, so no rule can change a match total --
#' only who is paid.
#'
#' Panna's row shape differs from torp's in one way worth stating, because it
#' changes where the decision term lives. `calculate_action_epv()` overrides a
#' shot's EPV with its xG and then recalculates the preceding row's delta to
#' target that xG. So the value of working the ball into a shot worth 0.30
#' instead of 0.05 -- the decision term, averaging +0.0716 goals a shot -- is
#' already paid on the pass before it. The shot row carries only `1 - xG` on a
#' goal or `-xG` on a miss, which is the surprise. This function therefore
#' splits surprises; it does not reconstruct a decision term that is already
#' allocated elsewhere.
#'
#' @param dt Actions with `value_home`, `action_type`, `result`, recipient
#'   columns, and optionally `true_possession_change` / `true_next_player` from
#'   `ng_build_adjacency()` and `xpass` from `add_xpass_to_spadl()`.
#' @param shares Output of `ng_shares()`.
#'
#' @return A long data.table of payments with `role` and `play_type` tags.
#' @keywords internal
.ng_credit_terms <- function(dt, shares) {
  d <- .ng_prepare_terms(dt)
  n <- nrow(d)
  NAc <- rep(NA_character_, n)

  p <- list()
  add <- function(sel, player, team, value, role) {
    sel <- sel %in% TRUE
    if (!any(sel)) return(invisible(NULL))
    p[[length(p) + 1L]] <<- data.table::data.table(
      match_id = d$match_id[sel], action_id = d$action_id[sel],
      player_id = player[sel], team_id = team[sel],
      role = role, play_type = d$action_type[sel], value_home = value[sel]
    )
    invisible(NULL)
  }

  v <- d$value_home
  sh <- shares

  # --- A. Goal: the shooter converted a chance worth xG into one worth 1. ----
  a <- d$is_shot & d$result %in% "success"
  add(a, d$player_id, d$team_id, v * (1 - sh$off_pool), "shooter")
  add(a, NAc, d$team_id, v * sh$off_pool, "pool_off")

  # --- B. Shot stopped or missed: the surprise is -xG. ----------------------
  b <- d$is_shot & !(d$result %in% "success")
  add(b, d$player_id, d$team_id, v * sh$exec_blame, "shooter")
  rest_b <- v * (1 - sh$exec_blame)
  b_named <- b & !is.na(d$stopper_id)
  add(b_named, d$stopper_id, d$opp_team_id, rest_b * sh$named_share, "stopper")
  add(b_named, NAc, d$opp_team_id, rest_b * (1 - sh$named_share), "pool_def")
  # No named stopper: it went wide, or the stop is one of the 0.9% orphans.
  add(b & is.na(d$stopper_id), NAc, d$opp_team_id, rest_b, "pool_def")

  # --- C. The stop row: its value is the rebound, and nothing else. ---------
  # Measured on ENG 2024-2025: mean epv_delta on a stop row is -0.0236 when the
  # attacking team regathers and +0.0236 when it does not. Exactly symmetric --
  # the row is a pure rebound indicator, which is why splitting it out is what
  # separates "he saved it" from "it came back to them".
  cc <- d$is_stop & d$stop_has_shot
  add(cc, d$player_id, d$team_id, v * sh$reb_named, "stopper_rebound")
  add(cc, NAc, d$team_id, v * (1 - sh$reb_named), "pool_def")
  # Orphan: no shot to credit, so no rebound to charge either.
  add(d$is_stop & !d$stop_has_shot, NAc, d$team_id, v, "pool_def")

  # --- D. Turnover on any other action. -------------------------------------
  e <- d$is_turnover & !d$is_shot & !d$is_stop
  add(e, d$player_id, d$team_id, v * sh$exec_blame, "actor")
  rest_e <- v * (1 - sh$exec_blame)
  e_named <- e & !is.na(d$winner_id)
  add(e_named, d$winner_id, d$opp_team_id, rest_e * sh$named_share, "ball_winner")
  add(e_named, NAc, d$opp_team_id, rest_e * (1 - sh$named_share), "pool_def")
  add(e & is.na(d$winner_id), NAc, d$opp_team_id, rest_e, "pool_def")

  # --- E. Retained pass with a named receiver and a difficulty score. -------
  # Torp D6: the disposer takes `p` of the surprise and the receiver `1 - p`,
  # with `p` the chance of losing it. With `p = 1 - xpass` that is the split the
  # package's own split_pass_credit() already uses -- the harder the pass, the
  # more of it belongs to the passer.
  f <- !d$is_turnover & !d$is_shot & !d$is_stop & d$has_receiver &
    d$action_type == "pass" & !is.na(d$xpass)
  keep_f <- v * (1 - sh$off_pool)
  add(f, d$player_id, d$team_id, keep_f * (1 - d$xpass), "actor")
  add(f, d$receiver_player_id, d$team_id, keep_f * d$xpass, "receiver")
  add(f, NAc, d$team_id, v * sh$off_pool, "pool_off")

  # --- F. Everything else retained: the actor keeps it. ---------------------
  g <- !d$is_turnover & !d$is_shot & !d$is_stop & !(f %in% TRUE)
  add(g, d$player_id, d$team_id, v * (1 - sh$off_pool), "actor")
  add(g, NAc, d$team_id, v * sh$off_pool, "pool_off")

  data.table::rbindlist(p, use.names = TRUE)
}


# =============================================================================
# STEP 4 -- SPREADING TEAM POOLS
# =============================================================================

#' Spread team pools across the players who were on the pitch
#'
#' A pool payment has a team but no player: it is the pressure, the marking and
#' the runs nobody is named for. Measured on ENG 2024-2025, the pools carry
#' **17.4%** of absolute ledger value under `convention = "margin"` and
#' **39.1%** under `convention = "team"` (which books a conceding half that is
#' mostly unnamed). Leaving them unspread would read as that share of football
#' being done by nobody, and would make any comparison between positions
#' meaningless. Every figure in this file is ENG 2024-2025 unless it says
#' otherwise, and the convention is named wherever it changes the number.
#'
#' The spread is **flat across the eleven on the pitch at that minute**. That is
#' a deliberate starting point, not a result. Torp swept the alternatives and
#' found that routing by positional mirror alone made the forward/defender gap
#' *wider*, because a midfielder's mirror is another midfielder; and that its
#' richer "context" spread (pairings, defensive acts, mirror, time on ground)
#' narrowed the gap but repeated worse year over year than a flat spread. Flat
#' is the option that cannot quietly encode a prior about who deserves it.
#'
#' Substitutions are honoured to the minute, which matters more in football than
#' in AFL: a player who came on in the 80th minute is credited only for pools
#' generated after he came on.
#'
#' @param pay Payments from `ng_build_ledger()`, containing `pool_off` /
#'   `pool_def` rows.
#' @param actions The action table, for the minute each pool was generated.
#'   Needs `match_id`, `action_id`, `time_seconds`.
#' @param lineups Opta lineups with `match_id`, `player_id`, `team_id`,
#'   `is_starter`, `sub_on_minute`, `sub_off_minute`.
#' @param dacts_share How much of the **defensive pool's credit half** to route
#'   by each player's defensive work rather than flat. 0 is a flat spread; 1
#'   routes it entirely by defensive work. Default **0.5** (Pete, 2026-09-21). See the note below on why
#'   only the credit half, and why the default is 0.
#' @param dacts_measure What "defensive work" means.
#'   `"act_value"` (the default) weights each act by how much its own
#'   `epv_delta` moved the game, so a clearance off the line outweighs one on
#'   the halfway line. `"count"` counts `NG_DEFENSIVE_ACTIONS` instead, treating
#'   every act alike. `"named_value"` uses the player's named defensive payments
#'   -- **measured and rejected**, see `.ng_defensive_value()`.
#'
#'   `"act_value"` is the default because it is the only one that stays monotone
#'   without overshooting. Swept on ENG 2024-2025 at `dacts_share` 0 to 1, the
#'   spread across all six positions falls 0.221 -> 0.061 and no position ever
#'   becomes an outlier (the keeper lands mid-pack at +0.019). `"count"` flips
#'   defenders above strikers at 1 and drives the keeper to -0.069;
#'   `"named_value"` sends him to +0.422. That is a behaviour argument, not a
#'   spread-minimising one -- the spread is not a validated target.
#' @param verbose Print a summary. Default `TRUE`.
#'
#' @return The same payments with every pool row replaced by one row per player
#'   who was on the pitch, tagged `role = "pool_off_spread"` /
#'   `"pool_def_spread"`. Pools whose team has no lineup are left unspread and
#'   reported, never silently dropped.
#'
#' @family net_goals
#' @export
ng_spread_pools <- function(pay, actions, lineups, dacts_share = 0.5,
                            dacts_measure = c("act_value", "count", "named_value"),
                            verbose = TRUE) {
  dacts_measure <- match.arg(dacts_measure)
  if (!is.numeric(dacts_share) || length(dacts_share) != 1L ||
      is.na(dacts_share) || dacts_share < 0 || dacts_share > 1) {
    cli::cli_abort("{.arg dacts_share} must be one number in [0, 1], not {.val {dacts_share}}.")
  }
  p <- data.table::as.data.table(pay)
  is_pool <- p$role %chin% c("pool_off", "pool_def")
  if (!any(is_pool)) return(p[])

  pools <- p[is_pool]
  rest  <- p[!is_pool]

  av <- data.table::as.data.table(actions)
  if (!"period_id" %in% names(av)) {
    cli::cli_abort(c(
      "{.fn ng_spread_pools} needs {.field period_id} on the actions.",
      "i" = "Minute bins 45-56 exist in BOTH halves -- first-half stoppage time overlaps the early second half -- so a bucket keyed on the minute alone mixes the two lineups."))
  }
  acts <- av[, .(match_id, action_id, period_id,
                 minute = as.numeric(time_seconds) / 60)]
  pools <- merge(pools, acts, by = c("match_id", "action_id"), all.x = TRUE)

  # Bucket by whole minute before exploding: a season has ~515k pool payments
  # but only ~75k (match, team, minute, role) buckets, and the eleven on the
  # pitch cannot change inside a minute in any way the feed records.
  # `time_seconds` is cumulative from kick-off, so a minute bin is NOT unique
  # within a match: first-half stoppage runs to minute 56 while the second half
  # starts at 45, and on ENG 2024-2025 **15.66% of all actions** fall in bins
  # 45-56 that exist in both halves. With 237 half-time substitutions across 160
  # of 377 matches, bucketing on the minute alone would spread a first-half
  # stoppage pool across the post-half-time eleven. `period_id` joins the key.
  pools[, mbin := floor(pmax(minute, 0))]
  # And the stint test needs the same care: a first-half stoppage action must be
  # tested against the first-half lineup, so its effective minute is capped just
  # below the interval.
  pools[, stint_min := fifelse(period_id %in% 1L, pmin(mbin, 44), mbin)]
  # Carry `entry` through when the team convention set it: an offensive pool and
  # a defensive pool are different accounts and must not merge.
  has_entry <- "entry" %in% names(pools)
  if (!has_entry) pools[, entry := NA_character_]

  # Credit and blame are bucketed apart so they can be routed apart. Under the
  # team convention on ENG 2024-2025 the defensive pool nets to -1,172.3 goals a
  # season but is made of +2,824.9 of credit (a ball won back with nobody named)
  # and -3,997.2 of blame (value conceded). The margin convention has no
  # conceding half, so its numbers differ; the shape of the argument does not. Routing the WHOLE pool by defensive acts would hand defenders
  # 44.9% of a net-negative account and make them worse -- which is the mirror
  # of what torp hit at rating vintage v11, where one setting spread a side's
  # OFFENCE pool by DEFENSIVE acts and charged defenders most for defending.
  pools[, half := fifelse(value_own >= 0, "credit", "blame")]
  buck <- pools[, .(value_home = sum(value_home), value_own = sum(value_own)),
                by = .(match_id, team_id, role, entry, half, period_id, mbin,
                       stint_min)]

  # Opta's lineup uses 0 as the sentinel for "not substituted", NOT NA: a
  # starter who played the full 90 carries sub_off_minute == 0, and an unused
  # substitute carries sub_on_minute == 0 with minutes_played == 0. Reading
  # those zeros as real minutes ends every starter's stint at kick-off, which
  # leaves ~3 players a bucket instead of 11 -- and conservation still passes,
  # because the pool is simply divided among the wrong, smaller group.
  lu <- data.table::as.data.table(lineups)[
    , .(match_id, player_id, team_id,
        is_starter = is_starter %in% TRUE,
        on = suppressWarnings(as.numeric(sub_on_minute)),
        off = suppressWarnings(as.numeric(sub_off_minute)),
        mins = suppressWarnings(as.numeric(minutes_played)))]
  lu <- unique(lu, by = c("match_id", "player_id", "team_id"))
  lu <- lu[!is.na(mins) & mins > 0]
  lu[, start_min := fifelse(is_starter, 0, on)]
  lu[, end_min := fifelse(is.na(off) | off <= 0, Inf, off)]
  # The 0 sentinel is handled above; a genuinely MISSING minute is a different
  # thing and must not be waved through as if it were the sentinel. A
  # substitute with no on-minute cannot be placed at all and would be dropped
  # silently, shrinking the recipient pool; a player who left early with no
  # off-minute would linger past his departure and dilute the players actually
  # on. Neither is visible to any total, so both are reported.
  n_no_on <- sum(!lu$is_starter & is.na(lu$on))
  n_no_off <- sum(lu$is_starter & is.na(lu$off) & lu$mins < 85)
  if (n_no_on > 0 || n_no_off > 0) {
    cli::cli_warn(c(
      "Lineup has {n_no_on} substitute{?s} with no on-minute and {n_no_off} starter{?s} who left early with no off-minute.",
      "i" = "The first are dropped from the spread; the second are treated as playing to the end.",
      "x" = "Both misallocate pools without changing any total."))
  }
  lu <- lu[!is.na(start_min)]

  # Cartesian within (match, team), then filter to the stint. Each bucket keeps
  # only the players actually on.
  j <- merge(buck, lu, by = c("match_id", "team_id"), allow.cartesian = TRUE)
  j <- j[stint_min >= start_min & stint_min < end_min]

  # Weight. Flat by default: every player on the pitch takes an equal share.
  # `dacts_share` tilts ONLY the defensive pool's credit half toward whoever
  # actually made defensive acts in the match.
  j[, w := 1]
  if (dacts_share > 0) {
    da <- switch(dacts_measure,
                 count       = .ng_defensive_acts(actions),
                 act_value   = .ng_defensive_acts(actions, weight_by_value = TRUE),
                 named_value = .ng_defensive_value(p))
    j <- merge(j, da, by = c("match_id", "player_id"), all.x = TRUE)
    j[is.na(dacts), dacts := 0]
    tilt <- j$entry %in% "defence" & j$half %in% "credit"
    # Normalised within the bucket, so the blend is between two shares rather
    # than between a share and a raw count.
    j[, dshare := dacts / sum(dacts), by = .(match_id, team_id, role, entry, half, period_id, mbin)]
    j[!is.finite(dshare), dshare := 1 / .N,
      by = .(match_id, team_id, role, entry, half, period_id, mbin)]
    j[, flat := 1 / .N, by = .(match_id, team_id, role, entry, half, period_id, mbin)]
    j[tilt, w := (1 - dacts_share) * flat + dacts_share * dshare]
  }

  j[, wsum := sum(w), by = .(match_id, team_id, role, entry, half, period_id, mbin)]
  n_on <- j[, .(n_on = .N), by = .(match_id, team_id, role, entry, half, period_id, mbin)]
  j <- merge(j, n_on, by = c("match_id", "team_id", "role", "entry", "half", "period_id", "mbin"))
  j[, `:=`(value_home = value_home * w / wsum, value_own = value_own * w / wsum)]

  # Conservation cannot see this: dividing a pool among three players instead of
  # eleven still balances perfectly, it just pays the wrong, smaller group. So
  # assert the squad size directly. A team is eleven players until a red card,
  # so a median outside 10-11 means the stint logic is wrong, not the football.
  # A MEDIAN IS THE WRONG GUARD ON ITS OWN. It is robust by construction, so
  # just under half of all buckets could hold 3 players and it would still read
  # 11 -- which is exactly the failure this check exists to catch, merely
  # confined to a minority (one competition's lineup feed, one substitution
  # pattern). So check the share of bad buckets too, not only the centre.
  med_on <- stats::median(n_on$n_on)
  bad_share <- mean(n_on$n_on < 10 | n_on$n_on > 11)
  if (nrow(n_on) == 0 || med_on < 10 || med_on > 11 || bad_share > 0.02) {
    cli::cli_abort(c(
      "Pool spread: median {round(med_on, 1)} player{?s} per (match, team, period, minute) and {round(100 * bad_share, 1)}% of buckets outside 10-11.",
      "x" = "A side is eleven players; anything else means the stint logic misread the lineup.",
      "i" = "A pool divided among the wrong, smaller group still conserves perfectly, so no total will tell you.",
      "i" = "Opta uses 0, not NA, for 'not substituted' -- check {.field sub_on_minute} / {.field sub_off_minute}."
    ))
  }

  spread <- j[, .(match_id, action_id = NA_integer_, player_id, team_id,
                  role = paste0(role, "_spread"), entry, play_type = "pool",
                  value_home, is_home = NA)]

  # Pools with no lineup to spread onto: reported, not dropped.
  keys <- unique(j[, .(match_id, team_id, role, entry, half, period_id, mbin)])
  orphan <- buck[!keys, on = c("match_id", "team_id", "role", "entry", "half", "period_id", "mbin")]
  if (nrow(orphan) > 0) {
    cli::cli_warn(paste0(
      "{format(nrow(orphan), big.mark = ',')} pool bucket{?s} worth ",
      "{round(sum(orphan$value_home), 2)} goals have no lineup for their team ",
      "and stay unspread -- they are kept so the match total still balances."))
    orphan_rows <- orphan[, .(match_id, action_id = NA_integer_,
                              player_id = NA_character_, team_id, role, entry,
                              play_type = "pool", value_home, is_home = NA)]
    spread <- data.table::rbindlist(list(spread, orphan_rows), use.names = TRUE)
  }

  out <- data.table::rbindlist(list(rest, spread), use.names = TRUE, fill = TRUE)

  # Re-derive the frame flags for the new rows.
  hm <- unique(p[!is.na(is_home), .(match_id, team_id, is_home)])
  out[, is_home := NULL]
  if ("value_own" %in% names(out)) out[, value_own := NULL]
  out <- merge(out, hm, by = c("match_id", "team_id"), all.x = TRUE)
  out[, value_own := fifelse(is_home %in% TRUE, value_home, -value_home)]
  if (!has_entry) out[, entry := NULL]

  if (isTRUE(verbose)) {
    cli::cli_alert_success(paste0(
      "Pools spread: {format(nrow(pools), big.mark = ',')} pool payment{?s} ",
      "became {format(nrow(spread), big.mark = ',')} player payment{?s} ",
      "across {format(nrow(buck), big.mark = ',')} (match, team, minute) bucket{?s}."))
  }

  out[]
}


# =============================================================================
# STEP 5 -- THE TEAM CONVENTION (double entry)
# =============================================================================

#' Split each action twice, once per side
#'
#' The ESPN Net Points convention, which is what the metric actually does.
#' ESPN's own worked example: "Adding the Net Points for all of the Sixers in
#' that game equates to a plus-2. They beat the Dallas Mavericks 118-116."
#'
#' Each team's players sum to **that team's own goal difference** -- +2 for the
#' winner, -2 for the loser, zero across the match. Every unit of value is
#' allocated twice: once as credit on the side that earned it, once as blame on
#' the side that conceded it.
#'
#' Torp ships this too, as of 1.7.0 / rating vintage v6 (2026-09-08). Its
#' scoping doc `torpverse/docs/plans/NET-POINTS-TEAM-SUM-CONVENTION.md`
#' (2026-09-07) recommended against building it on AFL data, and torp shipped it
#' the next day anyway on Pete's explicit call -- against its own repeatability
#' measurements, because what it buys is a number answering who won the game
#' rather than who played well. **Cite the decision log, not a dated plan:** an
#' earlier version of this comment repeated the plan's recommendation as if it
#' still stood, which it had not for two weeks.
#'
#' What that plan got right is the cost, and it still holds. AFL play-by-play
#' carries no per-moment on-ground state, so its defensive half is spread almost
#' entirely by proxy. Football has that state -- Opta lineups give each player's
#' exact sub-on and sub-off minute -- and names the defender on 17.6% of raw
#' ledger value where AFL chains names one on none of it. Measured here, 39.1%
#' of total absolute value is proxy-spread (68.4% of the defensive half, 21.8%
#' of the offensive half); the plan feared ~55% for AFL.
#'
#' Why the arithmetic works out. Team A's total is the sum of its own actions'
#' values minus the sum of B's, and that difference is already the goal
#' difference -- measured at cor 0.9839, slope 1.0009 on ENG 2024-2025. No
#' reconciliation, no residual, no forced level. The identity falls out of double
#' entry rather than being imposed on top of it.
#'
#' @param dt Prepared actions, as `.ng_credit_terms()` takes them.
#' @param shares Output of `ng_shares()`.
#'
#' @return Payments in each recipient's **own** frame, tagged `entry`
#'   ("offence" or "defence") as well as `role` and `play_type`. Oliver splits
#'   his published numbers the same way (Jokic +365 offence, +61 defence).
#' @keywords internal
.ng_credit_terms_team <- function(dt, shares) {
  d <- .ng_prepare_terms(dt)
  n <- nrow(d)
  NAc <- rep(NA_character_, n)
  sh <- shares

  # Own frame throughout: v is what the action was worth to the team that took
  # it. The defending side is charged -v.
  v <- d$epv_delta
  v[is.na(v)] <- 0
  att <- d$team_id
  def <- d$opp_team_id

  p <- list()
  add <- function(sel, player, team, value, role, entry) {
    sel <- sel %in% TRUE
    if (!any(sel)) return(invisible(NULL))
    p[[length(p) + 1L]] <<- data.table::data.table(
      match_id = d$match_id[sel], action_id = d$action_id[sel],
      player_id = player[sel], team_id = team[sel],
      role = role, entry = entry, play_type = d$action_type[sel],
      value_own = value[sel])
    invisible(NULL)
  }

  # ---- OFFENCE: +v to the side that acted --------------------------------
  keep <- v * (1 - sh$off_pool)

  goal <- d$is_shot & d$result %in% "success"
  add(goal, d$player_id, att, keep, "shooter", "offence")
  add(goal, NAc, att, v * sh$off_pool, "pool_off", "offence")

  miss <- d$is_shot & !(d$result %in% "success")
  add(miss, d$player_id, att, v * sh$exec_blame, "shooter", "offence")
  add(miss, NAc, att, v * (1 - sh$exec_blame), "pool_off", "offence")

  # A failed action: the actor keeps exec_blame of it and his own team-mates
  # carry the rest. This is Oliver's asymmetry in its proper place -- WITHIN a
  # team, not between them. His longshot example gives a missed heave 21.8% to
  # the shooter and 19.6% to each of four team-mates; the blame is shared by
  # people who did their own job, and it still sums to the whole miss.
  fail <- !d$is_shot & !d$is_stop & !(d$result %in% "success")
  add(fail, d$player_id, att, v * sh$exec_blame, "actor", "offence")
  add(fail, NAc, att, v * (1 - sh$exec_blame), "pool_off", "offence")

  # A completed pass with a named receiver splits by difficulty (torp D6).
  pass <- !d$is_shot & !d$is_stop & d$result %in% "success" & d$has_receiver &
    d$action_type == "pass" & !is.na(d$xpass)
  add(pass, d$player_id, att, keep * (1 - d$xpass), "actor", "offence")
  add(pass, d$receiver_player_id, att, keep * d$xpass, "receiver", "offence")
  add(pass, NAc, att, v * sh$off_pool, "pool_off", "offence")

  # A shot-stopping row's value IS the rebound (measured: mean epv_delta -0.0236
  # when the attacking team regathers and +0.0236 when it does not, exactly
  # symmetric). It must follow the rebound rule, not the generic branch, or the
  # stopper keeps 90% of it as an ordinary on-ball action and `reb_named` never
  # applies. That is what happened until 2026-09-21: `reb_named` was implemented
  # in the margin convention only, and the team convention -- the default -- fell
  # through to `other`. Neither row-level assertion could see it, because paying
  # the wrong recipient the right amount conserves perfectly.
  stop_row <- d$is_stop & d$stop_has_shot
  add(stop_row, d$player_id, att, v * sh$reb_named, "stopper_rebound", "offence")
  add(stop_row, NAc, att, v * (1 - sh$reb_named), "pool_def", "offence")
  # Orphan stop: no shot to credit, so no rebound to charge. Whole row to pool.
  add(d$is_stop & !d$stop_has_shot, NAc, att, v, "pool_def", "offence")

  other <- !d$is_shot & !d$is_stop & d$result %in% "success" & !(pass %in% TRUE)
  add(other, d$player_id, att, keep, "actor", "offence")
  add(other, NAc, att, v * sh$off_pool, "pool_off", "offence")

  # ---- DEFENCE: -v to the side that conceded it ---------------------------
  # Where the feed names the defender, he takes named_share and his team-mates
  # the rest; where it does not, the whole entry is the team's.
  w <- -v
  named <- data.table::fifelse(
    !is.na(d$stopper_id) & d$is_shot, d$stopper_id,
    data.table::fifelse(d$is_turnover, d$winner_id, NA_character_))

  has <- !is.na(named)
  add(has, named, def, w * sh$named_share, "defender", "defence")
  add(has, NAc, def, w * (1 - sh$named_share), "pool_def", "defence")
  add(!has, NAc, def, w, "pool_def", "defence")

  out <- data.table::rbindlist(p, use.names = TRUE)
  out[]
}


#' Check each team's players against that team's own goal difference
#'
#' The team convention's identity: for a 3-1 win the winners sum to +2 and the
#' losers to -2, and the match sums to zero. Unlike the margin convention this
#' is checked per team, not per match.
#'
#' Like `ng_check_conservation()` this is a report, not a gate. Forcing a team
#' to its goal difference would charge a residual and, as torp measured for the
#' analogous `half_margin` mode, a residual can be larger than the thing it
#' corrects and can reorder players.
#'
#' @param pay Payments from `ng_build_ledger(convention = "team")`
#' @param fixtures Fixtures with `match_id`, `home_team_id`, `home_score`,
#'   `away_score`
#' @param verbose Print the summary. Default `TRUE`.
#'
#' @return One row per team-match: `own_total`, `own_gd`, `err`.
#' @family net_goals
#' @export
ng_check_team_totals <- function(pay, fixtures, verbose = TRUE) {
  fx <- data.table::as.data.table(fixtures)[
    , .(match_id, home_team_id, away_team_id,
        home_score = as.numeric(home_score), away_score = as.numeric(away_score))]
  t <- data.table::as.data.table(pay)[
    , .(own_total = sum(value_own, na.rm = TRUE)), by = .(match_id, team_id)]
  t <- merge(t, fx, by = "match_id")
  t <- t[(team_id == home_team_id | team_id == away_team_id) &
           !is.na(home_score) & !is.na(away_score)]
  t[, own_gd := fifelse(team_id == home_team_id,
                        home_score - away_score, away_score - home_score)]
  t[, err := own_total - own_gd]

  if (isTRUE(verbose) && nrow(t) > 0) {
    zero <- t[, .(s = sum(own_total)), by = match_id]
    cli::cli_alert_info(paste0(
      "Team totals over {nrow(t)} team-match{?es}: ",
      "cor {round(stats::cor(t$own_total, t$own_gd), 4)}, ",
      "slope {round(stats::coef(stats::lm(own_gd ~ own_total, t))[2], 4)}, ",
      "median |error| {round(stats::median(abs(t$err)), 3)} goal{?s}, ",
      "max {round(max(abs(t$err)), 3)}; ",
      "the two sides cancel to {format(max(abs(zero$s)), digits = 3, scientific = TRUE)}"))
  }

  t[]
}


#' Assert the two entries for an action cancel
#'
#' Under the team convention every action is booked twice -- once to the side
#' that acted, once negated to the side that conceded -- so its payments sum to
#' zero. That is what makes a match sum to zero and each team to its own goal
#' difference, with no reconciliation anywhere.
#'
#' @param dt Prepared actions with `epv_delta`
#' @param pay Payments from `.ng_credit_terms_team()`
#' @param tol Absolute tolerance per action and per side. Default 1e-9.
#' @keywords internal
.ng_assert_double_entry <- function(dt, pay, tol = 1e-9) {
  both <- pay[, .(net = sum(value_own, na.rm = TRUE)), by = .(match_id, action_id)]
  bad <- both[abs(net) > tol]
  if (nrow(bad) > 0) {
    worst <- bad[which.max(abs(bad$net))]
    cli::cli_abort(c(
      "Double entry does not cancel on {format(nrow(bad), big.mark = ',')} action{?s}.",
      "x" = "Worst: match {worst$match_id} action {worst$action_id} nets {round(worst$net, 6)} instead of 0.",
      "i" = "Every action must be booked +v to the side that acted and -v to the side that conceded."
    ))
  }

  # And each side's half must equal the action's own value, not merely cancel:
  # two equal and opposite halves of the WRONG size would pass the check above.
  off <- pay[entry == "offence", .(o = sum(value_own, na.rm = TRUE)),
             by = .(match_id, action_id)]
  want <- dt[, .(match_id, action_id, v = fifelse(is.na(epv_delta), 0, epv_delta))]
  cmp <- merge(want, off, by = c("match_id", "action_id"), all.x = TRUE)
  cmp[is.na(o), o := 0]
  bad2 <- cmp[abs(o - v) > tol]
  if (nrow(bad2) > 0) {
    worst <- bad2[which.max(abs(bad2$o - bad2$v))]
    cli::cli_abort(c(
      "The offensive half does not equal the action's value on {format(nrow(bad2), big.mark = ',')} action{?s}.",
      "x" = "Worst: match {worst$match_id} action {worst$action_id} is worth {round(worst$v, 6)} and books {round(worst$o, 6)}."
    ))
  }
  invisible(TRUE)
}


#' Count each player's defensive acts in a match
#'
#' The weight behind `ng_spread_pools(dacts_share = )`. Successful tackles,
#' interceptions, clearances, recoveries, aerials and keeper actions -- the
#' things a player visibly did to win or deny the ball.
#'
#' Two caveats worth stating rather than discovering later. It counts the
#' **whole match**, so a pool generated in the 10th minute is routed partly by
#' acts made in the 80th; that is acceptable for a descriptive metric and would
#' not be for a predictive one. And it counts acts, not their value, so a
#' routine clearance weighs the same as a goal-line block.
#'
#' @param actions SPADL actions with `match_id`, `player_id`, `action_type`,
#'   `result`, and `epv_delta` when `weight_by_value` is `TRUE`.
#' @param weight_by_value Weight each act by how much its own `epv_delta` moved
#'   the game rather than counting it. Fixes "a routine clearance weighs the
#'   same as a goal-line block" without reaching for the player's own payments,
#'   which is what made `.ng_defensive_value()` run away.
#' @return `match_id`, `player_id`, `dacts`.
#' @keywords internal
.ng_defensive_acts <- function(actions, weight_by_value = FALSE) {
  a <- data.table::as.data.table(actions)
  a <- a[action_type %chin% NG_DEFENSIVE_ACTIONS & result %in% "success" &
           !is.na(player_id) & nzchar(as.character(player_id))]
  if (!isTRUE(weight_by_value)) return(a[, .(dacts = .N), by = .(match_id, player_id)])
  if (!"epv_delta" %in% names(a)) {
    cli::cli_abort("{.code weight_by_value = TRUE} needs {.field epv_delta} on the actions.")
  }
  a[, .(dacts = sum(pmax(epv_delta, 0), na.rm = TRUE)), by = .(match_id, player_id)]
}

#' Actions counted as defensive acts
#'
#' Measured per 90 on ENG 2024-2025: Defender 12.18, Defensive Midfielder 10.58,
#' Midfielder 10.02, Attacking Midfielder 6.89, Striker 5.48. So a fully
#' defensive-acts spread gives defenders 44.9% of their team's pool against 9.1%
#' under a flat one.
#'
#' **`keeper_pick_up` and `keeper_claim` are deliberately excluded.** With them
#' in, a keeper counted 11.88 acts a 90 -- effectively a defender's 12.18 -- of
#' which picking the ball up alone was 5.78 (48.6%). Routing on raw counts then
#' made the keeper the best position in the league at `dacts_share = 1`
#' (+0.171 a 90 against every outfield position between -0.039 and -0.001).
#'
#' The reason it is wrong is that the same event was paying him twice through
#' different doors: `keeper_pick_up` already credits him +0.1235 a 90 directly
#' as the actor, *and* bought him the defensive pool share on top. The two
#' figures quoted for that event are different quantities and should not be
#' expected to match: 4,396 events at mean `epv_delta` +0.0237 is +104.2 goals
#' of raw row value, of which he is paid +93.8 after the 10% `off_pool` slice. Collecting a ball that is
#' already safe is a state change, not a defensive act. Without the two,
#' a keeper counts 5.24 a 90 against outfielders' 9.81.
#'
#' `keeper_save` and `keeper_punch` stay: those are shot-stopping.
#'
#' @keywords internal
NG_DEFENSIVE_ACTIONS <- c("tackle", "interception", "clearance", "keeper_save",
                          "ball_recovery", "aerial", "keeper_punch")


#' Value each player's named defensive work in a match
#'
#' The default weight behind `ng_spread_pools(dacts_share = )`. Where
#' `.ng_defensive_acts()` counts acts and lets a routine clearance weigh the
#' same as a goal-line block, this uses the ledger's own valuation of the
#' defensive work the feed *did* name him for, and spreads the work it could not
#' name in proportion to it.
#'
#' Only positive payments count. A named defensive payment is a credit almost by
#' construction -- the defender is paid `-v` on an action that cost the attacker
#' `v` -- but the sign is clamped rather than assumed, so a negative one reduces
#' a player to no claim on the pool instead of a negative claim, which would
#' invert his share.
#'
#' This is self-referential by design: the pool is routed by the ledger's own
#' numbers. `dacts_share` is what keeps it from running away, because every
#' setting below 1 blends it with a flat share, so a player who was never named
#' still holds a floor.
#'
#' @param pay Payments from `ng_build_ledger(convention = "team")`.
#' @return `match_id`, `player_id`, `dacts`.
#' @keywords internal
.ng_defensive_value <- function(pay) {
  p <- data.table::as.data.table(pay)
  if (!"entry" %in% names(p)) {
    cli::cli_abort(c(
      "{.code dacts_measure = \"value\"} needs the {.field entry} tag.",
      "i" = "It is set by {.code ng_build_ledger(convention = \"team\")}; the margin convention does not produce one."))
  }
  d <- p[entry %in% "defence" & role %in% "defender" &
           !is.na(player_id) & nzchar(as.character(player_id))]
  d[, .(dacts = sum(pmax(value_own, 0), na.rm = TRUE)), by = .(match_id, player_id)]
}


#' Route a payment whose recipient the feed does not name into a team pool
#'
#' A payment with no player would be dropped by any per-player aggregation --
#' silently, because the team totals are untouched by the loss. Both conventions
#' need this, and for a while only the margin one had it: the team branch
#' returned before reaching it, so an action with no attributed player produced
#' an `actor` row with `player_id = NA` that sat outside `ng_spread_pools()`
#' (which only touches pool roles) and vanished from every player-level rollup.
#'
#' @param pay Payments with `player_id`, `role`.
#' @return The same payments with unnamed non-pool rows re-tagged as pools.
#' @keywords internal
.ng_pool_unnamed <- function(pay) {
  unnamed <- (is.na(pay$player_id) | !nzchar(as.character(pay$player_id))) &
    !(pay$role %chin% c("pool_off", "pool_def"))
  if (any(unnamed)) {
    pool_role <- if ("entry" %in% names(pay)) {
      fifelse(pay$entry[unnamed] %in% "defence", "pool_def", "pool_off")
    } else "pool_off"
    pay[unnamed, `:=`(role = pool_role, player_id = NA_character_)]
  }
  pay
}


# =============================================================================
# STEP 6 -- PER-GAME AGGREGATION
# =============================================================================

#' Aggregate net goals to one row per player-match
#'
#' Turns the payment ledger into the frame the rating layer consumes. The
#' column contract deliberately matches `aggregate_player_game_epv()`'s --
#' `player_id`, `player_name`, `match_date`, `minutes_played`,
#' `epv_offensive`, `epv_defensive` -- so `calculate_epr_regression()` can be
#' pointed at either without changing, and the two can be gated against each
#' other on identical footing.
#'
#' **The offence/defence split means something different here, and it is the
#' better of the two.** `aggregate_player_game_epv()` splits by bucketing action
#' types (passing and shooting are offensive, tackles and keeper handling
#' defensive), which is presentational -- re-bucketing an action changes the
#' split and not the total. Net goals splits by which half of the double entry
#' the payment sits on: `offence` is the side that acted, `defence` the side
#' that conceded. Every player is paid on both halves of every action his team
#' is involved in, so a defender who never touches the ball still has a
#' defensive number, which is the whole point.
#'
#' Pools must be spread first. A payment with no player is real value, and
#' dropping it here would quietly shrink a player-game total while every team
#' total stayed correct -- so an unspread pool aborts rather than being skipped.
#'
#' @param pay Payments from `ng_spread_pools()`.
#' @param lineups Opta lineups, for minutes, names, date, league and season.
#' @param verbose Print a summary. Default `TRUE`.
#'
#' @return One row per player-match: identifiers, `minutes_played`,
#'   `net_goals`, `epv_offensive`, `epv_defensive`, and one `ng_*` column per
#'   role so a rating layer can choose its own grouping rather than inheriting
#'   this one (torp D3: store tags, group later).
#'
#' @family net_goals
#' @export
ng_player_game <- function(pay, lineups, verbose = TRUE) {
  p <- data.table::as.data.table(pay)

  unspread <- p$role %chin% c("pool_off", "pool_def")
  if (any(unspread)) {
    cli::cli_abort(c(
      "{format(sum(unspread), big.mark = ',')} payment{?s} are still unspread team pools.",
      "i" = "Run {.fn ng_spread_pools} first: a pool has no player, so aggregating now would drop that value from every player-game while the team totals stayed correct.",
      "i" = "Unspread pools carry {round(sum(abs(p$value_own[unspread])), 1)} goals of absolute value here."))
  }
  p <- p[!is.na(player_id) & nzchar(as.character(player_id))]

  if (!"entry" %in% names(p)) {
    cli::cli_abort(c(
      "{.fn ng_player_game} needs the {.field entry} tag to split offence from defence.",
      "i" = "It is set by {.code ng_build_ledger(convention = \"team\")}."))
  }

  tot <- p[, .(net_goals = sum(value_own, na.rm = TRUE)),
           by = .(match_id, player_id, team_id)]
  halves <- data.table::dcast(
    p[, .(v = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id, entry)],
    match_id + player_id ~ entry, value.var = "v", fill = 0)
  for (nm in c("offence", "defence")) if (!nm %in% names(halves)) halves[, (nm) := 0]
  data.table::setnames(halves, c("offence", "defence"),
                       c("epv_offensive", "epv_defensive"))

  # One column per role. A rating layer that wants three channels, or five, can
  # build them from these rather than inheriting whatever grouping suited the
  # display -- the same reason torp keeps role tags rather than fixed channels.
  roles <- data.table::dcast(
    p[, .(v = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id, role)],
    match_id + player_id ~ role, value.var = "v", fill = 0)
  rcols <- setdiff(names(roles), c("match_id", "player_id"))
  data.table::setnames(roles, rcols, paste0("ng_", rcols))

  out <- merge(tot, halves, by = c("match_id", "player_id"))
  out <- merge(out, roles, by = c("match_id", "player_id"))

  lu <- data.table::as.data.table(lineups)
  keep <- intersect(c("match_id", "player_id", "player_name", "match_date",
                      "minutes_played", "competition", "season"), names(lu))
  lu <- unique(lu[, ..keep], by = c("match_id", "player_id"))
  out <- merge(out, lu, by = c("match_id", "player_id"), all.x = TRUE)
  if ("competition" %in% names(out)) data.table::setnames(out, "competition", "league")
  if ("minutes_played" %in% names(out)) {
    out[, minutes_played := suppressWarnings(as.numeric(minutes_played))]
  }

  n_nolu <- sum(is.na(out$minutes_played))
  if (n_nolu > 0) {
    cli::cli_warn(paste0(
      "{format(n_nolu, big.mark = ',')} player-match row{?s} have no lineup entry, ",
      "so no minutes, name, date or league. They keep their net goals and will ",
      "be dropped by any rating layer that needs minutes."))
  }

  data.table::setcolorder(out, intersect(
    c("player_id", "player_name", "match_id", "team_id", "match_date", "league",
      "season", "minutes_played", "net_goals", "epv_offensive", "epv_defensive"),
    names(out)))

  if (isTRUE(verbose)) {
    cli::cli_alert_success(paste0(
      "Per-game: {format(nrow(out), big.mark = ',')} player-match row{?s} over ",
      "{uniqueN(out$match_id)} match{?es}; net goals sum {round(sum(out$net_goals), 3)} ",
      "(zero across a full set of matches, because every action is booked to both sides)."))
  }

  out[]
}
