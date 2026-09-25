# Net goals by play type, for the website's "where their EPV comes from" view.
# The football twin of torp's .np_breakdown(); design and evidence:
# vault/plans/NET-LEDGER-PRODUCTION.md.

#' Play type for a net goals payment
#'
#' Labels one payment row of the ledger (after [ng_spread_pools()]) by its role
#' and play type. Role decides first, because one play type pays different
#' people: on a shot, the shooter is paid for the strike and the keeper named on
#' the finish. These are the columns of the "Where Net Goals Come From" artifact
#' (`data-raw/epv/net-goals/build_net_goals_artifacts.R`).
#'
#' @param play_type,role Columns of the payment table.
#' @return Character vector of category labels.
#' @keywords internal
.ng_play_type <- function(play_type, role) {
  data.table::fcase(
    grepl("^pool_", role),                                    "Team pool share",
    role == "shot_strike",                                    "Shooting: placement (xG to xGOT)",
    role == "shot_finish",                                    "Shooting: against the keeper",
    role == "shot_aftermath",                                 "Shooting: what it left behind",
    role == "shooter",                                        "Own goals & shots with no xGOT",
    role == "receiver",                                       "Receiving a pass",
    role == "stopper_rebound",                                "Keeper: rebound after a save",
    role == "defender" & play_type == "shot",                 "Stopping shots",
    role == "defender" & play_type == "pass",                 "Cutting out passes",
    role == "defender",                                       "Defending other actions",
    play_type %in% c("pass", "cross"),                        "Passing",
    play_type == "take_on",                                   "Take-ons",
    play_type == "dribble",                                   "Carrying",
    play_type == "aerial",                                    "Aerial duels",
    play_type %in% c("tackle", "interception"),               "Tackles & interceptions",
    play_type == "ball_recovery",                             "Ball recoveries",
    play_type == "clearance",                                 "Clearances",
    play_type %in% c("ball_touch", "dispossessed"),           "Losing the ball",
    play_type == "foul",                                      "Fouls",
    grepl("^keeper_", play_type),                             "Keeper: handling",
    default = "Other")
}

#' Split each published player-match's net goals by play type
#'
#' A published `net_goals` is built in three steps, and the breakdown keeps each
#' visible so the parts add up exactly:
#' 1. the ledger's payments to the player ([ng_player_game()] sums them), split
#'    here by play type, with the team pool share as its own category;
#' 2. [ng_fold_unpublished()], which hands the value of players the published
#'    frame does not carry (unused or late substitutes) to their team-mates by
#'    minutes: "Share of unlisted team-mates";
#' 3. [ng_reconcile_margin()]'s anchor to the real goal difference (`ng_recon`).
#'
#' @param pay Payment table after [ng_spread_pools()].
#' @param pre_fold [ng_player_game()] output (before folding), with `net_goals`.
#' @param published The published frame after [ng_reconcile_margin()]: one row
#'   per player-match with `net_goals` and `ng_recon`.
#' @param tol Largest allowed gap between a row's categories and its published
#'   `net_goals`. Rounding level: anything bigger is a dropped or doubled payment.
#' @return data.table: `match_id`, `player_id`, `category`, `value` (goals,
#'   positive is good for the player), for every published row with net goals.
#' @keywords internal
.ng_breakdown <- function(pay, pre_fold, published, tol = 1e-9) {
  p <- data.table::as.data.table(pay)[!is.na(player_id) & nzchar(as.character(player_id))]
  p[, `:=`(match_id = as.character(match_id), player_id = as.character(player_id))]
  p[, category := .ng_play_type(play_type, role)]
  named <- p[, .(value = sum(value_own, na.rm = TRUE)), by = .(match_id, player_id, category)]

  pub <- data.table::as.data.table(published)[!is.na(net_goals)]
  pub <- pub[, .(match_id = as.character(match_id), player_id = as.character(player_id),
                 net_goals, ng_recon = if ("ng_recon" %in% names(pub)) ng_recon else 0)]
  if (anyDuplicated(pub, by = c("match_id", "player_id"))) {
    cli::cli_abort("Published frame has more than one row for a (match_id, player_id).")
  }
  pre <- data.table::as.data.table(pre_fold)[
    , .(pre = sum(net_goals)), by = .(match_id = as.character(match_id), player_id = as.character(player_id))]

  # Named parts only for published players; each part of step 2 and 3 is read
  # off the published row, so none of them can quietly absorb a gap.
  named <- named[pub[, .(match_id, player_id)], on = .(match_id, player_id), nomatch = NULL]
  fold <- merge(pub[, .(match_id, player_id, net_goals, ng_recon)], pre,
                by = c("match_id", "player_id"), all.x = TRUE)
  fold[is.na(pre), pre := 0]
  fold[, value := net_goals - ng_recon - pre]   # what folding added to this row
  out <- rbind(named,
               fold[abs(value) > 0, .(match_id, player_id, category = "Share of unlisted team-mates", value)],
               pub[ng_recon != 0, .(match_id, player_id, category = "Anchor to the real goal difference", value = ng_recon)])

  # The gate. `pre` must equal the sum of the named payments for every player
  # the ledger paid: a mismatch there means the payment table and ng_player_game()
  # disagree, which folding would otherwise hide inside the fold part.
  chk_pre <- merge(named[, .(s = sum(value)), by = .(match_id, player_id)], pre,
                   by = c("match_id", "player_id"))
  gap_pre <- if (nrow(chk_pre)) max(abs(chk_pre$s - chk_pre$pre)) else 0
  # The fold part is computed as a remainder, so it needs its own check: per
  # team-match, what folding added must equal the value of that team's players
  # who have no published row (what ng_fold_unpublished() hands out).
  pre_t <- data.table::as.data.table(pre_fold)[
    , .(match_id = as.character(match_id), player_id = as.character(player_id), team_id, net_goals)]
  unlisted <- pre_t[!pub, on = .(match_id, player_id)][, .(owed = sum(net_goals)), by = .(match_id, team_id)]
  pub_team <- data.table::as.data.table(published)[!is.na(net_goals),
    .(match_id = as.character(match_id), player_id = as.character(player_id), team_id)]
  got <- merge(fold[, .(match_id, player_id, value)], pub_team, by = c("match_id", "player_id"))[
    , .(got = sum(value)), by = .(match_id, team_id)]
  # Only team-matches with a published row can receive a share; a team with none
  # is ng_fold_unpublished()'s own warning, and has no row here to be wrong.
  fchk <- merge(got, unlisted, by = c("match_id", "team_id"), all.x = TRUE)
  fchk[is.na(owed), owed := 0]
  gap_fold <- if (nrow(fchk)) max(abs(fchk$got - fchk$owed)) else 0
  if (!is.finite(gap_fold) || gap_fold > tol) {
    cli::cli_abort(c(
      "Net goals breakdown: the unlisted team-mates' share misses what folding handed out by {signif(gap_fold, 3)} goals.",
      "x" = "Published net_goals is not payments + fold + anchor for some team-match."
    ), class = "panna_ng_breakdown_mismatch")
  }
  chk <- merge(out[, .(tot = sum(value)), by = .(match_id, player_id)],
               pub[, .(match_id, player_id, net_goals)], by = c("match_id", "player_id"), all.y = TRUE)
  chk[is.na(tot), tot := 0]
  gap <- max(abs(chk$tot - chk$net_goals))
  if (!is.finite(gap_pre) || gap_pre > tol || !is.finite(gap) || gap > tol) {
    cli::cli_abort(c(
      "Net goals play types do not add up (payments vs ng_player_game: {signif(gap_pre, 3)}; parts vs published: {signif(gap, 3)}).",
      "x" = "A payment was dropped, doubled or relabelled; nothing is written."
    ), class = "panna_ng_breakdown_mismatch")
  }
  out[]
}

#' Season totals of the net goals breakdown, one block of rows per player
#'
#' The player page shows a season's EPV by play type for one player. Reading the
#' per-match breakdown for that took 13.7 s on a past season (2.4M rows, 21 MB),
#' because the page downloads every player's matches to draw one. This sums each
#' player's season across every competition they played, and the file is written
#' sorted by player in small row groups so the page reads one group.
#'
#' @param bd [.ng_breakdown()] rows for one season: `match_id`, `player_id`,
#'   `category`, `value`.
#' @return data.table sorted by `bucket` ([.ng_player_bucket()]) then `player_id`:
#'   `bucket`, `player_id`, `category`, `value`
#'   (season total, goals), `games` (matches with a breakdown) and `net_goals`
#'   (the player's season total, the same on each of their rows, so the page can
#'   check its parts add up).
#' @keywords internal
.ng_breakdown_players <- function(bd) {
  b <- data.table::as.data.table(bd)
  if (!nrow(b)) cli::cli_abort("No breakdown rows to total.")
  pl <- b[, .(games = data.table::uniqueN(match_id), net_goals = sum(value)), by = player_id]
  out <- b[, .(value = sum(value)), by = .(player_id, category)]
  out <- merge(out, pl, by = "player_id")
  gap <- max(abs(out[, .(s = sum(value), n = net_goals[1]), by = player_id][, s - n]))
  if (!is.finite(gap) || gap > 1e-9) {
    cli::cli_abort("Player season totals do not add up ({signif(gap, 3)}).",
                   class = "panna_ng_breakdown_mismatch")
  }
  out[, bucket := .ng_player_bucket(player_id)]
  data.table::setcolorder(out, "bucket")
  data.table::setorder(out, bucket, player_id, category)
  out[]
}

#' A number for each player id, the same in R and on the website
#'
#' The site's parquet reader only skips row groups when it filters on a numeric
#' column (min/max statistics on text are not safe to compare in the browser --
#' see `_rowGroupRanges` in the blog's `data-loader.js`). Filtering the player
#' file on `player_id` therefore read all 39 row groups, one request each, and
#' timed out at 30 s over R2. Sorting by this number and filtering on it lets the
#' reader fetch the one or two row groups that hold the player.
#'
#' Polynomial hash, base 31, modulo 1,000,003: every intermediate stays below
#' 2^53, so R doubles and JavaScript numbers give identical results. Twin:
#' `ngPlayerBucket()` in the blog's `football/player.qmd`. Change both or neither.
#'
#' @param id Character vector of player ids.
#' @return Integer vector in 0..1000002.
#' @keywords internal
.ng_player_bucket <- function(id) {
  u <- unique(id)
  h <- vapply(u, function(x) {
    v <- 0
    for (cp in utf8ToInt(x)) v <- (v * 31 + cp) %% 1000003
    v
  }, numeric(1), USE.NAMES = FALSE)
  as.integer(h[match(id, u)])
}
