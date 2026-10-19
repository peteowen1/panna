# Position groupings, one builder per level (docs: pannaverse
# docs/reference/POSITION-TAXONOMY.md section 6, Pete 2026-10-19).
#
# Column names are {level}_{grouping}: levels match_ / asof_ / season_ /
# career_, groupings role_full / role16 / role8 / role4. Every coarser grouping
# is derived from role_full through POSITION_ROLE_MAP, never re-derived from raw
# Opta text, which is how the older groupings drifted apart.

# Opta position x side -> role_full -> role16. role16 reproduces classify_role()
# exactly (a test pins this), so nothing downstream of role16 moves. role_full
# keeps the side that role16 merges: LCB / CB / RCB rather than one CB.
POSITION_ROLE_MAP <- data.table::data.table(
  position = c(rep("Goalkeeper", 5),
               rep("Defender", 5),
               rep("Wing Back", 5),
               rep("Defensive Midfielder", 5),
               rep("Midfielder", 5),
               rep("Attacking Midfielder", 5),
               rep("Striker", 5)),
  position_side = rep(c("Left", "Left/Centre", "Centre", "Centre/Right", "Right"), 7),
  role_full = c("GK", "GK", "GK", "GK", "GK",
                "LB", "LCB", "CB", "RCB", "RB",
                "LWB", "LCWB", "CWB", "RCWB", "RWB",
                "LWDM", "LDM", "DM", "RDM", "RWDM",
                "LM", "LCM", "CM", "RCM", "RM",
                "LW", "LAM", "AM", "RAM", "RW",
                "LF", "LST", "ST", "RST", "RF"),
  role16 = c("GK", "GK", "GK", "GK", "GK",
             "LB", "CB", "CB", "CB", "RB",
             "LWB", "RWB", "RWB", "RWB", "RWB",
             "DM", "DM", "DM", "DM", "DM",
             "LM", "CM", "CM", "CM", "RM",
             "LW", "CAM", "CAM", "CAM", "RW",
             "LF", "CF", "CF", "CF", "RF")
)

#' Every position grouping from one Opta position and side
#'
#' The single mapping behind the `role_full`, `role16`, `role8` and `role4`
#' groupings. `role_full` keeps left and right (LCB, RCB, LCM, ...);
#' `role16` is [classify_role()]'s 16 roles; `role8` mirrors left and right
#' into GK, CB, FB, DM, CM, AM, W, ST (the default grouping); `role4` is GK,
#' DEF, MID, FWD. A blank position, "Substitute" or a missing side gives `NA`
#' in every column.
#'
#' @param position,position_side Opta `position` and `position_side`.
#' @return A data.table with `role_full`, `role16`, `role8`, `role4`, one row
#'   per input element.
#' @family positions
#' @export
position_roles <- function(position, position_side) {
  key <- data.table::data.table(position = as.character(position),
                                position_side = as.character(position_side))
  # A keeper is a keeper whatever side Opta stamps (or omits).
  key[position == "Goalkeeper", position_side := "Centre"]
  out <- POSITION_ROLE_MAP[key, on = c("position", "position_side"),
                           .(role_full, role16)]
  out[, role8 := data.table::fifelse(is.na(role16), NA_character_, .role16_to_role8(role16))]
  out[, role4 := data.table::fifelse(is.na(role16), NA_character_, .role16_to_broad(role16))]
  out[]
}

# One qualifier's value from Opta's qualifier JSON, as text. NA where the key
# is absent or JSON null. A regex rather than jsonlite: this runs on every
# sub-on and formation event of every league, and the values are flat strings.
.opta_qualifier <- function(json, key) {
  m <- stringi::stri_match_first_regex(
    json, paste0('"', key, '"\\s*:\\s*(?:"([^"]*)"|(-?[0-9.]+))'))
  out <- data.table::fifelse(is.na(m[, 2L]), m[, 3L], m[, 2L])
  out[!is.na(out) & !nzchar(out)] <- NA_character_
  out
}

# Formation states from type 34 (team set-up, the kick-off shape) and type 40
# (formation change): one row per player in each state, with the formation id
# (qualifier 130) and the player's slot (131, aligned with the ids in 30).
# States whose player list is missing (JSON null; 1-20% of type 40 in
# 2011-2015) keep their formation with no players.
.formation_states <- function(ev) {
  st <- ev[type_id %in% c(34L, 40L)]
  if (!nrow(st)) {
    return(list(
      states = data.table::data.table(match_id = character(), team_id = character(),
                                      state_t = numeric(), is_kickoff = logical(),
                                      formation = integer()),
      players = data.table::data.table(match_id = character(), team_id = character(),
                                       state_t = numeric(), player_id = character(),
                                       slot = integer())))
  }
  st[, formation := suppressWarnings(as.integer(.opta_qualifier(qualifier_json, "130")))]
  st[, ids := .opta_qualifier(qualifier_json, "30")]
  st[, sl := .opta_qualifier(qualifier_json, "131")]
  st[, is_kickoff := type_id == 34L]
  # A team set-up is the kick-off shape whatever its stamped time.
  st[is_kickoff == TRUE, state_t := -Inf]
  states <- st[, .(match_id, team_id, state_t, is_kickoff, formation)]
  pl <- st[!is.na(ids) & !is.na(sl)]
  players <- if (!nrow(pl)) {
    data.table::data.table(match_id = character(), team_id = character(),
                           state_t = numeric(), player_id = character(), slot = integer())
  } else {
    pl[, .row := .I]
    pl[, {
      i <- trimws(strsplit(ids, ",", fixed = TRUE)[[1L]])
      s <- suppressWarnings(as.integer(trimws(strsplit(sl, ",", fixed = TRUE)[[1L]])))
      n <- min(length(i), length(s))
      list(player_id = i[seq_len(n)], slot = s[seq_len(n)])
    }, by = .(match_id, team_id, state_t, .row)][, .row := NULL][]
  }
  list(states = states, players = players[!is.na(slot) & slot >= 1L & slot <= 11L])
}

# Event time on one sortable scale: period first, so a second-half 45:00 sorts
# after first-half stoppage.
.event_t <- function(period_id, minute, second) {
  p <- suppressWarnings(as.numeric(period_id)); p[is.na(p)] <- 0
  s <- if (is.null(second)) 0 else suppressWarnings(as.numeric(second))
  s[is.na(s)] <- 0
  p * 1e4 + suppressWarnings(as.numeric(minute)) * 60 + s
}

.prepare_position_events <- function(events) {
  need <- c("match_id", "team_id", "player_id", "type_id", "period_id", "minute", "qualifier_json")
  miss <- setdiff(need, names(events))
  if (length(miss)) {
    cli::cli_abort("{.arg events} is missing column{?s} {.field {miss}}.")
  }
  ev <- data.table::as.data.table(events)[as.integer(type_id) %in% c(19L, 34L, 40L)]
  ev[, `:=`(match_id = as.character(match_id), team_id = as.character(team_id),
            player_id = as.character(player_id), type_id = as.integer(type_id))]
  ev[, state_t := .event_t(period_id, minute, if ("second" %in% names(ev)) second else NULL)]
  ev
}

#' Most common position per formation slot, learned from labelled starters
#'
#' Opta records the formation (qualifier 130) and each starter's slot
#' (qualifier 131) at kick-off. Across matches, a given formation and slot is
#' almost always the same position, so this table gives a position to anyone
#' whose own label is missing: a substitute after a formation change, or a
#' starter whose lineup row is blank. It describes Opta's slot numbering, not
#' any player, so building it from every match passed in does not leak one
#' player's future into his past.
#'
#' @param lineups Lineup rows with `match_id`, `team_id`, `player_id`,
#'   `position`, `position_side`, `is_starter`.
#' @param events Opta events with `match_id`, `team_id`, `player_id`,
#'   `type_id`, `period_id`, `minute`, `qualifier_json` (only types 34 and 40
#'   are used).
#' @return One row per formation and slot: `formation`, `slot`, `role_full`
#'   (the most common), `n` (labelled starters seen) and `share` (the share of
#'   them with that `role_full`).
#' @family positions
#' @export
position_slot_table <- function(lineups, events) {
  ev <- .prepare_position_events(events)
  fs <- .formation_states(ev)
  ko <- fs$players[is.infinite(state_t)][
    fs$states[is_kickoff == TRUE, .(match_id, team_id, formation)],
    on = c("match_id", "team_id"), nomatch = NULL]
  lu <- data.table::as.data.table(lineups)[is_starter %in% TRUE,
    .(match_id = as.character(match_id), player_id = as.character(player_id),
      position, position_side)]
  lu[, role_full := position_roles(position, position_side)$role_full]
  x <- ko[lu[!is.na(role_full)], on = c("match_id", "player_id"), nomatch = NULL]
  x <- x[!is.na(formation), .N, by = .(formation, slot, role_full)]
  # Ties go to the first code alphabetically, so the table is reproducible.
  data.table::setorder(x, formation, slot, -N, role_full)
  x[, .(role_full = role_full[1L], n = sum(N), share = N[1L] / sum(N)),
    by = .(formation, slot)][]
}

# Opta's own position for a substitute, from his sub-on event (type 19):
# qualifier 292 is a detailed position, 293 its side. Decoded 2026-10-19 from
# 101,484 subs in five leagues (no Opta documentation found): 292 1 keeper,
# 3 full-back, 4 centre-back, 5 holding midfielder, 6 attacking midfielder,
# 7 central midfielder, 8 wide forward, 9 striker, 10 second striker, 0
# unknown; 293 1 Left, 2 Right, 3 Centre, 4 Left/Centre, 5 Centre/Right, 0
# unknown. Returned as an Opta position and side so position_roles() maps it
# like any starter's label. Strikers' 1 and 2 are the left and right of a
# pair (the replaced starter is "Striker Left/Centre" 52% of the time), and a
# wide forward touches the ball where wingers do, so it maps as one. A second
# striker maps to attacking midfielder, Opta's code being separate from 9.
.sub_position_from_qualifiers <- function(q292, q293) {
  side <- c("1" = "Left", "2" = "Right", "3" = "Centre",
            "4" = "Left/Centre", "5" = "Centre/Right")[as.character(q293)]
  pos <- c("1" = "Goalkeeper", "3" = "Defender", "4" = "Defender",
           "5" = "Defensive Midfielder", "6" = "Attacking Midfielder",
           "7" = "Midfielder", "8" = "Attacking Midfielder", "9" = "Striker",
           "10" = "Attacking Midfielder")[as.character(q292)]
  pos <- unname(pos); side <- unname(side)
  side[pos %in% "Goalkeeper"] <- "Centre"
  side[pos %in% "Attacking Midfielder" & q292 %in% "10"] <- "Centre"
  # a full-back is wide and a centre-back central, whatever the side code says
  side[q292 %in% "3" & side %in% c("Left/Centre", "Centre")] <- "Left"
  side[q292 %in% "3" & side %in% "Centre/Right"] <- "Right"
  side[q292 %in% "4" & side %in% "Left"] <- "Left/Centre"
  side[q292 %in% "4" & side %in% "Right"] <- "Centre/Right"
  side[q292 %in% "9" & side %in% "Left"] <- "Left/Centre"
  side[q292 %in% "9" & side %in% "Right"] <- "Centre/Right"
  list(position = pos, position_side = side)
}

#' Each player's position in each match (the `match_` level)
#'
#' One row per lineup row, with the position he played in that match in every
#' grouping and where it came from:
#'
#' - `opta`: a starter with an Opta position and side.
#' - `opta_sub`: a substitute, from the position Opta records on his sub-on
#'   event (qualifiers 292 and 293; on 96-100% of sub-ons from 2015).
#'   Measured 2026-10-19 on 101,484 subs, this matches where he touched the
#'   ball as often as his usual position does (39.3% vs 39.6%) and more often
#'   than his slot (31.8%): the slot is the departing player's.
#' - `slot`: a substitute with no Opta sub position, or a starter with a blank
#'   label, who took a slot
#'   whose kick-off holder in this match had a label, with the formation
#'   unchanged since kick-off. He gets that holder's position. Opta records the
#'   slot on every sub-on event (qualifier 145).
#' - `slot_table`: the formation had changed, or the slot's holder had no
#'   label, so the position is the most common one for that formation and slot
#'   ([position_slot_table()]).
#' - `none`: he played but no slot is known (no event data for the match).
#'   Every role column is `NA`; fill from the `asof_` level where a value is
#'   needed.
#' - `unused`: an unused substitute. Every role column is `NA`.
#'
#' A player moved by a later formation change keeps the position he started
#' the match (or came on) in. `match_slot_role_full` keeps the position the
#' slot implies (for a sub, the departing player's), whatever the source.
#'
#' @param lineups Lineup rows with `match_id`, `team_id`, `player_id`,
#'   `position`, `position_side`, `is_starter`, `minutes_played`.
#' @inheritParams position_slot_table
#' @param slot_table A [position_slot_table()] result, or `NULL` to build one
#'   from `lineups` and `events`. Pass a fixed table when scoring a small
#'   subset of matches.
#' @return A data.table with `match_id`, `team_id`, `player_id`,
#'   `match_is_starter`, `match_formation`, `match_slot`,
#'   `match_slot_role_full`, `match_role_full`,
#'   `match_role16`, `match_role8`, `match_role4`, `match_role_source`, in the
#'   row order of `lineups`.
#' @family positions
#' @export
position_match <- function(lineups, events, slot_table = NULL) {
  need <- c("match_id", "team_id", "player_id", "position", "position_side",
            "is_starter", "minutes_played")
  miss <- setdiff(need, names(lineups))
  if (length(miss)) {
    cli::cli_abort("{.arg lineups} is missing column{?s} {.field {miss}}.")
  }
  ev <- .prepare_position_events(events)
  if (is.null(slot_table)) slot_table <- position_slot_table(lineups, events)
  slot_table <- data.table::as.data.table(slot_table)[, .(formation, slot, tbl_role_full = role_full)]

  lu <- data.table::as.data.table(lineups)[, .(
    .i = .I, match_id = as.character(match_id), team_id = as.character(team_id),
    player_id = as.character(player_id), position = as.character(position),
    position_side = as.character(position_side),
    match_is_starter = is_starter %in% TRUE,
    mins = suppressWarnings(as.numeric(minutes_played)))]
  lu[, own_full := position_roles(position, position_side)$role_full]

  fs <- .formation_states(ev)
  ko_form <- fs$states[is_kickoff == TRUE, .(ko_formation = formation[1L]), by = .(match_id, team_id)]
  ko_slot <- fs$players[is.infinite(state_t), .(slot = slot[1L]), by = .(match_id, team_id, player_id)]
  # The labelled kick-off holder of each slot: who a later sub inherits from.
  holder <- ko_slot[lu[match_is_starter == TRUE & !is.na(own_full), .(match_id, player_id, own_full)],
                    on = c("match_id", "player_id"), nomatch = NULL][
    , .(holder_full = own_full[1L]), by = .(match_id, team_id, slot)]

  # Sub-on events: his slot (qualifier 145) and the formation in effect then.
  on <- ev[type_id == 19L, .(match_id, team_id, player_id, on_t = state_t,
                             sub_slot = suppressWarnings(as.integer(.opta_qualifier(qualifier_json, "145"))),
                             q292 = .opta_qualifier(qualifier_json, "292"),
                             q293 = .opta_qualifier(qualifier_json, "293"))]
  on <- unique(on, by = c("match_id", "player_id"))
  sp <- .sub_position_from_qualifiers(on$q292, on$q293)
  on[, sub_full := position_roles(sp$position, sp$position_side)$role_full]
  st <- fs$states[!is.na(formation), .(match_id, team_id, on_t = state_t, formation)]
  data.table::setkey(st, match_id, team_id, on_t)
  # The shape in effect when he came on: the latest state at or before then
  # (a formation change logged in the same second as the sub counts, since the
  # sub's slot is numbered in the new shape).
  on[, sub_formation := st[on, formation, on = c("match_id", "team_id", "on_t"),
                           roll = TRUE, mult = "last"]]

  lu[, `:=`(slot = NA_integer_, formation = NA_integer_, ko_f = NA_integer_,
            sub_slot = NA_integer_, sub_formation = NA_integer_, came_on = FALSE,
            holder_full = NA_character_, tbl_full = NA_character_, sub_full = NA_character_)]
  lu[ko_form, ko_f := i.ko_formation, on = c("match_id", "team_id")]
  lu[ko_slot, slot := i.slot, on = c("match_id", "team_id", "player_id")]
  lu[on, `:=`(sub_slot = i.sub_slot, sub_formation = i.sub_formation, sub_full = i.sub_full, came_on = TRUE),
     on = c("match_id", "team_id", "player_id")]
  lu[match_is_starter == TRUE, formation := ko_f]
  lu[match_is_starter == FALSE, `:=`(slot = sub_slot, formation = sub_formation)]
  lu[holder, holder_full := i.holder_full, on = c("match_id", "team_id", "slot")]
  lu[slot_table, tbl_full := i.tbl_role_full, on = c("formation", "slot")]

  played <- lu$match_is_starter | lu$came_on | (!is.na(lu$mins) & lu$mins > 0)
  same_shape <- !is.na(lu$formation) & !is.na(lu$ko_f) & lu$formation == lu$ko_f
  lu[, match_role_source := data.table::fcase(
    !played, "unused",
    match_is_starter & !is.na(own_full), "opta",
    !match_is_starter & !is.na(sub_full), "opta_sub",
    !is.na(slot) & same_shape & !is.na(holder_full), "slot",
    !is.na(slot) & !is.na(tbl_full), "slot_table",
    default = "none")]
  lu[, match_role_full := data.table::fcase(
    match_role_source == "opta", own_full,
    match_role_source == "opta_sub", sub_full,
    match_role_source == "slot", holder_full,
    match_role_source == "slot_table", tbl_full,
    default = NA_character_)]

  lu[, match_slot_role_full := data.table::fifelse(
    !is.na(slot) & same_shape & !is.na(holder_full), holder_full, tbl_full)]
  lu[is.na(slot), match_slot_role_full := NA_character_]

  m <- POSITION_ROLE_MAP[, .(role16 = role16[1L]), by = role_full]
  lu[, match_role16 := NA_character_]
  lu[m, match_role16 := i.role16, on = c(match_role_full = "role_full")]
  lu[, match_role8 := data.table::fifelse(is.na(match_role16), NA_character_, .role16_to_role8(match_role16))]
  lu[, match_role4 := data.table::fifelse(is.na(match_role16), NA_character_, .role16_to_broad(match_role16))]
  data.table::setorder(lu, .i)
  lu[, .(match_id, team_id, player_id, match_is_starter,
         match_formation = formation, match_slot = slot, match_slot_role_full,
         match_role_full, match_role16, match_role8, match_role4, match_role_source)]
}

# Level order per grouping: ties in a most-minutes vote go to the first code.
.POSITION_LEVELS <- list(
  role_full = unique(POSITION_ROLE_MAP$role_full),
  role16 = c("GK", "CB", "LB", "RB", "LWB", "RWB", "DM", "CM", "LM", "RM",
             "CAM", "LW", "RW", "CF", "LF", "RF"),
  role8 = c("GK", "CB", "FB", "DM", "CM", "AM", "W", "ST"),  # PSR_ROLE8_LEVELS (psr.R loads after this file; a test pins them equal)
  role4 = c("GK", "DEF", "MID", "FWD"))

# The match_ sources that count as a real position for a vote. A value filled
# in from the asof_ level ("none") never votes for itself.
.POSITION_VOTING_SOURCES <- c("opta", "opta_sub", "slot", "slot_table")

#' Each player's usual position before each match (the `asof_` level)
#'
#' For every row, the position he played most minutes in over matches on
#' EARLIER dates only: never the same day, never later, so a 2016 row is not
#' decided by 2024 minutes. The vote uses the `window_days` before the match
#' (default 365, as `.role8_asof()` does for PSR) and falls back to his whole
#' earlier career when that window is empty (back from a long injury). Each
#' grouping is voted on separately, so `asof_role8` is his most common role8,
#' not a coarsening of his most common `role_full`. Ties go to the first code
#' in level order.
#'
#' Only rows whose `match_role_source` is a real position vote ("opta",
#' "opta_sub", "slot", "slot_table"), weighted by minutes.
#'
#' @param player_match One row per player-match: `player_id`, `match_date`,
#'   `minutes`, `match_role_full`, `match_role_source` (e.g.
#'   [position_match()] output with date and minutes joined on).
#' @param window_days Trailing window in days; `Inf` for the whole earlier
#'   career.
#' @return A data.table in the row order of `player_match` with
#'   `asof_role_full`, `asof_role16`, `asof_role8`, `asof_role4` and
#'   `asof_role_source`: "window" (voted in the trailing window), "all_prior"
#'   (window empty, whole earlier career used) or "none" (no earlier
#'   position; every role column `NA`).
#' @family positions
#' @export
position_asof <- function(player_match, window_days = 365L) {
  need <- c("player_id", "match_date", "minutes", "match_role_full", "match_role_source")
  miss <- setdiff(need, names(player_match))
  if (length(miss)) {
    cli::cli_abort("{.arg player_match} is missing column{?s} {.field {miss}}.")
  }
  d <- data.table::data.table(
    .i = seq_len(nrow(player_match)),
    player_id = as.character(player_match$player_id),
    md = as.integer(as.Date(player_match$match_date)),
    m = suppressWarnings(as.numeric(player_match$minutes)),
    full = as.character(player_match$match_role_full),
    src = as.character(player_match$match_role_source))
  .abort_na_player_id(d$player_id)
  if (anyNA(d$md)) {
    cli::cli_abort("{.arg player_match} has {sum(is.na(d$md))} row{?s} with no {.field match_date}.")
  }
  m16 <- POSITION_ROLE_MAP[, .(role16 = role16[1L]), by = role_full]
  d[, r16 := m16$role16[match(full, m16$role_full)]]
  d[, `:=`(r8 = data.table::fifelse(is.na(r16), NA_character_, .role16_to_role8(r16)),
           r4 = data.table::fifelse(is.na(r16), NA_character_, .role16_to_broad(r16)))]
  # only codes from the shared map vote; anything else would sort ahead of real
  # codes in a tie (its level is NA)
  votes <- d[src %in% .POSITION_VOTING_SOURCES & full %in% .POSITION_LEVELS$role_full &
               !is.na(m) & m > 0]
  queries <- unique(d[, .(player_id, md)])
  lookback <- if (is.finite(window_days)) as.integer(window_days) else NA_integer_

  vote_one <- function(col, levels) {
    v <- data.table::data.table(player_id = votes$player_id, code = votes[[col]],
                                md = votes$md, m = votes$m)[, .(m = sum(m)), by = .(player_id, code, md)]
    data.table::setorder(v, player_id, code, md)
    v[, cs := cumsum(m), by = .(player_id, code)]
    # every query date x every code that player has ever voted for
    pc <- unique(v[, .(player_id, code)])
    q <- pc[queries, on = "player_id", allow.cartesian = TRUE, nomatch = NULL]
    data.table::setkey(v, player_id, code, md)
    # minutes up to the day BEFORE the match, and up to the day before the window
    q[, before := v[.(q$player_id, q$code, q$md - 1L), cs, roll = TRUE]]
    q[, win_start := if (is.na(lookback)) NA_real_ else
        v[.(q$player_id, q$code, q$md - 1L - lookback), cs, roll = TRUE]]
    q[is.na(before), before := 0]
    q[is.na(win_start), win_start := 0]
    q[, win := if (is.na(lookback)) before else before - win_start]
    q[, lev := match(code, levels)]
    tot <- q[, .(win_tot = sum(win), all_tot = sum(before)), by = .(player_id, md)]
    q[tot, `:=`(win_tot = i.win_tot, all_tot = i.all_tot), on = c("player_id", "md")]
    q[, score := data.table::fifelse(win_tot > 0, win, before)]
    data.table::setorder(q, player_id, md, -score, lev)
    best <- q[score > 0, .(code = code[1L]), by = .(player_id, md)]
    list(best = best, tot = tot)
  }

  res <- list()
  for (g in names(.POSITION_LEVELS)) {
    col <- c(role_full = "full", role16 = "r16", role8 = "r8", role4 = "r4")[[g]]
    res[[g]] <- vote_one(col, .POSITION_LEVELS[[g]])
  }
  out <- d[, .(.i, player_id, md)]
  for (g in names(res)) {
    out[res[[g]]$best, (paste0("asof_", g)) := i.code, on = c("player_id", "md")]
    if (!paste0("asof_", g) %in% names(out)) out[, (paste0("asof_", g)) := NA_character_]
  }
  tot <- res$role_full$tot
  out[tot, `:=`(.win = i.win_tot, .all = i.all_tot), on = c("player_id", "md")]
  if (!".win" %in% names(out)) out[, `:=`(.win = NA_real_, .all = NA_real_)]
  out[, asof_role_source := data.table::fcase(
    .win > 0, "window", .all > 0, "all_prior", default = "none")]
  data.table::setorder(out, .i)
  out[, .(asof_role_full, asof_role16, asof_role8, asof_role4, asof_role_source)]
}

# Most-minutes code per group for every grouping: one row per `by` group with
# role_full / role16 / role8 / role4, ties to the first code in level order.
.position_vote_by <- function(d, by) {
  votes <- d[src %in% .POSITION_VOTING_SOURCES & full %in% .POSITION_LEVELS$role_full &
               !is.na(m) & m > 0]
  keys <- unique(d[, by, with = FALSE])
  for (g in names(.POSITION_LEVELS)) {
    col <- c(role_full = "full", role16 = "r16", role8 = "r8", role4 = "r4")[[g]]
    v <- data.table::data.table(votes[, by, with = FALSE], code = votes[[col]], m = votes$m)
    v <- v[, .(m = sum(m)), by = c(by, "code")]
    v[, lev := match(code, .POSITION_LEVELS[[g]])]
    data.table::setorderv(v, c(by, "m", "lev"), c(rep(1L, length(by)), -1L, 1L))
    best <- v[, .(code = code[1L]), by = by]
    keys[best, (g) := i.code, on = by]
    if (!g %in% names(keys)) keys[, (g) := NA_character_]
  }
  keys[]
}

# The shared front half of position_season() / position_career().
.position_frame <- function(player_match, need) {
  miss <- setdiff(need, names(player_match))
  if (length(miss)) {
    cli::cli_abort("{.arg player_match} is missing column{?s} {.field {miss}}.")
  }
  d <- data.table::data.table(
    .i = seq_len(nrow(player_match)),
    player_id = as.character(player_match$player_id),
    m = suppressWarnings(as.numeric(player_match$minutes)),
    full = as.character(player_match$match_role_full),
    src = as.character(player_match$match_role_source))
  .abort_na_player_id(d$player_id)
  m16 <- POSITION_ROLE_MAP[, .(role16 = role16[1L]), by = role_full]
  d[, r16 := m16$role16[match(full, m16$role_full)]]
  d[, `:=`(r8 = data.table::fifelse(is.na(r16), NA_character_, .role16_to_role8(r16)),
           r4 = data.table::fifelse(is.na(r16), NA_character_, .role16_to_broad(r16)))]
  d
}

# Rows with no player id would be pooled as one "player" and vote for each other.
.abort_na_player_id <- function(player_id) {
  bad <- is.na(player_id) | !nzchar(player_id)
  if (any(bad)) {
    cli::cli_abort("{sum(bad)} row{?s} of {.arg player_match} ha{?s/ve} no {.field player_id}.")
  }
}

#' Each player's position over his whole career (the `career_` level)
#'
#' The position he played most minutes in across every match passed in,
#' voted separately for each grouping, ties to the first code in level order.
#' Uses later matches by design: for player pages and displays, never as a
#' model input (use [position_asof()] there). Votes as [position_asof()] does:
#' only rows whose `match_role_source` is a real position, weighted by
#' minutes.
#'
#' @param player_match One row per player-match: `player_id`, `minutes`,
#'   `match_role_full`, `match_role_source`.
#' @return A data.table in the row order of `player_match` with
#'   `career_role_full`, `career_role16`, `career_role8`, `career_role4` and
#'   `career_role_source` ("career", or "none" with every role `NA`).
#' @family positions
#' @export
position_career <- function(player_match) {
  d <- .position_frame(player_match, c("player_id", "minutes", "match_role_full", "match_role_source"))
  cv <- .position_vote_by(d, "player_id")
  out <- cv[d[, .(.i, player_id)], on = "player_id"]
  data.table::setorder(out, .i)
  out[, .(career_role_full = role_full, career_role16 = role16, career_role8 = role8,
          career_role4 = role4,
          career_role_source = data.table::fifelse(is.na(role_full), "none", "career"))]
}

#' Each player's position over each season (the `season_` level)
#'
#' The position he played most minutes in across the season, voted as
#' [position_career()] does, falling back to his career position for a
#' season with no real position (a season of unused-sub appearances). A
#' season is the season END year ([extract_season_end_year()]), so a
#' calendar-year league and a European season ending the same year are one
#' season. Uses the whole season by design: season tables and pages only,
#' never a model input.
#'
#' @param player_match One row per player-match: `player_id`, `season`,
#'   `minutes`, `match_role_full`, `match_role_source`.
#' @return A data.table in the row order of `player_match` with
#'   `season_role_full`, `season_role16`, `season_role8`, `season_role4` and
#'   `season_role_source` ("season", "career" or "none").
#' @family positions
#' @export
position_season <- function(player_match) {
  d <- .position_frame(player_match, c("player_id", "season", "minutes", "match_role_full", "match_role_source"))
  d[, sey := as.integer(extract_season_end_year(player_match$season))]
  if (anyNA(d$sey)) {
    cli::cli_abort("{sum(is.na(d$sey))} row{?s} of {.arg player_match} ha{?s/ve} a {.field season} with no end year.")
  }
  sv <- .position_vote_by(d, c("player_id", "sey"))
  cv <- .position_vote_by(d, "player_id")
  out <- sv[d[, .(.i, player_id, sey)], on = c("player_id", "sey")]
  out[cv, paste0("c_", names(.POSITION_LEVELS)) := mget(paste0("i.", names(.POSITION_LEVELS))), on = "player_id"]
  out[, season_role_source := data.table::fcase(!is.na(role_full), "season",
                                                !is.na(c_role_full), "career", default = "none")]
  for (g in names(.POSITION_LEVELS)) {
    out[season_role_source == "career", (g) := get(paste0("c_", g))]
  }
  data.table::setorder(out, .i)
  out[, .(season_role_full = role_full, season_role16 = role16, season_role8 = role8,
          season_role4 = role4, season_role_source)]
}
