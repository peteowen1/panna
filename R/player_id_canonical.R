# Player-ID canonicalization
#
# Some players appear with multiple Opta player_ids -- typically a data-entry
# artifact when matches are ingested from different sources or before/after
# a transfer record gets reconciled. The dominant ID has the bulk of the
# player's history; the alt-IDs are sparse fragments.
#
# `build_player_id_canonical_map()` produces a safe lookup: alt_id -> main_id.
# A merge fires ONLY when all three are true:
#   1. Same accent-normalized name
#   2. Alt has < `min_dominance_ratio` * main's appearance count
#   3. Their team sets overlap (so we don't merge two unrelated namesakes
#      who happen to share a name)
#
# Genuine namesakes (e.g. several different "Danilo"s with disjoint clubs)
# are kept separate. Distinct players with identical names but similar
# career-volume are NEVER merged.

#' Build a player_id -> canonical_id lookup
#'
#' @param lineups Data.table of lineups (full or filtered).
#' @param min_dominance_ratio Numeric. Alt must have at most this fraction
#'   of the main's appearance count to be a merge candidate. Default 0.05
#'   (alt <= 5% of main). Catches data-entry errors (alt has 2-10 matches
#'   vs main has hundreds) without merging mid-tier namesakes.
#' @param require_team_overlap Logical. Require at least one common team
#'   between alt and main. Default TRUE. Catches the "two Danilos played
#'   for different national teams" namesake case.
#' @return Data.table with columns `player_id` (every observed id) and
#'   `canonical_id` (its main mapping; equals `player_id` for non-merged
#'   dominant ids).
#' @export
build_player_id_canonical_map <- function(lineups,
                                            min_dominance_ratio = 0.05,
                                            require_team_overlap = TRUE) {
  if (!data.table::is.data.table(lineups)) lineups <- data.table::as.data.table(lineups)

  ## Accent-normalize names for grouping
  if (!requireNamespace("stringi", quietly = TRUE))
    stop("Package 'stringi' required for accent normalization.")
  lu_meta <- unique(lineups[, .(player_id, player_name, team_name)])
  lu_meta[, name_norm := tolower(trimws(gsub("[-\u036f]", "",
                                                stringi::stri_trans_nfd(player_name))))]

  ## Appearance count per id
  appear <- lineups[, .(n_apps = .N), by = player_id]

  ## Each id's set of teams
  team_sets <- lineups[, .(teams = list(unique(team_name))), by = player_id]

  ## Roll up to canonical lookup, candidate by candidate
  ids_per_name <- lu_meta[, .(player_id = unique(player_id)), by = name_norm]
  groups <- ids_per_name[, .N, by = name_norm][N > 1, name_norm]

  canonical <- data.table::data.table(player_id = unique(lineups$player_id),
                                       canonical_id = unique(lineups$player_id))
  setkey(canonical, player_id)

  n_merged <- 0L
  for (nm in groups) {
    ids <- lu_meta[name_norm == nm, unique(player_id)]
    apps <- appear[player_id %in% ids][order(-n_apps)]
    if (nrow(apps) < 2L) next
    main_id <- apps$player_id[1]
    main_n  <- apps$n_apps[1]
    main_teams <- team_sets[player_id == main_id, teams][[1]]

    for (i in seq.int(2L, nrow(apps))) {
      alt_id <- apps$player_id[i]
      alt_n  <- apps$n_apps[i]
      if (alt_n / main_n > min_dominance_ratio) next  # too big to be artifact
      if (require_team_overlap) {
        alt_teams <- team_sets[player_id == alt_id, teams][[1]]
        if (length(intersect(main_teams, alt_teams)) == 0L) next
      }
      canonical[player_id == alt_id, canonical_id := main_id]
      n_merged <- n_merged + 1L
    }
  }

  attr(canonical, "n_merged") <- n_merged
  attr(canonical, "n_groups_checked") <- length(groups)
  canonical
}

#' Apply canonical-id mapping to a data.table that has a player_id column
#'
#' Replaces `player_id` with the canonical mapping in-place. Useful before
#' joining lineups to ratings, where the same person should map to a single
#' xRAPM row regardless of which alt-id their match used.
#'
#' @param dt Data.table with a player_id column.
#' @param canon Output of `build_player_id_canonical_map()`.
#' @return The input dt with `player_id` rewritten to canonical.
#' @export
apply_canonical_player_ids <- function(dt, canon) {
  if (!data.table::is.data.table(dt)) dt <- data.table::as.data.table(dt)
  ## Hash-style lookup
  m <- canon$canonical_id[match(dt$player_id, canon$player_id)]
  ## Anything not in the lookup keeps its original id
  m[is.na(m)] <- dt$player_id[is.na(m)]
  dt[, player_id := m]
  dt
}
