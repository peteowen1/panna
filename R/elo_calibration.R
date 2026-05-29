# Elo calibration helpers -- per-match-type K + cross-confederation multiplier.
#
# Motivation (2026-05-28): a single K=20 for all matches AND no
# cross-confederation tether produced absurd outputs like Uzbekistan
# (mostly AFC qualifier wins against weak opposition) > Norway (mostly
# UEFA qualifier results against strong opposition + a 4-1 over Italy).
# AFC teams climbed within an isolated pool because they never faced
# UEFA teams who would have shown them at their actual relative level.
#
# Two improvements together:
#
# 1. Per-match-type K. Conventional wisdom (FIFA / elorating.com):
#    tournament > qualifier > nations-league > friendly. Tournaments
#    matter most because (a) teams try harder, (b) outcomes are higher
#    information about real strength. K_club stays separate from the
#    international family since the domestic pipeline is a different
#    world.
#
# 2. Cross-confederation multiplier. When two teams from DIFFERENT
#    confederations meet (Uruguay vs Uzbekistan, Brazil vs Norway, etc.)
#    the result is unusually informative about cross-pool calibration.
#    Multiply K by `cross_conf_mult` (default 1.5) for these matches.
#    No effect on same-confederation matches.


#' Match-type K Lookup
#'
#' Maps a league code to a base Elo K-factor.
#'
#' Values OPTIMIZED via DEoptim v6 2026-05-29 using 3-fold CV with
#' 3-way logloss + Davidson draw model + venue factor + tunable decay.
#' v6 = v5 retrained on the expanded intl corpus (post-2026-05-29
#' scrapes: AFCON 2023 Cote d'Ivoire, AFC/CAF/CONMEBOL WCQ historical
#' cycles 2002-2014, etc., ~4,000 intl matches total). Decay halflife
#' converged at 7000 days (essentially "off" -- recent matches don't
#' need extra weight; the data set itself has enough recent signal).
#' Best CV-mean logloss = 0.9782 (vs v4 seed 1.0135, -3.49%).
#'
#' Pre-2026-05-29 values (in this file: WC=80, continental=80, qualifier=25,
#' friendly=5). v6 dropped WC + continental K substantially (80 -> 44 / 50)
#' and raised qualifier (25 -> 59) -- with WC 2022 + the extra qualifier
#' cycles in the training set, individual matches need to move Elos less
#' because the prior is better-anchored. Note: the 94/110/55 numbers that
#' appeared in some v3/v4 intermediate DEoptim seed comments are from
#' optimizer trial points, not from the production constants ever shipped
#' in this file.
#'
#' @format Named numeric vector: league code -> base K.
#' @keywords internal
ELO_MATCH_TYPE_K <- c(
  # International tournaments -- v6: K_wc=44, K_continental=50.
  # Lower than v3 (80) because the expanded training set (WC 2022 +
  # AFCON 2023 + many more qualifier cycles) reduces how much each
  # individual tournament match should move the Elo trajectory.
  WC     = 44,
  EURO   = 50,
  AFCON  = 50,
  COPA   = 50,
  GOLD   = 50,
  ACUP   = 50,
  GULF   = 50,
  # International qualifiers -- v6: K=59 (up slightly from v3's 25).
  # Increased because v6 has many more qualifier cycles in training (full
  # 2014/2010/2006 cycles for AFC/CAF/CONMEBOL after the May 29 scrape)
  # giving the optimizer more signal that qualifier results carry weight.
  WCQ_UEFA     = 59,
  WCQ_CONMEBOL = 59,
  WCQ_CAF      = 59,
  WCQ_AFC      = 59,
  EUROQ        = 59,
  AFCONQ       = 59,
  ACUPQ        = 59,
  # Nations League -- kept at 20 (semi-competitive, within-conf)
  NL = 20,
  # Friendlies -- v6: K=15. Higher than v3's 5 -- friendlies between
  # high-strength teams (often the only cross-conf signal between WCs)
  # turn out to carry calibration value once the cross_conf multiplier
  # scales them up.
  INTL_FR = 15,
  # Club leagues + continental cups -- kept at K=20 (untouched by the
  # intl optimization since they're a different world)
  ENG = 20, ENG2 = 20, ESP = 20, GER = 20, ITA = 20, FRA = 20,
  NED = 20, POR = 20, TUR = 20, SCO = 20, BEL = 20, BRA = 20,
  AUS = 20, TUN = 20, CAFCL = 20,
  UCL = 20, UEL = 20, UECL = 20
)

#' Default base K for any league not in `ELO_MATCH_TYPE_K`
#' @keywords internal
ELO_DEFAULT_K <- 20

#' Default cross-confederation K multiplier
#'
#' Multiply K by this factor when the two teams are from different
#' confederations. v6 optimized to 2.49 (was 1.5 in v3, 2.49 in v5/v6) --
#' cross-confederation matches are rare but high-information signal for
#' calibrating pools against each other; multiplying their K up means
#' the rare WC / friendly cross-conf matches drive most of the
#' confederation-vs-confederation Elo divergence. 1.0 disables.
#' @keywords internal
ELO_CROSS_CONF_MULT <- 2.49

#' Confederation Initial-Elo Priors
#'
#' Per-confederation starting Elo. Without this, every team starts at
#' 1500 and confederations only diverge via match results -- but with
#' few cross-conf matches per year, that divergence is slow and stays
#' biased toward whichever confederation has the most internal-pool
#' matches (= AFC pool drifts up because they play each other a lot).
#'
#' Confederation priors give each pool a sensible starting position
#' informed by historical World Cup performance. v6 values come directly
#' from the DEoptim optimization (no parametric spread anymore -- each
#' delta is tuned independently).
#'
#' @format Named numeric vector mapping confederation -> initial Elo.
#' @keywords internal
ELO_CONFEDERATION_PRIORS <- c(
  UEFA     = 1500,  # anchor
  CONMEBOL = 1519,  # v6 delta +19
  CONCACAF = 1471,  # v6 delta -29
  CAF      = 1331,  # v6 delta -169 (CAF much weaker than UEFA per cross-conf record)
  AFC      = 963,   # v6 delta -537 (AFC pool dominates own matches, weak cross-conf)
  OFC      = 1082   # v6 delta -418
)
# Values from DEoptim v6, 2026-05-29 (debug/optimize_elo_deoptim_v6.R).
# Improvement: -3.49% logloss vs v4 seed. v3's spread=200 framing
# (legacy) is replaced by independent per-conf deltas.

#' Default conf_spread (LEGACY; not used by v6+ optimizer which tunes
#' per-conf deltas independently). Kept for backwards-compat with the
#' parametric `elo_conf_priors_from_spread()` helper which v6 step 03
#' no longer calls. New callers should use `ELO_CONFEDERATION_PRIORS`
#' directly.
#' @keywords internal
ELO_CONF_SPREAD <- 200

#' Optimized home-advantage value (Elo points; v6 = 88, v3 = 65)
#' @keywords internal
ELO_HOME_ADV <- 88

#' Optimized Davidson draw parameter (v6 = 0.89)
#' @keywords internal
ELO_DAVIDSON_NU <- 0.89

#' Build Initial-Elo Vector With Confederation Priors
#'
#' Returns a named-vector starting Elo for each team. Teams whose
#' confederation is in `conf_priors` get the confederation's prior;
#' teams whose confederation is unknown (NA in lookup) get
#' `initial_elo` (default 1500).
#'
#' @param teams Character vector of team names.
#' @param conf_lookup Named character vector built by
#'   `build_team_confederations()`.
#' @param conf_priors Named numeric vector (confederation -> initial Elo).
#'   Default ELO_CONFEDERATION_PRIORS.
#' @param initial_elo Fallback for teams whose conf is unknown.
#' @return Named numeric vector -- same length as `teams` (after dropping
#'   NA names).
#' @keywords internal
init_team_elos_with_priors <- function(teams, conf_lookup,
                                         conf_priors = ELO_CONFEDERATION_PRIORS,
                                         initial_elo = 1500) {
  teams <- teams[!is.na(teams)]
  team_conf <- conf_lookup[teams]
  elos <- ifelse(is.na(team_conf), initial_elo,
                 unname(conf_priors[team_conf]))
  # If a team's conf IS in lookup but conf NOT in priors (shouldn't
  # happen but defend), fall back to initial_elo
  elos[is.na(elos)] <- initial_elo
  names(elos) <- teams
  elos
}

# =============================================================================
# Tournament Host Extraction + Venue Factor
# =============================================================================
# Most tournament matches are at a host country, NOT at the designated
# home_team's real stadium. Opta assigns one team as `home_team` per
# match for scheduling -- that team gets the +65 home advantage in
# compute_match_elos even when the match is at a neutral venue or in
# the OPPONENT's country (when the opponent is the host).
#
# venue_factor scales home_advantage per-match:
#   +1   home_team is at home (domestic league, or tournament where they host)
#    0   neutral (tournament match where neither team is host)
#   -1   away_team is the host (rare -- home_team is visiting host's country)
#
# Hosts are extracted from the season string, which already encodes them:
#   "2025 Morocco" -> Morocco
#   "2026 Canada-Mexico-USA" -> Canada, Mexico, USA
#   "2024 Germany" -> Germany
#   "Intl_Friendlies_2024" -> no host (friendlies, treat as +1)
#   "2024-2025" -> no host (domestic, treat as +1)

#' Map Host-Name Aliases to Canonical Opta team_name
#'
#' Tournament season strings encode the host country (e.g., "2024 USA")
#' but the host may appear under a different spelling in opta_lineups
#' (here "United States"). This map normalises common aliases.
#' @keywords internal
ELO_HOST_NAME_ALIASES <- c(
  "USA"                = "United States",
  "Korea Rep"          = "Korea Republic",
  "United States"      = "United States",
  "Cote d'Ivoire"      = "C\u00f4te d\'Ivoire",
  "Cote d Ivoire"      = "C\u00f4te d\'Ivoire",
  "C\u00f4te d\'Ivoire" = "C\u00f4te d\'Ivoire"
)

#' Extract Tournament Host(s) From a Season String
#'
#' @param season Single season string.
#' @return Character vector of host team names (Opta-canonical). Empty
#'   character() if season has no host concept (domestic / friendlies).
#' @keywords internal
extract_tournament_hosts <- function(season) {
  if (is.na(season) || !nzchar(season)) return(character(0))
  # Domestic season "YYYY-YYYY" -> no host
  if (grepl("^\\d{4}-\\d{4}$", season)) return(character(0))
  # Friendlies-style "Intl_Friendlies_YYYY" -> no host
  if (grepl("Friendlies", season, ignore.case = TRUE)) return(character(0))
  # Tournament: strip leading 4-digit year + whitespace
  rest <- trimws(sub("^\\d{4}\\s*", "", season))
  if (!nzchar(rest)) return(character(0))

  hosts <- if (grepl(" - ", rest, fixed = TRUE)) {
    # Multi-host with " - " separator: "2019 USA - Costa Rica - Jamaica"
    trimws(strsplit(rest, " - ", fixed = TRUE)[[1]])
  } else if (grepl("-", rest, fixed = TRUE) && !grepl("'", rest)) {
    # Multi-host with "-" separator (no spaces): "Canada-Mexico-USA"
    # Skip if there's an apostrophe (Cote d'Ivoire etc.)
    parts <- trimws(strsplit(rest, "-", fixed = TRUE)[[1]])
    if (length(parts) > 1L && all(nchar(parts) > 1L)) parts else rest
  } else {
    rest
  }

  # Normalize via alias map
  ifelse(hosts %in% names(ELO_HOST_NAME_ALIASES),
         unname(ELO_HOST_NAME_ALIASES[hosts]),
         hosts)
}

#' Compute Per-Match Venue Factor
#'
#' Returns a numeric vector (+1 / 0 / -1) matching the length of
#' `home_team`/`away_team`. See file header for the convention.
#'
#' Domestic leagues + UCL/UEL/UECL: always +1 (home team at home).
#' Qualifiers (WCQ_* / EUROQ / AFCONQ / ACUPQ) + Nations League: +1
#' (these are scheduled home/away at real stadiums).
#' Intl_Friendlies: default +1 (we don't know venue; friendlies are
#' usually at the home_team's stadium but can be neutral).
#' Tournament matches: parse host from season; +1 if home_team is host,
#' -1 if away_team is host, 0 if neither.
#'
#' @param home_team,away_team,league,season Vectors of equal length.
#' @return Numeric vector of -1 / 0 / +1.
#' @keywords internal
compute_venue_factor <- function(home_team, away_team, league, season) {
  n <- length(home_team)
  vf <- rep(1, n)  # default: home_team at real home

  tournament_leagues <- c("WC", "EURO", "AFCON", "COPA", "GOLD",
                          "ACUP", "GULF", "Club_World_Cup", "UEFA_Super_Cup")
  is_tournament <- league %in% tournament_leagues
  if (!any(is_tournament)) return(vf)

  # For tournament matches: extract hosts once per unique (league, season)
  ls_keys <- paste(league, season, sep = "||")
  unique_keys <- unique(ls_keys[is_tournament])
  host_lookup <- setNames(vector("list", length(unique_keys)), unique_keys)
  for (k in unique_keys) {
    s <- sub("^.+\\|\\|", "", k)
    host_lookup[[k]] <- extract_tournament_hosts(s)
  }

  for (i in which(is_tournament)) {
    hosts <- host_lookup[[ls_keys[i]]]
    if (length(hosts) == 0L) {
      vf[i] <- 0  # tournament season with no parseable host -> assume neutral
      next
    }
    home_is_host <- home_team[i] %in% hosts
    away_is_host <- away_team[i] %in% hosts
    vf[i] <- if (home_is_host && !away_is_host) 1
             else if (!home_is_host && away_is_host) -1
             else if (home_is_host && away_is_host) 1  # both host -- treat as home
             else 0  # neither host -- neutral
  }
  vf
}


#' Scale Confederation Priors by a Single Spread Parameter
#'
#' Given a "spread" value, returns confederation priors centered on
#' 1500 with offsets proportional to `spread`. Used by the optimizer to
#' search over a single 1-dim parameter instead of 5-6 conf-specific Elos.
#'
#' Offset ratios (relative to spread):
#'   UEFA     = +1.0 x spread
#'   CONMEBOL = +1.0 x spread
#'   CONCACAF = -0.5 x spread
#'   CAF      = -0.5 x spread
#'   AFC      = -0.75 x spread
#'   OFC      = -1.5 x spread
#'
#' At spread = 100, that's UEFA=1600, CONMEBOL=1600, CONCACAF=1450,
#' CAF=1450, AFC=1425, OFC=1350. At spread = 0, every confederation
#' starts at 1500 (no prior).
#'
#' @keywords internal
elo_conf_priors_from_spread <- function(spread) {
  ratios <- c(UEFA = 1.0, CONMEBOL = 1.0, CONCACAF = -0.5,
              CAF = -0.5, AFC = -0.75, OFC = -1.5)
  1500 + spread * ratios
}


#' Look Up Base K for a Match
#'
#' @param league Character vector of league codes.
#' @param k_table Named numeric vector mapping league -> base K. Defaults
#'   to ELO_MATCH_TYPE_K. Pass a different vector to override per-match
#'   (e.g., for the optimization grid search).
#' @param default Base K for any league not in `k_table`. Defaults to
#'   ELO_DEFAULT_K.
#' @return Numeric vector of base K values, same length as `league`.
#' @keywords internal
elo_match_k <- function(league, k_table = ELO_MATCH_TYPE_K,
                         default = ELO_DEFAULT_K) {
  k <- unname(k_table[league])
  k[is.na(k)] <- default
  k
}


#' Build Team -> Confederation Lookup From Played Matches
#'
#' Each WC2026-era confederation has a unique qualifying competition
#' code. We use that as a stable identifier: a team is in confederation X
#' iff they've appeared in X's qualifiers / continental tournament in our
#' data. Returns a named character vector (team_name -> confederation).
#'
#' Teams that never appear in any confederation-coded competition
#' (extremely rare for nations actively playing intl football) get NA
#' and the cross_conf_mult treats them as "unknown" (multiplier = 1).
#'
#' @param played Data frame with `league`, `home_team`, `away_team`
#'   columns. Typically `fixture_results[match_status == "Played", ]`.
#' @return Named character vector -- names are team names, values are
#'   confederation codes ("UEFA", "CONMEBOL", "CAF", "AFC", "CONCACAF",
#'   or "OFC").
#' @keywords internal
build_team_confederations <- function(played) {
  # Map of confederation-defining competitions
  conf_leagues <- list(
    UEFA     = c("WCQ_UEFA", "EUROQ", "EURO", "NL"),
    CONMEBOL = c("WCQ_CONMEBOL", "COPA"),
    CAF      = c("WCQ_CAF", "AFCON", "AFCONQ"),
    AFC      = c("WCQ_AFC", "ACUP", "ACUPQ", "GULF"),
    CONCACAF = c("GOLD"),
    OFC      = c()  # OFC has no codes in our data; New Zealand may
                    # fall here unrecognised -- handled by NA default
  )

  # For each team, gather the set of confederations they've appeared in.
  # Conflicts (a team in two pools) shouldn't happen in real intl football
  # but if they did we'd pick the most-frequent confederation.
  team_conf_counts <- list()
  for (conf in names(conf_leagues)) {
    leagues <- conf_leagues[[conf]]
    if (length(leagues) == 0L) next
    rows <- played[played$league %in% leagues, , drop = FALSE]
    if (nrow(rows) == 0L) next
    teams <- unique(c(rows$home_team, rows$away_team))
    teams <- teams[!is.na(teams)]
    for (t in teams) {
      n <- sum(rows$home_team == t, na.rm = TRUE) +
           sum(rows$away_team == t, na.rm = TRUE)
      team_conf_counts[[t]][[conf]] <- (team_conf_counts[[t]][[conf]] %||% 0L) + n
    }
  }

  # Pick the most-frequent confederation per team
  team_conf <- vapply(team_conf_counts, function(counts) {
    names(counts)[which.max(unlist(counts))]
  }, character(1))
  team_conf
}


# `%||%` is imported from rlang via panna-package.R (single source).

#' Compute Cross-Confederation Multiplier for a Match
#'
#' @param home_team,away_team Team names.
#' @param conf_lookup Named character vector built by
#'   `build_team_confederations()`.
#' @param mult Multiplier when the two teams are from different
#'   confederations. Default ELO_CROSS_CONF_MULT (1.5).
#' @return 1.0 if same-conf or either team's conf is unknown, else `mult`.
#' @keywords internal
cross_conf_multiplier <- function(home_team, away_team, conf_lookup,
                                    mult = ELO_CROSS_CONF_MULT) {
  c1 <- conf_lookup[home_team]
  c2 <- conf_lookup[away_team]
  if (is.na(c1) || is.na(c2)) return(1.0)
  if (c1 == c2) return(1.0)
  mult
}
