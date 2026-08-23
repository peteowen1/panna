# 12d_export_domestic_team_strength.R
# Export Tiento (the composite team rating) for EVERY club panna tracks --
# not just the World Cup 48. Domestic sibling of 12_export_wc2026_blog.R
# section 5 (panna#193): the blog's Team Ratings page currently shows the sum
# of the top-20 players' panna, not Tiento, because Tiento's inputs (squad EM,
# Elo as a team property) had never been pointed at club football. This step
# is that wiring.
#
# Output:
#   team_strength.parquet -- one row per team, ALL teams appearing in the
#   current fixture window (domestic-league clubs AND cup-only clubs from
#   leagues panna does not scrape, e.g. Champions League opponents), columns:
#     team, league, panna, offense, defense, epr, psr, elo, tiento, rank_tiento,
#     squad_n, n_rated, elo_seeded, is_domestic_league, build_id
#
# Squad aggregation mirrors 12_export_wc2026_blog.R section 5 EXACTLY (same
# minutes-weighted formula, same rating sources) so a domestic Tiento means the
# same thing as a WC Tiento. The one genuinely new piece is Elo: WC pulls it
# from the match dataset because every WC team already has qualifying-cycle
# history. Domestic football has real promotion/relegation, so some teams have
# NO tracked history at all (promoted from a division panna doesn't scrape) --
# see the Elo-seeding section below.
#
# Inputs:
#   01_fixture_results.rds        -- league/season/team roster + full match
#                                     history (for Elo re-derivation)
#   opta_lineups.parquet          -- squad EM source (ALL competitions; this
#                                     is domestic, so international_only=FALSE)
#   career_panna.parquet, opta_epr_weekly.parquet, cache-skills/06 or
#   cache-opta/07 seasonal ratings -- same rating sources section 5 reads

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all()   # PANNA_LEAGUE_GROUPS, ELO_MATCH_TYPE_K, etc.

# Config-override pattern (see panna/CLAUDE.md "Config override pattern"):
# tests set these before sourcing to shrink the scope to a couple of fake
# leagues instead of the full 14 domestic + 4 continental codes.
if (!exists("domestic_codes")) {
  domestic_codes <- c(PANNA_LEAGUE_GROUPS$domestic, PANNA_LEAGUE_GROUPS$calendar)
}
if (!exists("cup_codes")) cup_codes <- PANNA_LEAGUE_GROUPS$continental
if (!exists("as_of_domestic")) as_of_domestic <- Sys.Date()
# Below this, a squad is not a small roster -- the squad join found nothing.
# Since the join is keyed on team_id (section 4), that means opta_lineups has
# no rows for this club at all, not a spelling mismatch: a club promoted from
# a division panna does not scrape has an Elo seed but no players. 26-man cap
# makes even a genuinely thin squad clear the bar comfortably.
if (!exists("MIN_PLAUSIBLE_SQUAD_N")) MIN_PLAUSIBLE_SQUAD_N <- 5L

TIENTO_WEIGHTS <- c(panna = 0.40, epr = 0.20, elo = 0.30, psr = 0.10)  # DO NOT re-fit; 12_export_wc2026_blog.R:359 is the one derivation

message("\n=== Exporting domestic + cup team strength (Tiento, all clubs) ===\n")

# 2. Helper functions (defined here so tests can source() this file and call
#    them directly on hand-built fixtures, same idiom the WC exporter uses for
#    team_metric()) ----

#' Classify every team's (league, is_domestic) for the CURRENT season
#'
#' A club plays in exactly one domestic league but can ALSO appear this same
#' season under a continental cup code (UCL/UEL/UECL/CAFCL) -- 19 such teams
#' at spec-writing time. Domestic always wins (Pete's call, DOMESTIC-TIENTO
#' plan "pin the 19 dual-labelled teams to their domestic league"): a team's
#' cup run does not change what league it is scored against.
#'
#' "Current season" is determined PER LEAGUE (max season_end_year present for
#' that league in fixture_results) rather than one global season, because
#' domestic leagues use different label formats (European "2025-2026" vs
#' calendar-year MLS/ARG/BRA) that can sit at different season_end_years at
#' the same wall-clock date.
#'
#' @param fixture_results data.table (or data.frame) with league, season_end_year,
#'   home_team, away_team, match_status.
#' @param domestic_codes,cup_codes Character vectors of league codes.
#' @return data.table(team, league, is_domestic_league, cur_sey)
.classify_team_leagues <- function(fixture_results, domestic_codes, cup_codes) {
  fr <- data.table::as.data.table(fixture_results)
  scope <- c(domestic_codes, cup_codes)
  fr <- fr[league %in% scope]
  if (nrow(fr) == 0L) {
    return(data.table::data.table(team = character(0), team_id = character(0),
                                   league = character(0),
                                   is_domestic_league = logical(0), cur_sey = numeric(0)))
  }
  cur_sey_by_league <- fr[, .(cur_sey = max(season_end_year, na.rm = TRUE)), by = league]
  fr <- merge(fr, cur_sey_by_league, by = "league")
  fr <- fr[season_end_year == cur_sey]

  # team_id, not just team NAME. Opta reuses club names across competitions and
  # countries: "Liverpool" is Liverpool FC, Liverpool FC Women (WSL) and
  # Liverpool FC Montevideo; "Arsenal" is Arsenal FC, Arsenal WFC and Arsenal de
  # Sarandi. Keying the squad join on the name merged all of them into one
  # 168-player "Liverpool" (vs 69 real), about a quarter of it the women's
  # squad -- see the id-keyed join in section 4. Step 01 backfills these ids
  # (its section 6b), but tolerate their absence rather than dropping teams:
  # a NA id falls back to the name join and is logged, never silently merged.
  .id_col <- function(nm) if (nm %in% names(fr)) fr[[nm]] else NA_character_
  rows <- data.table::rbindlist(list(
    fr[, .(team = home_team, team_id = .id_col("home_team_id"), league, cur_sey)],
    fr[, .(team = away_team, team_id = .id_col("away_team_id"), league, cur_sey)]
  ))
  rows <- rows[!is.na(team) & nzchar(team)]
  rows[is.na(team_id) | !nzchar(team_id), team_id := NA_character_]
  # Grouping by team_id as well splits a shared name into its real clubs; the
  # modal-row pick below then keeps the one this league actually means.
  tally <- rows[, .N, by = .(team, team_id, league, cur_sey)]

  is_dom_league <- tally$league %in% domestic_codes
  dom <- tally[is_dom_league]
  cup <- tally[!is_dom_league]

  # Pick the most-frequent league per team within each pool (a genuine data
  # anomaly -- mid-season rename, dual domestic entries -- should not crash
  # the export; deterministic tie-break by row count then league code).
  .pick_one <- function(dt) {
    if (nrow(dt) == 0L) return(dt)
    data.table::setorder(dt, team, -N, league)
    dt[, .SD[1L], by = team]
  }
  dom1 <- .pick_one(dom)
  cup1 <- .pick_one(cup)

  # uniqueN(league), not .N: now that the tally is split by team_id too, a
  # single club with two ids in ONE league would otherwise read as "matched >1
  # domestic league" and print a note about a thing that did not happen.
  multi_dom <- dom[, .(n_lg = data.table::uniqueN(league)), by = team][n_lg > 1L]
  if (nrow(multi_dom) > 0L) {
    message(sprintf(
      "  NOTE: %d team(s) matched >1 domestic league this season (kept the most-frequent): %s",
      nrow(multi_dom), paste(multi_dom$team, collapse = ", ")))
  }

  # Domestic wins over cup for any team present in both pools.
  dom_teams <- dom1$team
  cup_only <- cup1[!team %in% dom_teams]

  out <- data.table::rbindlist(list(
    dom1[, .(team, team_id, league, is_domestic_league = TRUE, cur_sey)],
    cup_only[, .(team, team_id, league, is_domestic_league = FALSE, cur_sey)]
  ))
  out
}

#' Teams relegated OUT of `league` between `prev_sey` and `cur_sey`
#'
#' "Relegated" here is inferred, not flagged in the data: teams that played a
#' full season in `league` at `prev_sey` and do not appear in `league` at
#' `cur_sey`. Within the set of leagues panna tracks, dropping out of a
#' tracked top flight overwhelmingly means relegation to an untracked division
#' (Ligue 2, 2. Bundesliga, ...) rather than a folded club -- the one
#' operationalisation of Pete's rule that needs no relegation-table data panna
#' doesn't have.
#'
#' @return Character vector of team names (possibly empty).
.relegated_cohort <- function(fixture_results, league_code, cur_sey) {
  fr <- data.table::as.data.table(fixture_results)
  lg <- fr[league == league_code]
  prior_seys <- lg$season_end_year[lg$season_end_year < cur_sey]
  if (length(prior_seys) == 0L) return(character(0))
  prev_sey <- max(prior_seys, na.rm = TRUE)

  prev_played <- lg[season_end_year == prev_sey & match_status == "Played"]
  teams_prev <- unique(c(prev_played$home_team, prev_played$away_team))
  teams_prev <- teams_prev[!is.na(teams_prev) & nzchar(teams_prev)]

  curr <- lg[season_end_year == cur_sey]
  teams_curr <- unique(c(curr$home_team, curr$away_team))
  teams_curr <- teams_curr[!is.na(teams_curr) & nzchar(teams_curr)]

  setdiff(teams_prev, teams_curr)
}

#' Seed Elo for every team with no tracked history
#'
#' Measured rule (panna#193 spec, 57 clean league-seasons ENG/ESP/ITA/FRA/GER):
#' seeding a promoted team at the mean final Elo of the PREVIOUS season's
#' relegated cohort is essentially unbiased (mean error +15, MAE 69, sd 89) --
#' clearly better than the league mean (179 MAE) or ELO_INITIAL=1500 (97
#' MAE). No correction term: the bias is small enough relative to the spread
#' to not be worth adding one.
#'
#' Re-derive with data-raw/debug/keep/_elo_seed_rule_measurement.R rather than
#' trusting these figures -- it names the one trap that matters (738 of 7,135
#' club-seasons start at exactly ELO_INITIAL, and leaving them in inflates the
#' apparent bias from +15 to +50).
#'
#' Falls back to the league mean (excluding NA-elo teams) when no relegated
#' cohort is identifiable -- no prior season tracked at all (a league's first
#' tracked season), or the team is cup-only (a cup has no relegation concept
#' to infer from). Every fallback is logged, per the "say so in the log, don't
#' silently use 1500" instruction.
#'
#' @param final_elos Named numeric vector, team -> final Elo (NA/absent = no
#'   tracked history at all).
#' @param team_league data.table from `.classify_team_leagues()`.
#' @param fixture_results Full fixture history (for `.relegated_cohort()`).
#' @return data.table(team, elo, elo_seeded, seed_method, seed_n)
.seed_missing_elo <- function(final_elos, team_league, fixture_results) {
  elo_of <- function(team) {
    v <- unname(final_elos[team])
    if (length(v) == 0L) NA_real_ else v
  }
  out <- data.table::copy(team_league)
  out[, elo := vapply(team, elo_of, numeric(1))]
  out[, elo_seeded := is.na(elo)]
  out[, seed_method := ifelse(elo_seeded, NA_character_, "earned")]
  out[, seed_n := NA_integer_]

  needs_seed <- out[elo_seeded == TRUE]
  if (nrow(needs_seed) == 0L) return(out[, .(team, elo, elo_seeded, seed_method, seed_n)])

  for (i in seq_len(nrow(needs_seed))) {
    tm  <- needs_seed$team[i]
    lg  <- needs_seed$league[i]
    sey <- needs_seed$cur_sey[i]
    is_dom <- isTRUE(needs_seed$is_domestic_league[i])

    seed <- NA_real_; method <- NA_character_; n_used <- 0L

    if (is_dom) {
      relegated <- .relegated_cohort(fixture_results, lg, sey)
      relegated <- setdiff(relegated, tm)
      releg_elos <- vapply(relegated, elo_of, numeric(1))
      releg_elos <- releg_elos[!is.na(releg_elos)]
      if (length(releg_elos) > 0L) {
        seed <- mean(releg_elos); method <- "relegated_cohort"; n_used <- length(releg_elos)
      }
    }

    if (is.na(seed)) {
      # League-mean fallback: this season's OTHER teams in the same league
      # (or cup) with earned (non-seeded, non-NA) Elo.
      peers <- out[league == lg & team != tm & !elo_seeded & !is.na(elo)]
      if (nrow(peers) > 0L) {
        seed <- mean(peers$elo); n_used <- nrow(peers)
        method <- if (is_dom) "league_mean_fallback" else "league_mean_fallback_cup"
        message(sprintf(
          "  Elo seed [%s / %s]: no identifiable relegated cohort -- falling back to league mean (n=%d peers, mean=%.0f)",
          tm, lg, n_used, seed))
      }
    }

    if (!is.na(seed)) {
      out[team == tm, `:=`(elo = seed, seed_method = method, seed_n = n_used)]
    } else {
      message(sprintf(
        "  WARNING: Elo seed [%s / %s]: no relegated cohort AND no league-mean peers -- elo stays NA",
        tm, lg))
    }
  }

  out[, .(team, elo, elo_seeded, seed_method, seed_n)]
}

#' Weighted z-blend Tiento, z-scored across the GLOBAL pool (all teams)
#'
#' Same math as 12_export_wc2026_blog.R:360-366, factored out so it can be
#' unit-tested against hand-computed z-scores. DOMESTIC-TIENTO-2026-08-21.md
#' "The one real design decision" -- global pool, not per-league -- so a
#' Championship side reads low by construction (honest; goes in the blog
#' tooltip, not fixed here).
.compute_tiento <- function(strength_dt, weights = TIENTO_WEIGHTS) {
  dt <- data.table::copy(strength_dt)
  .z_score <- function(v) {
    m <- mean(v, na.rm = TRUE); s <- stats::sd(v, na.rm = TRUE)
    if (is.na(s) || s == 0) rep(0, length(v)) else ifelse(is.na(v), 0, (v - m) / s)
  }
  dt[, tiento := rowSums(sapply(names(weights),
                    function(col) weights[[col]] * .z_score(dt[[col]])))]
  dt[, rank_tiento := data.table::frank(-tiento, ties.method = "min")]
  dt
}

#' Assert-before-publish guards. Refuses rather than ships something
#' half-right (panna#193 instruction). Each check here is mutation-tested in
#' tests/testthat/test-domestic-team-strength.R.
.validate_domestic_strength <- function(strength_dt, min_squad_n = MIN_PLAUSIBLE_SQUAD_N) {
  dt <- strength_dt

  no_league <- dt$team[is.na(dt$league) | !nzchar(dt$league)]
  if (length(no_league) > 0L) {
    stop(sprintf("domestic_team_strength: %d team(s) have no league: %s",
                 length(no_league), paste(no_league, collapse = ", ")), call. = FALSE)
  }
  no_tiento <- dt$team[is.na(dt$tiento)]
  if (length(no_tiento) > 0L) {
    stop(sprintf("domestic_team_strength: %d team(s) have NA tiento: %s",
                 length(no_tiento), paste(no_tiento, collapse = ", ")), call. = FALSE)
  }
  elo_na <- dt$team[is.na(dt$elo)]
  if (length(elo_na) > 0L) {
    stop(sprintf(paste("domestic_team_strength: elo missing for %d team(s) even after",
                       "seeding: %s -- no relegated cohort AND no league-mean peers."),
                 length(elo_na), paste(elo_na, collapse = ", ")), call. = FALSE)
  }
  small_squads <- dt[squad_n < min_squad_n]
  if (nrow(small_squads) > 0L) {
    stop(sprintf(paste("domestic_team_strength: %d team(s) have implausibly small squads",
                       "(<%d players -- the team_id squad join found no lineups for them,",
                       "not a genuinely small squad): %s"),
                 nrow(small_squads), min_squad_n,
                 paste(sprintf("%s(%d)", small_squads$team, small_squads$squad_n), collapse = ", ")),
         call. = FALSE)
  }
  for (mt in c("panna", "epr", "psr")) {
    nz <- mean(dt[[mt]] != 0, na.rm = TRUE)
    if (is.nan(nz) || nz < 0.5) {
      stop(sprintf(paste("domestic_team_strength: %s is zero (or all-NA) for >=50%% of teams --",
                         "likely a squad rating-join failure. Refusing to publish."), mt),
           call. = FALSE)
    }
  }

  message(sprintf("  Guard: squad_n range %d-%d across %d teams",
                  min(dt$squad_n), max(dt$squad_n), nrow(dt)))
  message(sprintf("  Guard: tiento mean=%.3f sd=%.3f", mean(dt$tiento), stats::sd(dt$tiento)))
  top5 <- utils::head(dt[order(-tiento)], 5L)
  bot5 <- utils::head(dt[order(tiento)], 5L)
  message("  Top 5 by tiento: ", paste(sprintf("%s (%.2f)", top5$team, top5$tiento), collapse = ", "))
  message("  Bottom 5 by tiento: ", paste(sprintf("%s (%.2f)", bot5$team, bot5$tiento), collapse = ", "))
  invisible(TRUE)
}

# 3. Driver ----
# Guarded so tests can source() this file to get the helper functions above
# (for hand-built-fixture unit tests, e.g. `.seed_missing_elo()` on a tiny
# synthetic final_elos vector) WITHOUT needing a full opta_lineups.parquet +
# career_panna.parquet + seasonal-ratings fixture set on disk. Same idiom as
# announced_squads.R's `WC2026_ANNOUNCED_SQUADS_SOURCE_ONLY` guard. Production
# (run_predictions_opta.R's step 12d) never sets this flag, so the driver
# always runs there -- this is purely a testability seam.
if (!exists("DOMESTIC_TEAM_STRENGTH_SOURCE_ONLY") ||
    !isTRUE(DOMESTIC_TEAM_STRENGTH_SOURCE_ONLY)) {

fr_path <- file.path(cache_dir, "01_fixture_results.rds")
if (!file.exists(fr_path)) {
  stop("01_fixture_results.rds not found in ", cache_dir, " -- run step 01 first.", call. = FALSE)
}
fixture_results <- data.table::as.data.table(readRDS(fr_path))

team_league <- .classify_team_leagues(fixture_results, domestic_codes, cup_codes)
if (nrow(team_league) == 0L) {
  stop("domestic_team_strength: 0 teams matched domestic_codes/cup_codes in ",
       "01_fixture_results.rds -- check the league-code scope.", call. = FALSE)
}
message(sprintf("  Team scope: %d teams (%d domestic-league, %d cup-only)",
                nrow(team_league), sum(team_league$is_domestic_league),
                sum(!team_league$is_domestic_league)))

# 3b. Elo: re-derive final_elos ----
# compute_match_elos()'s `final_elos` (post-iteration team state) is exactly
# what step 3 (03_team_rolling_features.R) computes internally to look up
# upcoming-fixture Elo, but it is never persisted -- step 3 keeps only the
# per-match PRE-match features it derives from it. A relegated team's frozen
# final Elo needs the vector itself, so this duplicates step 3's call. K=20 /
# home_advantage=88 / initial_elo=1500 are step 3's own local constants
# (03_team_rolling_features.R:17-19, "ELO_K"/"ELO_HOME_ADV"/"ELO_INITIAL") --
# not package constants, so this MUST be kept in sync by hand if those change.
# k_table/cross_conf_mult/conf_priors ARE package constants (R/elo_calibration.R)
# and stay in sync automatically.
played <- fixture_results[match_status == "Played"]
played <- played[order(match_date)]
elo_result <- compute_match_elos(
  played,
  k = 20, home_advantage = 88, initial_elo = 1500,
  k_table = ELO_MATCH_TYPE_K,
  cross_conf_mult = ELO_CROSS_CONF_MULT,
  conf_priors = ELO_CONFEDERATION_PRIORS,
  use_venue_factor = TRUE
)
final_elos <- elo_result$final_elos

elo_dt <- .seed_missing_elo(final_elos, team_league, fixture_results)
n_seeded <- sum(elo_dt$elo_seeded)
if (n_seeded > 0L) {
  message(sprintf("  Elo: %d/%d teams seeded (no tracked history): %s",
                  n_seeded, nrow(elo_dt),
                  paste(elo_dt$team[elo_dt$elo_seeded], collapse = ", ")))
}

team_league <- merge(team_league, elo_dt, by = "team")

# 4. Squad ratings + team strength (mirrors 12_export_wc2026_blog.R section 5
#    exactly, minus the WC-only announced-squad step -- built directly from
#    opta_lineups via build_team_expected_minutes(international_only=FALSE),
#    per the panna#193 brief) ----

lineups_path <- file.path(opta_data_dir(), "opta_lineups.parquet")
if (!file.exists(lineups_path)) {
  stop("opta_lineups.parquet not found at ", lineups_path, call. = FALSE)
}
lu_all <- as.data.table(read_parquet(
  lineups_path,
  col_select = c("team_id", "team_name", "match_id", "match_date", "player_id",
                "player_name", "position", "is_starter", "minutes_played",
                "competition")))
data.table::setkey(lu_all, team_id)
lu_known_ids <- unique(lu_all$team_id)
lu_known_teams <- unique(lu_all$team_name)

cp_path <- file.path(opta_data_dir(), "career_panna.parquet")
if (!file.exists(cp_path)) {
  stop("career_panna.parquet not found at ", cp_path, " -- domestic Tiento needs ",
       "the same career-trait panna the blog publishes elsewhere.", call. = FALSE)
}
sq_panna <- as.data.table(read_parquet(cp_path))[
  , .(player_id, panna, offense = panna_offense, defense = panna_defense)]

if (!exists("skills_cache_dir")) skills_cache_dir <- file.path("data-raw", "cache-skills")
if (!exists("opta_cache_dir")) opta_cache_dir <- file.path("data-raw", "cache-opta")
sq_skill_path <- file.path(skills_cache_dir, "06_seasonal_ratings.rds")
sq_raw_path   <- file.path(opta_cache_dir, "07_seasonal_ratings.rds")
sq_seasonal <- if (file.exists(sq_skill_path)) {
  readRDS(sq_skill_path)
} else if (file.exists(sq_raw_path)) {
  readRDS(sq_raw_path)
} else {
  NULL
}
sq_psr <- if (!is.null(sq_seasonal) && !is.null(sq_seasonal$seasonal_psr) &&
              nrow(sq_seasonal$seasonal_psr) > 0) {
  p <- as.data.table(sq_seasonal$seasonal_psr)
  p[order(player_id, -season_end_year), .SD[1L], by = player_id, .SDcols = "psr"]
} else NULL

sq_epr <- {
  ep <- file.path(opta_data_dir(), "opta_epr_weekly.parquet")
  if (file.exists(ep)) {
    e <- as.data.table(read_parquet(ep))
    e[, snapshot_date := as.Date(snapshot_date)]
    e[order(player_id, -snapshot_date), .SD[1L], by = player_id, .SDcols = "epr"]
  } else NULL
}

.wsum <- function(x, w) sum(w * data.table::fifelse(is.na(x), 0, x))

teams <- team_league$team
team_ids <- team_league$team_id
name_fallback <- character(0)   # teams resolved by name because no id was available
agg_rows <- vector("list", length(teams))
for (i in seq_along(teams)) {
  tm  <- teams[i]
  tid <- team_ids[i]
  if (!is.na(tid) && nzchar(tid) && tid %in% lu_known_ids) {
    lu_team <- lu_all[.(tid)]
  } else {
    # No usable id: fall back to the old name join, but SAY SO. This is the
    # path that silently merged clubs before, so it must never be invisible.
    lu_team <- if (tm %in% lu_known_teams) lu_all[team_name == tm] else lu_all[0L]
    name_fallback <- c(name_fallback, tm)
  }
  # Structural, not a magic threshold: one row of this export is one club, so
  # its lineup slice must contain exactly one team_id. A size ceiling would
  # have to guess where a real squad ends and a merged one begins; this cannot
  # be fooled by a merge that happens to look plausible.
  if (nrow(lu_team) > 0L && data.table::uniqueN(lu_team$team_id) > 1L) {
    stop(sprintf(paste("domestic_team_strength: squad slice for %s covers %d distinct",
                       "team_id(s) (%s) -- distinct clubs sharing a name have been",
                       "merged into one squad. Refusing to publish."),
                 tm, data.table::uniqueN(lu_team$team_id),
                 paste(utils::head(unique(lu_team$team_id), 4L), collapse = ", ")),
         call. = FALSE)
  }
  # The NAME to filter on comes from the lineups slice, not from the fixture
  # side. build_team_expected_minutes() does `lineups[team_name == team]`
  # internally, and the two sources spell clubs differently -- fixtures say
  # "Le Mans FC" and "SV 07 Elversberg" where lineups say "Le Mans" and
  # "Elversberg". Passing the fixture spelling emptied both squads to 0
  # players, which is what the min-squad guard caught on the 2026-08-23 run.
  em_team <- if (nrow(lu_team) > 0L) lu_team$team_name[1L] else tm
  # lookback_days = 1095L, NOT the 730L library default. announced_squads.R
  # passes 1095 at both of its build_team_expected_minutes() call sites, so
  # taking the default here would have quietly made domestic Tiento mean
  # something different from WC Tiento -- which is the one thing this step is
  # not allowed to do. Caught in review; the header claimed parity the code did
  # not have. Practical effect is on which fringe players clear the evidence
  # bar, which matters most for exactly the thin-history clubs this step
  # handles specially.
  em <- panna::build_team_expected_minutes(
    team = em_team, lineups = lu_team, as_of = as_of_domestic,
    international_only = FALSE, lookback_days = 1095L
  )
  if (is.null(em) || nrow(em) == 0L || !"player_id" %in% names(em)) {
    agg_rows[[i]] <- data.table::data.table(
      team = tm, panna = NA_real_, offense = NA_real_, defense = NA_real_,
      epr = NA_real_, psr = NA_real_, squad_n = 0L, n_rated = 0L)
    next
  }
  em <- data.table::as.data.table(em)
  em <- merge(em[, .(player_id, expected_minutes_norm)], sq_panna, by = "player_id", all.x = TRUE)
  if (!is.null(sq_psr)) em <- merge(em, sq_psr, by = "player_id", all.x = TRUE)
  if (!is.null(sq_epr)) em <- merge(em, sq_epr, by = "player_id", all.x = TRUE)
  for (col in c("panna", "offense", "defense", "epr", "psr"))
    if (!col %in% names(em)) em[[col]] <- NA_real_

  w <- em$expected_minutes_norm / 90
  agg_rows[[i]] <- data.table::data.table(
    team = tm,
    panna   = .wsum(em$panna,   w),
    offense = .wsum(em$offense, w),
    defense = .wsum(em$defense, w),
    epr     = .wsum(em$epr,     w),
    psr     = .wsum(em$psr,     w),
    squad_n = nrow(em),
    n_rated = sum(!is.na(em$panna))
  )
}
agg <- data.table::rbindlist(agg_rows)

message(sprintf("  Squad join: %d/%d team(s) resolved by team_id",
                length(teams) - length(name_fallback), length(teams)))
if (length(name_fallback) > 0L) {
  message(sprintf(paste("  NOTE: %d team(s) had no usable team_id and fell back to the",
                        "NAME join -- these are the only rows where distinct clubs",
                        "sharing a name could still merge: %s"),
                  length(name_fallback), paste(name_fallback, collapse = ", ")))
}

strength <- merge(team_league, agg, by = "team", all.x = TRUE)
for (m in c("panna", "offense", "defense", "epr", "psr", "elo")) {
  strength[[m]] <- round(strength[[m]], 4)
}
# Published convention: defence positive = good (internal model negative = good).
strength[, defense := -defense]

strength <- .compute_tiento(strength, TIENTO_WEIGHTS)

.validate_domestic_strength(strength)

# 5. Stamp build_id, order columns, write ----

.build_id_val <- .vb_generation_stamp()
strength[, build_id := .build_id_val]

# Explicit column selection (not a blind setcolorder pass-through) so an
# internal-only field (cur_sey, used only to pick the relegated cohort) never
# leaks into the published schema by accident. seed_method is kept -- it is
# the "why" behind elo_seeded and cheap forensic value for a wrong-looking
# seed on the blog side.
strength <- strength[, .(
  team, league, panna, offense, defense, epr, psr, elo,
  tiento, rank_tiento, squad_n, n_rated, elo_seeded, seed_method,
  is_domestic_league, build_id)]
data.table::setorder(strength, -tiento)

out_path <- file.path(cache_dir, "team_strength.parquet")
write_parquet(strength, out_path)
csv_path <- sub("\\.parquet$", ".csv", out_path)
write.csv(strength, csv_path, row.names = FALSE)
message(sprintf("  team_strength.parquet: %d teams (build_id=%s)", nrow(strength), .build_id_val))

# 6. Register for step-13 publish (registers ONLY at the very end, after every
#    write above has succeeded -- deliberately, same reasoning as step 12: a
#    mid-step failure must register nothing rather than a half-written file) ----

if (exists("publish_files", envir = .GlobalEnv)) {
  publish_files$blog_latest <<- c(publish_files$blog_latest, out_path, csv_path)
  message("  Registered team_strength.parquet (+ CSV) for blog-latest publish (step 13).")
} else {
  message("  (standalone run -- not registered for step-13 publish)")
}

message("\n=== Domestic + cup team strength export complete ===")

}  # end DOMESTIC_TEAM_STRENGTH_SOURCE_ONLY guard
