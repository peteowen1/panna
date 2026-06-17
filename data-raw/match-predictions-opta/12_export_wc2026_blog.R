# 12_export_wc2026_blog.R
# Export WC 2026 data for the blog's World Cup section and upload to the
# blog-latest release on peteowen1/pannadata. pannadata's build-blog-data.yml
# pulls wc2026_*.parquet into blog/ and the R2 step ships them to
# inthegame-data/football/.
#
# Produces five parquet files:
#   wc2026_predictions.parquet    — 72 group-stage match predictions (H/D/A + xG)
#   wc2026_simulation.parquet     — per-team round + champion probabilities
#   wc2026_groups.parquet         — per-team group-finish probabilities
#   wc2026_team_strength.parquet  — per-team strength across rating categories
#   wc2026_squads.parquet         — per-player squad rows with ratings
#
# Inputs (all produced upstream by steps 07 + 11):
#   07_predictions.rds, 04_match_dataset.rds, wc2026_groups.rds,
#   wc2026_simulation.parquet, wc2026_group_expectations.parquet,
#   wc2026_bt_ratings.parquet

# 1. Configuration ----

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")
repo <- "peteowen1/pannadata"
tag  <- "blog-latest"

suppressPackageStartupMessages({ library(data.table); library(arrow) })
devtools::load_all()   # WC2026_LEAGUE / WC2026_SEASON_LABEL from R/constants.R
wc_season <- WC2026_SEASON_LABEL

message("\n=== Exporting WC 2026 blog data ===\n")

# WC group assignments — same fallback as 11_simulate_wc2026.R: prefer the
# cache RDS (lets devs override for what-if scenarios), fall back to the
# inst/extdata package asset on a clean checkout / GHA runner.
groups_cache_rds <- file.path(cache_dir, "wc2026_groups.rds")
groups_pkg_csv   <- system.file("extdata", "wc2026_groups.csv", package = "panna")
groups <- if (file.exists(groups_cache_rds)) {
  g <- as.data.table(readRDS(groups_cache_rds))
  message("  wc2026 groups: cache RDS (", groups_cache_rds, ")")
  # Same stale-shadow guard as 11_simulate_wc2026.R — group letters are
  # published to the blog and drive the FIFA bracket; warn on divergence.
  if (nzchar(groups_pkg_csv) && file.exists(groups_pkg_csv)) {
    csv_g <- data.table::fread(groups_pkg_csv)
    chk <- merge(g[, .(team, group_rds = group)],
                 csv_g[, .(team, group_csv = group)], by = "team", all = TRUE)
    bad <- chk[is.na(group_rds) | is.na(group_csv) | group_rds != group_csv]
    if (nrow(bad) > 0L) {
      warning(sprintf(paste(
        "wc2026_groups.rds cache DISAGREES with inst/extdata/wc2026_groups.csv",
        "for %d team(s): %s — delete the stale RDS unless this is a",
        "deliberate what-if run"),
        nrow(bad), paste(bad$team, collapse = ", ")),
        call. = FALSE, immediate. = TRUE)
    }
  }
  g
} else if (nzchar(groups_pkg_csv) && file.exists(groups_pkg_csv)) {
  message("  wc2026 groups: package CSV (inst/extdata/wc2026_groups.csv)")
  data.table::fread(groups_pkg_csv)
} else {
  stop("wc2026_groups not found: neither ", groups_cache_rds,
       " nor inst/extdata/wc2026_groups.csv is available")
}
team_group <- stats::setNames(groups$group, groups$team)

# 2. Match predictions ----

preds <- as.data.table(readRDS(file.path(cache_dir, "07_predictions.rds")))
wc_pred <- preds[league == WC2026_LEAGUE & season == wc_season &
                   home_team != "" & away_team != ""]
wc_pred <- wc_pred[, .(
  match_date,
  group      = unname(team_group[home_team]),
  home_team, away_team,
  prob_home  = prob_H, prob_draw = prob_D, prob_away = prob_A,
  pred_home_goals, pred_away_goals,
  predicted  = predicted_result
)]
setorder(wc_pred, match_date, group)
write_parquet(wc_pred, file.path(cache_dir, "wc2026_predictions.parquet"))
message(sprintf("  wc2026_predictions.parquet: %d fixtures", nrow(wc_pred)))

# 3. Simulation — per-team round/champion probabilities ----

sim <- as.data.table(read_parquet(file.path(cache_dir, "wc2026_simulation.parquet"), mmap = FALSE))
# p_R32 (top-2 + best-thirds reach, tracked by simulate_world_cup since
# 2026-06-12) is intersect-guarded so a stale upstream parquet still exports.
sim_cols <- intersect(c("team", "group", "p_R32", "p_R16", "p_QF", "p_SF", "p_final", "p_champ"), names(sim))
sim <- sim[, ..sim_cols]
setorder(sim, -p_champ)
write_parquet(sim, file.path(cache_dir, "wc2026_simulation.parquet"))
message(sprintf("  wc2026_simulation.parquet: %d teams", nrow(sim)))

# 4. Group-stage finish probabilities ----

grp <- as.data.table(read_parquet(file.path(cache_dir,
                                            "wc2026_group_expectations.parquet")))
grp[, advance := round(pos1 + pos2, 1)]   # win or runner-up
grp <- grp[, .(group, team,
               win_group = pos1, runner_up = pos2, third = pos3, fourth = pos4,
               advance)]
setorder(grp, group, -win_group)
write_parquet(grp, file.path(cache_dir, "wc2026_groups.parquet"))
message(sprintf("  wc2026_groups.parquet: %d team-rows", nrow(grp)))

# 5. Squad player ratings + team strength ----
# Team strength is the MINUTES-WEIGHTED SUM of the player ratings shown in
# wc2026_squads.parquet: team_metric = Σ_squad (expected_minutes_norm / 90) *
# player_metric. So a team's rating literally equals the weighted sum of its
# displayed players and reconciles by construction.
#
# We deliberately do NOT reuse the match-dataset home_sum_* features here. Those
# carry the PREDICTION-MODEL versions of the ratings — date-specific live SPM for
# panna, and WC-population-centered live PSR (step 02's date-specific path centers
# over only the upcoming WC players, not the league). Those are correct for the
# XGBoost match model (which uses home-away diffs, so the centering constant
# cancels) but are a DIFFERENT estimator than the career-trait panna / league-
# centered seasonal PSR the blog displays per player — which made the old team PSR collapse
# to ~0 for all but the deepest squads. elo stays a match-dataset team property
# (not squad-derived). BT strength + champ prob come from the sim. See METRICS.md
# §14 and the step-02 WC-centering note.

md <- as.data.frame(readRDS(file.path(cache_dir, "04_match_dataset.rds")))
wc <- md[md$league == WC2026_LEAGUE & md$season == wc_season &
           !is.na(md$home_team) & md$home_team != "" &
           !is.na(md$away_team) & md$away_team != "", ]
teams <- sort(unique(c(wc$home_team, wc$away_team)))

# elo is a team property in the match dataset: pull home_elo (fall back away_elo).
# Returns NA (not numeric(0)) on missing data so the guard below can surface it.
team_metric <- function(tm, base) {
  pick <- function(rows, col) {
    if (!col %in% names(rows) || nrow(rows) == 0L) return(NA_real_)
    v <- rows[[col]][1]
    if (length(v) == 0L) NA_real_ else as.numeric(v)
  }
  hr <- wc[!is.na(wc$home_team) & wc$home_team == tm, ]
  if (nrow(hr) > 0) {
    val <- pick(hr, paste0("home_", base))
    if (!is.na(val)) return(val)
  }
  ar <- wc[!is.na(wc$away_team) & wc$away_team == tm, ]
  pick(ar, paste0("away_", base))
}

# --- Squad player ratings: career-trait panna + league-centered seasonal PSR +
# latest weekly EPR — all point-in-time player RATINGS (not season aggregates).
# SAME source the per-player squad table publishes below (section 5c reuses
# squad_out), so team == Σ(players shown). ---
squads_path <- file.path(cache_dir, "wc2026_announced_squads.parquet")
if (!file.exists(squads_path)) {
  stop("wc2026_announced_squads.parquet not found in ", cache_dir,
       " — run announced_squads.R (step 02) first.")
}
squads <- as.data.table(read_parquet(squads_path))
# Dedup: one row per (team, player), keep the highest expected-minutes source row.
setorder(squads, team_name, player_id, -expected_minutes_norm)
squads <- unique(squads, by = c("team_name", "player_id"))

sq_skill_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
sq_raw_path   <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")
sq_seasonal <- if (file.exists(sq_skill_path)) readRDS(sq_skill_path) else readRDS(sq_raw_path)

# panna = the career-trait RATING (decay-weighted multi-season xRAPM, point-in-time
# "best guess of next game") — the SAME `panna` the main blog ratings publish since
# 2026-06-09. NOT the single-season xRAPM (that's a season aggregate, a different
# quantity that used to be mislabeled `panna`). Source: career_panna.parquet
# (estimated-skills/09_career_panna.R via fit_career_rapm), on pannadata's
# ratings-data release. offense/defense = the career-trait decomposition
# (panna_offense/panna_defense; internal negative=good, flipped at display below).
cp_path <- file.path(opta_data_dir(), "career_panna.parquet")
if (!file.exists(cp_path)) {
  stop("career_panna.parquet not found at ", cp_path, " — the WC squad panna IS the ",
       "career trait, not season xRAPM. Add it to the predictions-pipeline download ",
       "list (pannadata ratings-data release) or run estimated-skills/09_career_panna.R.",
       call. = FALSE)
}
sq_panna <- as.data.table(read_parquet(cp_path))[
  , .(player_id, panna, offense = panna_offense, defense = panna_defense, total_minutes)]

sq_psr <- if (!is.null(sq_seasonal$seasonal_psr) && nrow(sq_seasonal$seasonal_psr) > 0) {
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

squad_out <- squads[, .(team = team_name, player_id, player_name, position,
                        expected_minutes_norm, is_starter_pred)]
squad_out[, group := unname(team_group[team])]
squad_out <- merge(squad_out, sq_panna, by = "player_id", all.x = TRUE)
if (!is.null(sq_psr)) squad_out <- merge(squad_out, sq_psr, by = "player_id", all.x = TRUE)
if (!is.null(sq_epr)) squad_out <- merge(squad_out, sq_epr, by = "player_id", all.x = TRUE)
for (col in c("panna", "offense", "defense", "epr", "psr", "total_minutes"))
  if (!col %in% names(squad_out)) squad_out[[col]] <- NA_real_

# --- Team strength = Σ_squad (expected_minutes_norm / 90) * player_metric.
# NA (unrated player) contributes 0 to the sum but stays in the squad headcount. ---
.wsum <- function(x, w) sum(w * data.table::fifelse(is.na(x), 0, x))
agg <- squad_out[, {
  w <- expected_minutes_norm / 90
  list(panna   = .wsum(panna,   w),
       offense = .wsum(offense, w),
       defense = .wsum(defense, w),
       epr     = .wsum(epr,     w),
       psr     = .wsum(psr,     w),
       squad_n = .N,
       n_rated = sum(!is.na(panna)))
}, by = team]

strength <- data.table(team = teams, group = unname(team_group[teams]))
strength <- merge(strength, agg[, .(team, panna, offense, defense, epr, psr)],
                  by = "team", all.x = TRUE)
strength[, elo := vapply(teams, team_metric, numeric(1), base = "elo")]
for (m in c("panna", "offense", "defense", "epr", "psr", "elo"))
  strength[[m]] <- round(strength[[m]], 4)

# HARD STOP guards. Per [[feedback-no-silent-imputation]] a published rating must
# HALT on a structural failure, not ship partial/zeroed data. Two failure modes:
#   1. elo missing for a team — match-dataset property absent (name drift / no row).
#   2. a team has NO squad aggregate — its name didn't match the announced-squad
#      team_name, or it has no announced squad (would leave panna NA after the join).
# Plus a wholesale-zero tripwire: now that the aggregate sums NA->0, a squad
# rating-join failure surfaces as a metric that is 0 for (nearly) every team
# rather than the old all-NA symptom — catch that too.
elo_na <- strength$team[is.na(strength$elo)]
if (length(elo_na) > 0L) {
  stop(sprintf("wc2026_team_strength: elo missing for %d team(s): %s — match-dataset name drift or missing WC row.",
               length(elo_na), paste(elo_na, collapse = ", ")), call. = FALSE)
}
squad_missing <- strength$team[is.na(strength$panna)]
if (length(squad_missing) > 0L) {
  stop(sprintf("wc2026_team_strength: no squad aggregate for %d team(s): %s — announced-squad team_name mismatch vs the match dataset, or a missing announced squad. Fix the name mapping (or announced_squads.R) before publishing.",
               length(squad_missing), paste(squad_missing, collapse = ", ")), call. = FALSE)
}
for (mt in c("panna", "epr", "psr")) {
  nz <- mean(strength[[mt]] != 0, na.rm = TRUE)
  if (nz < 0.5) {
    stop(sprintf("wc2026_team_strength: %s is zero for %.0f%% of teams — likely a squad rating-join failure (check seasonal_%s in cache-skills/06 or cache-opta/07, and the player_id join). Refusing to publish.",
                 mt, 100 * (1 - nz), mt), call. = FALSE)
  }
}

# Published convention: defence as positive = good (internal model has
# negative = good, since defense is "xG added to the opponent").
strength[, defense := -defense]

bt <- as.data.table(read_parquet(file.path(cache_dir, "wc2026_bt_ratings.parquet")))
strength <- merge(strength, bt[, .(team, bt = rating)], by = "team", all.x = TRUE)
strength <- merge(strength, sim[, .(team, p_champ)], by = "team", all.x = TRUE)

# Per-category rank (1 = strongest). Defence already flipped so higher = better.
for (m in c("panna", "offense", "defense", "epr", "psr", "elo", "bt", "p_champ")) {
  strength[[paste0("rank_", m)]] <- frank(-strength[[m]], ties.method = "min")
}

# Tiento: composite team rating — computed HERE (pannaverse) instead of on ITG
# (was inthegame-blog/football/wc-maps.js::computeTeamRating). A z-blend of the
# headline metrics. Weights are HAND-SET — a deliberate, fairly even spread that
# keeps a small PSR contribution. For reference, a ridge (alpha=0 cv.glmnet) of
# historical goal margin on the standardized metric diffs suggested a more
# panna/Elo-heavy split (panna 0.47 / elo 0.36 / epr 0.17 / psr 0.00, PSR
# dropping out on a negative coef; derivation in data-raw/debug/keep/_tiento_weights.R),
# but these chosen weights trade some of that for balance. Each metric is
# z-scored across the 48 teams (NA -> 0), then weighted-summed. ITG reads this
# `tiento` column directly instead of recomputing the blend in the browser.
TIENTO_WEIGHTS <- c(panna = 0.40, epr = 0.20, elo = 0.30, psr = 0.10)
.z_score <- function(v) {
  m <- mean(v, na.rm = TRUE); s <- stats::sd(v, na.rm = TRUE)
  if (is.na(s) || s == 0) rep(0, length(v)) else ifelse(is.na(v), 0, (v - m) / s)
}
strength[, tiento := rowSums(sapply(names(TIENTO_WEIGHTS),
                  function(col) TIENTO_WEIGHTS[[col]] * .z_score(strength[[col]])))]
strength[, rank_tiento := frank(-tiento, ties.method = "min")]

setorder(strength, -p_champ)
write_parquet(strength, file.path(cache_dir, "wc2026_team_strength.parquet"))
message(sprintf("  wc2026_team_strength.parquet: %d teams (panna/offense/defense/epr/psr = squad minutes-weighted; elo/bt/p_champ + tiento)",
                nrow(strength)))

# 5c. Squad rows with player ratings ----
# squad_out (one row per squad player, joined to career-trait panna + league-
# centered seasonal PSR + latest weekly EPR) was already built in section 5 — the
# team strength above is its minutes-weighted aggregate. Here we add the
# authoritative club, set column order, and publish. Ratings stay NA when a
# player has no rated club minutes (smaller leagues) — the blog renders a dash;
# per [[feedback-no-silent-imputation]] we do NOT zero-fill the per-player rows.

# Authoritative club per player: latest CLUB (non-international) appearance
# in opta_lineups across every scraped competition — covers Saudi/MLS/
# Liga MX/Argentina squads the ratings join can't reach (blog previously
# shimmed these from Wikidata, stale ~40% of the time). club_last_seen lets
# the blog grey out genuinely stale clubs (transfers, retirements).
lu_club_path <- file.path(opta_data_dir(), "opta_lineups.parquet")
if (file.exists(lu_club_path)) {
  # build_team_expected_minutes()'s international list MINUS UEFA_Super_Cup
  # (that's a club fixture — its appearances should count as club evidence
  # here; harmless there because EM pre-filters to the national team's rows).
  intl_comps <- c("World_Cup", "UEFA_WC_Qualifiers", "UEFA_Euros",
                  "UEFA_Euro_Qualifiers", "UEFA_Nations_League",
                  "Copa_America", "AFCON", "AFCON_Qualifiers",
                  "CONCACAF_Gold_Cup", "AFC_Asian_Cup", "AFC_WC_Qualifiers",
                  "Asian_Cup_Qualifiers", "Gulf_Cup_of_Nations",
                  "CAF_WC_Qualifiers", "CONMEBOL_WC_Qualifiers",
                  "Intl_Friendlies")
  lu_club <- as.data.table(read_parquet(
    lu_club_path,
    col_select = c("player_id", "team_name", "match_date", "competition")))
  lu_club <- lu_club[!competition %in% intl_comps &
                     player_id %in% squad_out$player_id]
  lu_club[, match_date := as.Date(substr(match_date, 1, 10))]
  setorder(lu_club, player_id, -match_date)
  club_latest <- lu_club[, .SD[1L], by = player_id][
    , .(player_id, club_name = team_name, club_last_seen = match_date)]
  squad_out <- merge(squad_out, club_latest, by = "player_id", all.x = TRUE)
  n_club <- sum(!is.na(squad_out$club_name))
  message(sprintf("  club_name resolved for %d/%d squad players (latest club appearance)",
                  n_club, nrow(squad_out)))
  # Loudness guards: a player_id-format or date-format drift upstream would
  # otherwise ship 0 clubs (or arbitrary "latest" picks) behind a green run.
  # Normal coverage is ~97%; a few unscraped-league players are expected.
  if (n_club < 0.8 * nrow(squad_out)) {
    warning(sprintf(paste(
      "club_name coverage is %d/%d (<80%%) — check player_id/competition",
      "drift between announced squads and opta_lineups"),
      n_club, nrow(squad_out)), call. = FALSE, immediate. = TRUE)
  }
  if (any(!is.na(squad_out$club_name) & is.na(squad_out$club_last_seen))) {
    warning("club_name present with NA club_last_seen — match_date parse drift in opta_lineups",
            call. = FALSE, immediate. = TRUE)
  }
  # Tripwire for an international comp missing from the exclusion blacklist:
  # the symptom is a national team reported as someone's club.
  nat_as_club <- intersect(unique(squad_out$club_name), groups$team)
  if (length(nat_as_club) > 0L) {
    warning("national team(s) resolved as club_name (intl_comps blacklist miss?): ",
            paste(nat_as_club, collapse = ", "), call. = FALSE, immediate. = TRUE)
  }
} else {
  squad_out[, `:=`(club_name = NA_character_, club_last_seen = as.Date(NA))]
  warning("opta_lineups.parquet not found — wc2026_squads.parquet ships without club_name",
          call. = FALSE, immediate. = TRUE)
}

setcolorder(squad_out, c("team", "group", "player_id", "player_name", "position",
                         "club_name", "club_last_seen",
                         "expected_minutes_norm", "is_starter_pred",
                         "panna", "offense", "defense", "epr", "psr", "total_minutes"))
setorder(squad_out, team, -expected_minutes_norm)
write_parquet(squad_out, file.path(cache_dir, "wc2026_squads.parquet"))
message(sprintf("  wc2026_squads.parquet: %d players across %d squads (%d with panna ratings)",
                nrow(squad_out), uniqueN(squad_out$team), sum(!is.na(squad_out$panna))))

# 6. Save CSV companions for the small published tables ----
# Per feedback 2026-05-28: small tables (<100KB / <10k rows) get a CSV
# alongside the parquet for easy human inspection. The parquet remains
# the format the blog reads programmatically; the CSV is the companion
# you can `cat` or open in any editor without arrow installed.
wc_parquets <- c("wc2026_predictions.parquet",
                 "wc2026_simulation.parquet",
                 "wc2026_groups.parquet",
                 "wc2026_team_strength.parquet",
                 "wc2026_squads.parquet",
                 "wc2026_knockout_probs.parquet")
for (p in wc_parquets) {
  pp <- file.path(cache_dir, p)
  if (!file.exists(pp)) {
    # knockout_probs is written by step 11, the rest by this step — a
    # standalone step-12 run against a pre-2026-06-11 cache lacks it.
    warning(p, " not in cache — skipping its CSV companion and upload",
            call. = FALSE, immediate. = TRUE)
    next
  }
  cp <- sub("\\.parquet$", ".csv", pp)
  write.csv(read_parquet(pp), cp, row.names = FALSE)
}
message(sprintf("  Wrote %d CSV companions for the small published tables.",
                length(wc_parquets)))

# 7. Upload to blog-latest ----

wc_files <- c(
  file.path(cache_dir, wc_parquets),
  # CSV companions uploaded alongside parquet
  file.path(cache_dir, sub("\\.parquet$", ".csv", wc_parquets))
)
# Drop entries skipped above (warned already) so one absent optional file
# doesn't stop() the whole upload mid-loop.
wc_files <- wc_files[file.exists(wc_files)]

no_upload <- isTRUE(Sys.getenv("WC2026_NO_UPLOAD", "") == "1")
gh_ok <- !is.null(tryCatch(system2("gh", "--version", stdout = TRUE,
                                    stderr = TRUE), error = function(e) NULL))
if (no_upload) {
  message("  WC2026_NO_UPLOAD=1 — files written locally, skipping upload")
} else if (!gh_ok) {
  message("  gh CLI not available — files written locally, skipping upload")
} else {
  rel <- system2("gh", c("release", "view", tag, "--repo", repo),
                  stdout = TRUE, stderr = TRUE)
  if (!is.null(attr(rel, "status")) && attr(rel, "status") != 0) {
    system2("gh", c("release", "create", tag, "--repo", repo,
                     "--title", shQuote("Blog Data (Latest)"),
                     "--notes", shQuote("Blog data.")),
            stdout = TRUE, stderr = TRUE)
  }
  for (f in wc_files) {
    message(sprintf("  Uploading %s (%.1f KB)...", basename(f),
                    file.size(f) / 1024))
    res <- system2("gh", c("release", "upload", tag, shQuote(f),
                            "--repo", repo, "--clobber"),
                   stdout = TRUE, stderr = TRUE)
    if (!is.null(attr(res, "status")) && attr(res, "status") != 0) {
      stop(sprintf("Failed to upload %s: %s", basename(f),
                   paste(res, collapse = "\n")))
    }
  }
  message(sprintf("  Uploaded %d wc2026 files to blog-latest (parquet + CSV).",
                  length(wc_files)))
}

message("\n=== WC 2026 blog export complete ===")

# 8. Reference-fact validation ----
# Run the WC2026_REFERENCE_FACTS library against the just-published
# outputs. Each fact encodes a real-world claim (Norway topped UEFA
# qualifying → Elo > 1550; top 8 by champ% should include >=6 perennial
# favourites; etc.). When a fact fails, either the pipeline regressed
# OR a fact itself is stale and needs updating — both worth attention.
run_wc2026_reference_checks(cache_dir)
