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
  as.data.table(readRDS(groups_cache_rds))
} else if (nzchar(groups_pkg_csv) && file.exists(groups_pkg_csv)) {
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
sim <- sim[, .(team, group, p_R16, p_QF, p_SF, p_final, p_champ)]
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

# 5. Team strength across rating categories ----
# Squad-aggregate metrics are pulled straight from the WC2026 rows of the
# match dataset (each team's home_* values equal its away_* values — they are
# team properties). BT strength + champion probability come from the sim.

md <- as.data.frame(readRDS(file.path(cache_dir, "04_match_dataset.rds")))
wc <- md[md$league == WC2026_LEAGUE & md$season == wc_season &
           !is.na(md$home_team) & md$home_team != "" &
           !is.na(md$away_team) & md$away_team != "", ]
teams <- sort(unique(c(wc$home_team, wc$away_team)))

# metric -> (home column, away column) in the match dataset
metric_cols <- c(panna = "sum_panna", offense = "sum_offense",
                 defense = "sum_defense", epr = "sum_epr",
                 psr = "sum_psr", elo = "elo")
# Return NA (not numeric(0)) on missing data so vapply(numeric(1)) doesn't
# abort the whole export. The error then surfaces as visible NAs in the
# published parquet and the warning below, rather than killing step 12.
# Causes can include: a column genuinely missing from match_dataset (step
# 02b/03 schema drift), a team appearing in `teams` without a matching row
# (despite the filter — defensive belt-and-braces), or NA in a single cell.
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
strength <- data.table(team = teams, group = unname(team_group[teams]))
for (m in names(metric_cols)) {
  strength[[m]] <- round(vapply(teams, team_metric, numeric(1),
                                base = metric_cols[[m]]), 4)
}

# HARD STOP if any critical metric is NA for any WC team. The previous
# warning-only pattern shipped wc2026_team_strength.parquet with epr = NA
# for all 48 teams (5pm 2026-05-29: root cause was step 02 silently
# skipping the EPR merge on GHA due to a relative-path bug — fix landed,
# but adding the stop here as a second layer of defense so the next
# silent-skip elsewhere can't ship to the blog).
#
# Per [[feedback-no-silent-imputation]]: missing values in a published
# rating must HALT, not be silently published. If you genuinely want to
# ship partial data (e.g., a metric truly doesn't apply to intl tournaments),
# remove that metric from metric_cols rather than papering over the NA.
na_cells <- which(is.na(as.matrix(strength[, names(metric_cols), with = FALSE])),
                  arr.ind = TRUE)
if (nrow(na_cells) > 0L) {
  na_summary <- data.table(
    team   = strength$team[na_cells[, "row"]],
    metric = names(metric_cols)[na_cells[, "col"]]
  )[order(metric, team)]
  per_metric <- na_summary[, .N, by = metric][order(-N)]
  stop(sprintf(
    "wc2026_team_strength has %d NA cells across %d team(s) and %d metric(s) — refusing to publish a partial rating table.\n\nPer-metric NA counts:\n%s\n\nFull list:\n  %s\n\nFix the upstream missing-data root cause (likely step 02 EPR/PSR merge or step 03/04 column drift) before re-running step 12. To exclude a metric that's expected to be NA for intl, remove it from metric_cols in this script.",
    nrow(na_cells), uniqueN(na_summary$team), nrow(per_metric),
    paste(sprintf("  %s: %d / 48", per_metric$metric, per_metric$N),
          collapse = "\n"),
    paste(sprintf("%s/%s", na_summary$team, na_summary$metric),
          collapse = ", ")
  ), call. = FALSE)
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
setorder(strength, -p_champ)
write_parquet(strength, file.path(cache_dir, "wc2026_team_strength.parquet"))
message(sprintf("  wc2026_team_strength.parquet: %d teams x %d categories",
                nrow(strength), length(metric_cols) + 2L))

# 5b. Squad rows with player ratings ----
# One row per squad player: announced/derived squad membership (step 02's
# announced_squads cache) joined to the latest-season player ratings. Feeds
# the blog's world-cup-team.qmd squad table. Ratings stay NA when a player
# has no rated club minutes (smaller leagues) — the blog renders a dash;
# per [[feedback-no-silent-imputation]] we do NOT zero-fill here (the 0s in
# step 02 exist for the team aggregator, not for published per-player rows).

squads_path <- file.path(cache_dir, "wc2026_announced_squads.parquet")
if (!file.exists(squads_path)) {
  stop("wc2026_announced_squads.parquet not found in ", cache_dir,
       " — run announced_squads.R (step 02) first.")
}
squads <- as.data.table(read_parquet(squads_path))

# Latest-season per-player ratings: same source preference as step 02
# (skill-based first, raw-stat fallback).
sq_skill_path <- file.path("data-raw", "cache-skills", "06_seasonal_ratings.rds")
sq_raw_path   <- file.path("data-raw", "cache-opta", "07_seasonal_ratings.rds")
sq_seasonal <- if (file.exists(sq_skill_path)) readRDS(sq_skill_path) else readRDS(sq_raw_path)

sq_xrapm <- as.data.table(sq_seasonal$seasonal_xrapm)
sq_xrapm <- sq_xrapm[order(player_id, -season_end_year), .SD[1L], by = player_id,
                     .SDcols = intersect(c("xrapm", "offense", "defense", "total_minutes"),
                                         names(sq_xrapm))]
setnames(sq_xrapm, "xrapm", "panna", skip_absent = TRUE)

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
squad_out <- merge(squad_out, sq_xrapm, by = "player_id", all.x = TRUE)
if (!is.null(sq_psr)) squad_out <- merge(squad_out, sq_psr, by = "player_id", all.x = TRUE)
if (!is.null(sq_epr)) squad_out <- merge(squad_out, sq_epr, by = "player_id", all.x = TRUE)
for (col in c("psr", "epr")) if (!col %in% names(squad_out)) squad_out[[col]] <- NA_real_

setcolorder(squad_out, c("team", "group", "player_id", "player_name", "position",
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
                 "wc2026_squads.parquet")
for (p in wc_parquets) {
  pp <- file.path(cache_dir, p)
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
