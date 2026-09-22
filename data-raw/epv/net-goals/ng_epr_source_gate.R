# ng_epr_source_gate.R -- does EPR built on net goals beat EPR built on EPV,
# inside the PRODUCTION match models rather than a one-feature proxy?
#
# The metric is LOCKED and lives in pannaverse/docs/plans/EPR-SOURCE-SWITCH-GATE.md,
# written before the candidate data existed. This script does not choose it:
#   primary  -- outcome multiclass log loss (lower better), goals RMSE (lower better)
#   decision -- net goals ships only if log loss is <= EPV's AND neither goals
#               RMSE is worse by more than 0.005. A TIE SHIPS NOTHING.
# Do not edit the rule here. If it needs changing, change it in the doc first
# and say why, or the gate stops being a gate.
#
# WHY NOT JUST FLIP THE TOGGLE. `home_sum_epr` / `away_sum_epr` are features of
# both XGBoost models (05 takes every numeric column), so changing what EPR
# means moves a feature distribution under models fitted on the old one.
#
# TWO TRAPS THIS SCRIPT EXISTS TO AVOID, both found by reading the code:
#
#  1. `build_epr_weekly.R` is INCREMENTAL. It reuses every snapshot older than
#     a 28-day buffer from the existing parquet. A net-goals arm run normally
#     would be ~99% EPV-derived with a 28-day tail, both arms would look the
#     same, and the gate would report a FALSE NULL that reads like diligence.
#     Both arms therefore run with EPR_FORCE_FULL_REBUILD=1 -- both, because
#     building them differently makes the rebuild method a confound.
#
#  2. Its output path is hardcoded to ../pannadata/data/opta/, the shared data
#     bus that step 02 and the predictions pipeline read. This script backs that
#     file up and restores it in an on.exit handler, so an interruption does not
#     leave production holding whichever arm died.
#
# It lives HERE rather than under match-predictions-opta/ with the other
# runners because .gitignore excludes `data-raw/**/_run_*.R` -- those are local
# throwaways by convention. This is not one: it carries the locked decision rule
# and the two traps that would otherwise have to be rediscovered.
#
# Run from panna/, DETACHED (it is ~2 hours and Claude Code's reaper kills
# tracked background shells):
#   Rscript data-raw/epv/net-goals/ng_epr_source_gate.R

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

CACHE <- "data-raw/cache-predictions-opta"
EPR_PATH <- "../pannadata/data/opta/opta_epr_weekly.parquet"
OUT <- "data-raw/cache/epv/net-goals"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
say <- function(...) { cat("[", format(Sys.time(), "%H:%M:%S"), "] ", ..., "\n", sep = ""); flush.console() }

# ---- protect the shared data bus ------------------------------------------
BACKUP <- file.path(tempdir(), "opta_epr_weekly_PREGATE.parquet")
if (!file.exists(EPR_PATH)) stop("No existing ", EPR_PATH, " to protect; refusing to run.")
file.copy(EPR_PATH, BACKUP, overwrite = TRUE)
say("backed up production EPR to ", BACKUP)
restored <- FALSE
on.exit({
  if (!restored) {
    file.copy(BACKUP, EPR_PATH, overwrite = TRUE)
    say("RESTORED production EPR from backup (on.exit)")
  }
}, add = TRUE)

# ---- preconditions ---------------------------------------------------------
# Checked here rather than trusted: the gate is worthless on a partial backfill,
# and "it looked built" has already cost a night this week.
MIN_SEASON_END_YEAR <- 2016L   # must match what run_arm() sets; see below
gl_files <- list.files(CACHE, pattern = "^game_logs_\\d{4}-\\d{4}\\.parquet$", full.names = TRUE)
# Check only the seasons the arms will actually USE. Checking the full set would
# abort on pre-2015 files that are deliberately excluded -- the gate would refuse
# to run on exactly the condition it was just configured to handle.
.sy <- suppressWarnings(as.integer(
  sub("^game_logs_[0-9]{4}-([0-9]{4})[.]parquet$", "\\1", basename(gl_files))))
gl_files <- gl_files[!is.na(.sy) & .sy >= MIN_SEASON_END_YEAR]
if (length(gl_files) == 0L) stop("No game-log seasons at or after ", MIN_SEASON_END_YEAR, ".")
say("checking ", length(gl_files), " season(s) from ", MIN_SEASON_END_YEAR, " onward")
cov <- rbindlist(lapply(gl_files, function(f) {
  d <- as.data.table(arrow::read_parquet(f))
  data.table(file = basename(f), rows = nrow(d),
             has_ng = "net_goals" %in% names(d),
             ng_rows = if ("net_goals" %in% names(d)) sum(!is.na(d$net_goals)) else 0L)
}))
cov[, pct := round(100 * ng_rows / rows, 2)]
say("game-log seasons on disk: ", nrow(cov), "; carrying net_goals: ", sum(cov$has_ng))
print(cov[order(file)], row.names = FALSE)
if (any(!cov$has_ng) || min(cov$pct) < 95) {
  stop("Preconditions not met: every season must carry net_goals at >=95% coverage. ",
       "Run the backfill first -- a gate on a partial history compares a biased slice.")
}

# ---- one arm ---------------------------------------------------------------
run_arm <- function(source_name) {
  say("================ ARM: EPR_SOURCE = ", source_name, " ================")
  # Clear the downstream caches so nothing is silently reused between arms.
  for (f in c("04_match_dataset.rds", "05_goals_model.rds",
              "06_outcome_model.rds", "07_predictions.rds")) {
    p <- file.path(CACHE, f); if (file.exists(p)) file.remove(p)
  }
  Sys.setenv(EPR_FORCE_FULL_REBUILD = "1")
  assign("EPR_SOURCE", source_name, envir = globalenv())
  # BOTH arms restricted to the same window (Pete, 2026-09-23). net_goals
  # cannot exist before 2015-2016 -- those seasons have events but no lineups
  # for most leagues -- so leaving it off would fail the 95% coverage guard,
  # and applying it to one arm only would confound the rating change with a
  # population change. This is the whole reason the flag exists.
  assign("EPR_MIN_SEASON_END_YEAR", MIN_SEASON_END_YEAR, envir = globalenv())
  say("building EPR (full rebuild, ~25 min) ...")
  source("data-raw/match-predictions-opta/build_epr_weekly.R", local = FALSE)

  for (st in c("02_player_ratings_to_team", "02b_team_skill_features",
               "03_team_rolling_features", "04_build_match_dataset",
               "05_fit_goals_model", "06_fit_outcome_model",
               "08_evaluate_model")) {
    say("  step ", st)
    source(sprintf("data-raw/match-predictions-opta/%s.R", st), local = FALSE)
  }

  # 08 leaves its numbers in the global env; pull the ones the locked metric
  # names, and fail loudly rather than reporting NA if any is absent.
  grab <- function(nm) {
    if (!exists(nm, envir = globalenv())) {
      stop("08_evaluate_model did not leave `", nm, "` -- the metric cannot be read, ",
           "and reporting NA here would look like a tie.")
    }
    get(nm, envir = globalenv())
  }
  data.table(arm = source_name,
             logloss   = grab("test_logloss"),
             accuracy  = grab("test_accuracy"),
             home_rmse = grab("home_rmse"),
             away_rmse = grab("away_rmse"))
}

res <- rbindlist(list(run_arm("epv"), run_arm("net_goals")), fill = TRUE)

file.copy(BACKUP, EPR_PATH, overwrite = TRUE); restored <- TRUE
say("restored production EPR")

# ---- the locked decision ---------------------------------------------------
say("\n==== EPR SOURCE GATE ====")
say("Lower is better on log loss and both RMSEs. Higher is better on accuracy.")
print(res[, .(arm, logloss = round(logloss, 5), accuracy = round(accuracy, 4),
              home_rmse = round(home_rmse, 4), away_rmse = round(away_rmse, 4))],
      row.names = FALSE)

a <- res[arm == "epv"]; b <- res[arm == "net_goals"]
d_ll <- b$logloss - a$logloss
d_h  <- b$home_rmse - a$home_rmse
d_a  <- b$away_rmse - a$away_rmse
say(sprintf("\nlog loss difference (net goals - epv): %+.5f", d_ll))
say(sprintf("home RMSE difference               : %+.5f", d_h))
say(sprintf("away RMSE difference               : %+.5f", d_a))

ships <- (d_ll <= 0) && (d_h <= 0.005) && (d_a <= 0.005)
say("")
say(if (ships)
      "NET GOALS SHIPS on the locked rule: log loss no worse, neither RMSE worse by >0.005."
    else
      "NET GOALS DOES NOT SHIP on the rule fixed before the run. This is not a failure of the ledger -- it stays the more correct accounting object and can publish as its own column without feeding the rating.")
fwrite(res, file.path(OUT, "epr_source_gate.csv"))
say("wrote ", file.path(OUT, "epr_source_gate.csv"))
