# epr_centring_gate.R -- does EPR from role8-centred game logs predict matches at
# least as well as EPR from today's start_label centring, inside the PRODUCTION
# match models? (Pete 2026-10-19; pannaverse docs/reference/POSITION-TAXONOMY.md
# section 6.)
#
# Both EPR files are already built, from two dry game-log rebuilds on the same
# code, data and models that differ only in `epv_position_grouping`:
#   control: EPR from game-logs rebuilds 39209465598 + 39218406669 (start_label)
#   role8  : EPR from game-logs rebuilds 39209461564 + 39218401108 (role8)
# built by epr-weekly-snapshot.yml runs 39230927287 (control), 39230907202 (role8).
# The workflow puts them at /tmp/epr_control.parquet and /tmp/epr_role8.parquet.
#
# LOCKED RULE, written before either arm's result exists, the same as the EPR
# source gate (pannaverse docs/plans/EPR-SOURCE-SWITCH-GATE.md):
#   primary  -- outcome multiclass log loss (lower better), goals RMSE (lower better)
#   decision -- role8 ships only if its log loss is <= control's AND neither goals
#               RMSE is worse by more than 0.005. Exactly equal log loss ships.
# Run-to-run noise on this log loss is about 0.001, so a difference smaller than
# that either way is read as "costs nothing", not as a win or a loss; the rule
# itself is unchanged from the source gate.
#
# The production EPR at ../pannadata/data/opta/opta_epr_weekly.parquet is backed
# up and restored around the arms; this runs on a throwaway branch with no write
# token, so nothing publishes either way.

suppressPackageStartupMessages({library(data.table); library(dplyr)})
devtools::load_all(quiet = TRUE)

CACHE <- "data-raw/cache-predictions-opta"
EPR_PATH <- "../pannadata/data/opta/opta_epr_weekly.parquet"
OUT <- "data-raw/cache/epr-centring-gate"
dir.create(OUT, recursive = TRUE, showWarnings = FALSE)
say <- function(...) { cat("[", format(Sys.time(), "%H:%M:%S"), "] ", ..., "\n", sep = ""); flush.console() }

ARMS <- c(control = "/tmp/epr_control.parquet", role8 = "/tmp/epr_role8.parquet")
for (a in names(ARMS)) {
  if (!file.exists(ARMS[[a]])) stop("missing EPR file for arm ", a, ": ", ARMS[[a]])
  d <- arrow::read_parquet(ARMS[[a]], col_select = c("player_id", "snapshot_date", "epr"))
  say(sprintf("arm %s: %d rows, %d snapshots, %d players", a, nrow(d),
              length(unique(d$snapshot_date)), length(unique(d$player_id))))
}

BACKUP <- file.path(OUT, "opta_epr_weekly_PREGATE.parquet")
if (!file.exists(EPR_PATH)) stop("No existing ", EPR_PATH, " to protect; refusing to run.")
if (!file.copy(EPR_PATH, BACKUP, overwrite = TRUE)) stop("Could not back up ", EPR_PATH)
say("backed up production EPR to ", BACKUP)

ARM_CACHES <- c("02_team_ratings.rds", "02b_team_skill_features.rds",
                "03_rolling_features.rds", "04_match_dataset.rds",
                "05_goals_model.rds", "06_outcome_model.rds",
                "07_predictions.rds", "08_evaluation.rds")

.gate_reset <- function() {
  drop <- setdiff(ls(globalenv(), all.names = TRUE), .gate_keep)
  rm(list = drop, envir = globalenv())
  g <- gc(reset = TRUE)
  say(sprintf("dropped %d globals from before this arm; R heap now %.1f GB", length(drop), sum(g[, 2]) / 1024))
}

run_arm <- function(arm) {
  say("================ ARM: ", arm, " ================")
  .gate_reset()
  # Steps 02-06 reuse their .rds when present: clear them, or both arms get the
  # same features (step 02 is where EPR becomes home_sum_epr / away_sum_epr).
  for (f in ARM_CACHES) { p <- file.path(CACHE, f); if (file.exists(p)) file.remove(p) }
  assign("force_rebuild", TRUE, envir = globalenv())
  if (!file.copy(ARMS[[arm]], EPR_PATH, overwrite = TRUE)) stop("could not install arm ", arm)
  say("installed ", ARMS[[arm]], " as ", EPR_PATH)
  for (st in c("02_player_ratings_to_team", "02b_team_skill_features",
               "03_team_rolling_features", "04_build_match_dataset",
               "05_fit_goals_model", "06_fit_outcome_model",
               "08_evaluate_model")) {
    say("  step ", st)
    source(sprintf("data-raw/match-predictions-opta/%s.R", st), local = FALSE)
  }
  file.copy(file.path(CACHE, "08_evaluation.rds"),
            file.path(OUT, paste0("08_evaluation_", arm, ".rds")), overwrite = TRUE)
  grab <- function(nm) {
    if (!exists(nm, envir = globalenv())) {
      stop("08_evaluate_model did not leave `", nm, "`; reporting NA would look like a tie.")
    }
    get(nm, envir = globalenv())
  }
  .g <- gc()
  say(sprintf("arm %s: R heap max used %.1f GB", arm, sum(.g[, 6]) / 1024))
  data.table(arm = arm, logloss = grab("test_logloss"), accuracy = grab("test_accuracy"),
             home_rmse = grab("home_rmse"), away_rmse = grab("away_rmse"))
}
.gate_keep <- c(ls(globalenv(), all.names = TRUE), ".gate_keep")

res <- tryCatch(
  rbindlist(list(run_arm("control"), run_arm("role8")), fill = TRUE),
  finally = {
    if (!file.copy(BACKUP, EPR_PATH, overwrite = TRUE)) {
      stop("RESTORE FAILED: copy ", BACKUP, " back to ", EPR_PATH, " by hand")
    }
    say("restored production EPR from ", BACKUP)
    file.remove(file.path(CACHE, ARM_CACHES)[file.exists(file.path(CACHE, ARM_CACHES))])
  }
)

say("\n==== EPR CENTRING GATE (role8 vs start_label) ====")
say("Lower is better on log loss and both RMSEs. Higher is better on accuracy.")
print(res[, .(arm, logloss = round(logloss, 5), accuracy = round(accuracy, 4),
              home_rmse = round(home_rmse, 4), away_rmse = round(away_rmse, 4))], row.names = FALSE)
a <- res[arm == "control"]; b <- res[arm == "role8"]
d_ll <- b$logloss - a$logloss; d_h <- b$home_rmse - a$home_rmse; d_a <- b$away_rmse - a$away_rmse
say(sprintf("log loss difference (role8 - control): %+.5f", d_ll))
say(sprintf("home RMSE difference                 : %+.5f", d_h))
say(sprintf("away RMSE difference                 : %+.5f", d_a))
ships <- (d_ll <= 0) && (d_h <= 0.005) && (d_a <= 0.005)
say(if (ships) "ROLE8 SHIPS on the locked rule: log loss no worse, neither RMSE worse by >0.005."
    else "ROLE8 DOES NOT SHIP on the rule fixed before the run.")
fwrite(res, file.path(OUT, "epr_centring_gate.csv"))
