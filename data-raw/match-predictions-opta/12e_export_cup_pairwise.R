# 12e_export_cup_pairwise.R
# Export the full-model pairwise knockout lookup for UCL/UEL/UECL (the club
# cup sibling of step 11's wc2026_knockout_probs.parquet). See
# R/cup_pairwise_model.R for the three ways this differs from the WC version
# (live team state, pooled-only routing, two unaveraged legs).
#
# Inputs:
#   cache-predictions-opta/04_match_dataset.rds -- per-team feature rows
#   cache-predictions-opta/05_goals_model.rds    -- goals models
#   cache-predictions-opta/06_outcome_model.rds  -- outcome model
#
# Output:
#   cache-predictions-opta/cup_pairwise.parquet -- one row per unordered
#   team pair PER COMPETITION (league column: UCL/UEL/UECL), with both legs'
#   predicted goals + outcome probabilities. The blog's cup-knockout-phase
#   Sim tab reads this instead of a client-side Tiento-difference regression.

if (!exists("cache_dir")) cache_dir <- file.path("data-raw", "cache-predictions-opta")

devtools::load_all()
library(data.table)

message("\n=== Exporting cup pairwise knockout lookup (UCL/UEL/UECL) ===\n")

match_dataset  <- readRDS(file.path(cache_dir, "04_match_dataset.rds"))
goals_models   <- readRDS(file.path(cache_dir, "05_goals_model.rds"))
outcome_result <- readRDS(file.path(cache_dir, "06_outcome_model.rds"))

md_df <- as.data.frame(match_dataset)

all_probs <- vector("list", 3)
names(all_probs) <- c("UCL", "UEL", "UECL")
freshness <- list()

for (lg in c("UCL", "UEL", "UECL")) {
  seasons_lg <- unique(md_df$season[md_df$league == lg])
  if (length(seasons_lg) == 0) {
    message(sprintf("  %s: no rows in match_dataset -- skipping (competition not in scope this run?)", lg))
    next
  }
  season <- sort(seasons_lg, decreasing = TRUE)[1]
  lk <- build_cup_pairwise_lookup(match_dataset, goals_models, outcome_result, lg, season)
  probs <- lk$probs
  probs[, league := lg]
  probs[, season := season]
  all_probs[[lg]] <- probs

  as_of_days <- vapply(names(lk$team_as_of), function(tm) {
    as.integer(abs(as.numeric(lk$team_as_of[[tm]] - Sys.Date())))
  }, integer(1))
  freshness[[lg]] <- list(n_teams = lk$n_teams, max_stale = max(as_of_days), median_stale = median(as_of_days))
  message(sprintf("  %s: %d teams, %d pairs, staleness median=%.1fd max=%dd",
                  lg, lk$n_teams, nrow(probs), median(as_of_days), max(as_of_days)))
}

all_probs <- all_probs[!vapply(all_probs, is.null, logical(1))]
if (length(all_probs) == 0) {
  stop("12e_export_cup_pairwise: no cup competitions produced a lookup -- nothing to publish.", call. = FALSE)
}
combined <- rbindlist(all_probs)

# Refuse rather than ship a badly stale read for an entire competition --
# same "publish + flag, never silently degrade" instinct as issue #85's
# degraded_features guard, but a hard gate here: 33/7/14 days measured on
# 2026-09-10 real data is comfortably fresh (mid-week/matchday cadence), so
# 60 days (missed most of a month of scheduled action) is a genuine anomaly
# worth stopping the publish over, not a normal gap.
STALE_DAYS_ABORT <- 60L
stale_leagues <- names(freshness)[vapply(freshness, function(f) f$max_stale > STALE_DAYS_ABORT, logical(1))]
if (length(stale_leagues) > 0) {
  stop(sprintf(
    "12e_export_cup_pairwise: %s exceeded the %d-day staleness gate (see freshness log above). Refusing to publish a cup_pairwise.parquet built on stale team state.",
    paste(stale_leagues, collapse = ", "), STALE_DAYS_ABORT), call. = FALSE)
}

out_path <- file.path(cache_dir, "cup_pairwise.parquet")
arrow::write_parquet(combined, out_path)
csv_path <- sub("\\.parquet$", ".csv", out_path)
write.csv(combined, csv_path, row.names = FALSE)
message(sprintf("\n  cup_pairwise.parquet: %d rows across %d competition(s)", nrow(combined), length(all_probs)))

if (exists("publish_files", envir = .GlobalEnv)) {
  publish_files$blog_latest <<- c(publish_files$blog_latest, out_path, csv_path)
  message("  Registered cup_pairwise.parquet (+ CSV) for blog-latest publish (step 13).")
} else {
  message("  (standalone run -- not registered for step-13 publish)")
}

message("\n=== Cup pairwise export complete ===")
