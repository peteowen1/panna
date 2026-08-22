# 04b_export_match_features.R
# Export the team-strength features the goals/outcome models actually consume,
# per fixture, as a compact parquet.
#
# Why this exists: 04_match_dataset.rds is a build artifact. The GHA runner
# creates it, uses it, and throws it away -- it is not uploaded anywhere. So
# any question of the form "what did the model actually see for this fixture?"
# is unanswerable outside a live pipeline run. Two separate investigations
# stalled on exactly that in the same week (panna#190, panna#192, both of which
# say so in as many words), and the second one needs these numbers to size a
# shrinkage prior rather than guess at one.
#
# Deliberately NOT the whole match dataset: ~170 feature columns x 58k rows is
# a large asset to publish for a diagnostic. This exports the identity columns,
# the strength subset, and the outcome columns needed to calibrate against
# actuals.
#
# Inputs:
#   cache-predictions-opta/04_match_dataset.rds
#   cache-predictions-opta/05_goals_model.rds   -- for feature_cols
#
# Outputs:
#   cache-predictions-opta/match_features.parquet  (registered for
#   predictions-latest; additive, no existing consumer reads it)

suppressMessages({library(data.table); library(arrow)})

md_path <- file.path(cache_dir, "04_match_dataset.rds")
gm_path <- file.path(cache_dir, "05_goals_model.rds")
stopifnot(file.exists(md_path))

md <- as.data.table(readRDS(md_path))
message(sprintf("  match dataset: %d rows x %d cols", nrow(md), ncol(md)))

# The strength subset, defined EXACTLY as 07_predict_fixtures.R defines it for
# its degraded-features guard. Same regex, deliberately -- a diagnostic that
# describes a different set of columns than the guard inspects would send the
# next investigation somewhere the guard never looks. If that regex changes,
# this must change with it.
feature_cols <- if (file.exists(gm_path)) readRDS(gm_path)$feature_cols else names(md)
strength_cols <- unique(c(
  grep("^(home|away)_elo$|^elo_diff$", feature_cols, value = TRUE),
  grep("^(home|away)_(sum|avg|max|min|gk|stdev)_", feature_cols, value = TRUE),
  grep("^(home|away)_sk_", feature_cols, value = TRUE),
  grep("_diff$", feature_cols, value = TRUE)
))
strength_cols <- intersect(strength_cols, names(md))

id_cols <- intersect(c("match_id", "match_date", "league", "season",
                       "season_end_year", "split", "match_status",
                       "home_team", "away_team", "home_team_id", "away_team_id",
                       "home_goals", "away_goals", "home_xg", "away_xg"),
                     names(md))

out <- md[, c(id_cols, strength_cols), with = FALSE]
message(sprintf("  exporting %d identity + %d strength column(s)",
                length(id_cols), length(strength_cols)))

# Assert the export is actually usable for the question it exists to answer:
# a strength subset that came back empty would still write a valid parquet and
# still look like a successful step.
if (length(strength_cols) == 0L) {
  stop("04b: no strength feature columns matched -- the regex and the match ",
       "dataset have diverged. Fix before trusting any diagnostic built on this.",
       call. = FALSE)
}

out_path <- file.path(cache_dir, "match_features.parquet")
arrow::write_parquet(out, out_path)
message(sprintf("  Written: %s (%d rows, %.1f MB)",
                basename(out_path), nrow(out), file.size(out_path) / 1024^2))

# Coverage summary in the log, so a run tells you whether the thing you came
# for is present without downloading the asset first.
if ("split" %in% names(out)) {
  message("  rows by split: ",
          paste(sprintf("%s=%d", names(table(out$split)), as.integer(table(out$split))),
                collapse = ", "))
}

if (exists("publish_files", envir = .GlobalEnv)) {
  publish_files$predictions_latest <<- c(publish_files$predictions_latest, out_path)
  message("  Registered match_features.parquet for predictions-latest publish (step 13)")
} else {
  message("  (standalone run -- not registered for step-13 publish)")
}
