# profile_pipeline.R
# Benchmark key pipeline functions to identify bottlenecks
#
# Usage:
#   Rscript data-raw/analysis/profile_pipeline.R
#   # Or with profvis (interactive):
#   source("data-raw/analysis/profile_pipeline.R")

# 1. Setup ----

devtools::load_all()
library(data.table)

cat("=== Panna Pipeline Profiler ===\n\n")

# 2. Configuration ----

# Use a small dataset for profiling
LEAGUE <- "ENG"
SEASONS <- c("2023-2024")
USE_PROFVIS <- requireNamespace("profvis", quietly = TRUE)

cat(sprintf("League: %s\n", LEAGUE))
cat(sprintf("Seasons: %s\n", paste(SEASONS, collapse = ", ")))
cat(sprintf("profvis available: %s\n\n", USE_PROFVIS))

timings <- list()

# 3. Data Loading ----

cat("--- Data Loading ---\n")

timings$load_summary <- system.time({
  summary_data <- load_summary(LEAGUE, SEASONS[1], source = "local")
})
cat(sprintf("  load_summary(): %.2fs\n", timings$load_summary["elapsed"]))

timings$load_metadata <- system.time({
  metadata <- load_metadata(LEAGUE, SEASONS[1], source = "local")
})
cat(sprintf("  load_metadata(): %.2fs\n", timings$load_metadata["elapsed"]))

timings$load_passing <- system.time({
  passing <- load_passing(LEAGUE, SEASONS[1], source = "local")
})
cat(sprintf("  load_passing(): %.2fs\n", timings$load_passing["elapsed"]))

# 4. Data Processing ----

cat("\n--- Data Processing ---\n")

timings$process_all <- system.time({
  processed <- tryCatch(
    process_all_data(
      results = metadata,
      lineups = NULL,
      events = NULL,
      shooting = load_shots(LEAGUE, SEASONS[1], source = "local"),
      stats_summary = summary_data
    ),
    error = function(e) {
      cat(sprintf("  process_all_data() error: %s\n", conditionMessage(e)))
      NULL
    }
  )
})
cat(sprintf("  process_all_data(): %.2fs\n", timings$process_all["elapsed"]))

# 5. Splint Creation ----

cat("\n--- Splint Creation ---\n")

if (!is.null(processed)) {
  timings$create_splints <- system.time({
    splints <- tryCatch(
      create_all_splints(processed),
      error = function(e) {
        cat(sprintf("  create_all_splints() error: %s\n", conditionMessage(e)))
        NULL
      }
    )
  })
  cat(sprintf("  create_all_splints(): %.2fs\n", timings$create_splints["elapsed"]))

  if (!is.null(splints)) {
    cat(sprintf("    Splints: %d total, %d matches\n",
                nrow(splints), length(unique(splints$match_id))))
  }
} else {
  cat("  Skipped (no processed data)\n")
}

# 6. RAPM ----

cat("\n--- RAPM Pipeline ---\n")

if (!is.null(splints) && exists("splints") && nrow(splints) > 0) {
  timings$rapm_matrix <- system.time({
    rapm_data <- tryCatch(
      create_rapm_design_matrix(splints),
      error = function(e) {
        cat(sprintf("  create_rapm_design_matrix() error: %s\n", conditionMessage(e)))
        NULL
      }
    )
  })
  cat(sprintf("  create_rapm_design_matrix(): %.2fs\n", timings$rapm_matrix["elapsed"]))

  if (!is.null(rapm_data)) {
    cat(sprintf("    Matrix: %d x %d\n", nrow(rapm_data$X), ncol(rapm_data$X)))

    timings$fit_rapm <- system.time({
      rapm_model <- tryCatch(
        fit_rapm(rapm_data, parallel = FALSE),
        error = function(e) {
          cat(sprintf("  fit_rapm() error: %s\n", conditionMessage(e)))
          NULL
        }
      )
    })
    cat(sprintf("  fit_rapm(): %.2fs\n", timings$fit_rapm["elapsed"]))
  }
} else {
  cat("  Skipped (no splints)\n")
}

# 7. SPADL Conversion (Opta) ----

cat("\n--- SPADL Conversion ---\n")

opta_events <- tryCatch(
  load_opta_match_events("EPL", "2023-2024"),
  error = function(e) NULL
)

if (!is.null(opta_events) && nrow(opta_events) > 0) {
  # Pick first match
  first_match <- unique(opta_events$match_id)[1]
  match_events <- opta_events[opta_events$match_id == first_match, ]

  timings$spadl_single <- system.time({
    spadl <- tryCatch(
      convert_opta_to_spadl(match_events),
      error = function(e) {
        cat(sprintf("  convert_opta_to_spadl() error: %s\n", conditionMessage(e)))
        NULL
      }
    )
  })
  cat(sprintf("  convert_opta_to_spadl() [1 match]: %.2fs\n",
              timings$spadl_single["elapsed"]))

  if (!is.null(spadl)) {
    cat(sprintf("    Actions: %d from %d events\n", nrow(spadl), nrow(match_events)))
  }
} else {
  cat("  Skipped (no Opta event data)\n")
}

# 8. Summary ----

cat("\n=== Timing Summary ===\n\n")

timing_df <- data.frame(
  function_name = names(timings),
  elapsed_secs = sapply(timings, function(x) round(x["elapsed"], 3)),
  row.names = NULL,
  stringsAsFactors = FALSE
)
timing_df <- timing_df[order(-timing_df$elapsed_secs), ]
print(timing_df, row.names = FALSE)

total_time <- sum(timing_df$elapsed_secs)
cat(sprintf("\nTotal pipeline time: %.2fs\n", total_time))

# 9. Optional: profvis ----

if (USE_PROFVIS && interactive()) {
  cat("\nRunning profvis on critical path...\n")
  cat("(View in RStudio Viewer)\n")

  if (!is.null(processed)) {
    profvis::profvis({
      splints_prof <- create_all_splints(processed)
      if (nrow(splints_prof) > 0) {
        rapm_data_prof <- create_rapm_design_matrix(splints_prof)
        fit_rapm(rapm_data_prof, parallel = FALSE)
      }
    })
  }
}
