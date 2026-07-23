# 04c_build_spm_panel.R
#
# Wave 2 (BOX-SCORE-VALUE-SPM-REDESIGN.md sec 2.2, sec 3.1): builds the SPM
# training panel -- one row per (player, vintage year Y) -- via
# build_spm_panel() (R/spm_panel.R), reading:
#   - data-raw/cache-skills/01_match_stats.rds (match-grain box stats,
#     compute_match_level_opta_stats() output)
#   - data-raw/cache-opta/rapm_window_targets.rds (Wave 1's windowed
#     prior-free RAPM target, 04b_rapm_window_targets.R)
#
# DO NOT read cache-opta/04_rapm.rds while a step-04 refresh may be
# mid-write (see task brief) -- this script never touches it; the window
# targets cache is the only RAPM artifact it depends on.
#
# Caches to cache-opta/spm_panel.rds as a list(panel = <data.table>,
# target_provenance = "prior_free_rapm_window", builder_params = <list>) --
# the panel itself also carries these as attributes (see build_spm_panel()),
# duplicated at the top level of the saved list so downstream scripts don't
# need to know about R attribute semantics to read them back.
#
# Config overrides (exists() pattern, set before sourcing):
#   vintage_years    default 2019:2026
#   window_years     default 5L
#   leagues          default NULL (all leagues) -- pass e.g. c("ENG","ESP")
#                    for a smoke-scale build
#   include_xmetrics default TRUE (best-effort; degrades to box-only per
#                    vintage if local/remote xMetrics coverage is missing)
#   xmetrics_source  default "local"
#   include_gk       default FALSE (outfield-only panel; panna#159 owns GK)
#   force_rebuild    default FALSE
#
# Run from panna/ (relative cache paths assume cwd = panna/).

# 1. Setup ----

devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-opta")
if (!exists("cache_skills_dir", inherits = FALSE)) cache_skills_dir <- file.path("data-raw", "cache-skills")

vintage_years <- if (exists("vintage_years", inherits = FALSE)) vintage_years else 2019:2026
window_years <- if (exists("window_years", inherits = FALSE)) window_years else 5L
leagues <- if (exists("leagues", inherits = FALSE)) leagues else NULL
include_xmetrics <- if (exists("include_xmetrics", inherits = FALSE)) include_xmetrics else TRUE
xmetrics_source <- if (exists("xmetrics_source", inherits = FALSE)) xmetrics_source else "local"
include_gk <- if (exists("include_gk", inherits = FALSE)) include_gk else FALSE
force_rebuild <- if (exists("force_rebuild", inherits = FALSE)) force_rebuild else FALSE

output_path <- if (exists("output_path", inherits = FALSE)) output_path else
  file.path(cache_dir, "spm_panel.rds")

if (!isTRUE(force_rebuild) && file.exists(output_path)) {
  cat(sprintf("%s already exists (force_rebuild = FALSE) -- nothing to do.\n", output_path))
} else {

  # 2. Load inputs ----

  cat("\n=== Loading match-grain box stats + windowed prior-free RAPM targets ===\n")

  targets_path <- file.path(cache_dir, "rapm_window_targets.rds")
  if (!file.exists(targets_path)) {
    cli::cli_abort("Expected {.file {targets_path}} (built by 04b_rapm_window_targets.R) -- run that first.")
  }
  rapm_window_targets <- readRDS(targets_path)
  cat(sprintf("Windowed RAPM targets: %d vintage(s) [%s]\n",
              length(rapm_window_targets), paste(names(rapm_window_targets), collapse = ",")))

  match_stats_path <- file.path(cache_skills_dir, "01_match_stats.rds")
  if (!file.exists(match_stats_path)) {
    cli::cli_abort("Expected {.file {match_stats_path}} -- run the estimated-skills pipeline's step 01 first.")
  }
  match_stats <- readRDS(match_stats_path)
  cat(sprintf("Match-grain box stats: %d rows, %d columns\n", nrow(match_stats), ncol(match_stats)))
  if (!is.null(leagues)) {
    cat(sprintf("Restricting to leagues: %s\n", paste(leagues, collapse = ", ")))
  }

  # 3. Build panel ----

  cat("\n=== Building SPM panel ===\n")
  t0 <- Sys.time()
  panel <- build_spm_panel(
    match_stats, rapm_window_targets,
    vintage_years = vintage_years, window_years = window_years,
    leagues = leagues, include_xmetrics = include_xmetrics,
    xmetrics_source = xmetrics_source, include_gk = include_gk,
    strict_window_check = TRUE
  )
  cat(sprintf("Panel built in %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))

  # 4. Sanity-print + save ----

  cat(sprintf("\nPanel dimensions: %d rows x %d cols\n", nrow(panel), ncol(panel)))
  cat(sprintf("Players: %d | Vintages: %s\n",
              data.table::uniqueN(panel$player_id),
              paste(sort(unique(panel$vintage_year)), collapse = ",")))
  cat("Rows per vintage:\n")
  print(panel[, .N, by = vintage_year][order(vintage_year)])
  cat("Rows per role group:\n")
  print(panel[, .N, by = role_group][order(-N)])
  cat(sprintf("Window minutes: median=%.0f, p10=%.0f, p90=%.0f\n",
              stats::median(panel$window_minutes), stats::quantile(panel$window_minutes, 0.1),
              stats::quantile(panel$window_minutes, 0.9)))

  out <- list(
    panel = panel,
    target_provenance = attr(panel, "target_provenance"),
    builder_params = attr(panel, "builder_params")
  )
  saveRDS(out, output_path)
  cat(sprintf("\nSaved %s\n", output_path))
}
