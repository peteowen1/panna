# 04d: legacy SPM prior AS OF season S (R080). Local, like 04c's panel: run
# once per new season, not in the daily pipeline.
#
# Builds the legacy box-score SPM using only seasons <= S: pooled RAPM target
# over seasons <= S, box stats + xMetrics from seasons <= S, then production
# 05_spm.R (legacy path) on that filtered cache. 07_seasonal_ratings.R gives
# vintage Y's off-panel players (every GK, newcomers) the table built at
# S = Y - 1, the same cut-off as the S6 vintage itself. 04e combines them.
#
# Cost (measured 2026-10-13/14): ~8 min and ~7 GB peak per season.
# Usage (from panna/): Rscript data-raw/player-ratings-opta/04d_build_legacy_spm_asof.R <S>
#   Output: data-raw/cache-legacy-asof/<S>/05_spm.rds (skipped if it exists).
#   S = 9999 builds the no-cutoff baseline, which must reproduce 05_spm.rds's
#   offense_spm_ratings_legacy.
suppressMessages({library(data.table); devtools::load_all(quiet = TRUE)})
source(file.path("data-raw", "pipeline_utils.R"))
S <- as.integer(commandArgs(trailingOnly = TRUE)[1]); stopifnot(!is.na(S))
src <- file.path("data-raw", "cache-opta")
root <- Sys.getenv("LEGACY_ASOF_DIR", file.path("data-raw", "cache-legacy-asof"))
D <- file.path(root, S)
if (file.exists(file.path(D, "05_spm.rds"))) {
  cat(sprintf("%s already built, skipping\n", file.path(D, "05_spm.rds"))); quit(save = "no")
}
dir.create(D, recursive = TRUE, showWarnings = FALSE)
t0 <- Sys.time()
ts <- function(...) cat(sprintf("[%s +%.1fm] ", format(Sys.time(), "%H:%M:%S"),
                                as.numeric(difftime(Sys.time(), t0, units = "mins"))), ..., "\n")

# 1. Pooled RAPM target over seasons <= S (production 04 settings)
rr <- readRDS(file.path(src, "04_rapm.rds"))
if (S == 9999L) {
  ratings <- rr$ratings[, c("player_id", "rapm", "offense", "defense")]
  ts("baseline: production 04_rapm ratings,", nrow(ratings), "players")
} else {
  sp <- readRDS(file.path(src, "03_splints.rds"))$splints
  smap <- unique(data.table(splint_id = sp$splint_id, season_end_year = sp$season_end_year)); rm(sp)
  set.seed(1)
  fit <- fit_expanding_pooled_rapm(rr$rapm_data, smap, cutoff_year = S + 1L, seed = 1)
  stopifnot(!is.null(fit))
  ratings <- fit$ratings
  ts(sprintf("pooled RAPM <= %d: n_obs %d, lambda %.5f, %d players", S, fit$n_obs, fit$lambda_min, nrow(ratings)))
}
rm(rr); invisible(gc())
saveRDS(list(ratings = ratings), file.path(D, "04_rapm.rds"))

# 2. Box stats + xMetrics from seasons <= S
b <- read_spm_opta_stats(src)
sey <- function(x) if ("season_end_year" %in% names(x)) as.integer(x$season_end_year) else extract_season_end_year(x$season)
os <- b$opta_stats; xm <- b$opta_xmetrics; rm(b)
if (S != 9999L) {
  os <- os[sey(os) <= S, ]
  if (!is.null(xm)) xm <- xm[sey(xm) <= S, ]
}
stopifnot(nrow(os) > 0)
ts(sprintf("stats rows %d (max season %d), xmetrics rows %d", nrow(os), max(sey(os)), if (is.null(xm)) 0L else nrow(xm)))
saveRDS(list(opta_stats = os, opta_xmetrics = xm), file.path(D, "02_opta_stats_narrow.rds"))
rm(os, xm); invisible(gc())

# 3. Production 05_spm.R on the filtered cache, legacy path only
cache_dir <- D; spm_use_panel <- FALSE; run_multi_target <- FALSE
source(file.path("data-raw", "player-ratings-opta", "05_spm.R"))
ts("05_spm done:", nrow(offense_spm_ratings), "offense rows")
