# 13f_r080_fallback_gate.R -- review R080: what prior should players with no
# S6 vintage row get?
#
# Production 07_seasonal_ratings.R gives each season S the S6 prior from
# vintage S (window [S-5, S)). Players absent from that vintage -- every
# goalkeeper (S6 is outfield-only) and every outfielder with no panel history
# in the window -- get prior 0. 2019-2026 that is 18.3% of xRAPM minutes
# (GK 8.7%, newcomers 9.6%). The review proposed falling back to the career
# hybrid table, which is built on all seasons (hindsight, the leak panna#258
# removed), so that fallback is NOT tested here. Tested instead:
#   arm Z (production): vintage-S S6 prior, 0 for anyone missing
#   arm F (fallback):   vintage-S S6 prior; anyone missing gets the legacy
#                       SPM rebuilt on stats + pooled RAPM from seasons <= S
#                       only (13e0_legacy_spm_asof.R -> cache-r083/<S>/)
# Endpoint (as 13e): raw prior-free seasonal RAPM for S+1, pairs with >= 900
# season-S minutes; paired bootstrap on cor(F, raw_{S+1}) - cor(Z, raw_{S+1}),
# overall and for the affected players only (the ones arm F changes).
#
# Inputs: production 05_spm.rds (its panel_s6$by_vintage is the shipped
# prior; pass its path as arg 1), local cache-opta/03_splints.rds, and
# cache-r083/<S>/05_spm.rds for S in 2022:2025.
# Run from panna/:  Rscript <this file> <path/to/production/05_spm.rds>

suppressMessages({devtools::load_all(quiet = TRUE); library(data.table)})

args <- commandArgs(trailingOnly = TRUE)
prod_spm_path <- if (length(args)) args[1] else file.path("data-raw", "cache-opta", "05_spm.rds")
out_dir <- file.path("data-raw", "spm-redesign")
# LEGACY_LAG=1 builds the fallback from seasons <= S-1 (same cut-off as the
# S6 vintage-S prior, which excludes season S); 0 = seasons <= S.
legacy_lag <- as.integer(Sys.getenv("LEGACY_LAG", "0"))
gate_seasons <- if (legacy_lag == 0L) 2022:2025 else 2023:2025
min_minutes_pairs <- 900
t0 <- Sys.time()
ts <- function(...) cat(sprintf("[%s +%.1fm] ", format(Sys.time(), "%H:%M:%S"),
                                as.numeric(difftime(Sys.time(), t0, units = "mins"))), ..., "\n", sep = "")

bv <- as.data.table(readRDS(prod_spm_path)$panel_s6$by_vintage)
stopifnot(nrow(bv) > 0, all(gate_seasons %in% bv$vintage_year))
ts(sprintf("production by_vintage: %d rows, vintages %s", nrow(bv), paste(range(bv$vintage_year), collapse = "-")))

legacy_asof <- function(S) {
  r <- readRDS(file.path("data-raw", "cache-r083", S, "05_spm.rds"))
  list(off = as.data.table(r$offense_spm_ratings)[, .(player_id, offense_spm)],
       def = as.data.table(r$defense_spm_ratings)[, .(player_id, defense_spm)])
}

splint_data <- readRDS(file.path("data-raw", "cache-opta", "03_splints.rds"))

# Per-season results are saved as they finish, so a killed run resumes.
part_dir <- file.path("data-raw", "cache-r083", "r080_parts")
dir.create(part_dir, recursive = TRUE, showWarnings = FALSE)

season_fit <- function(season, arms) {
  part <- file.path(part_dir, sprintf("%d_%s.rds", season,
                                      if (!arms) "raw" else if (legacy_lag == 0L) "arms" else sprintf("arms_lag%d", legacy_lag)))
  if (file.exists(part)) { ts("reusing ", part); return(readRDS(part)) }
  ss <- splint_data$splints[splint_data$splints$season_end_year == season, ]
  sp <- splint_data$players[splint_data$players$splint_id %in% ss$splint_id, ]
  rapm_data <- prepare_rapm_data(list(splints = ss, players = sp, match_info = splint_data$match_info),
                                 min_minutes = 200, include_covariates = TRUE)
  n_folds <- max(3, min(10, floor(nrow(rapm_data$X) / 20)))
  out <- list()
  if (!arms) {
    m <- fit_rapm(rapm_data, alpha = 0, nfolds = n_folds, use_weights = TRUE, parallel = FALSE)
    out$raw <- extract_rapm_ratings(m, lambda = "min"); rm(m)
  } else {
    pm <- rapm_data$player_mapping
    v <- bv[vintage_year == season]
    z_off <- v[, .(player_id, offense_spm = offense_spm_s6)]
    z_def <- v[, .(player_id, defense_spm = defense_spm_s6)]
    lg <- legacy_asof(season - legacy_lag)
    f_off <- rbind(z_off, lg$off[!player_id %in% z_off$player_id])
    f_def <- rbind(z_def, lg$def[!player_id %in% z_def$player_id])
    out$filled_ids <- setdiff(lg$off$player_id, z_off$player_id)
    for (arm in c("Z", "F")) {
      op <- build_prior_vector(if (arm == "Z") z_off else f_off, "offense_spm", pm)
      dp <- build_prior_vector(if (arm == "Z") z_def else f_def, "defense_spm", pm, negate = TRUE)
      ts(sprintf("season %d arm %s: %d players, prior set %d off / %d def", season, arm,
                 length(op), sum(op != 0), sum(dp != 0)))
      m <- fit_rapm_with_prior(rapm_data, offense_prior = op, defense_prior = dp, alpha = 0,
                               nfolds = n_folds, use_weights = TRUE, penalize_covariates = FALSE,
                               parallel = FALSE)
      out[[paste0("xrapm_", arm)]] <- extract_xrapm_ratings(m, lambda = "min"); rm(m)
      invisible(gc(verbose = FALSE))
    }
  }
  rm(rapm_data); invisible(gc(verbose = FALSE))
  saveRDS(out, part)
  ts(sprintf("season %d %s done", season, if (arms) "arms" else "raw"))
  out
}

fits <- list(); raws <- list()
for (s in gate_seasons) fits[[as.character(s)]] <- season_fit(s, arms = TRUE)
for (s in gate_seasons + 1L) raws[[as.character(s)]] <- season_fit(s, arms = FALSE)$raw

boot_delta <- function(f, z, y, n_boot = 2000, seed = 1) {
  set.seed(seed); n <- length(f)
  d <- vapply(seq_len(n_boot), function(i) {
    k <- sample.int(n, n, replace = TRUE); stats::cor(f[k], y[k]) - stats::cor(z[k], y[k])
  }, numeric(1))
  c(delta = mean(d), lo = stats::quantile(d, 0.025, names = FALSE), hi = stats::quantile(d, 0.975, names = FALSE))
}

pairs <- rbindlist(lapply(gate_seasons, function(s) {
  fs <- fits[[as.character(s)]]  # not `f`: that is a column name inside p[]
  filled <- fs$filled_ids
  p <- Reduce(function(a, b) merge(a, b, by = "player_id"), list(
    as.data.table(fs$xrapm_Z)[, .(player_id, z = xrapm, total_minutes)],
    as.data.table(fs$xrapm_F)[, .(player_id, f = xrapm)],
    as.data.table(raws[[as.character(s + 1L)]])[, .(player_id, raw_next = rapm)]))
  p[, `:=`(season = s, affected = player_id %in% filled)]
  p[total_minutes >= min_minutes_pairs]
}))

res <- rbindlist(lapply(list(all = pairs, affected = pairs[affected == TRUE], unaffected = pairs[affected == FALSE]),
  function(p) {
    b <- boot_delta(p$f, p$z, p$raw_next)
    data.table(n = nrow(p), cor_zero = stats::cor(p$z, p$raw_next), cor_fallback = stats::cor(p$f, p$raw_next),
               delta = b[["delta"]], ci_lo = b[["lo"]], ci_hi = b[["hi"]])
  }), idcol = "players")
by_season <- pairs[affected == TRUE, .(n = .N, cor_zero = stats::cor(z, raw_next), cor_fallback = stats::cor(f, raw_next)), by = season]
sfx <- if (legacy_lag == 0L) "" else sprintf("_lag%d", legacy_lag)
fwrite(res, file.path(out_dir, sprintf("r080_fallback_gate%s.csv", sfx)))
fwrite(by_season, file.path(out_dir, sprintf("r080_fallback_gate_by_season%s.csv", sfx)))
cat("\n== R080 fallback gate: next-season raw RAPM correlation, higher is better ==\n")
print(res); cat("\naffected players by season:\n"); print(by_season)
ts("done")
