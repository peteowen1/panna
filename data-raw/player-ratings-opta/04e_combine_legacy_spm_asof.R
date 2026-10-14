# 04e: combine the per-season 04d tables into the one file step 07 reads (R080).
#
# Vintage Y (the S6 panel's window [Y-5, Y)) takes the legacy table built on
# seasons <= Y-1. Writes data-raw/cache-opta/legacy_spm_asof.parquet with
# vintage_year, player_id, offense_spm, defense_spm (signs as 05_spm.R writes
# them, the same convention as the S6 columns 07 uses). Then upload:
#   gh release upload predictions-cache data-raw/cache-opta/legacy_spm_asof.parquet \
#     --repo peteowen1/pannadata --clobber
# Usage (from panna/): Rscript data-raw/player-ratings-opta/04e_combine_legacy_spm_asof.R
#   Vintages default to 2019:2026 (spm_panel.R's default); set LEGACY_ASOF_VINTAGES="2019:2027".
suppressMessages({library(data.table)})
root <- Sys.getenv("LEGACY_ASOF_DIR", file.path("data-raw", "cache-legacy-asof"))
v_rng <- as.integer(strsplit(Sys.getenv("LEGACY_ASOF_VINTAGES", "2019:2026"), ":", fixed = TRUE)[[1]])
stopifnot(length(v_rng) == 2L, !anyNA(v_rng), v_rng[1] <= v_rng[2])
vintages <- seq(v_rng[1], v_rng[2])
out <- rbindlist(lapply(vintages, function(Y) {
  f <- file.path(root, Y - 1L, "05_spm.rds")
  if (!file.exists(f)) stop(sprintf("missing %s: run 04d_build_legacy_spm_asof.R %d", f, Y - 1L))
  r <- readRDS(f)
  off <- as.data.table(r$offense_spm_ratings)[, .(player_id, offense_spm)]
  def <- as.data.table(r$defense_spm_ratings)[, .(player_id, defense_spm)]
  m <- merge(off, def, by = "player_id", all = TRUE)
  m[, vintage_year := Y]
  cat(sprintf("vintage %d <- built on seasons <= %d: %d players (off %d, def %d)\n",
              Y, Y - 1L, nrow(m), nrow(off), nrow(def)))
  m
}))
setcolorder(out, c("vintage_year", "player_id", "offense_spm", "defense_spm"))
stopifnot(!anyDuplicated(out[, .(vintage_year, player_id)]))
nas <- out[, .(off_na = sum(is.na(offense_spm)), def_na = sum(is.na(defense_spm)))]
if (nas$off_na + nas$def_na > 0) stop(sprintf("NA priors: %d offense, %d defense", nas$off_na, nas$def_na))
path <- file.path("data-raw", "cache-opta", "legacy_spm_asof.parquet")
arrow::write_parquet(out, path)
cat(sprintf("wrote %s: %d rows, vintages %s\n", path, nrow(out), paste(range(out$vintage_year), collapse = "-")))
