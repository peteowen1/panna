# 01c_keeper_style.R
# As-of keeper distribution style table (panna#429).
#
# Fits, for each season Y on seasons before Y, keeper distribution stat =
# league-season + club + club-season + keeper (minutes-weighted ridge,
# R/keeper_style.R), and saves each club's style going into Y plus each keeper's
# as-of effect. enrich_match_stats_with_xmetrics() reads it and writes the
# club-adjusted `<stat>_kadj` columns that keeper PSR uses instead of the raw
# distribution stats. Every step that scores keepers needs this file, so it
# runs right after step 01 and travels with the other skills caches.
#
# Run from panna/:  Rscript data-raw/estimated-skills/01c_keeper_style.R

library(data.table)
devtools::load_all()

if (!exists("cache_dir", inherits = FALSE)) cache_dir <- file.path("data-raw", "cache-skills")
in_path  <- file.path(cache_dir, "01_match_stats.rds")
out_path <- file.path(cache_dir, "keeper_style_asof.rds")

ms <- readRDS(in_path)
ms <- if (data.table::is.data.table(ms)) ms else data.table::as.data.table(ms)
cat(sprintf("01_match_stats: %s rows x %d cols, %s to %s\n", format(nrow(ms), big.mark = ","), ncol(ms),
            format(min(as.Date(ms$match_date), na.rm = TRUE)), format(max(as.Date(ms$match_date), na.rm = TRUE))))
stats <- intersect(.keeper_style_stats(), names(ms))
miss <- setdiff(.keeper_style_stats(), stats)
if (length(miss)) stop(sprintf("01_match_stats lacks keeper style stats: %s", paste(miss, collapse = ", ")))

t0 <- Sys.time()
ks <- fit_keeper_style_asof(ms, stats = stats, verbose = TRUE)
kf <- attr(ks, "keeper_effects")
cat(sprintf("\nKeeper style: %d stats x %d seasons, %s club rows, %s keeper rows (%.1f min)\n",
            uniqueN(ks$stat), uniqueN(ks$season_end_year), format(nrow(ks), big.mark = ","),
            format(nrow(kf), big.mark = ","), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
print(ks[, .(clubs = .N, sd_club_style = round(sd(club_style), 2)), by = stat])
# Coverage: what share of keeper rows in the latest season get a club style?
yr <- if ("season_end_year" %in% names(ms)) as.integer(ms$season_end_year) else as.integer(extract_season_end_year(ms$season))
gk <- .detect_gk_rows(ms)
latest <- max(yr[gk], na.rm = TRUE)
cov <- mean(paste(ms$team_id, yr)[gk & yr == latest] %in% paste(ks$team_id, ks$season_end_year))
cat(sprintf("Season %d: %.1f%% of keeper rows have a club style (the rest keep raw values)\n", latest, 100 * cov))
if (cov < 0.8) stop(sprintf("Only %.1f%% of season %d keeper rows have a club style; check team_id coverage.", 100 * cov, latest))

saveRDS(ks, out_path)
cat(sprintf("Saved to %s (%s)\n", out_path, format(file.mtime(out_path))))
