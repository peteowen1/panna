# Player Ratings: EPR, PSR, Panna and Piero

panna produces two families of player numbers, plus one headline
composite that blends the rating family together. This vignette is a map
of what each metric means and the fastest way to look one up; for how
the numbers are actually computed, see
[`vignette("pipeline-walkthrough")`](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md)
(“Pipeline Anatomy”).

## Two families: Ratings vs Production

**Ratings** are forward-looking skill estimates – “how good is this
player, independent of any one match”. **Production** metrics are
backward-looking – “what did this player actually do in this game”. The
mental model:

                                       +-> action EPV credit -> per-game EPV -> EPR --+
                                       |                                              |
        Opta events -> SPADL -> chains+-> action WP     -> per-game WPA --+           +-> Piero
                                       |                                  +-> piero_value  (headline
                         match stats ->+-> PSV (per-game box-score value)-+           |   composite)
                                             |                                        |
                                             +-> PSR (multi-season smoothed skill) ---+
                                             |
        RAPM splints -> RAPM (ridge) --------+-> SPM (XGBoost prior) -> xRAPM (season)
                                                                           -> panna (career trait) -+

| Metric                 | Family                 | Question it answers                                                                                        | Produced by                                                                                                                                                                                           | Published as                                                                                                                       |
|------------------------|------------------------|------------------------------------------------------------------------------------------------------------|-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------|
| `panna` (career trait) | Rating                 | Best point-in-time guess of the player’s next game (365-day decay-weighted xRAPM over all seasons at once) | `estimated-skills/09_career_panna.R`                                                                                                                                                                  | `career_panna.parquet` (`ratings-data` release)                                                                                    |
| `xrapm` (season)       | Rating                 | This season’s on-field impact, SPM-shrunk                                                                  | `player-ratings-opta/06_xrapm.R`                                                                                                                                                                      | `seasonal_xrapm.csv`/parquet (`ratings-data`)                                                                                      |
| EPR                    | Rating                 | Decay-weighted rating built from per-game EPV                                                              | [`calculate_epr_regression()`](https://peteowen1.github.io/panna/reference/calculate_epr_regression.md); weekly via `build_epr_weekly.R`                                                              | `opta_epr_weekly.parquet` (`opta-latest`)                                                                                          |
| PSR (+ OSR/DSR)        | Rating                 | Multi-season smoothed box-score skill, cross-league offset adjusted                                        | [`calculate_psr()`](https://peteowen1.github.io/panna/reference/calculate_psr.md); weekly via `08b_export_psr_weekly.R`                                                                               | `opta_psr_weekly.parquet` (`opta-latest`) – query with [`player_psr()`](https://peteowen1.github.io/panna/reference/player_psr.md) |
| Estimated skills       | Rating                 | Per-stat skill estimate (30+ stats) with percentiles                                                       | [`estimate_player_skills()`](https://peteowen1.github.io/panna/reference/estimate_player_skills.md)                                                                                                   | `opta_skills.parquet` – query with [`player_skill_profile()`](https://peteowen1.github.io/panna/reference/player_skill_profile.md) |
| **Piero**              | Rating (composite)     | The headline blog rating – a blend of panna/EPR/PSR                                                        | `pannadata/scripts/build_blog_data.R` (outside this package)                                                                                                                                          | blog `panna_ratings.parquet`, column `piero`                                                                                       |
| EPV                    | Production             | Value of *this action*: P(team scores the next goal)                                                       | [`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md) / [`aggregate_player_game_epv()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_epv.md) | `game_logs_*.parquet` (`blog-latest`)                                                                                              |
| WPA                    | Production             | Win-probability credit for *this action*                                                                   | [`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md) / [`aggregate_player_game_wpa()`](https://peteowen1.github.io/panna/reference/aggregate_player_game_wpa.md) | `game_logs_*.parquet`                                                                                                              |
| PSV (+ OSV/DSV)        | Production             | Box-score-derived value for *this game*                                                                    | [`calculate_psv()`](https://peteowen1.github.io/panna/reference/calculate_psv.md) / [`compute_player_psv()`](https://peteowen1.github.io/panna/reference/compute_player_psv.md)                       | `game_logs_*.parquet` – query with [`player_value()`](https://peteowen1.github.io/panna/reference/player_value.md)                 |
| **piero_value**        | Production (composite) | Combined per-match value added                                                                             | [`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md)                                                                                             | `game_logs_*.parquet`, column `piero_value`                                                                                        |

Sign convention: internally, defense is stored *negative-is-good*
(additive contribution to opponent xG); published/blog columns flip this
so that positive always means “better”.

## The Piero composite

Piero is the number shown as the main player rating on the blog. It is a
z-scored blend of the three complementary rating estimators, rescaled
back into panna’s own units:

    piero = z(0.5 * z(panna) + 0.3 * z(epr) + 0.2 * z(psr)) * sd(panna) + mean(panna)

The weights (panna 0.5 / EPR 0.3 / PSR 0.2) live in `PIERO_WEIGHTS`
inside `pannadata/scripts/build_blog_data.R` – Piero itself is **not** a
function exported from this package. panna supplies the three ingredient
ratings (`panna`, EPR, PSR); the blend and publish step happen
downstream in `pannadata`. The per-match twin follows the same idea but
blends the *production* metrics instead:
`piero_value = 0.5 * epv_total_adj + 0.5 * psv`, controlled by this
package’s own `PANNA_EPR_WEIGHT` / `PANNA_PSR_WEIGHT` constants
(0.5/0.5) inside
[`build_player_game_ratings()`](https://peteowen1.github.io/panna/reference/build_player_game_ratings.md).
There is no per-match twin of `panna` itself – single-game RAPM is too
noisy to be useful, so the match-level “value added” side is EPV + PSV
only.

## Cookbook: look up a player

These are the functions you actually call interactively. All default to
`source = "remote"`, so a fresh
[`library(panna)`](https://github.com/peteowen1/panna) with no local
pipeline run is enough – they download small pre-computed snapshots from
GitHub Releases.

``` r
library(panna)

# Latest PSR leaderboard (top 50 by default)
player_psr()

# As of a specific date -- snaps to the nearest weekly snapshot at or before it
player_psr(date = "2026-03-18")

# Look up one player (partial, case-insensitive match)
player_psr(date = "2026-03-18", player = "Salah")

# Top midfielders only
player_psr(position = "MID")
```

``` r
# Full per-stat skill profile with league/position percentiles.
# Downloads ~2-3 MB of pre-computed skills + match stats on first call.
player_skill_profile("Lionel Messi")

# Skill profile as of a past date (still uses the downloaded data)
player_skill_profile("Kylian Mbappe", date = "2025-06-01")
```

``` r
# Per-match value profile: EPV/WPA/PSV totals + per-90 rates + an EPR
# estimate. Requires LOCAL pipeline cache output (data-raw/cache/epv/players,
# data-raw/cache-skills/player_game_psv.rds) -- this one is not a fresh-clone
# one-liner, it reads files the RAPM/EPV pipelines leave behind.
player_value("Salah")
player_value("Kane", season = "2024-2025")
```

``` r
# Raw/derived Opta box-score aggregates -- the player_opta_*() family.
# All share the same argument shape: player, league, season, min_minutes.
player_opta_summary(player = "Kane", league = "ENG", season = "2024-2025")
player_opta_shots(player = "Haaland", league = "GER")
player_opta_passing(league = "ESP", season = "2024-2025", min_minutes = 900)

# Side-by-side comparison across shooting/passing/defending/chains
compare_players(c("Salah", "Mbappe", "Haaland"))
```

## Where the numbers come from

Every metric above is produced by one of four numbered `data-raw/`
pipelines (RAPM/SPM, Estimated Skills, Match Predictions, EPV/xMetrics)
run from inside `panna/`. See
[`vignette("pipeline-walkthrough")`](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md)
(“Pipeline Anatomy”) for the step-by-step map, cache layout, and how to
run a subset.

Published artifacts flow **GitHub Releases -\> Cloudflare R2 -\> the
`inthegame-blog` website**; see
[`vignette("data-bus")`](https://peteowen1.github.io/panna/articles/data-bus.md)
for how to pull them down or publish new ones yourself.

## Next steps

- [Pipeline
  Anatomy](https://peteowen1.github.io/panna/articles/pipeline-walkthrough.md)
  – how these numbers are computed
- [Data Access and
  Publishing](https://peteowen1.github.io/panna/articles/data-bus.md) –
  downloading and publishing data
- [Match Prediction and Tournament
  Simulation](https://peteowen1.github.io/panna/articles/match-prediction.md)
  – how ratings feed match predictions
- [Getting
  Started](https://peteowen1.github.io/panna/articles/getting-started.md)
  – installation and data loading
- [Data
  Sources](https://peteowen1.github.io/panna/articles/data-sources.md) –
  Opta league codes, season formats, and loaders
- [Data
  Dictionary](https://peteowen1.github.io/panna/DATA_DICTIONARY.md) –
  column definitions at each pipeline stage
