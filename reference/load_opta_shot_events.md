# Load Opta Shot Events (Individual Shots with Coordinates)

Loads individual shot events with x/y coordinates from Opta/TheAnalyst
data. Each row is a single shot with location, outcome, body part, and
situation. Useful for xG modeling as it includes shot coordinates.

## Usage

``` r
load_opta_shot_events(
  league,
  season = NULL,
  columns = NULL,
  source = c("remote", "local")
)
```

## Arguments

- league:

  League code. Accepts panna format (ENG, ESP, GER, ITA, FRA) or Opta
  format (EPL, La_Liga, Bundesliga, Serie_A, Ligue_1).

- season:

  Optional season filter (e.g., "2021-2022"). If NULL, loads all
  seasons.

- columns:

  Optional character vector of columns to select. If NULL, selects all.

- source:

  Data source: "remote" (default) downloads from GitHub releases,
  "local" loads from local files (requires prior
  [`pb_download_opta()`](https://peteowen1.github.io/panna/reference/pb_download_opta.md)).

## Value

Data frame of shot events with columns:

- match_id: Match identifier

- event_id: Unique event identifier

- player_id, player_name: Shooter info

- team_id: Team that took the shot

- minute, second: Time of shot

- x, y: Shot coordinates (0-100 scale)

- outcome: 1=on target, 0=off target

- is_goal: Whether shot resulted in goal

- type_id: 13=miss, 14=post, 15=saved, 16=goal

- body_part: Head, LeftFoot, RightFoot

- situation: OpenPlay, SetPiece, Corner, Penalty

- big_chance: TRUE if big chance

## The `xg` column is OPTA's, not ours

`opta_shot_events.parquet` ships an `xg` column supplied by Opta. It is
NOT panna's model output and must never be used as such – production
always uses our own xG (via SPADL and
[`predict_xg()`](https://peteowen1.github.io/panna/reference/predict_xg.md));
Opta's is a benchmark only, for confirming ours is better on the shots
they scored.

They are trivial to tell apart: Opta's is quantised to 3 decimal places
(956 distinct values across 3.3M shots, penalties exactly 0.800), ours
is float-continuous with ~84% of values unique. Neither of our models
reproduces the stored column (correlations 0.964 / 0.967) because it was
never ours.

Measured head-to-head on 1,839,859 identical shots (2026-09-03, penalty
override applied): ours wins on logloss (0.2519 vs 0.2549), Opta
marginally better on bias (1.028 vs 1.033).

## See also

Other opta loaders:
[`load_opta_big5()`](https://peteowen1.github.io/panna/reference/load_opta_big5.md),
[`load_opta_events()`](https://peteowen1.github.io/panna/reference/load_opta_events.md),
[`load_opta_fixtures()`](https://peteowen1.github.io/panna/reference/load_opta_fixtures.md),
[`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md),
[`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md),
[`load_opta_match_stats()`](https://peteowen1.github.io/panna/reference/load_opta_match_stats.md),
[`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md),
[`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md),
[`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md),
[`load_opta_xmetrics()`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load EPL shot events with coordinates
epl_shots <- load_opta_shot_events("ENG", season = "2024-2025")

# Analyze shots by location
library(ggplot2)
ggplot(epl_shots, aes(x = x, y = y, color = is_goal)) +
  geom_point(alpha = 0.5)
} # }
```
