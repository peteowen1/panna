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

- type_id: 13=saved, 14=post, 15=miss, 16=goal

- body_part: Head, LeftFoot, RightFoot

- situation: OpenPlay, SetPiece, Corner, Penalty

- big_chance: TRUE if big chance

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
