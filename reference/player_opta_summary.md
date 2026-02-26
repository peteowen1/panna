# Opta Player Summary Statistics

Aggregate basic statistics from Opta data. Returns totals and per-90
rates. Note: Opta does not have xG data.

## Usage

``` r
player_opta_summary(
  player = NULL,
  league = NULL,
  season = NULL,
  min_minutes = 450,
  by_team = FALSE,
  source = c("local", "remote")
)
```

## Arguments

- player:

  Character. Player name to filter (case-insensitive partial match).

- league:

  Character. League code (ENG, ESP, GER, ITA, FRA).

- season:

  Character. Season (e.g., "2024-2025").

- min_minutes:

  Integer. Minimum minutes for inclusion (default 450).

- by_team:

  Logical. If TRUE, aggregate by player and team separately.

- source:

  Character. "local" (default) or "remote".

## Value

Data frame with player summary statistics.

## Examples

``` r
if (FALSE) { # \dontrun{
player_opta_summary(league = "ENG", season = "2024-2025")
} # }
```
