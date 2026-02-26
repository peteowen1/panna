# Opta Player xG and xA Statistics

Aggregate shooting and assisting statistics with xG/xA from our trained
Opta xG model. Requires pre-computed xmetrics parquet files.

## Usage

``` r
player_opta_xg(
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

Data frame with xG/xA statistics per player.

## Examples

``` r
if (FALSE) { # \dontrun{
# Top xG in EPL
player_opta_xg(league = "ENG", season = "2024-2025")

# Specific player
player_opta_xg("B. Saka", league = "ENG")
} # }
```
