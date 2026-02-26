# Opta Player xPass Statistics

Aggregate passing statistics with xPass (expected pass completion) from
our trained model. Shows passing volume, accuracy, and overperformance.

## Usage

``` r
player_opta_xpass(
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

Data frame with xPass statistics per player.

## Examples

``` r
if (FALSE) { # \dontrun{
# Top xPass overperformers
player_opta_xpass(league = "ENG", season = "2024-2025")

# Specific player
player_opta_xpass("B. Saka", league = "ENG")
} # }
```
