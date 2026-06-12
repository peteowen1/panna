# Compare Two or More Players

Pulls stats from multiple sources (xG, xPass, chains, defense,
possession) and presents a consolidated side-by-side comparison. All
stats are per-90 where applicable.

## Usage

``` r
compare_players(
  players,
  league = NULL,
  season = NULL,
  min_minutes = 0,
  source = c("remote", "local")
)
```

## Arguments

- players:

  Character vector of player names (case-insensitive partial match).

- league:

  Character. League code to filter (NULL for all leagues).

- season:

  Character. Season to filter (NULL for all seasons combined).

- min_minutes:

  Numeric. Minimum minutes for inclusion (default 0 when players are
  specified).

- source:

  Character. "remote" (default) or "local".

## Value

Data frame with one row per player and columns grouped by category:
identity, shooting, creating, passing, chains, defending, possession.

## Examples

``` r
if (FALSE) { # \dontrun{
compare_players(c("Salah", "Mbapp\u00e9", "Haaland"))
compare_players(c("B. Saka", "Lamine Yamal"), league = "ESP", season = "2025-2026")
compare_players(c("V. van Dijk", "W. Saliba"), league = "ENG", season = "2024-2025")
} # }
```
