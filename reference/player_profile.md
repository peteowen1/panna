# Player Profile - Combined Statistics Across Sources

Combines key statistics from FBref, Opta, and Understat into a single
consolidated data frame with one row per player-season-league. Merges
the most useful columns from each source.

## Usage

``` r
player_profile(
  player,
  league = NULL,
  season = NULL,
  min_minutes = 450,
  source = c("remote", "local")
)
```

## Arguments

- player:

  Character. Player name to filter (case-insensitive partial match).
  Required.

- league:

  Character. Filter by league code (ENG, ESP, GER, ITA, FRA). NULL
  returns all leagues.

- season:

  Character. Filter by FBref season format (e.g., "2024-2025"). NULL
  returns all seasons.

- min_minutes:

  Integer. Minimum minutes for inclusion (default 450).

- source:

  Character. "remote" (default) or "local".

## Value

Data frame with one row per player-season-league containing:

- **Identity**: player, team, league, season, minutes

- **FBref**: goals, assists, xg, npxg, xag, sca, gca, passes_completed,
  pass_pct, progressive_passes, key_passes

- **Opta**: progressive_carries, final_third_entries, pen_area_entries,
  big_chances_created (when available)

- **Understat**: xg_chain, xg_buildup (when available)

## Examples

``` r
if (FALSE) { # \dontrun{
# Full profile for a player
player_profile("Saka", league = "ENG")

# Specific season
player_profile("Salah", league = "ENG", season = "2024-2025")

# All Big 5 leagues
player_profile("Mbappe")
} # }
```
