# Load All Opta Data for Big 5 Leagues

Convenience function to load Opta stats for all Big 5 European leagues.

## Usage

``` r
load_opta_big5(season = NULL, columns = NULL, source = c("remote", "local"))
```

## Arguments

- season:

  Optional season filter. If NULL, loads all available seasons.

- columns:

  Optional character vector of columns to select.

- source:

  Character. "remote" (default) or "local".

## Value

Data frame with league column added.

## See also

Other opta loaders:
[`load_opta_events()`](https://peteowen1.github.io/panna/reference/load_opta_events.md),
[`load_opta_fixtures()`](https://peteowen1.github.io/panna/reference/load_opta_fixtures.md),
[`load_opta_lineups()`](https://peteowen1.github.io/panna/reference/load_opta_lineups.md),
[`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md),
[`load_opta_match_stats()`](https://peteowen1.github.io/panna/reference/load_opta_match_stats.md),
[`load_opta_shot_events()`](https://peteowen1.github.io/panna/reference/load_opta_shot_events.md),
[`load_opta_shots()`](https://peteowen1.github.io/panna/reference/load_opta_shots.md),
[`load_opta_skills()`](https://peteowen1.github.io/panna/reference/load_opta_skills.md),
[`load_opta_stats()`](https://peteowen1.github.io/panna/reference/load_opta_stats.md),
[`load_opta_xmetrics()`](https://peteowen1.github.io/panna/reference/load_opta_xmetrics.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all Big 5 data (warning: large!)
big5 <- load_opta_big5()

# Load specific season across all leagues
big5_2122 <- load_opta_big5(season = "2021-2022")
} # }
```
