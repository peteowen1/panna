# Report xMetrics coverage of each competition-season's minutes (R079)

SPM imputes xg/npxg/xa to 0 for a player with no xMetrics row, so a
competition-season whose xMetrics never got built silently trains as
"nobody shot". Coverage is all-or-nothing in practice (2026-10-14: 562
of 894 competition-seasons at exactly 0%, the rest at 100%), mostly
seasons before xMetrics starts (2013-14) and Scottish 2014-15 to
2018-19. This prints one summary of the uncovered seasons of
competitions that DO have xMetrics elsewhere, and raises a GitHub
Actions warning only when one of them is among the latest
`recent_seasons` – the case that means a build failed rather than a
known historical gap. It never changes data.

## Usage

``` r
.report_xmetrics_coverage(
  opta_stats,
  opta_xmetrics,
  recent_seasons = 2L,
  floor = 0.8
)
```

## Arguments

- opta_stats:

  Player-match stats with player_id, competition, season and a minutes
  column.

- opta_xmetrics:

  xMetrics season table with player_id, season and either competition
  (Opta name) or league (panna code, as step 01 adds).

- recent_seasons:

  Number of latest season end years that warn.

- floor:

  Share of minutes below which a competition-season counts as uncovered.

## Value

The coverage table (competition, season, minutes, share), invisibly;
NULL when either input lacks the needed columns.
