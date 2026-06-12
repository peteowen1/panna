# Assert Events Coverage Across Multiple Leagues

Runs
[`check_events_coverage()`](https://peteowen1.github.io/panna/reference/check_events_coverage.md)
for each (league, season) pair and decides whether to proceed. Emits a
per-league summary; aborts loudly if any league's gap exceeds
`abort_threshold`, otherwise emits warnings for gaps above
`warn_threshold`.

## Usage

``` r
assert_events_coverage(
  league_seasons,
  season = NULL,
  warn_threshold = 5L,
  abort_threshold = Inf,
  source = c("remote", "local")
)
```

## Arguments

- league_seasons:

  Either a character vector of league codes (all checked against the
  same `season` argument) OR a list of `list(league=..., season=...)`
  pairs.

- season:

  Default season if `league_seasons` is a vector.

- warn_threshold:

  Per-league gap above which to warn. Default 5.

- abort_threshold:

  Per-league gap above which to abort. Default `Inf` (warn-only). Set to
  a numeric (e.g. 20) to make the pipeline refuse to continue.

- source:

  One of "remote" or "local".

## Value

Invisibly: list with per-league reports + summary stats.

## Details

Intended as a guard at the top of pipeline steps that consume events
(step 10b export_game_logs, step 10c export_equity). Catches the
"events_consolidated is short" pattern BEFORE producing incomplete
game_logs that get silently shipped to blog-latest.
