# Check events_consolidated Coverage vs Played Fixtures

Counts unique match_ids in `events_consolidated/events_<comp>.parquet`
(what the EPV pipeline reads) and compares to the number of played
fixtures from `opta_fixtures.parquet` (the canonical source of truth for
which matches actually occurred) for a given league-season. Surfaces the
gap as data so callers (step 10b in the predictions pipeline) can refuse
to silently ship game_logs that miss matches.

## Usage

``` r
check_events_coverage(league, season, source = c("remote", "local"))
```

## Arguments

- league:

  panna league code (e.g. "EPL", "ENG2", "TUR")

- season:

  Season string (e.g. "2025-2026")

- source:

  One of "remote" (default) or "local" — where to read from.

## Value

Invisibly: list with

- `league`, `season`: identifiers

- `n_played`: distinct played fixtures (context)

- `n_player_stats`: distinct matches Opta covers (the universe)

- `n_eventless`: registry matches excluded (no Opta event feed)

- `n_expected`: `n_player_stats - n_eventless` — matches that should
  have events

- `n_events`: distinct match_ids in events_consolidated

- `gap`: expected matches missing from events

- `missing_match_ids`: vector of expected match_ids not in events
  (length == gap)

## Details

Background: the events_consolidated build step in pannadata's daily
scraper occasionally produces a per-comp parquet that's short of the
actual match count — observed during the 2026-05-29 audit where
`events_Championship.parquet` on `opta-latest` had only 265 of 557
played Championship 2025-2026 matches, causing the blog Value tab to cap
at GP=24 instead of 46. Without an explicit check, step 10b silently
produced game_logs covering only the events it could see.

## See also

Other validation:
[`assert_events_coverage()`](https://peteowen1.github.io/panna/reference/assert_events_coverage.md),
[`assert_step_output()`](https://peteowen1.github.io/panna/reference/assert_step_output.md)
