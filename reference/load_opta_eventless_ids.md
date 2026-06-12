# Load the event-less match_id registry

Returns match_ids that Opta has player_stats for but provides NO event
feed for (e.g. cup qualifier rounds), as recorded by pannadata's
`rebuild_events.py` into `event_less_match_ids.parquet` on the
`opta-latest` release.
[`check_events_coverage()`](https://peteowen1.github.io/panna/reference/check_events_coverage.md)
subtracts these from the expected-events denominator so genuinely
event-less matches don't register as a coverage shortfall (an
unsatisfiable gate for the continental cups). Degrades gracefully: if
the registry asset/file is absent (it won't exist until the first
rebuild has run), returns `character(0)` and the coverage check falls
back to its stricter all-player_stats denominator.

## Usage

``` r
load_opta_eventless_ids(league, season = NULL, source = c("remote", "local"))
```

## Arguments

- league:

  panna league code (filtered to its Opta competition).

- season:

  Optional season label filter.

- source:

  "remote" (download from opta-latest) or "local".

## Value

Character vector of event-less match_ids (possibly empty).
