# Get or Build SPADL (with disk cache)

Thin wrapper over
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
that caches the result on disk, keyed by `league` and `season`. SPADL
conversion is the single most expensive step in downstream EPV pipelines
(~25% of total runtime per league-season) and is fully deterministic
given raw events, so caching yields a large win on repeat runs and
backfills.

## Usage

``` r
get_or_build_spadl(
  events,
  league,
  season,
  cache_dir = SPADL_CACHE_DIR,
  force_rebuild = FALSE
)
```

## Arguments

- events:

  Raw Opta events from
  [`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md).

- league:

  League code (e.g. "ENG"). Used only to build the cache key.

- season:

  Season string (e.g. "2024-2025"). Used only to build the cache key.

- cache_dir:

  Directory to read/write the cached `.rds`. Defaults to
  `SPADL_CACHE_DIR` so all pipelines share one cache.

- force_rebuild:

  If `TRUE`, rebuild and overwrite the cache.

## Value

Data frame in SPADL format, identical to
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md).
