# Minimum resolved announced-squad players required to apply the override

If fewer than this many of a team's announced-squad names resolve to
Opta player_ids, the override is refused and the team falls back to the
most-recent intl XI. Prevents the silent "near-empty synthetic team"
failure mode where the override fires with 1-2 resolved players and the
EM-weighted aggregation collapses to ~zero sum_panna.

## Usage

``` r
WC2026_OVERRIDE_MIN_RESOLVED
```

## Format

An object of class `integer` of length 1.
