# A genuine keeper's own position label reads "Substitute" – a lineup STATUS, not a position – for any appearance off the bench, so a per-row grep alone misses every bench appearance a keeper makes (measured 2026-09-13: 3,756 rows, 0.184% of `01_match_stats.rds`). Those rows were being scored/trained through the outfield model instead, with no shot-stopping credit.

Falls back to a MAJORITY VOTE across the player's other rows in `dt`,
not "ever GK". Measured on the same table: of 3,403 players who show a
GK label at least once, 119 have a GK share under 50% – overwhelmingly
outfielders with a rare emergency-keeper appearance (e.g. after a red
card) or a stray mislabel, not genuine keepers, several as low as
0.3-16% GK share among players who ALSO appear as substitutes. "Ever GK"
would misroute every one of THEIR substitute rows to the GK model too. A
\>50% majority correctly separates genuine keepers (2,276 of 3,403 sit
above 99% GK share) from that tail. An exact 50/50 split defaults to
NOT-GK, the conservative side for a genuinely rare dual-role case.

## Usage

``` r
.detect_gk_rows(dt)
```

## Details

The vote is computed WITHIN whatever `dt` is passed in, not cached
globally – consistent with
[`resolve_position_group`](https://peteowen1.github.io/panna/reference/resolve_position_group.md)
and
[`.psv_pos_grp`](https://peteowen1.github.io/panna/reference/dot-psv_pos_grp.md),
so a call scoped to one season resolves using that season's own rows and
a call scoped to the whole table uses the whole table.
