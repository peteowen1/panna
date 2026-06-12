# Build Team -\> Confederation Lookup From Played Matches

Each WC2026-era confederation has a unique qualifying competition code.
We use that as a stable identifier: a team is in confederation X iff
they've appeared in X's qualifiers / continental tournament in our data.
Returns a named character vector (team_name -\> confederation).

## Usage

``` r
build_team_confederations(played)
```

## Arguments

- played:

  Data frame with `league`, `home_team`, `away_team` columns. Typically
  `fixture_results[match_status == "Played", ]`.

## Value

Named character vector – names are team names, values are confederation
codes ("UEFA", "CONMEBOL", "CAF", "AFC", "CONCACAF", or "OFC").

## Details

Teams that never appear in any confederation-coded competition
(extremely rare for nations actively playing intl football) get NA and
the cross_conf_mult treats them as "unknown" (multiplier = 1).
