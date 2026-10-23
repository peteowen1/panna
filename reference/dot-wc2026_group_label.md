# Group letter for a WC 2026 fixture, or `NA` for a knockout tie

A group-stage match pairs two teams from the same group, dated on or
before `WC2026_GROUP_STAGE_END`. A knockout tie pairs teams from
different groups, so the same-group test does the separating. The date
window stops a same-group pairing played later from being labelled.

## Usage

``` r
.wc2026_group_label(home_team, away_team, match_date, team_group)
```

## Arguments

- home_team, away_team:

  Character vectors of team names.

- match_date:

  Date, or a character/POSIXct vector whose first 10 characters are the
  UTC date (`"2026-06-11Z"`).

- team_group:

  Named character vector: team name to group letter.

## Value

Character vector, the group letter for group-stage rows and `NA`
otherwise.
