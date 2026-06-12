# Compute Per-Match Venue Factor

Returns a numeric vector (+1 / 0 / -1) matching the length of
`home_team`/`away_team`. See file header for the convention.

## Usage

``` r
compute_venue_factor(home_team, away_team, league, season)
```

## Arguments

- home_team, away_team, league, season:

  Vectors of equal length.

## Value

Numeric vector of -1 / 0 / +1.

## Details

Domestic leagues + UCL/UEL/UECL: always +1 (home team at home).
Qualifiers (WCQ\_\* / EUROQ / AFCONQ / ACUPQ) + Nations League: +1
(these are scheduled home/away at real stadiums). Intl_Friendlies:
default +1 (we don't know venue; friendlies are usually at the
home_team's stadium but can be neutral). Tournament matches: parse host
from season; +1 if home_team is host, -1 if away_team is host, 0 if
neither.
