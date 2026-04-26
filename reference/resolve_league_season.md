# Resolve a League-Season String for Blog-Style Pipelines

Given a league and the "domestic" season a pipeline is iterating over,
returns the Opta season string to pass to
[`load_opta_match_events()`](https://peteowen1.github.io/panna/reference/load_opta_match_events.md).
Continental club comps (UCL/UEL/UECL) share the "YYYY-YYYY" format with
domestic leagues and are returned as-is. International tournaments (WC,
EURO) use "YYYY Country" (or bare "YYYY" for pan-European EURO 2020);
this helper maps a tournament played in summer YYYY onto the domestic
season ending in YYYY (e.g. WC 2014 Brazil → "2013-2014").

## Usage

``` r
resolve_league_season(
  league,
  domestic_season,
  tournament_leagues = c("WC", "EURO")
)
```

## Arguments

- league:

  League code (e.g. "ENG", "UCL", "WC", "EURO").

- domestic_season:

  Domestic season string, e.g. "2013-2014".

- tournament_leagues:

  Character vector of league codes that use "YYYY Country"-style season
  strings. Defaults to `c("WC", "EURO")`.

## Value

Season string to pass to `load_opta_*()`, or `NULL` if no matching
tournament exists for the given year.

## Details

Returns `NULL` when there is no tournament in the given year so callers
can skip gracefully.

## Examples

``` r
if (FALSE) { # \dontrun{
resolve_league_season("ENG",  "2013-2014")  # → "2013-2014"
resolve_league_season("UCL",  "2013-2014")  # → "2013-2014"
resolve_league_season("WC",   "2013-2014")  # → "2014 Brazil"
resolve_league_season("EURO", "2019-2020")  # → "2020" (pan-European)
resolve_league_season("WC",   "2018-2019")  # → NULL (no WC that year)
} # }
```
