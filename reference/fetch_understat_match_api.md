# Fetch match data from Understat API

Calls the undocumented API endpoint that returns full match data. This
endpoint provides rosters and shots data that isn't in the main HTML.

## Usage

``` r
fetch_understat_match_api(understat_id)
```

## Arguments

- understat_id:

  Understat match ID

## Value

List with rosters and shots data, or NULL on failure
