# Extract roster data from Understat API response

Parses player statistics from the API rosters data.

## Usage

``` r
extract_understat_roster(api_data, understat_id)
```

## Arguments

- api_data:

  List returned from fetch_understat_match_api

- understat_id:

  Understat match ID

## Value

Data frame with player stats, or NULL if not found
