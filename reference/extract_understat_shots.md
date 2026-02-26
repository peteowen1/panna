# Extract shots data from Understat API response

Parses shot-level xG data from the API shots data.

## Usage

``` r
extract_understat_shots(api_data, understat_id)
```

## Arguments

- api_data:

  List returned from fetch_understat_match_api

- understat_id:

  Understat match ID

## Value

Data frame with shot data, or NULL if not found
