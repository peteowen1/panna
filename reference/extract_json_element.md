# Extract JSON data from Understat script

Understat embeds data as JSON in script tags with pattern:
`varName = JSON.parse('...')`

## Usage

``` r
extract_json_element(scripts, var_name)
```

## Arguments

- scripts:

  Character vector of script contents

- var_name:

  Variable name to extract (e.g., "rostersData", "shotsData")

## Value

Parsed JSON as R list, or NULL if not found
