# Parse and clean Understat JSON string

Unescapes hex and unicode characters and fixes JSON formatting issues.
Understat uses \xNN hex escapes which need to be converted to unicode.

## Usage

``` r
parse_understat_json(json_str)
```

## Arguments

- json_str:

  Raw JSON string from Understat

## Value

Cleaned JSON string
