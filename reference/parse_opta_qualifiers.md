# Parse Opta Qualifiers JSON

Extracts qualifier information from the qualifier_json column. Opta JSON
format is a dictionary where keys are qualifier IDs:
`{"108":null,"55":"145","28":null,...}`

## Usage

``` r
parse_opta_qualifiers(dt)
```

## Arguments

- dt:

  Data.table with qualifier_json column (modified in place)

## Value

Data.table with parsed qualifier columns added

## Details

Uses fast regex-based extraction instead of JSON parsing.
