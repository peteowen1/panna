# Parse FBref stats table with multi-row headers

Handles the complex header structure of FBref stats tables. Uses
multiple parsing strategies to handle different table structures (some
lower-league cup matches have different HTML structure).

## Usage

``` r
parse_stats_table(page, table_id)
```

## Arguments

- page:

  Parsed HTML document

- table_id:

  ID of the table to parse

## Value

Data frame with parsed stats, or NULL if table not found
