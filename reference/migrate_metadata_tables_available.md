# Migrate old metadata files to include tables_available field

Updates metadata files that were created before the tables_available
field was added. Checks which table types actually exist in the cache
and updates the metadata accordingly.

## Usage

``` r
migrate_metadata_tables_available(league = NULL, season = NULL, verbose = TRUE)
```

## Arguments

- league:

  Optional league filter

- season:

  Optional season filter

- verbose:

  Print progress (default TRUE)

## Value

Number of metadata files updated

## Details

Uses hierarchical path: `{table_type}/{league}/{season}/{id}.rds`

## Examples

``` r
if (FALSE) { # \dontrun{
# Migrate all old metadata
migrate_metadata_tables_available()

# Migrate only ENG 2017-2018
migrate_metadata_tables_available(league = "ENG", season = "2017-2018")
} # }
```
