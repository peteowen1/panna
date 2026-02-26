# Get FBref match cache directory

Returns path using hierarchical structure:
`{pannadata_dir}/{table_type}/{league}/{season}/`

## Usage

``` r
get_fbref_match_cache_dir(
  table_type = NULL,
  league = NULL,
  season = NULL,
  create = TRUE
)
```

## Arguments

- table_type:

  Optional table type for subdirectory

- league:

  Optional league for subdirectory (new hierarchical structure)

- season:

  Optional season for subdirectory (new hierarchical structure)

- create:

  Whether to create directory if missing (default TRUE)

## Value

Path to cache directory
