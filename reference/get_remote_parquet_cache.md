# Get remote parquet cache directory

Downloads and extracts parquet zip from GitHub releases to a temp
directory. Cached for the R session - subsequent calls return the cached
path.

## Usage

``` r
get_remote_parquet_cache(
  repo = "peteowen1/pannadata",
  tag = "latest",
  force = FALSE
)
```

## Arguments

- repo:

  GitHub repo (default: "peteowen1/pannadata")

- tag:

  Release tag (default: "latest")

- force:

  Re-download even if cached

## Value

Path to extracted parquet directory
