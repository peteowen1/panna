# Download a file from an Opta GitHub release with fallback

Handles session caching, parquet validation, and falls back to a direct
GitHub URL if piggyback's memoised asset list is stale.

## Usage

``` r
download_opta_release_file(
  file_name,
  source = c("remote", "local"),
  repo = "peteowen1/pannadata",
  tag = "opta-latest"
)
```

## Arguments

- file_name:

  Name of the file to download.

- source:

  "remote" or "local".

- repo:

  GitHub repository.

- tag:

  Release tag.

## Value

Path to the downloaded file.
