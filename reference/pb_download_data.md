# Download data from GitHub Releases

Downloads the pannadata.zip file from a GitHub Release and extracts it
to the local pannadata directory.

## Usage

``` r
pb_download_data(
  repo = "peteowen1/pannadata",
  tag = "latest",
  dest = NULL,
  overwrite = TRUE,
  show_progress = TRUE
)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format (default:
  "peteowen1/pannadata")

- tag:

  Release tag to download from (default: "latest")

- dest:

  Destination directory (default: pannadata_dir())

- overwrite:

  Overwrite existing files (default: TRUE)

- show_progress:

  Show download progress (default: TRUE)

## Value

Invisible path to destination directory

## Deprecation Notice

For loading data, prefer the new DuckDB-based functions which are more
efficient:

- [`load_summary()`](https://peteowen1.github.io/panna/reference/load_summary.md),
  [`load_events()`](https://peteowen1.github.io/panna/reference/load_events.md),
  [`load_shots()`](https://peteowen1.github.io/panna/reference/load_shots.md),
  etc.

- These download only what's needed and filter via SQL

This function is still useful for downloading the complete dataset for
local development.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download all data
pb_download_data()

# Download to custom location
pb_download_data(dest = "my/data/path")
} # }
```
