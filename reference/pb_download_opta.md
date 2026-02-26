# Download Opta Data to Local Directory

Downloads consolidated Opta parquet files from GitHub releases to the
local pannadata/data/opta/ directory. Match events are excluded (too
large for a single file) - use
`load_opta_match_events(source = "remote")` to load them on-demand from
per-league release files.

## Usage

``` r
pb_download_opta(
  repo = "peteowen1/pannadata",
  tag = "opta-latest",
  dest = NULL
)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format.

- tag:

  Release tag (default: "opta-latest").

- dest:

  Destination directory. If NULL, uses pannadata_dir()/opta.

## Value

Invisibly returns the path to the installed data.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download latest Opta data to pannadata/data/opta/
pb_download_opta()

# Then load with:
load_opta_stats("ENG", season = "2024-2025")
} # }
```
