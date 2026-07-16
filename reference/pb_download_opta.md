# Incrementally download release assets that are missing or stale

Syncs a GitHub release to a local directory by downloading only the
assets that are **missing**, a **different size**, or **updated more
recently** on the release than the local copy. Avoids re-pulling the
full multi-GB dataset when only a few consolidated files changed (e.g.
the daily Opta scrape refreshes ~10 of ~125 assets). Unlike the old
tarball-based downloads, it does not require a tarball asset — it
operates on the individual release files.

## Usage

``` r
pb_download_opta(
  dest = NULL,
  repo = "peteowen1/pannadata",
  tag = "opta-latest",
  pattern = NULL,
  check_timestamp = FALSE,
  force = FALSE,
  dry_run = FALSE,
  verbose = TRUE
)
```

## Arguments

- dest:

  Destination directory (default: the Opta data dir,
  [`opta_data_dir()`](https://peteowen1.github.io/panna/reference/opta_data_dir.md)).
  Files are written flat into this directory, matching the consolidated
  layout the loaders read.

- repo:

  GitHub repository in "owner/repo" format.

- tag:

  Release tag (default `"opta-latest"`).

- pattern:

  Optional regex on asset names to restrict the sync (e.g.
  `"^events_|^opta_"`). `NULL` considers all assets.

- check_timestamp:

  Logical. If `TRUE`, also re-download assets whose release timestamp is
  newer than the local file even when the size matches (stricter;
  catches same-size content changes but re-pulls files that were merely
  re-uploaded). Default `FALSE` — size + presence only, which is the
  efficient "what actually changed" sync for these append-growing files.

- force:

  Re-download every matching asset regardless of local state.

- dry_run:

  Report what would be downloaded without downloading.

- verbose:

  Print per-file status.

## Value

Invisibly, a data.frame with one row per asset: `file_name`, `action`
("download"/"skip"), `reason`, and `size`.

## See also

Other data distribution:
[`load_predictions()`](https://peteowen1.github.io/panna/reference/load_predictions.md),
[`pb_download_predictions()`](https://peteowen1.github.io/panna/reference/pb_download_predictions.md),
[`pb_list_sources()`](https://peteowen1.github.io/panna/reference/pb_list_sources.md)

## Examples

``` r
if (FALSE) { # \dontrun{
# See what's out of date without downloading
pb_download_opta(dry_run = TRUE)

# Pull only the changed consolidated files
pb_download_opta()

# Just the event files
pb_download_opta(pattern = "^events_")
} # }
```
