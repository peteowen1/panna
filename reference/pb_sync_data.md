# Sync local data with GitHub Releases

Convenience function that uploads local data to GitHub Releases. This
replaces the existing data in the release.

## Usage

``` r
pb_sync_data(repo = "peteowen1/pannadata", tag = "latest", source = NULL)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format

- tag:

  Release tag (default: "latest")

- source:

  Source directory (default: pannadata_dir())

## Value

Invisible NULL

## Examples

``` r
if (FALSE) { # \dontrun{
pb_sync_data()
} # }
```
