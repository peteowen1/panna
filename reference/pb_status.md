# Check if local data is in sync with GitHub Releases

Compares local data count with what's in the release.

## Usage

``` r
pb_status(repo = "peteowen1/pannadata", tag = "latest", source = NULL)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format

- tag:

  Release tag (default: "latest")

- source:

  Source directory (default: pannadata_dir())

## Value

List with sync status information

## Examples

``` r
if (FALSE) { # \dontrun{
status <- pb_status()
status$remote$exists
status$local$n_files
} # }
```
