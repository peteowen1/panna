# Upload data to GitHub Releases

Zips the local data directory and uploads it to a GitHub Release.
Creates the release if it doesn't exist.

## Usage

``` r
pb_upload_data(repo = "peteowen1/pannadata", tag = "latest", source = NULL)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format (default:
  "peteowen1/pannadata")

- tag:

  Release tag to upload to (default: "latest")

- source:

  Source directory containing 'data' folder (default: pannadata_dir())

## Value

Invisible path to uploaded zip file

## Examples

``` r
if (FALSE) { # \dontrun{
# Upload all data
pb_upload_data()
} # }
```
