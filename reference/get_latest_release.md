# Get Release Info

Fetches information about a GitHub release for pannadata.

## Usage

``` r
get_latest_release(repo = "peteowen1/pannadata", tag = NULL)
```

## Arguments

- repo:

  GitHub repository in "owner/repo" format.

- tag:

  Release tag to fetch. Use "fbref-latest", "understat-latest",
  "opta-latest", or NULL for the most recent release.

## Value

List with release information (tag_name, assets, etc.).

## Examples

``` r
if (FALSE) { # \dontrun{
# Get FBref release
release <- get_latest_release(tag = "fbref-latest")
print(release$tag_name)

# Get most recent release
release <- get_latest_release()
} # }
```
