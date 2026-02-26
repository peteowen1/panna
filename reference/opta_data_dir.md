# Get or Set Opta Data Directory

Returns the path to the Opta data directory within pannadata. Uses
pannadata/data/opta/ as the base path.

## Usage

``` r
opta_data_dir(path = NULL)
```

## Arguments

- path:

  Optional path to set as the Opta data directory.

## Value

Character path to Opta data directory.

## Examples

``` r
if (FALSE) { # \dontrun{
# Get current path (auto-detected from pannadata)
opta_data_dir()

# Set explicit path
opta_data_dir("C:/path/to/pannadata/data/opta")
} # }
```
