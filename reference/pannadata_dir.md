# Get or set pannadata directory

Gets or sets the base directory for parquet/RDS data storage.

## Usage

``` r
pannadata_dir(path = NULL)
```

## Arguments

- path:

  Optional new path to set. If NULL, returns current path.

## Value

Current pannadata directory path (invisibly when setting)

## Details

Resolution order (first match wins):

1.  Explicitly set via `pannadata_dir("path")`

2.  `PANNADATA_DIR` environment variable

3.  `../pannadata/data` relative to working directory (for the
    pannaverse monorepo layout)

4.  `tools::R_user_dir("panna", "data")` — OS-standard user data dir

OS-standard fallback paths:

- Windows: `C:/Users/you/AppData/Local/R/panna/data`

- Mac: `~/Library/Application Support/org.R-project.R/panna/data`

- Linux: `~/.local/share/R/panna/data`

## Examples

``` r
# Get current path
pannadata_dir()
#> [1] "/home/runner/.local/share/R/panna"

# Set custom path
pannadata_dir("~/my/football/data")
```
