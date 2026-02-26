# Process all raw data

Master function to process all collected data. Supports per-component
caching to avoid reprocessing on crashes.

## Usage

``` r
process_all_data(
  raw_data,
  show_progress = TRUE,
  cache_dir = NULL,
  raw_data_mtime = NULL
)
```

## Arguments

- raw_data:

  List of raw data from pannadata

- show_progress:

  Logical, whether to show progress bar (default TRUE)

- cache_dir:

  Optional directory for per-component caching. If provided, each
  component is cached separately and only reprocessed if raw data
  changed.

- raw_data_mtime:

  Optional modification time of raw data file for cache invalidation. If
  not provided and cache_dir is set, all components will be reprocessed.

## Value

List of processed data frames
