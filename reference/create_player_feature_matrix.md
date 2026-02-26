# Create full player feature matrix

Builds the complete feature matrix for SPM model.

## Usage

``` r
create_player_feature_matrix(processed_data, min_minutes = 180)
```

## Arguments

- processed_data:

  List of processed data

- min_minutes:

  Minimum minutes for inclusion

## Value

Data frame with all player features

## Examples

``` r
if (FALSE) { # \dontrun{
processed_data <- process_all_data("ENG", "2024-2025")
features <- create_player_feature_matrix(processed_data, min_minutes = 180)
head(features)
} # }
```
