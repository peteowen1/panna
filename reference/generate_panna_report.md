# Generate prediction report

Creates a formatted summary of panna ratings.

## Usage

``` r
generate_panna_report(panna_model, top_n = 10)
```

## Arguments

- panna_model:

  Fitted panna model

- top_n:

  Number of top/bottom players to show

## Value

Character string report

## Examples

``` r
if (FALSE) { # \dontrun{
panna_model <- fit_panna_model(splint_data, player_features)
report <- generate_panna_report(panna_model, top_n = 15)
cat(report)
} # }
```
