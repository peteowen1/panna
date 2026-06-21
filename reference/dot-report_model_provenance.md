# Load EPV Model

Loads pre-trained EPV model from disk.

## Usage

``` r
.report_model_provenance(model_label, model_path, source, max_age_days = 14)
```

## Arguments

- model_label:

  e.g. "EPV", "WP".

- model_path:

  Resolved file path, or NULL for package-provided models.

- source:

  Human label for where it came from.

- max_age_days:

  Warn above this age (default 14).

## Details

Report which model file was loaded, with date + staleness warning

The model-loader fallback chains (explicit path → pannamodels → local)
used to announce only the source, not the file DATE — so a silent
fallback to a stale model (the 2026-06-21 inflated-EPV incident) looked
identical to a correct load in the logs. This always prints the resolved
file's modification date and WARNS if it's older than `max_age_days`, so
a stale model is visible.
