# Opta match period identifiers

Opta F24 `period_id`: 1 = first half, 2 = second half (regulation); 3 =
first half extra time, 4 = second half extra time; \>= 5 = penalty
shootout. Confirmed against UCL 2025-2026 PSG-Arsenal (match
6sb5ga83yrll15624x1z0gwt0, 2026-05-30): the minute clock runs
continuously across periods (ET actions are minute 90-120, not reset),
and shootout kicks are stamped at minute 120 under period_id 5.

## Usage

``` r
OPTA_REGULATION_PERIODS

OPTA_EXTRA_TIME_PERIODS
```

## Format

Integer vectors

An object of class `integer` of length 2.
