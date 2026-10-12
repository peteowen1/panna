# Give goal/sub/card events their match period

The scraped events table (goals, substitutions, cards) has no period_id,
and Opta's clock restarts the second half at 45, so an event at 45-57
minutes can be first-half stoppage or second half. Matches each row to
the raw event stream on (match_id, minute, second, team_id) among goal /
sub / card types and takes that period; unmatched rows are period 1
before 45 and period 2 after the first half's end, else NA (left
unshifted). Review R009.

## Usage

``` r
.attach_event_periods(events, match_events)
```

## Arguments

- events:

  Events table (match_id, minute, second, team_id, ...).

- match_events:

  Raw Opta match events with period_id.

## Value

`events` with a `period_id` column.
