# Derive goal events from shooting data

When match summary data isn't available in the pre-scraped cache, we can
derive goal events from the shooting data (which IS available). This
gives us goal timing for splint creation, though not substitution
timing.

## Usage

``` r
derive_events_from_shooting(shooting_data)
```

## Arguments

- shooting_data:

  Shooting data from load_fb_match_shooting (already snake_case)

## Value

Data frame of goal events with timing
