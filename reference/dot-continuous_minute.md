# Convert Opta (period, clock) times to one continuous match clock

Opta's clock runs ON through first-half stoppage (period 1 can reach
minute 56) and the second half RESTARTS at 45, so first-half stoppage
and the start of the second half share clock values (review R009). This
shifts period-2 times by the real first-half length and extra-time
periods by the real end of the second half, so every splint time
(boundaries, shots, player on/off, value streams) sits on one timeline.

## Usage

``` r
.continuous_minute(
  t,
  period_id,
  first_half_end,
  second_half_end_raw = NA_real_
)
```

## Arguments

- t:

  Numeric clock minutes (minute + second/60).

- period_id:

  Integer period (1, 2; 3/4 extra time).

- first_half_end:

  First-half end on the raw clock (period-1 type 30 marker), recycled.
  NA leaves period-2+ times unshifted.

- second_half_end_raw:

  Second-half end on the raw clock (period-2 marker). NA shifts extra
  time like period 2.

## Value

Numeric continuous minutes.
