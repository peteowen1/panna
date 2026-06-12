# Default cross-confederation K multiplier

Multiply K by this factor when the two teams are from different
confederations. v6 optimized to 2.49 (was 1.5 in v3, 2.49 in v5/v6) –
cross-confederation matches are rare but high-information signal for
calibrating pools against each other; multiplying their K up means the
rare WC / friendly cross-conf matches drive most of the
confederation-vs-confederation Elo divergence. 1.0 disables.

## Usage

``` r
ELO_CROSS_CONF_MULT
```

## Format

An object of class `numeric` of length 1.
