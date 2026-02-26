# Split Pass Credit (Legacy Helper)

Splits pass EPV between passer and receiver based on difficulty.

Splits EPV credit between passer and receiver based on pass difficulty.
Harder passes (lower xPass) give more credit to the passer.

## Usage

``` r
split_pass_credit(pass_value, xpass)

split_pass_credit(pass_value, xpass)
```

## Arguments

- pass_value:

  Total EPV value of the pass

- xpass:

  Predicted pass completion probability

## Value

List with passer_credit and receiver_credit vectors

List with passer_credit and receiver_credit

## Examples

``` r
# Easy pass (80% completion): receiver gets most credit
split_pass_credit(0.1, 0.8)
#> $passer_credit
#> [1] 0.02
#> 
#> $receiver_credit
#> [1] 0.08
#> 

# Difficult pass (30% completion): passer gets most credit
split_pass_credit(0.1, 0.3)
#> $passer_credit
#> [1] 0.07
#> 
#> $receiver_credit
#> [1] 0.03
#> 
```
