# A number for each player id, the same in R and on the website

The site's parquet reader only skips row groups when it filters on a
numeric column (min/max statistics on text are not safe to compare in
the browser – see `_rowGroupRanges` in the blog's `data-loader.js`).
Filtering the player file on `player_id` therefore read all 39 row
groups, one request each, and timed out at 30 s over R2. Sorting by this
number and filtering on it lets the reader fetch the one or two row
groups that hold the player.

## Usage

``` r
.ng_player_bucket(id)
```

## Arguments

- id:

  Character vector of player ids.

## Value

Integer vector in 0..1000002.

## Details

Polynomial hash, base 31, modulo 1,000,003: every intermediate stays
below 2^53, so R doubles and JavaScript numbers give identical results.
Twin: `ngPlayerBucket()` in the blog's `football/player.qmd`. Change
both or neither.
