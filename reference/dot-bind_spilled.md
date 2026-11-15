# Bind spilled pieces into one data.table, reading each piece once

Pre-allocates every column at its final length and type, then reads the
pieces one at a time and writes their rows in place with
data.table::set(), so the peak is the result plus one piece. The result
equals `rbindlist(pieces, use.names = TRUE, fill = TRUE)` when every
column is a plain logical / integer / double / character vector and no
column mixes character with another type: column order is first
appearance, the type is the widest of logical \< integer \< double
(character stays character, and a logical piece beside it becomes
character), and rows a piece lacks are NA. Anything else (factors,
Dates, list-columns, character mixed with numbers) falls back to reading
every piece and .rbindlist_freeing(), and says so.

## Usage

``` r
.bind_spilled(spilled, delete = TRUE)
```

## Arguments

- spilled:

  List of
  [`.spill_piece()`](https://peteowen1.github.io/panna/reference/dot-spill_piece.md)
  results, in bind order (NULLs skipped).

- delete:

  Remove each piece file after reading it.

## Value

A data.table.
