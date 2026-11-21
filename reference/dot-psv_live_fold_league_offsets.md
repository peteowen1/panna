# Fold the PSV league offsets into the live-PSV centring constants

Step 10b adds a per-league offset to game-log PSV after centring and
position calibration (`psv + offset * minutes / 90`, with half of it on
each of `osv` and `dsv`). The live scorer computes
`(f * raw - K) * minutes / 90`, so K has to carry the same offset with
the opposite sign or every league's live PSV sits `-offset` away from
its game-log PSV (panna#516). This subtracts the offset from `psr` and
half of it from each of `osr` and `dsr`, which keeps `osr + dsr = psr`.

## Usage

``` r
.psv_live_fold_league_offsets(constants, offsets)
```

## Arguments

- constants:

  data.table of live constants with columns `league`, `psr`, `osr`,
  `dsr`.

- offsets:

  Offset table (columns `league` in Opta naming, `offset`), as written
  to `psr_league_offsets.parquet`.

## Value

`constants` with `psr`/`osr`/`dsr` shifted and a `psv_league_offset`
column recording the per-90 offset used.

## Details

League codes are mapped the way 10b maps them
([`to_opta_league()`](https://peteowen1.github.io/panna/reference/to_opta_league.md),
falling back to the code itself). A league with no offset row gets 0, as
in 10b. The `"__default__"` rows are left alone: the live scorer only
uses them for a league that has no constants of its own, and such a
league has no offset to apply.
