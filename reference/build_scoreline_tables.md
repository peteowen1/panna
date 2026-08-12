# Build conditional scoreline samplers (internal helper)

The simulator draws a match OUTCOME from the model's win/draw/loss split
(after the run-hot Elo nudge), then needs a SCORELINE consistent with
it. Drawing goals from independent Poissons and patching them to agree –
what this file did until 2026-08-12 – biased goal difference in one
direction: a win whose Poisson draw disagreed was snapped to the
smallest consistent margin (`if (g1 <= g2) g1 <- g2 + 1L`), and a draw
overwrote side 2's goals with side 1's (`g2 <- g1`), discarding `lam2`
entirely. (Sides here are the two teams as listed, `t1`/`t2` in the
group loop and `ta`/`tb` in
[`play_knockout_round()`](https://peteowen1.github.io/panna/reference/play_knockout_round.md)
– neither is a home/away designation.) Group GD is a FIFA tiebreak and
feeds the best-8 third-place cut, so that bias landed in the published
advancement numbers.

## Usage

``` r
build_scoreline_tables(lam1, lam2, max_goals = 8L)
```

## Arguments

- lam1, lam2:

  Expected goals for side 1 and side 2, one entry per match. Must be
  finite; callers are expected to floor them (the simulator uses
  `pmax(0.2, .)`).

- max_goals:

  Goal cap. Mass above it is lumped onto the cap, matching the previous
  `pmin(rpois(), 8L)` behaviour.

## Value

A list holding, for each outcome region (`win` = side 1 scores more,
`draw`, `loss`): `<region>_g1` / `<region>_g2`, the region's goal pairs
(shared across matches), and `<region>_cum`, a cumulative-probability
matrix with one column per match. Sample with
`findInterval(u, cum[, i]) + 1L` and index the goal-pair vectors.

## Details

Instead: sample from the independent-Poisson joint pmf restricted to the
region the outcome selected, renormalised. The model's W/D/L
probabilities are preserved exactly – they still choose the region – and
the margin distribution within a region is the one the lambdas actually
imply.
