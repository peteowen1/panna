# WPA scale regression — wp_model.rds producing values ~30× too large

> **STATUS: RESOLVED 2026-05-29** (commits 52efcea + 6b41750). Retained
> as a retro because the original “Suspect” section guessed wrong and
> the actual cause is worth recording. See “Actual root cause” below.

> Filed from an inthegame-blog session 2026-05-29. Surfaced after the
> panna catch-up run that fixed the step-10b cadence (panna#71) shipped
> the new pannamodels-backed `wp_model.rds` (pannamodels e78ce0f). The
> fresh `game_logs.parquet` it produced has WPA columns on a wildly
> wrong scale — every player’s season totals are 10-30× larger than the
> old (April 21) build that was retired.

## Symptom

Verified live against
`https://pub-ee4bf5b599a047f9ac2b9facc1587008.r2.dev/football/game-logs.parquet`
(built 2026-05-29 ~01:00 UTC from this catch-up run):

| Stat                                                                 | Observed   | Expected (old build) |
|----------------------------------------------------------------------|------------|----------------------|
| Per-match `wpa_total` median (EPL 2025-26, 10,944 player-match rows) | **1.23**   | ~0.05                |
| Per-match `wpa_total` max — single game                              | **16.59**  | ~0.5                 |
| 99th-percentile per-match `\|wpa\|`                                  | **7.16**   | ~0.3                 |
| Top season `wpa_total` (E. Konsa)                                    | **+51.49** | ~+5                  |
| Bottom season `wpa_total` (N. O’Reilly)                              | **-30.79** | ~-5                  |

Spot example: **Aleix García** (Bayer Leverkusen) shows
`wpa_total = -5.78 in a single Bundesliga match` (2025-08-30) and
`+67.08 cumulative over 32 Bundesliga games`. Both are physically
impossible — a single match’s possession-POV WP swing is bounded by 1.0;
a season’s signed `WPA` for a top player typically sits around ±3-5.

Box-score columns (Scoring / Passing / Defending / Duels / Discipline)
are unaffected — they read from a different parquet
(`match-stats-{CODE}.parquet`) built by pannadata directly from
`opta_player_stats.parquet`. Only the WPA columns in `game_logs.parquet`
are wrong.

EPV columns in the same parquet (`epv_total`, `epv_total_adj`,
`epv_offensive_adj`, `epv_defensive_adj`, `epv_passing`, `epv_shooting`,
`epv_dribbling`, `epv_aerial`, `epv_keeping`, `epv_defending`) look
correct — those came from a different model.

`psv` / `osv` / `dsv` also look correct.

## Suspect (original — WRONG, see “Actual root cause” below)

The wp_model.rds asset uploaded to pannamodels (`epv` release, 123 KB,
registered as `wp_model` in `.EPV_MODELS` via pannamodels e78ce0f) is
producing output on the wrong scale relative to the old local
`wp_model.rds` that the pipeline used pre-pannamodels.

Most likely *(none of these turned out to be correct)*:

1.  ~~**Logit-vs-probability output**: if the new model emits logits…~~
    — Disproven: `range(predict_wp(m, sample))` was already inside
    `[0, 1]`. The model was correctly emitting probabilities.
2.  ~~**Different training target**~~ — Disproven: target was still
    win-binary.
3.  ~~**Feature pipeline mismatch**~~ — Disproven: features matched.

## Actual root cause (resolved 2026-05-29)

**The model was retrained to a possession-team POV (commit b20b6b3,
2026-05-19) — it predicts `P(team in possession wins)`. The consumer
code in
[`add_wp_vars()`](https://peteowen1.github.io/panna/reference/add_wp_vars.md)
was still computing `wp_after - wp_before` as if both numbers were on
the same fixed POV (home).**

When possession changes between consecutive events, `wp_next` is from a
*different team’s* POV than `wp`. Subtracting them produces a delta that
crosses the POV boundary — for a near-coin-flip game, a possession
switch flips `wp` from e.g. 0.50 (home POV) to 0.50 (away POV), but the
model’s possession-POV output for the next event might be 0.50 (still
~coin flip) from the *new* possessor’s side. Mixing POVs makes the
per-event delta swing ±0.5 on most events instead of the ±0.001–0.05 a
true WPA should show. Summed over ~100 events/match, that’s the 30×
inflation we observed.

EPV columns were unaffected because the EPV model is trained from the
perspective of the acting team natively, and
[`assign_epv_credit()`](https://peteowen1.github.io/panna/reference/assign_epv_credit.md)
was always written for that convention.

## Fix

Two stages, both shipped 2026-05-29:

1.  **52efcea (interim)** — convert possession-POV → home-POV inside
    `add_wp_vars` before computing the delta. Correct math, but ugly: it
    converts to a fixed POV just to immediately re-pivot per actor
    downstream.

2.  **6b41750 (final)** — adopt torpverse’s `add_variables.R`
    convention: keep everything in possession (acting-team) POV
    throughout, and compute the delta as

    ``` r
    data.table::fcase(
      team_id_next == team_id, wp_next - wp,            # same team continues
      default = (1 - wp_next) - wp                       # possession flipped
    )
    ```

    This matches the WP model’s training POV and removes the conversion
    step entirely. `wpa` is then naturally on the acting-team POV, which
    is what
    [`assign_wpa_credit()`](https://peteowen1.github.io/panna/reference/assign_wpa_credit.md)
    expects.

A worked example (OLD vs INTERIM vs FINAL on the same three-event
sequence) is preserved at `debug/demo_wpa_logic.R` for posterity.

## Lessons

- **Always check the POV convention of any model that emits a per-event
  probability before consuming its deltas.** “Probabilities” alone don’t
  tell you which side they’re from.
- The logit-vs-probability hypothesis was tempting because of the rough
  magnitude match, but a simple `range(preds)` check on a sample would
  have disproven it in seconds — that diagnostic *was* available; it
  just wasn’t run before patching. Run the cheap diagnostic first.
- When porting a torpverse pattern (the WP model retraining was a port
  from torp’s possession-POV WP), check both the model definition *and*
  its consumer; a half-port leaves a coordinate-system mismatch that
  compiles cleanly and produces visually-plausible-but-wrong output.

## Sanity-check on output

Once the fix lands, blog can verify in one shot:

``` js
// Load fresh game-logs.parquet, aggregate EPL 2025-26 wpa_total per player
// Expected top season WPA ≤ ~5, bottom ≥ ~-5, per-match max ≤ ~0.5
```

## Related

- panna#71 (closed) — step-10b cadence fix
- panna#72 (closed) —
  [`load_wp_model()`](https://peteowen1.github.io/panna/reference/load_wp_model.md)
  fallback to pannamodels
- pannamodels e78ce0f — `.EPV_MODELS` registration + asset upload
- inthegame-blog will file an issue tracking this WPA regression and
  link it back to whatever panna/pannamodels issue gets opened here.
  **Blog#244 is fully fixed (GP cap unblocked); don’t reopen — this is a
  separate regression introduced by the catch-up run.**

## Out of scope (don’t fix in this issue)

- Box-score stats (Scoring / Passing / Defending / Duels / Discipline) —
  fine
- EPV columns — fine
- PSV / OSV / DSV — fine
- Match predictions, ratings, simulations — fine
