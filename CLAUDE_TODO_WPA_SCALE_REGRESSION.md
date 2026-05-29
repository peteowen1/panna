# WPA scale regression — `wp_model.rds` producing values ~30× too large

> Filed from an inthegame-blog session 2026-05-29. Surfaced after the
> panna catch-up run that fixed the step-10b cadence (panna#71) shipped
> the new pannamodels-backed `wp_model.rds` (pannamodels e78ce0f).
> The fresh `game_logs.parquet` it produced has WPA columns on a wildly
> wrong scale — every player's season totals are 10-30× larger than
> the old (April 21) build that was retired.

## Symptom

Verified live against `https://pub-ee4bf5b599a047f9ac2b9facc1587008.r2.dev/football/game-logs.parquet` (built 2026-05-29 ~01:00 UTC from this catch-up run):

| Stat | Observed | Expected (old build) |
|------|----------|----------------------|
| Per-match `wpa_total` median (EPL 2025-26, 10,944 player-match rows) | **1.23** | ~0.05 |
| Per-match `wpa_total` max — single game | **16.59** | ~0.5 |
| 99th-percentile per-match `\|wpa\|` | **7.16** | ~0.3 |
| Top season `wpa_total` (E. Konsa) | **+51.49** | ~+5 |
| Bottom season `wpa_total` (N. O'Reilly) | **-30.79** | ~-5 |

Spot example: **Aleix García** (Bayer Leverkusen) shows `wpa_total = -5.78 in a single Bundesliga match` (2025-08-30) and `+67.08 cumulative over 32 Bundesliga games`. Both are physically impossible — a single match's possession-POV WP swing is bounded by 1.0; a season's signed `WPA` for a top player typically sits around ±3-5.

Box-score columns (Scoring / Passing / Defending / Duels / Discipline) are unaffected — they read from a different parquet (`match-stats-{CODE}.parquet`) built by pannadata directly from `opta_player_stats.parquet`. Only the WPA columns in `game_logs.parquet` are wrong.

EPV columns in the same parquet (`epv_total`, `epv_total_adj`, `epv_offensive_adj`, `epv_defensive_adj`, `epv_passing`, `epv_shooting`, `epv_dribbling`, `epv_aerial`, `epv_keeping`, `epv_defending`) look correct — those came from a different model.

`psv` / `osv` / `dsv` also look correct.

## Suspect

The wp_model.rds asset uploaded to pannamodels (`epv` release, 123 KB, registered as `wp_model` in `.EPV_MODELS` via pannamodels e78ce0f) is producing output on the wrong scale relative to the old local `wp_model.rds` that the pipeline used pre-pannamodels.

Most likely:

1. **Logit-vs-probability output**: if the new model emits logits (typically [-5, +5] range for football scenarios) instead of probabilities ([0, 1]), each `wp_after - wp_before` per-event delta inflates by roughly the magnitude of `4 × |dlogit/dp|` at p≈0.5 (i.e. ~10×) and much more at extreme p. Sum over ~100 events per match gives the per-match WPAs of ±5+ we're observing.
2. **Different training target**: e.g. score-margin model instead of win-binary.
3. **Feature pipeline mismatch**: if features fed to the model at inference don't match what the model was trained on, predictions can be wildly miscalibrated in either direction.

## Diagnostic checklist

```r
# 1. Load the asset that pannamodels just shipped
m <- pannamodels::load_epv_model("wp_model")
class(m)              # expected: a glm / xgboost / mlr3 wrapper / ...
m$params              # if xgboost, look at "objective"

# 2. Pump a handful of typical pre-event states through it
sample_feats <- # ... pull a few rows of features that step_10b would feed
preds <- predict(m, sample_feats)
range(preds)          # expected: [0, 1]; if [-5, +5] -> logits, wrap in plogis()
```

If `range(preds)` extends outside [0, 1]:
- Wrap output in `plogis()` (or `1 / (1 + exp(-x))`) wherever step_10b consumes the WP predictions.
- OR retrain the model with `objective = "binary:logistic"` (xgboost) / `family = binomial(link = "logit")` (glm) so it natively emits probabilities.

## Sanity-check on output

Once the fix lands, blog can verify in one shot:

```js
// Load fresh game-logs.parquet, aggregate EPL 2025-26 wpa_total per player
// Expected top season WPA ≤ ~5, bottom ≥ ~-5, per-match max ≤ ~0.5
```

## Related

- panna#71 (closed) — step-10b cadence fix
- panna#72 (closed) — `load_wp_model()` fallback to pannamodels
- pannamodels e78ce0f — `.EPV_MODELS` registration + asset upload
- inthegame-blog will file an issue tracking this WPA regression and link it back to whatever panna/pannamodels issue gets opened here. **Blog#244 is fully fixed (GP cap unblocked); don't reopen — this is a separate regression introduced by the catch-up run.**

## Out of scope (don't fix in this issue)

- Box-score stats (Scoring / Passing / Defending / Duels / Discipline) — fine
- EPV columns — fine
- PSV / OSV / DSV — fine
- Match predictions, ratings, simulations — fine
