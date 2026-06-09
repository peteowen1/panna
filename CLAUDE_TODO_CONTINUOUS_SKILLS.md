# TODO / DESIGN — Skills as a continuous career trait (not season-level)

**Status:** design agreed 2026-06-09 (Pete + Claude). **Not yet implemented.**
**Origin:** football blog stat-coverage thread (`pannadata/CLAUDE_TODO_FOOTBALL_STAT_COVERAGE.md`).
F. Chiesa has 8 seasons of skills in `opta_skills.parquet` (2017–2024) but **no
2025/2026 row** — even though his career-decay-weighted skill plainly still exists.
That exposed a conceptual mismatch we want to fix.

---

## The unifying principle (Pete's definition — this is the spec)

> **A *Rating* — the `-R` suffix family: EPR, OSR, DSR, PSR, plus the metric "skill"
> traits — is the model's best guess at a player's _next game_ in that dimension**,
> formed from ALL prior info, recency-weighted (decay) and shrunk by sample size.
>
> **Production / value metrics — EPV, WPA, PSV/OSV/DSV — are what _actually happened_**
> in a game or season (summed, accumulated).

Corollary: **a Rating/trait has no business being "season-level."** It is a continuous
function of a player's whole career, *evaluated at a date*. Only production accumulates
into seasons. The two were tangled together in the skills export; this note untangles
them.

## Metric taxonomy (agreed)

| Layer | Metrics | Time-nature | Changes? |
|---|---|---|---|
| **Ratings / traits** ("who the player is", next-game predictor) | metric skills (`*_p90`, accuracies), **PSR / OSR / DSR**, **EPR** | Career, decay-weighted, **as-of any date** | ✅ make continuous (this note) |
| **Production / value** ("what they did") | **EPV, WPA, PSV / OSV / DSV** | Per-game → per-season (accumulate) | ⬜ no change — season-level is correct |
| **Contribution** (adjusted impact) | RAPM, **xRAPM / Panna** | Per-season splint fit | ⬜ no change |
| **Prior** | **SPM** (XGBoost: box-score → RAPM) | Per-season | ⬜ **stays the stat-based prior for xRAPM** (Pete: "perfect, keep it") |

EPR was the straddler — resolved: it's a **trait** (the `R` = Rating = best guess of
next-game EPV, decay-weighted). Filed under Ratings.

---

## Key finding — the skill VALUES are already continuous; only a publishing gate makes them look season-level

`aggregate_skills_for_spm()` (`R/estimated_skills.R:831`) is **already** career-decay-weighted:

- **L892** — `estimate_player_skills(..., target_date = target_date)` uses **ALL history
  up to that date**, `weight = exp(-lambda * days_since_match)` (half-life ~8mo volume,
  ~1yr efficiency). The "season" is just *the date the career function is evaluated at*
  (default 30 Jun of each year).
- **L883–885** — the only thing season-level is the **eligibility gate**:
  `eligible_players <- season_minutes[season_minutes >= min_minutes]` with
  `min_minutes = 450` **in that season**. This is a *publishing filter, not a model choice.*

So a "player-season" skills row = *"career-decay-weighted skill as of 30 Jun YYYY, emitted
only because the player also logged ≥450 min in season YYYY."* That gate is why Chiesa
disappears: the career function would gladly value him today; the gate refuses to emit a
2026 row because his current-season minutes are low.

**The fix is small:** keep the as-of-date evaluation, **drop the per-season minutes gate**,
gate instead on **career sample** (`weighted_90s`). No new model.

### Precedent already in the repo

`08b_export_psr_weekly.R:428` already does exactly this — as-of-date PSR snapshots gated
on `min_weighted_90s = 3`, **no season-minutes gate.** The continuous-trait pattern exists;
we're aligning the main skills export (`02_estimate_skills.R` → `08_export_skills.R`) to it.

---

## The change (decided)

**Change locus:** `data-raw/estimated-skills/02_estimate_skills.R:58–62` — the
`aggregate_skills_for_spm(min_minutes = 450, ...)` call that feeds both
`opta_skills.parquet` and the skill-SPM/PSR path.

1. **Drop the per-season `min_minutes` (450) eligibility gate.** Keep `min_weighted_90s`
   (career sample) as the sole trait gate.
2. **Keep per-date snapshots** (Pete Q2: yes — we want "how good was a player around date
   D"). Evaluate the career function at each snapshot date; emit every player whose
   *career* `weighted_90s` clears the threshold as-of that date.
3. **Blog serves the latest snapshot** (as-of today / latest date) → Chiesa returns;
   coverage 80% → ~89%+ (604 players like him have prior career sample).

### Self-limiting property (why this doesn't balloon or include the long-retired)

Because `weighted_90s` is **decay-weighted**, a player who stops playing sees it decay
below threshold within a few seasons and naturally drops out — no separate "still active"
filter needed. The career-sample gate *is* the activity gate, on a smooth ramp.

---

## What explicitly does NOT change (Pete confirmed)

- **SPM stays the stat-based prior for xRAPM** — untouched.
- **xRAPM / Panna** stay per-season adjusted contribution.
- **EPV / WPA / PSV** stay season production (summed).
- We are **not** rebuilding the xRAPM prior on career skills (the bigger idea floated
  earlier — explicitly declined; current SPM is "perfect" for that role).

---

## Resolved decisions (Pete, 2026-06-09)

1. **Drop the gate EVERYWHERE.** Remove the per-season `min_minutes` (450) gate from the
   whole skills pipeline — both the published `opta_skills.parquet` *and* the skill-SPM/PSR
   fits (`03`/`04`/`07`). `weighted_90s` becomes the sole trait gate. (More low-minute
   players enter the PSR fits; `weighted_90s` shrinkage should protect stability — **verify
   PSR fit sanity after the change**, but the decision is: one consistent un-gated path.)
2. **`weighted_90s` threshold = 3** (matches `08b`, maximises coverage).
3. **Cadence = weekly** (the as-at-date resolution Pete wants), implemented in two parts
   because one file can't serve both jobs:
   - **(a) Current-skills snapshot for the card game** — a single as-of-*latest-date*
     snapshot of the **full skill feature set** (incl. raw `*_p90`s + accuracies), `w90≥3`,
     no season gate. Small (~6k rows). This is what the blog Top Trumps loads. Fixes Chiesa.
   - **(b) Weekly historical snapshots for date-scrubbing** — reuse `08b`'s
     `seq(by = "7 days")` + monthly-before-2yr + **incremental "recompute last 4 weeks"**
     machinery, but extend it to carry the **raw `*_p90` skill features**, not just PSR
     (08b currently emits PSR ratings only — confirm/extend its column set).
   - ⚠ **File-size guard:** weekly × full-skill-features over all history is ~700MB — do
     **not** make that the default browser load. The card game loads (a) only; the weekly
     full-feature file (b) is a separate, on-demand / server-side resource. (PSR-only weekly
     `opta_psr_weekly.parquet` already exists and stays.)
4. **Blog reads TRAIT columns, not season context.** Rows still carry per-season
   `total_minutes` / `n_matches` (`estimated_skills.R` L906–909) which go ~0 for a snapshot
   with little current-season play. Blog must derive display totals as
   **`*_p90 × weighted_90s`** (the trait sample), NOT season `total_minutes` — else a
   returning Chiesa shows skills but "0 games". **Verify in `inthegame-blog`
   `football/cards.qmd` + `cards-game.js`** before/after.

### Implementation sketch (order)

1. `02_estimate_skills.R` — drop `min_minutes` (= 0) AND add an explicit
   `weighted_90s >= min_career_w90` inclusion gate. ⚠ **`min_weighted_90s` is NOT a gate**
   — its docstring says "Not used for shrinkage (handled by Bayesian prior)" (it's the
   estimator's regression threshold). Dropping `min_minutes` alone leaves NO inclusion
   filter, so `w90=0.04` noise leaks in. The real gate is a post-aggregation
   `skill_features[weighted_90s >= min_career_w90]` with `min_career_w90 = 3`.
2. New/extended export for **(3a)** — emit the single latest-date full-skill snapshot the
   blog reads (could be a thin addition to `08`, evaluating `estimate_player_skills` at
   `Sys.Date()` with `w90≥3`).
3. Extend **(3b)** `08b` (or a sibling) to retain raw `*_p90` columns for weekly history.
4. Re-run skills `01–08` (manual — no scheduled CI; see verify section) and re-publish.
5. Blog-side: point the card game at the current-skills snapshot; confirm `*_p90 ×
   weighted_90s` totals (#4).

### Implemented + validated 2026-06-09 (step 1 done; 2–5 pending)

Step 1 shipped to the working tree (`run_skills_pipeline.R` config + `02_estimate_skills.R`:
`min_minutes_spm = 0`, new `min_career_w90 = 3` gate). Validated non-destructively against
the existing cache (`debug/validate_continuous_skills.R`, `debug/analyze_w90_gate.R`):

| gate | latest-season players | rated-pool coverage |
|---|---|---|
| old 450-min | 5,365 | 80.0% |
| **w90 ≥ 3 (chosen)** | **5,963** | **85.6%** |
| w90 ≥ 5 | 5,071 | 74.2% (worse than today) |
| w90 ≥ 0 (no gate) | 8,867 | 100% but full of `w90≈0.04` noise |

- **Corrected number: 80% → 85.6%** (not the earlier 89% — that estimate counted *any*
  prior row, including thin-sample noise; the principled `w90≥3` career gate is 85.6% and
  every covered player has a real decay-weighted sample).
- F. Chiesa returns: 2026 row, `weighted_90s = 5.27`, `goals_p90 = 0.35` (season minutes
  were 345 — under the old 450 gate; that's why he'd vanished).
- `w90 ≥ 5` is *worse* than today (74%), confirming `3` is the right threshold.

**Blog verify (#4) — PASS:** `inthegame-blog/football/cards.qmd:488/493/495` already takes
the latest-season row per player and derives totals as `*_p90 × weighted_90s` (primary;
`total_minutes/90` only as fallback). No blog change needed for the derivation. Two notes:
the card deck is separately gated at `total_minutes >= 600` (`cards.qmd:47`) — the real
binding constraint on card candidates, independent of skills; and `cards.qmd:497` has a
now-stale "~66%" comment.

**Still pending:** full skills re-run `01–08` + **PSR fit sanity check** (the un-gate
enlarges the training set — the one thing to verify before shipping), the weekly **(3a/3b)**
exports, and re-publish to `opta-latest`.

## Non-goals

- No change to SPM / xRAPM / Panna / EPV / WPA / PSV.
- No fbref/understat back-fill (orthogonal, already ruled out).
- Not the CAF_CL/Tunisian blog-filter (separate, already implemented in pannadata).

## When implemented — verify

- Chiesa (`player_id c6nl81c1q4unq5js94s6s942x`) has a current-season skills row.
- Blog `player-skills.parquet` coverage of the rated pool ≥ ~89%.
- PSR/skill-SPM fits didn't degrade (compare held-out / coefficient sanity vs pre-change).
- Re-run skills steps 01–08 and re-publish `opta_skills.parquet` to `opta-latest`
  (reminder: **no scheduled CI rebuilds this** — steps 01–08 are manual; see
  `pannadata/CLAUDE_TODO_FOOTBALL_STAT_COVERAGE.md` operational gotcha).
