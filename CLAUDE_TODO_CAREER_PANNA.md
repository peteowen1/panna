# TODO / DESIGN — Career-trait Panna (decay-weighted multi-season xRAPM)

**Status:** design agreed 2026-06-09 (Pete + Claude). **Build approach: Option 1** (proper
decay-weighted multi-season RAPM fit). Naming LOCKED below. Not yet implemented.

**Origin:** the continuous-skills thread surfaced an asymmetry — every metric family has a
season form AND a career-trait (`-R`) form, *except impact*, which only had season xRAPM.
This adds the missing career-trait impact metric and names it **Panna**.

---

## ⚠ NAMING — LOCKED. Do not deviate. (Naming confusion has bitten before.)

There are now **TWO distinct impact metrics**, and they must never be conflated:

| Name | What it is | Time-base | Prior | Answers |
|---|---|---|---|---|
| **`xrapm`** (+ `xrapm_offense`, `xrapm_defense`) | per-**season** adjusted plus-minus | one season's splints | **season box-score SPM** | "how much did they contribute **in 2025-26**" |
| **`panna`** (+ `panna_offense`, `panna_defense`) | **career** decay-weighted adjusted plus-minus | ALL splints, exp recency-weighted | **career-trait skill-SPM** | "**how good is this player** / next-game impact" |

- **`xrapm` = the season contribution metric.** This is what the blog column *currently
  called `panna`* actually is today. It gets **renamed** `panna -> xrapm` (see Migration).
- **`panna` = the career trait.** NEW. The headline "how good is this player" number,
  recency-weighted across their whole career — stable, doesn't swing on one thin season.
- Both are **published and surfaced distinctly** (different questions; keep both columns).

### Why "Panna" for the trait (not an `-R` name)
The `-R` convention (EPR/OSR/DSR/PSR) names the *value/skill* sub-traits. **Panna is the
top-level brand/headline composite** — so the headline number being the career *trait* (the
thing you quote for "how good is X") is the right call. Mentally: **Panna : xRAPM :: PSR :
PSV :: EPR : EPV** — the trait is the decay-weighted career version of the season metric.

---

## Metric definitions (precise)

- **`xrapm`** — fit the plus-minus ridge/elastic-net over **one season's** splints, with the
  **season box-score SPM** (`05_spm`-style) as the Bayesian prior. Per-season. (= today's
  `panna`.) Use for "best player in season X", season leaderboards.
- **`panna`** — fit the plus-minus regression over **the player's whole career of splints**,
  weighting each splint observation by **`exp(-age_days / halflife)`** (recent splints count
  more), with the **career-trait skill-SPM** (`03_skill_spm`-style, built on the continuous
  career skills) as the prior. One value per player, as-of-now. Use for "how good is X",
  predictions, the headline card/profile number.
  - This pairs the **career-trait metric with the career-trait prior** — unifying with the
    skills work: Panna's prior is the skill-SPM (which already uses career-decay skills),
    NOT the season box-score SPM.

Precedent: `calculate_epr_regression()` already does exactly this shape for EPV (decay-
weighted career regression, β_player IS the rating). Panna is the same move for plus-minus.

---

## Build approach — Option 1 (decay-weighted multi-season RAPM fit)

Chosen over the cheap "decay-average the season ratings" because it's the principled version
(weights the raw splint observations, not pre-aggregated season ratings).

Sketch (to be made concrete after the code-map exploration):
1. **Pool all splints across seasons** (the season loop in `07_seasonal_ratings.R` fits one
   season at a time; the career fit pools them).
2. **Add a per-observation time-decay weight** `w = exp(-(max_date - splint_date)/halflife)`,
   composed with the existing minutes weighting (`use_weights`). Needs splint dates available
   at the observation level.
3. **Fit one RAPM** over the pooled, recency-weighted design matrix (player columns are
   career-spanning, not season-specific — one coefficient per player = their career RAPM).
4. **Apply the career-trait skill-SPM as the prior** → career xRAPM = **Panna**.
5. **Offense/Defense**: same fit on the O/D-decomposed targets → `panna_offense/defense`.
6. New pipeline step + export; surface alongside `xrapm` (renamed) in `ratings.parquet`.

**Decay half-life:** start ~ EPR's range (400–900 days), then tune on held-out next-match
prediction like `optimize_epr_decay()`. Make it a config param, not hardcoded.

---

## Migration (rename with reach — do it atomically per repo)

`panna` (season) -> `xrapm`; introduce NEW `panna` (career trait). Touch points:
- **panna pkg**: seasonal ratings export (`09_export_ratings.R` / `08_panna_ratings.R`),
  any `panna` column producers; add the career-fit step + its export.
- **pannadata**: `build_blog_data.R` (selects `panna = xrapm`...), `ratings.parquet` schema.
- **inthegame-blog**: team profile + cards (column labels, sort keys, `ratings.parquet`
  readers) — show both `xRAPM (season)` and `Panna (career)`.
- **predictions pipeline**: anything consuming `panna` as a player-quality input should move
  to the new career `panna` (that's the "how good" signal predictions want).
- **docs**: panna `CLAUDE.md`, `DATA_DICTIONARY.md`, `ARCHITECTURE.md`.

Guard against the classic trap: grep every repo for `\bpanna\b` (column sense) before/after,
and keep season vs career labels explicit everywhere.

## Build plan — GROUNDED in code (2026-06-09)

The pooled all-season fit **already exists**: `04_rapm.R` calls `prepare_rapm_data(full
splint_data)` + `fit_rapm()` over every season (→ `cache-opta/04_rapm.rds`). Career-Panna =
that pooled fit + (a) decay weights + (b) skill-SPM prior. Resolved questions:

- **Observation dates?** ✅ `prepare_rapm_data` returns `$row_data` with `match_id`
  (`R/rapm_matrix.R:246`); `match_info`/splints carry `match_date` (`splint_creation.R:1771`).
  Join → `age_days = ref_date - match_date`.
- **Per-observation weights?** ✅ `$weights` (`rapm_matrix.R:554`, = `minutes/90`) is passed
  straight to `glmnet(weights=)` by both `fit_rapm` and `fit_rapm_with_prior`. Compose:
  `rd$weights <- rd$weights * exp(-age_days/halflife)`.
- **Prior plumbing?** ✅ `fit_rapm_with_prior(rd, offense_prior, defense_prior, ...)`
  (`rapm_model.R:262`) takes named (by player_id) prior vectors, shifts `y - X%*%prior`,
  fits weighted ridge on the residual. Use the skill-SPM's **`offense_spm_ratings$offense_spm`
  + `defense_spm_ratings$defense_spm`** (from `cache-skills/03_skill_spm.rds`) directly — do
  NOT split `spm/2`, they exist separately.
- **Cost?** ✅ NOT a blocker — `04_rapm` already fits the pooled ~all-season matrix locally
  (140MB output). The explorer's "260GB" estimate was a dense-matrix miscalc; glmnet's sparse
  path handles it (the GHA OOM is the unrelated step-01 *combine* stage, not the RAPM fit).

### Build steps
1. ✅ **Prototype** (`debug/proto_career_panna.R`) — VALIDATED 2026-06-09: top-20 sensible
   (Kimmich/Salah/Kane/Rodri/Haaland...); Chiesa stabilises (career 0.088 on 19,790 mins vs
   thin-season 0.094 on 637); fringe players shrink to ~0; fit ~70s. Career Panna is on a
   *more confident* (larger) scale than season xRAPM for sustained-elite players — expected,
   but a **calibration/labelling decision** (present as distinct metrics, or rescale).
2. ✅ **Package fn** `fit_career_rapm()` + ✅ `optimize_panna_decay()` (`R/career_rapm.R`) —
   pooled fit + per-observation recency decay (`0.5 ^ (age/halflife)`) + skill-SPM prior;
   exact match dates from **`opta_fixtures.parquet`**. **Half-life TUNED = 365 days** (held-out
   match prediction, 626,880 train / 37,086 holdout splints): 365d best (wMSE 4.20928),
   monotone "shorter is better" (1460d worst 4.21182), but spread ~0.06% — near-flat like
   EPR's decay tuning, so value is non-critical; 365d matches "best guess of next game". Baked
   in as the `halflife_days` default.
3. ⬜ **Pipeline step** in `player-ratings-opta/` (e.g. `08c_career_panna.R`) → export `panna`,
   `panna_offense`, `panna_defense` to `ratings-data` alongside the renamed season `xrapm`.
4. ⬜ **Naming migration** (see above) — `panna`→`xrapm`, add career `panna`, across all repos.
5. ⬜ **Blog** — surface both columns; **docs** — update DATA_DICTIONARY / ARCHITECTURE.

> NOTE devtools::document() must be run before commit (new roxygen in `R/career_rapm.R`).
