# Opta RAPM/SPM Pipeline — intermittent runner-shutdown failures

**Status:** OPEN — diagnosis narrowed, root cause not yet confirmed.
**Last updated:** 2026-05-20.
**Workflow:** `.github/workflows/opta-pipeline.yml` ("Opta RAPM/SPM Pipeline").

---

## TL;DR

The pipeline fails ~50% of runs. It always dies the same way: `##[error]The runner
has received a shutdown signal` partway through **step 07** (`07_seasonal_ratings.R`),
on the **last and largest season (2024)** of the per-season RAPM/xRAPM loop.

The leading hypothesis is **memory exhaustion (OOM)**, but it is **not confirmed** —
and the reason it isn't confirmed is the real blocker:

> A previous session added `[mem]` probes to `07_seasonal_ratings.R` to measure this.
> Those probes are on **`dev`**. The workflow triggers on `repository_dispatch`, which
> checks out the **default branch (`main`)**. **The instrumentation has never run.**

**Next action:** get instrumentation onto `main`, let one run execute, read the
memory trace, then fix.

---

## Symptom

- Failure signature in the log:
  ```
  ##[error]The runner has received a shutdown signal. This can happen when the
  runner service is stopped, or a manually started runner is canceled.
  ##[error]The operation was canceled.
  ```
- Job conclusion `failure`; the "Run Opta RAPM/SPM pipeline" step shows `cancelled`.
  No R error, no stack trace — the runner agent itself died.
- Recent run history (≈50% failure): ❌05-14 ✅05-15 ✅05-16 ❌05-17 ✅05-18 ❌05-19 ✅05-20.
- Successful runs take ~72 min; failures die at ~70 min. Job `timeout-minutes` is 120 —
  **not** a timeout.

## Where it dies

Step 07, `data-raw/player-ratings-opta/07_seasonal_ratings.R` — the per-season loop
`lapply(seasons, fit_season_ratings_opta)` (line ~354), processing seasons 2014→2024
in order (~30 s each).

It dies during the **last, largest season**. From the 2026-05-19 failure log
(run `26086274084`):
```
09:51:11  Fitting xRAPM: 63552 observations, 11041 columns   (season 2024)
09:51:59  ##[error]The runner has received a shutdown signal  (killed 48s in)
```
The big *global* fit earlier (`668k × 38k`, steps 04/06) succeeds fine — it is
specifically the last per-season fit that tips over.

## Ruled out

| Hypothesis | Verdict | Evidence |
|---|---|---|
| Dense design matrix (5.6 GB) | **No** — `X_full` is sparse | `rapm_matrix.R:712` does `cbind(X_players, X_covariates)` where `X_covariates` is dense, but `cbind(dgCMatrix, dense)` → `dgCMatrix` in Matrix 1.7.4 (verified). `X_players` built via `Matrix::sparseMatrix` (`rapm_matrix.R:419`). |
| Job timeout | **No** | `timeout-minutes: 120`; failures die at ~70 min. |
| Concurrency cancellation | **No** | No `concurrency:` block in `opta-pipeline.yml`. |
| Billing / minutes limit | **No** | panna is a **public** repo → unlimited free GHA minutes. |
| One-off infra reclaim | **No** | ~50% recurrence is far too frequent for random reclaim. |

## Runner

`runs-on: ubuntu-latest`. panna is **public**, so this is GitHub's larger free
public-repo runner: **4-core / 16 GB RAM**. (In-code comments in
`07_seasonal_ratings.R` referring to a "7 GB" ceiling are **stale/incorrect** — they
predate the public-repo runner upgrade. Treat the ceiling as ~16 GB.)

## Not yet confirmed

Whether this is OOM. The circumstantial case is strong (dies in the largest fit, last
in a loop, ~50%, "shutdown signal" with no R error = runner agent killed). But static
reading found **no unbounded memory leak**:

- Per-iteration objects (`rapm_data`, `xrapm_model`) are function-local to
  `fit_season_ratings_opta` and bounded (~2 iterations' worth at most).
- `splint_data` is a large but **fixed** baseline held for the whole of step 07
  (its `players` table is ~14M+ player-splint rows).

Confirming OOM needs **runtime memory numbers**, which brings us to the blocker.

## The blocker — instrumentation stranded on `dev`

`07_seasonal_ratings.R` already contains, on `dev`:
- `[mem]` probes — `cat("[mem] ...")` at lines ~289-290 and ~331-333.
- Mitigations from a prior session — `fit_rapm(..., parallel = FALSE)` (line ~278)
  and `rm(rapm_model); gc()` before the xRAPM fit (line ~288).

**None of this has ever executed.** `opta-pipeline.yml` runs on `repository_dispatch`,
which checks out `main`. Grepping every recent run log for `[mem]` returns nothing —
confirming the production runs are on a `main` that lacks these changes.

## Instrumentation added this session (UNCOMMITTED)

In `.github/workflows/opta-pipeline.yml`, the "Run Opta RAPM/SPM pipeline" step now
has a background system-RAM sampler that echoes `[MEM]` lines (`free -m`) to the step
log every 20 s. It survives a runner kill (the log is preserved; an uploaded artifact
would not be) and captures *system-wide* RAM (the R `[mem]` probes only count R's own
heap and only print twice per season).

> **This edit is uncommitted in the `dev` working tree** and, like the `[mem]` probes,
> only takes effect once it reaches `main`.

## Next steps

1. **Deploy instrumentation to `main`.** Both the existing `[mem]` probes in
   `07_seasonal_ratings.R` and the new `opta-pipeline.yml` `[MEM]` tracer. Either
   cherry-pick them into a focused `dev → main` PR, or merge `dev → main` wholesale
   (note: `dev` also carries other in-progress work and uncommitted changes —
   `NAMESPACE`, `R/constants.R`, `R/player_ratings_epv.R`, etc.).
2. **Let one daily run execute on `main`** and read the log:
   - `[MEM]` lines — does system `avail` RAM approach 0 near the moment of death?
   - `[mem]` lines — does R's heap climb across the 2014→2024 season loop?
3. **Then fix** — if confirmed OOM, see candidates below; if not, the trace shows
   where to look instead.

## Candidate fixes (hypotheses — pending the memory trace)

- In `fit_season_ratings_opta()`: `rm(xrapm_model, rapm_data)` before the function
  returns. Currently neither is ever `rm()`'d; the `gc()` at line ~331 runs while both
  are still in scope, so it cannot reclaim them. *(Likely a modest win, not the whole
  fix — bounded at ~2 iterations.)*
- Force `gc()` at the **top of each `lapply` iteration** so the previous season's
  garbage is reclaimed before the next season allocates.
- If the fixed baseline is the bulk: step 07 holds the full `splint_data` for the
  whole loop. Consider running step 07 as its **own `Rscript` process** so it starts
  with a clean heap instead of inheriting steps 01-06's memory.
- Stopgap: a larger runner — but a public-repo `ubuntu-latest` is already 16 GB;
  going bigger needs a paid larger-runner configuration.

## Evidence / reference

- Failure runs: `26086274084` (05-19), `25985065357` (05-17). Success: `26151806283`
  (05-20), `26023924894` (05-18). View with
  `gh run view <id> --repo peteowen1/panna --log` / `--log-failed`.
- Key files: `.github/workflows/opta-pipeline.yml` (the workflow),
  `data-raw/player-ratings-opta/07_seasonal_ratings.R` (where it dies),
  `R/rapm_model.R` (`fit_rapm` / `fit_rapm_with_prior`),
  `R/rapm_matrix.R` (`create_rapm_design_matrix` / `prepare_rapm_data`).
- Per-season xRAPM fit sizes (05-19 run), 2014→2024:
  `30312×5430 → 37860×7499 → 54494×9765 → 51408×9095 → 39200×7284 → 48254×8799 →
  48980×9230 → 55906×10284 → 57084×10201 → 61632×10742 → 63552×11041` (died here).
