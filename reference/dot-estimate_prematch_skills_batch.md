# Estimate pre-match skills at multiple dates (incremental batch version)

Highly optimized for sequential date estimation. Instead of
re-processing all historical data at each date, maintains running
cumulative sums that are decayed forward and incrementally updated with
new observations. Uses [`rowsum()`](https://rdrr.io/r/base/rowsum.html)
(C-level) for grouped matrix sums.

## Usage

``` r
.estimate_prematch_skills_batch(
  match_stats,
  ref_dates,
  decay_params = NULL,
  min_weighted_90s = 3,
  keep_players = NULL,
  output_min_w90 = 0,
  stream_dir = NULL,
  source_fingerprint = NULL,
  verbose = TRUE
)
```

## Arguments

- match_stats:

  Match-level stats (output of `compute_match_level_opta_stats`).

- ref_dates:

  Character or Date vector of dates to estimate skills at.

- decay_params:

  Decay parameters (default:
  [`get_default_decay_params()`](https://peteowen1.github.io/panna/reference/get_default_decay_params.md)).

- min_weighted_90s:

  The estimator's REGRESSION threshold, not an inclusion gate (default
  3). Shrinkage toward the prior is what handles thin samples – see
  `estimate_player_skills` and the note at
  `data-raw/estimated-skills/02_estimate_skills.R:23`. Do NOT use this
  to trim output rows; that is `output_min_w90`.

- keep_players:

  Optional data.frame/data.table whose first two columns are `date` and
  `player_id`, restricting each date's OUTPUT to the players a caller
  actually needs. The running sums still cover every player (they must,
  for the decay recurrence to stay correct) — only the returned snapshot
  is narrowed, before the table is built. Without it an as-of run over N
  dates returns N x 46,044 rows. Note chunking the call is NOT an
  alternative: this function deep-copies `match_stats` on entry, so
  chunking pays that copy once per chunk instead of once in total.

- output_min_w90:

  Drop rows from each per-date snapshot whose `weighted_90s` is below
  this (default 0 = keep every row). This is a pure memory lever for
  callers that only need active players at each date. **Leave it at 0
  for anything that trains a model.** Setting it to 3 for all callers on
  2026-09-01 (456d8329) dropped `07_train_psr_model.R`'s skill coverage
  from 100.0% to 84.7%: the trimmed players still have player-match
  rows, so they matched no skill row, and step 07 imputes missing skills
  to 0 – which attenuates every coefficient. That commit's claim that
  "every consumer joins on (player_id, exact date), so the dropped rows
  were never read" was wrong; step 07 reads exactly those rows. Its
  coverage guard caught it.

- stream_dir:

  If supplied, write each date's full (uncoverage-filtered) snapshot to
  `file.path(stream_dir, "<date>.rds")` as soon as it's computed,
  freeing it from R's memory immediately, instead of accumulating all
  dates in a single in-memory list (default `NULL` = old in-memory
  behaviour, unchanged). This is a MEMORY lever, orthogonal to
  `output_min_w90` – it changes WHERE full-coverage results live, not
  WHICH rows exist, so it carries none of `output_min_w90`'s coverage
  risk. Added 2026-09-04: at full multi-season history (~684 weekly
  dates x ~47k players x ~142 stats), the in-memory list alone peaks at
  70GB+, which does not reliably fit alongside other load on a shared
  machine (two observed OOM near-misses the same night). `stream_dir`
  bounds peak memory to roughly one date's snapshot regardless of
  history length. The directory is NOT cleaned up by this function on
  success – the caller owns that (orphaned chunks from a previous run
  ARE cleared at entry, see below).

  **Do not point two concurrent runs at the same `stream_dir`.** For
  resume to work the caller must pass a STABLE path (a
  [`tempfile()`](https://rdrr.io/r/base/tempfile.html) is unique per R
  session and would never be found again), which means two overlapping
  invocations would share one checkpoint and clobber each other's state.
  `07_train_psr_model.R`'s outfield and GK paths deliberately use two
  different directories for this reason; an accidental double-dispatch
  of the whole script is the case to avoid.

- source_fingerprint:

  Optional caller-supplied value folded into the checkpoint fingerprint
  – e.g.
  [`file.mtime()`](https://rdrr.io/r/base/file.info.html)/[`file.size()`](https://rdrr.io/r/base/file.info.html)
  of the file `match_stats` was read from. The count-based fingerprint
  cannot see a data change that preserves n_rows/n_players/n_dates and
  the date sum; passing this closes most of that gap for a few bytes.
  Optional because not every caller has a single source file.

- verbose:

  Print progress (default TRUE).

## Value

Named list keyed by date string. Each element is a data.table (one row
per player with skill columns) when `stream_dir` is `NULL`, or the file
path it was streamed to (a length-1 character string) when `stream_dir`
is supplied – use
[`.read_skill_chunk()`](https://peteowen1.github.io/panna/reference/dot-read_skill_chunk.md)
to transparently handle either case.

## Details

Complexity: O(N + D \* new_rows_per_date) instead of O(N \* D). For
typical data (~1M rows, 659 dates), this is ~100-300x faster.
