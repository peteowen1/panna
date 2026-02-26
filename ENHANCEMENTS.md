# EPV Enhancement Ideas

This file tracks potential improvements to the EPV (Expected Possession
Value) model that require further research or significant implementation
effort.

------------------------------------------------------------------------

## Opta Team-Relative Coordinates (RESOLVED)

**Status**: Fixed in
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md) -
default `normalize_direction = FALSE` **Priority**: Critical (was
breaking coordinate continuity) **Resolved**: 2026-02

### Problem

Investigation revealed Opta uses **team-relative coordinates**: - Each
team’s events are from THEIR OWN perspective - x=0 is the team’s own
goal (defensive end) - x=100 is the opponent’s goal (attacking end) - Y
coordinates are also team-relative (y=0 is one touchline from that
team’s view)

This means a shot at (88, 30) by Team A and a save at (10.9, 70.6) by
Team B are the **same physical location** from opposite perspectives
(88+10.9≈100, 30+70.6≈100).

The original normalization logic tried to flip coordinates based on
mean_x per team, but this doesn’t work with team-relative data since
both teams naturally see themselves attacking toward x=100.

### Solution

Changed `normalize_direction = FALSE` as the default in
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md).

**Why this is correct for EPV:** - EPV models evaluate each action from
the ball-carrier’s perspective - The ball-carrier always attacks toward
x=100 in their native coordinates - At possession changes, the
coordinate frame naturally changes (different perspective) - This
discontinuity at possession changes is expected and correctly reflects
the game state change

**Validation results with the fix:** - Shots correctly at x=84 (mean)
near attacking goal ✓ - Saves correctly at x=9 (mean) near defending
goal ✓ - Same-team pass discontinuity: median 2.28 (low, as expected)
✓ - Different-team discontinuity: higher (expected, perspective change)
✓

------------------------------------------------------------------------

## Opta end_x/end_y Data Issue (RESOLVED)

**Status**: Fixed in
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md)
**Priority**: Critical (was breaking EPV chain continuity) **Resolved**:
2026-02

### Problem

Analysis revealed that most Opta action types have `end_x=0, end_y=0` -
only passes and clearances have proper end coordinates. This breaks EPV
chain continuity where `end_x` of action N should match `start_x` of
action N+1.

**Findings from `debug/analyze_spadl_continuity.R`:**

| Action Type        | end_x=0 % | Mean Gap | Notes                       |
|--------------------|-----------|----------|-----------------------------|
| pass (1)           | 0%        | 2.79     | ✓ Proper coordinates        |
| clearance (12)     | 14%       | 1.78     | ✓ Mostly proper coordinates |
| tackle (7)         | 100%      | 39.05    | Fixed: set end=start        |
| interception (8)   | 100%      | 40.44    | Fixed: set end=start        |
| ball_recovery (49) | 100%      | 38.84    | Fixed: set end=start        |
| take_on (3)        | 100%      | 64.58    | Fixed: set end=start        |
| ball_touch (61)    | 100%      | 52.22    | Fixed: set end=start        |
| aerial (44)        | 100%      | 51.53    | Filtered (contested)        |
| dispossessed (50)  | 100%      | 53.46    | Fixed: set end=start        |
| keeper actions     | 100%      | varies   | Fixed: set end=start        |

### Solution Implemented

In
[`convert_opta_to_spadl()`](https://peteowen1.github.io/panna/reference/convert_opta_to_spadl.md):

1.  **Stationary actions** (ball stays with player): Set
    `end_x = start_x, end_y = start_y`
    - tackle, interception, ball_recovery, ball_touch, take_on, foul,
      dispossessed
    - keeper_save, keeper_pick_up, keeper_claim, keeper_punch
    - **aerial** (added to stationary list - duel winner keeps ball at
      their position)
    - clearance (fallback for rare data quality issues)
2.  **Duel row merging**: Opta records both participants of a duel as
    separate rows
    - [`merge_duel_rows()`](https://peteowen1.github.io/panna/reference/merge_duel_rows.md)
      combines these, keeping the winner’s row
    - Adds opponent info (opponent_player_id, opponent_player_name)
    - This avoids double-counting duel events
3.  **Actions with proper coordinates**: Keep as-is
    - pass (type_id=1)
    - clearance (type_id=12) - usually has end coords

This ensures EPV chain continuity while preserving all meaningful
actions including aerial duels.

------------------------------------------------------------------------

## Aerial Duels Credit Assignment

**Status**: Included (aerials treated as stationary actions with duel
merging) **Priority**: Low (basic handling works) **Complexity**: Medium
(for advanced credit assignment)

### Current Implementation

Aerials are now handled like other stationary actions: 1. `end_x = 0`
issue fixed by setting `end = start` (winner keeps ball at their
position) 2. Duel rows merged via
[`merge_duel_rows()`](https://peteowen1.github.io/panna/reference/merge_duel_rows.md) -
keeps winner’s row, removes loser’s 3. Winner gets EPV credit for what
happens next; loser’s row is merged out

This is a reasonable approximation where the aerial winner gets credit
proportional to the value they create with their subsequent action.

### Potential Enhancement

Aerials represent a unique opportunity for proper **zero-sum credit
assignment**: 1. Both participants are recorded (winner and loser
identified) 2. We could calculate a “contested baseline EPV” at that
location 3. Winner credit: `(outcome_value - contested_baseline)` 4.
Loser debit: `-(outcome_value - contested_baseline)`

This would be one of the few cases where we properly assign **negative
credit** to the losing player. Currently the loser gets no credit (their
row is merged out) rather than negative credit.

### Research Needed for Enhancement

- Build model for “contested aerial baseline EPV” at different pitch
  locations
- Validate that winner/loser credit sums to zero (zero-sum game)
- Test whether explicit loser debit improves player ratings

### Related Files

- `R/spadl_conversion.R`: Handles aerials as stationary actions, merges
  duel rows

------------------------------------------------------------------------

## Additional Enhancement Ideas

### Set Piece Valuation

Set pieces (corners, free kicks) have different EPV dynamics than open
play. Consider: - Separate models for set piece situations - Credit
assignment to set piece taker vs finisher

### Pressing/Counterpressing Value

Currently defensive actions get a flat 1.5x boost. Could model: - Time
since possession loss (counterpress window) - Position-dependent
defensive value - Chain of defensive actions (pressure sequences)

### xG Integration in EPV

The EPV model uses multinomial goal prediction. Could integrate: -
Shot-level xG as terminal values (partially done for failed shots) -
xG-based features for pre-shot actions

------------------------------------------------------------------------

## Code Organization: Large File Splits

**Status**: Recommended for v0.2.0 **Priority**: Medium
(maintainability) **Complexity**: Medium (requires careful testing)

### Current Large Files

| File                    | Lines | Problem                                                        |
|-------------------------|-------|----------------------------------------------------------------|
| `scrape_fbref_direct.R` | 2028  | Multiple concerns: HTTP, parsing, batching, data loading       |
| `spm_model.R`           | 1690  | FBref + Opta SPM combined, internal helpers mixed with exports |

### Proposed Split: scrape_fbref_direct.R → 4 files

**1. `scrape_fbref_http.R` (~150 lines)** -
[`get_fbref_headers()`](https://peteowen1.github.io/panna/reference/get_fbref_headers.md) -
Browser headers -
[`add_delay_jitter()`](https://peteowen1.github.io/panna/reference/add_delay_jitter.md) -
Request timing -
[`get_fbref_session()`](https://peteowen1.github.io/panna/reference/get_fbref_session.md) -
Session management -
[`reset_fbref_session()`](https://peteowen1.github.io/panna/reference/reset_fbref_session.md) -
Session reset - `check_fbref_status()` - Connection check - `.fbref_env`
environment

**2. `scrape_fbref_pages.R` (~500 lines)** - `fetch_fbref_page()` - Core
page fetching -
[`fetch_with_retry()`](https://peteowen1.github.io/panna/reference/fetch_with_retry.md) -
Retry logic - `parse_match_tables()` - Table extraction -
[`extract_match_events()`](https://peteowen1.github.io/panna/reference/extract_match_events.md) -
Event parsing - `get_cached_path()` - Cache path utilities -
[`list_cached_matches()`](https://peteowen1.github.io/panna/reference/list_cached_matches.md) -
Cache listing

**3. `scrape_fbref_batch.R` (~600 lines)** -
[`scrape_fixtures()`](https://peteowen1.github.io/panna/reference/scrape_fixtures.md) -
Fixture scraping -
[`scrape_fbref_matches()`](https://peteowen1.github.io/panna/reference/scrape_fbref_matches.md) -
Main batch scraper -
[`aggregate_cached_matches()`](https://peteowen1.github.io/panna/reference/aggregate_cached_matches.md) -
RDS aggregation - `rescrape_matches()` - Force rescrape

**4. `scrape_fbref_parquet.R` (~450 lines)** -
[`build_parquet()`](https://peteowen1.github.io/panna/reference/build_parquet.md) -
Single parquet build -
[`build_consolidated_parquet()`](https://peteowen1.github.io/panna/reference/build_consolidated_parquet.md) -
Multi-league consolidation -
[`get_parquet_path()`](https://peteowen1.github.io/panna/reference/get_parquet_path.md) -
Path utilities -
[`migrate_metadata_tables_available()`](https://peteowen1.github.io/panna/reference/migrate_metadata_tables_available.md) -
Migration utilities

**Keep in `data_loaders.R`** (already exists): -
[`load_summary()`](https://peteowen1.github.io/panna/reference/load_summary.md),
[`load_passing()`](https://peteowen1.github.io/panna/reference/load_passing.md),
etc. could be moved there - Currently uses DuckDB for remote parquet
loading

### Proposed Split: spm_model.R → 2 files

**1. `spm_model.R` (~700 lines) - FBref SPM** - Internal helpers:
[`.aggregate_stat_table()`](https://peteowen1.github.io/panna/reference/dot-aggregate_stat_table.md),
`.get_*_col_mapping()` -
[`aggregate_player_stats()`](https://peteowen1.github.io/panna/reference/aggregate_player_stats.md) -
FBref aggregation -
[`fit_spm_model()`](https://peteowen1.github.io/panna/reference/fit_spm_model.md) -
Model fitting -
[`calculate_spm_ratings()`](https://peteowen1.github.io/panna/reference/calculate_spm_ratings.md) -
Rating extraction - `extract_spm_ratings()`,
[`compare_spm_features()`](https://peteowen1.github.io/panna/reference/compare_spm_features.md)

**2. `spm_opta.R` (~500 lines) - Opta SPM** -
[`aggregate_opta_stats()`](https://peteowen1.github.io/panna/reference/aggregate_opta_stats.md) -
Opta aggregation (263 columns) -
[`fit_spm_opta()`](https://peteowen1.github.io/panna/reference/fit_spm_opta.md) -
Opta-specific model -
[`compare_spm_features()`](https://peteowen1.github.io/panna/reference/compare_spm_features.md) -
Feature comparison - Opta-specific column mappings and helpers

### Implementation Notes

1.  **Source order matters**: R loads files alphabetically, but
    `@importFrom` handles dependencies
2.  **Internal functions**: Use `@keywords internal` for helpers that
    stay in same file
3.  **Namespace**: Run `devtools::document()` after split to regenerate
    NAMESPACE
4.  **Testing**: Run `devtools::check()` and full test suite after split
5.  **Git history**: Consider `git mv` for better history tracking

### Benefits

- Faster code navigation (smaller files)
- Clearer responsibility boundaries
- Easier testing of individual modules
- Better parallelization of code review
