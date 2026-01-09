# Panna Data Issues

Tracking known data quality issues and their status.

---

## Active Issues

### 1. 2025-2026 Season - Incomplete Data
**Status:** EXPECTED
**Severity:** Low
**Updated:** 2026-01-03

**Problem:**
2025-2026 season is currently in progress. Partial data available:

| League | Matches Available | Expected |
|--------|-------------------|----------|
| Premier League | 180 | 380 |
| La Liga | 171 | 380 |
| Serie A | ~170 | 380 |
| Bundesliga | ~150 | 306 |
| Ligue 1 | ~150 | 306 |

**Impact:**
- Partial season data may skew player ratings if included
- Current season players have fewer observations

**Recommendation:**
Exclude `season == "2025-2026"` from RAPM analysis until season completes.

---

### 2. Ligue 1 2020 - COVID Season Gap
**Status:** Minor
**Severity:** Low
**Discovered:** 2024-12-30

**Problem:**
Ligue 1 2019-20 season has only 279 matches (vs 380 expected). This was the COVID-shortened season where the league was cancelled after matchday 28.

**Impact:**
- ~100 fewer matches than normal season
- May slightly underweight Ligue 1 2020 players

**Recommendation:**
Keep as-is. Ridge regression handles the reduced sample size appropriately.

---

## Resolved Issues

### La Liga - No xG/Shooting Data
**Status:** RESOLVED
**Resolved:** 2026-01-03
**Original Issue:** La Liga had 0% shooting/xG data across all seasons.

**Resolution:**
pannadata now contains complete La Liga shooting data from 2017-2018 onwards.
All seasons 2017-2025 have full match coverage (380 matches per season).

---

### 2024-2025 Season - Incomplete Data
**Status:** RESOLVED
**Resolved:** 2026-01-03
**Original Issue:** Season was incomplete with 25-42% missing xG data.

**Resolution:**
2024-2025 season is now complete with full data for all leagues:
- Premier League: 380 matches
- La Liga: 380 matches
- Serie A: 380 matches
- Bundesliga: 306 matches
- Ligue 1: 306 matches

---

## Data Quality Summary

| League | Seasons | Shooting Data | Status |
|--------|---------|---------------|--------|
| Premier League | 2017-2025 | Complete | OK |
| La Liga | 2017-2025 | Complete | OK |
| Serie A | 2017-2025 | Complete | OK |
| Bundesliga | 2017-2025 | Complete | OK |
| Ligue 1 | 2017-2019, 2021-2025 | Complete | OK |
| Ligue 1 | 2020 | 279/380 matches | Minor (COVID) |
| All leagues | 2025-2026 | Partial | Exclude (ongoing) |

---

## Recommended Data Filter

For RAPM analysis, exclude:
- `season == "2025-2026"` (incomplete current season)

This leaves 5 leagues x 8 seasons = 40 league-seasons of clean data (with minor Ligue 1 2020 caveat).
