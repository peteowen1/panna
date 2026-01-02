# Panna Data Issues

Tracking known data quality issues and their status.

---

## Active Issues

### 1. La Liga - No xG/Shooting Data
**Status:** BLOCKING
**Severity:** Critical
**Discovered:** 2024-12-30

**Problem:**
La Liga has 0% shooting/xG data across all seasons (2019-2025). The `load_fb_match_shooting()` function from worldfootballR returns no La Liga data in the pre-scraped dataset. Direct scraping via `fb_match_shooting()` returns HTTP 403 Forbidden.

**Impact:**
- 2,660 matches affected (100% of La Liga)
- All La Liga splints have `npxg_home = 0` and `npxg_away = 0`
- RAPM model gave La Liga coefficient of -0.66 (massive artifact)

**Root Cause:**
FBref likely doesn't have xG data for La Liga due to Opta licensing restrictions.

**Potential Solutions:**
1. Exclude La Liga from analysis (recommended for now)
2. Source La Liga xG from Understat (different scraping required)
3. Use goals instead of xG for La Liga only (inconsistent methodology)

---

### 2. 2025 Season - Incomplete Data
**Status:** BLOCKING
**Severity:** High
**Discovered:** 2024-12-30

**Problem:**
2025 season has 25-42% zero xG splints across all leagues due to incomplete/ongoing season.

| League | % Zero xG |
|--------|-----------|
| Bundesliga | 41.6% |
| Ligue 1 | 41.6% |
| Premier League | 25.2% |
| Serie A | 40.3% |

**Impact:**
- Season 2025 coefficient was -0.44 in RAPM (artifact of missing data)
- Partial season data skews player ratings

**Potential Solutions:**
1. Exclude 2025 season from analysis (recommended)
2. Wait until season completes and re-scrape
3. Use partial data with appropriate weighting

---

### 3. Ligue 1 2020 - COVID Season Gap
**Status:** Minor
**Severity:** Low
**Discovered:** 2024-12-30

**Problem:**
Ligue 1 2019-20 season (season_end_year = 2020) has 26.6% zero xG splints. This was the COVID-shortened season where the league was cancelled early.

**Impact:**
- 202 of 760 splints have zero xG
- May slightly skew Ligue 1 2020 data

**Potential Solutions:**
1. Keep as-is (ridge regression handles some noise)
2. Exclude Ligue 1 2020 specifically
3. Down-weight 2020 season observations

---

## Resolved Issues

*None yet*

---

## Data Quality Summary

| League | Seasons | Shooting Data | Status |
|--------|---------|---------------|--------|
| Bundesliga | 2019-2024 | Complete | OK |
| Bundesliga | 2025 | 41.6% missing | Exclude |
| La Liga | 2019-2025 | 0% available | Exclude All |
| Ligue 1 | 2019, 2021-2024 | Complete | OK |
| Ligue 1 | 2020 | 26.6% missing | Minor issue |
| Ligue 1 | 2025 | 41.6% missing | Exclude |
| Premier League | 2019-2024 | Complete | OK |
| Premier League | 2025 | 25.2% missing | Exclude |
| Serie A | 2019-2024 | Complete | OK |
| Serie A | 2025 | 40.3% missing | Exclude |

---

## Recommended Data Filter

For RAPM analysis, exclude:
- `league == "La Liga"` (no xG data)
- `season_end_year == 2025` (incomplete season)

This leaves 4 leagues x 6 seasons = 24 league-seasons of clean data.
