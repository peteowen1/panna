# Cross-League Opponent Adjustment

## Problem

`adjust_epv_for_opponents()` computes opponent strength profiles within a single league. Each league's teams are calibrated against their own domestic peers. There's no way to compare opponent difficulty across leagues — Man City's opponent profile is relative to EPL teams, PSV's is relative to Eredivisie teams.

This matters for:
- Blog data (10 leagues shown together — panna_value from EPL and Eredivisie aren't comparable)
- Match predictions (cross-league fixtures in UCL/UEL)
- Player comparison across leagues

## Proposed Approach: ELO on EPV Residuals

### Why ELO

- **Self-calibrating**: no manual league tier constants needed
- **Cross-league bridges exist**: UCL/UEL/UECL matches pit domestic teams against each other
- **Decay-weighted**: naturally handles team form changes
- **Already proven**: panna's opponent adjustment uses the same residual concept, just within-league

### How It Works

1. **Compute team EPV residuals for ALL matches** (domestic + European)
   - Same as current: `residual = team_credit - team_season_avg`
   - But pool across all competitions, not per-league

2. **Run ELO on match-level residuals**
   - Each match produces a "winner" (team with higher EPV residual) and "loser"
   - ELO update: `new_elo = old_elo + K * (actual - expected)`
   - `actual = 1` if team's residual > opponent's, `0.5` if similar, `0` if lower
   - `expected` from ELO formula: `1 / (1 + 10^((opp_elo - team_elo) / 400))`
   - K-factor: ~20 for domestic, ~30 for European (higher weight since cross-league signal is rarer and more informative)

3. **Use team ELO as opponent strength**
   - Replace current within-league rolling profile with global ELO
   - Adjustment = `-(opponent_elo - league_median_elo)` scaled to EPV units
   - Playing a team 200 ELO points above median → positive boost to your EPV credit

4. **European matches are the key**
   - Domestic matches calibrate within-league rankings (same as current)
   - UCL/UEL/UECL matches bridge leagues: when Napoli plays Bayern, the residual comparison directly informs the Italy-Germany ELO gap
   - ~800 European matches/season across UCL+UEL+UECL provide substantial cross-league signal

### Data Available

| Competition | Matches/season | Cross-league signal |
|-------------|---------------|---------------------|
| UCL | ~125 group + ~60 knockout | Strong — top teams from all leagues |
| UEL | ~200 | Medium — 2nd-tier teams |
| UECL | ~250 | Weaker — 3rd-tier, smaller leagues |
| Domestic | ~380 per league × 10 | None (within-league only) |

### Implementation Plan

#### New file: `R/cross_league_elo.R`

```r
# Core functions:
compute_cross_league_elo(all_match_residuals, k_domestic = 20, k_european = 30)
  # Input: data.table with match_id, team_id, opp_team_id, residual, competition_type, match_date
  # Output: data.table with team_id, elo (final), elo_history (per-match)

get_opponent_elo_adjustment(player_match, team_elos)
  # Input: player-match data + team ELOs
  # Output: same data with elo_opp_adjustment column
```

#### Changes to existing code

1. **`R/epv_adjustments.R`**: Add `adjust_epv_for_opponents_global()` that uses ELO instead of within-league profiles
2. **`10b_export_game_logs.R`**: Call global adjustment instead of (or alongside) per-league
3. **Training script**: Compute ELOs across all leagues before the per-league loop, pass to adjustment

#### Validation

- **Sanity check**: ELO rankings should roughly match UEFA coefficients
- **Predictive**: Does ELO-adjusted EPV predict next-season RAPM better than raw EPV?
- **Cross-league**: Do adjusted player ratings produce sensible cross-league comparisons? (e.g., top Eredivisie player shouldn't consistently outrank mid-table EPL players)

### Considerations

- **Cold start**: Teams new to European competition have no cross-league signal. Fall back to league-median ELO for unobserved teams.
- **Sample size**: European matches are ~10% of total. The ELO system needs enough European matches to converge. With 4 seasons of data, there are ~2,000 European matches — enough for stable estimates for teams that regularly compete in Europe.
- **League promotion/relegation**: Championship (ENG2) teams promoted to EPL get an ELO from their Championship domestic performance. The EPL ELO will adjust quickly once they play established EPL opponents.
- **Tournament format**: European group stages are good (6+ matches per team). Knockout rounds have small samples but high-quality opponents.

### Estimated Effort

- Core ELO computation: ~100 lines
- Integration with existing adjustment pipeline: ~50 lines
- Validation script: ~100 lines
- Testing: ~50 lines
- Total: ~1 session of focused work
