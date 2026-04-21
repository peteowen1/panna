# panna_value Ranking Shift — Raw vs Adjusted EPV

Generated: 2026-04-21

After switching `panna_value` from `0.5 * epv_total + 0.5 * psv` (raw)
to `0.5 * epv_total_adj + 0.5 * psv` (position-centered + opponent-adjusted),
this analysis compares season-level leaderboards under both formulas.

## Headline numbers

- **Qualified player-seasons** (≥900 min): **33,332** across 11 seasons, 13 leagues
- **Pearson correlation** (old vs new season totals): **0.886**
- **Spearman correlation**: **0.815**
- **Top 10 stability per season**: **8-9 out of 10 players overlap**
- **Mean absolute rank shift** by position:

  | Position | Mean abs shift | n | Mean old total | Mean new total |
  |---|---|---|---|---|
  | GK  | **902** | 2,605  | 2.5 | **−3.1** |
  | DEF | 398     | 11,437 | 2.8 | −0.5 |
  | OTHER | 373   | 4,058  | 3.5 | +0.7 |
  | MID | 310     | 11,132 | 4.7 | +0.8 |
  | FWD | 242     | 4,100  | 6.1 | +1.9 |

## Interpretation — not a regression, by design

The shift is **significant but intended**, not a bug:

### 1. Top fallers are all goalkeepers

Top 15 biggest-faller list is *entirely* GKs:

| player | season | rank shift | old | new |
|---|---|---|---|---|
| F. Muslera | 2023-24 | −2621 | 6.51 | −3.91 |
| K. Roos | 2023-24 | −2404 | 5.41 | −3.88 |
| Anthony Lopes | 2016-17 | −2385 | 7.63 | −2.90 |
| M. Günok | 2019-20 | −2382 | 6.35 | −2.73 |
| Matheus | 2023-24 | −2312 | 5.02 | −4.02 |
| David Raya | 2024-25 | −2302 | 4.88 | −5.17 |
| J. Pickford | 2023-24 | −2277 | 4.99 | −3.76 |

Under raw EPV, keepers accumulated positive credit for routine ball-handling (pickups, claims, punches). Position centering subtracts the GK baseline, so keepers collapse to around 0 or negative — which is exactly what "value added *relative to positional peers*" should produce. A starting GK is good only by how much they exceed an average GK, not by how much they touch the ball.

### 2. Top climbers are undervalued defenders

Low-minute centre-backs and defensive utility players jump ~1200 places:
- J. Rankin-Costello, E. Velázquez, D. O'Shea, E. Pinnock, Tafazolli…

Raw EPV under-rewards defenders because most tackles/interceptions happen in lower-EPV-delta zones. Position centering corrects this imbalance — a CB should be compared to other CBs, not strikers.

### 3. Position shift magnitudes align with the intended effect

- **GK 902-place mean shift** — massive, full positional baseline subtracted.
- **FWD 242** — smallest, strikers were already tracked reasonably by raw EPV.
- The progression GK > DEF > MID > FWD mirrors how far each position sits from the outfield-average EPV trajectory.

### 4. Headline leaders stable (top-10 overlap per season)

| Season | Top-10 overlap |
|---|---|
| 2015-16 | 8/10 |
| 2016-17 | 9/10 |
| 2017-18 | 9/10 |
| 2018-19 | 8/10 |
| 2019-20 | 8/10 |
| 2020-21 | 9/10 |
| 2021-22 | 9/10 |
| 2022-23 | 8/10 |
| 2023-24 | 9/10 |
| 2024-25 | 8/10 |
| 2025-26 | 9/10 |

Salah, KDB, Messi, Mbappé, Haaland, Kane, Lewandowski remain at the top under both formulas — the adjustment doesn't disturb who the best attackers are.

## Action items surfaced

1. **Blog UX**: under adjusted convention, GKs will show **negative** `panna_value` on per-match pages. Correct mathematically (comparing to position peers) but may surprise users. Options:
   - Display a position badge with "compared to peers at this position"
   - Add a secondary leaderboard using raw `epv_total` for cross-position comparison (e.g. "Most ball-touches this match")

2. **Position-stratified percentiles** — already flagged in memory as tech debt. More important now: the adjusted formula only makes sense within position. `panna_rank_position` / `panna_percentile_position` columns (added in `pannadata/scripts/build_blog_data.R`) should be the default leaderboard sort on the blog.

3. **Consider keeping both variants on the blog** — `panna_value_raw` vs `panna_value_adj` as two columns, so users can pick the comparison frame they want.

## Files

- Analysis script: `panna/data-raw/analysis/panna_value_ranking_shift.R`
- Underlying data: `data-raw/cache-predictions-opta/game_logs_*.parquet` (11 seasons)
