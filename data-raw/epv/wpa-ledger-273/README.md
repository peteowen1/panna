# panna#273 prototypes: WPA as a net ledger, and the WP model behind it

Working scripts from 2026-11-09/10, kept so the numbers in the issue can be rebuilt.
Not part of any pipeline. Outputs and models go to `$WPA273_DIR` (default
`data-raw/cache/wpa273`, gitignored). Run from `panna/` under PowerShell (arrow).

| script | what it does |
|---|---|
| `wp_build_feats.R` | WP training features per EPL season into `feats/` |
| `wp_train_arms.R`, `wp_train_d2.R` | Fix 1 arms: per-match team weights; depth 2 (rejected) |
| `wp_threat_check.R` | shows the `epv` input over-weighted 3-12x inside its real range |
| `wp_structural.R` | possession-frame structural model (first try) |
| `wp_structural_home.R` | home-frame structural model: H(home margin, time, red, half, ET), threat as a next-goal mixture. The candidate |
| `wpa_ledger.R`, `wpa_ledger_struct.R` | Liverpool 2026-27 through the ledger, published vs structural WP |
| `xgdiff_size.R` | the `xg_diff = 0` serve-time skew in 10b, sized |
| `walk_rows.R` | example action rows for the walk-through in `docs/plans/WPA-LEDGER-273.md` (pannaverse) |

The package function is `wpa_build_ledger()` (`R/wpa_ledger.R`); these scripts predate it
and call the net-goals engine directly.
