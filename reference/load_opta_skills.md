# Load pre-computed Opta skill estimates

Downloads and queries `opta_skills.parquet` from the `opta-latest`
GitHub release. This file contains Bayesian decay-weighted skill
estimates produced by the estimated skills pipeline (~15K
player-seasons, ~2-3 MB).

## Usage

``` r
load_opta_skills(
  season = NULL,
  columns = NULL,
  source = c("remote", "local"),
  repo = "peteowen1/pannadata",
  tag = "opta-latest"
)
```

## Arguments

- season:

  Optional season filter as end year (e.g., `2025` for 2024-2025
  season).

- columns:

  Optional character vector of columns to select.

- source:

  Data source: `"remote"` (default, downloads from GitHub) or `"local"`
  (reads from
  [`opta_data_dir()`](https://peteowen1.github.io/panna/reference/opta_data_dir.md)).

- repo:

  GitHub repository (default: "peteowen1/pannadata").

- tag:

  Release tag (default: "opta-latest").

## Value

Data frame with one row per player-season containing skill estimates,
player metadata (`player_id`, `player_name`, `primary_position`), and
context columns (`season_end_year`, `weighted_90s`, `total_minutes`).

## Examples

``` r
if (FALSE) { # \dontrun{
# Load all skills
skills <- load_opta_skills()

# Load specific season
skills_2025 <- load_opta_skills(season = 2025)

# Use with player_skill_profile
player_skill_profile("H. Kane", skills = skills)
} # }
```
