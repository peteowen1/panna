# Global variable declarations to avoid R CMD check NOTEs
# These are column names used in NSE (non-standard evaluation) operations

#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' @importFrom stats setNames na.pass
NULL

# Suppress R CMD check notes for data.frame column names used in dplyr/tidyr
# Add column names as they are used in package functions

utils::globalVariables(c(
  # All column names are snake_case (via janitor::clean_names)

  # Match data columns
  "match_id",
  "match_url",
  "season",
  "season_end_year",
  "league",
  "date",
  "match_date",
  "home_team",
  "away_team",
  "home_goals",
  "away_goals",
  "home_score",
  "away_score",
  "home_xg",
  "away_xg",
  "home_x_g",
  "away_x_g",
  "fbref_id",
  "home_team_id",
  "away_team_id",

  # Player data columns
  "player",
  "player_id",
  "player_name",
  "player_href",
  "team",
  "home_away",
  "is_home",
  "pos",
  "position",
  "nation",
  "age",
  "min",
  "minutes",
  "is_starter",

  # Splint columns
  "splint_id",
  "splint_num",
  "start_minute",
  "end_minute",
  "duration",
  "npxgd",
  "npxgd_per_90",
  "npxg_home",
  "npxg_away",

  # Event columns
  "event_type",
  "minute",
  "is_goal",
  "is_sub",
  "is_penalty",
  "is_own_goal",

  # Statistics columns (snake_case versions)
  "gls",
  "ast",
  "sh",
  "so_t",
  "xg",
  "xg_expected",
  "npxg",
  "npxg_expected",
  "xag",
  "xag_expected",
  "pk",
  "pkatt",
  "crd_y",
  "crd_r",
  "touches",
  "tkl",
  "int",
  "blocks",
  "sca_sca",
  "gca_sca",
  "cmp_passes",
  "att_passes",
  "cmp_percent_passes",
  "prg_p_passes",
  "carries_carries",
  "prg_c_carries",
  "att_take_ons",
  "succ_take_ons",

  # Rate stat columns
  "per_100_seq",
  "team_sequences",
  "estimated_sequences",

  # RAPM/SPM/Panna columns
  "rapm",
  "o_rapm",
  "d_rapm",
  "spm",
  "o_spm",
  "d_spm",
  "panna",
  "o_panna",
  "d_panna",
  "spm_prior",
  "deviation",
  "coefficient",

  # Helper columns
  "n",
  "n_games",
  "total_minutes",
  "row_num",
  "value",
  "name",
  "label",
  ".",

  # Player timing columns
  "on_minute",
  "off_minute",

  # Player stats function columns
  "x1_3",
  "kp",
  "ppa",
  "crs_pa",
  "tkl_w",
  "clr",
  "err",
  "so_ta",
  "ga",
  "saves",
  "clean_sheet"
))
