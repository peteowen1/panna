# SPM-priced per-game box-score value metric (spm_value)
# BOX-SCORE-VALUE-SPM-REDESIGN.md sec 4 & Wave 3.
#
# spm_value prices single-match box-score per-90 stat lines using context-adjusted
# (prior-free RAPM) pricing. Coexists with PSV (result-priced value):
#   - PSV = stats * result prices (GD / team outcome target)
#   - spm_value = stats * context-adjusted prices (prior-free RAPM target)
#   - value_context_gap = psv - spm_value (pricing disagreement signal)


#' Load shipping spm_value coefficients
#'
#' Reads the shipping `spm_value_coefficients.csv` from `inst/extdata/` (or package
#' installation directory). Returns a list with `offense` and `defense` coefficient vectors
#' (keyed by feature/deviation column names, including `(Intercept)`).
#'
#' @param file_path Optional path to custom coefficient CSV. If `NULL` (default), loads from
#'   `inst/extdata/spm_value_coefficients.csv`.
#' @return List with `offense` and `defense` data.tables or named numeric vectors.
#' @family spm_value
#' @export
load_spm_value_coefficients <- function(file_path = NULL) {
  if (is.null(file_path)) {
    file_path <- system.file("extdata", "spm_value_coefficients.csv", package = "panna")
    if (file_path == "" || !file.exists(file_path)) {
      # Fallback for dev mode / local runs before installation
      dev_path <- file.path("inst", "extdata", "spm_value_coefficients.csv")
      if (file.exists(dev_path)) {
        file_path <- dev_path
      } else {
        cli::cli_abort("spm_value_coefficients.csv not found in package extdata or inst/extdata/.")
      }
    }
  }

  dt <- data.table::fread(file_path)
  list(
    offense = dt[target == "offense"],
    defense = dt[target == "defense"]
  )
}


#' Score single-match box-score stat lines with spm_value prices
#'
#' Given match-level per-90 statistics and role classifications, scores each row using the C1
#' context-adjusted SPM prices.
#'
#' @param match_stats data.frame/data.table with per-90 box-score features and position roles.
#' @param coefs List output of `load_spm_value_coefficients()`.
#' @param gd_scale Numeric multiplier to anchor to GD scale units (default 1.0, derived by GD scale calibration).
#' @return data.table with `spm_value_off`, `spm_value_def`, and net `spm_value`.
#' @family spm_value
#' @export
calculate_spm_value <- function(match_stats, coefs = NULL, gd_scale = 1.0) {
  if (is.null(coefs)) {
    coefs <- load_spm_value_coefficients()
  }

  dt <- data.table::as.data.table(match_stats)
  n <- nrow(dt)
  if (n == 0) {
    return(data.table::data.table(
      spm_value_off = numeric(0),
      spm_value_def = numeric(0),
      spm_value = numeric(0)
    ))
  }

  # 1. Classify role group per row
  roles <- if ("position_role" %in% names(dt)) dt$position_role else if ("position" %in% names(dt)) dt$position else rep("UNK", n)
  role_groups <- classify_role_group(roles)

  # 2. Score Offense & Defense
  score_component <- function(coef_table) {
    val <- rep(0.0, n)
    if ("(Intercept)" %in% coef_table$feature) {
      val <- val + coef_table[feature == "(Intercept)", coef]
    }

    # Global features
    globals <- coef_table[is_deviation == FALSE & feature != "(Intercept)"]
    for (i in seq_len(nrow(globals))) {
      feat <- globals$feature[i]
      b <- globals$coef[i]
      if (feat %in% names(dt)) {
        col_vals <- dt[[feat]]
        col_vals[is.na(col_vals)] <- 0
        val <- val + b * col_vals
      }
    }

    # Deviation features
    devs <- coef_table[is_deviation == TRUE]
    for (i in seq_len(nrow(devs))) {
      rg <- devs$role_group[i]
      base_f <- devs$base_feature[i]
      b <- devs$coef[i]
      if (base_f %in% names(dt)) {
        mask <- !is.na(role_groups) & role_groups == rg
        if (any(mask)) {
          col_vals <- dt[[base_f]][mask]
          col_vals[is.na(col_vals)] <- 0
          val[mask] <- val[mask] + b * col_vals
        }
      }
    }
    val
  }

  off_val <- score_component(coefs$offense) * gd_scale
  def_val <- score_component(coefs$defense) * gd_scale
  net_val <- off_val + def_val

  data.table::data.table(
    spm_value_off = off_val,
    spm_value_def = def_val,
    spm_value = net_val
  )
}


#' Compute diagnostic value context gap (psv - spm_value)
#'
#' @param psv Numeric vector of PSV values (result-priced per-game metric).
#' @param spm_value Numeric vector of spm_value values (context-priced per-game metric).
#' @return Numeric vector of `psv - spm_value`.
#' @family spm_value
#' @export
calculate_value_context_gap <- function(psv, spm_value) {
  psv - spm_value
}
