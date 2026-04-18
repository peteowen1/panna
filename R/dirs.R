# Directory resolution helpers — data-source-agnostic.
# Previously lived in scrape_fbref_utils.R (misleading name); extracted here
# so they survive the FBref/understat archival sweep. opta_data_dir() in
# opta_loaders.R depends on pannadata_dir(), so this MUST stay loaded.

# Environment for storing pannadata path
.panna_env <- new.env(parent = emptyenv())

#' Get or set pannadata directory
#'
#' Gets or sets the base directory for parquet/RDS data storage.
#'
#' Resolution order (first match wins):
#' \enumerate{
#'   \item Explicitly set via \code{pannadata_dir("path")}
#'   \item \code{PANNADATA_DIR} environment variable
#'   \item \code{../pannadata/data} relative to working directory (for the
#'     pannaverse monorepo layout)
#'   \item \code{tools::R_user_dir("panna", "data")} — OS-standard user data dir
#' }
#'
#' OS-standard fallback paths:
#' \itemize{
#'   \item Windows: \code{C:/Users/you/AppData/Local/R/panna/data}
#'   \item Mac: \code{~/Library/Application Support/org.R-project.R/panna/data}
#'   \item Linux: \code{~/.local/share/R/panna/data}
#' }
#'
#' @param path Optional new path to set. If NULL, returns current path.
#'
#' @return Current pannadata directory path (invisibly when setting)
#' @export
#'
#' @examples
#' # Get current path
#' pannadata_dir()
#'
#' # Set custom path
#' pannadata_dir("~/my/football/data")
pannadata_dir <- function(path = NULL) {

  if (!is.null(path)) {
    .panna_env$pannadata_dir <- normalizePath(path, mustWork = FALSE)
    return(invisible(.panna_env$pannadata_dir))
  }

  # 1. Return cached value if explicitly set
  if (exists("pannadata_dir", envir = .panna_env)) {
    return(.panna_env$pannadata_dir)
  }

  # 2. Check environment variable
  env_path <- Sys.getenv("PANNADATA_DIR", "")
  if (env_path != "") {
    return(env_path)
  }

  # 3. Check for pannaverse structure (for developers)
  # Look for ../pannadata/data relative to working directory
  pannaverse_path <- file.path(dirname(getwd()), "pannadata", "data")
  if (dir.exists(pannaverse_path)) {
    return(normalizePath(pannaverse_path))
  }

  # 4. Default: R's standard user data directory (works across sessions)
  tools::R_user_dir("panna", "data")
}
