# HTTP layer for FBref scraping
#
# Handles HTTP requests, browser headers, session management for Cloudflare bypass.
# Used by scrape_fbref_batch.R for actual scraping operations.


# ============================================================================
# HTTP Layer
# ============================================================================

#' Get browser headers for FBref requests
#'
#' Returns headers that mimic a real browser to avoid Cloudflare blocking.
#' Rotates through different User-Agent strings for safety.
#'
#' @param referer Optional referer URL (default: FBref competitions page)
#'
#' @return Named character vector of HTTP headers
#' @keywords internal
get_fbref_headers <- function(referer = "https://fbref.com/en/comps/") {
  # Pool of realistic User-Agent strings (recent Chrome/Firefox/Edge on Windows/Mac)
  user_agents <- c(
    # Chrome on Windows
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/129.0.0.0 Safari/537.36",
    # Chrome on Mac
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36",
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/130.0.0.0 Safari/537.36",
    # Firefox on Windows
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:133.0) Gecko/20100101 Firefox/133.0",
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:132.0) Gecko/20100101 Firefox/132.0",
    # Firefox on Mac
    "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:133.0) Gecko/20100101 Firefox/133.0",
    # Edge on Windows
    "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36 Edg/131.0.0.0"
  )

  # Pick a random User-Agent
  ua <- sample(user_agents, 1)

  # Extract browser version for sec-ch-ua header
  chrome_version <- regmatches(ua, regexpr("Chrome/([0-9]+)", ua))
  chrome_version <- gsub("Chrome/", "", chrome_version)
  if (length(chrome_version) == 0 || chrome_version == "") {
    chrome_version <- "131"
  }

  # Determine platform from UA
  platform <- if (grepl("Macintosh", ua)) '"macOS"' else '"Windows"'

  c(
    "User-Agent" = ua,
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,image/apng,*/*;q=0.8",
    "Accept-Language" = sample(c("en-US,en;q=0.9", "en-GB,en;q=0.9,en-US;q=0.8", "en;q=0.9"), 1),
    "Accept-Encoding" = "gzip, deflate, br",
    "sec-ch-ua" = sprintf('"Chromium";v="%s", "Not?A_Brand";v="99"', chrome_version),
    "sec-ch-ua-mobile" = "?0",
    "sec-ch-ua-platform" = platform,
    "sec-fetch-dest" = "document",
    "sec-fetch-mode" = "navigate",
    "sec-fetch-site" = "same-origin",
    "sec-fetch-user" = "?1",
    "Upgrade-Insecure-Requests" = "1",
    "Referer" = referer,
    "DNT" = "1"
  )
}


#' Add random jitter to delay
#'
#' Returns the base delay plus random jitter to appear more human.
#'
#' @param base_delay Base delay in seconds
#' @param jitter_pct Percentage of jitter (default 0.3 = +/- 30%)
#'
#' @return Delay in seconds with jitter applied
#' @keywords internal
add_delay_jitter <- function(base_delay, jitter_pct = 0.3) {
  jitter <- base_delay * jitter_pct
  base_delay + stats::runif(1, -jitter, jitter)
}


# Environment for session storage (avoids namespace locking issues)
.fbref_env <- new.env(parent = emptyenv())

#' Get or create FBref session
#'
#' Returns a persistent httr session handle that maintains cookies
#' across requests, like a real browser session.
#'
#' @param reset If TRUE, creates a new session (default FALSE)
#'
#' @return httr handle object
#' @keywords internal
get_fbref_session <- function(reset = FALSE) {
  if (!exists("session", envir = .fbref_env) || reset) {
    .fbref_env$session <- httr::handle("https://fbref.com")
  }
  .fbref_env$session
}

#' Reset FBref session
#'
#' Clears cookies and creates fresh session. Use after changing VPN/IP.
#'
#' @keywords internal
reset_fbref_session <- function() {
  .fbref_env$session <- httr::handle("https://fbref.com")
  message("FBref session reset - cookies cleared")
}


#' Fetch FBref match page HTML
#'
#' Makes HTTP request with browser headers to avoid Cloudflare blocking.
#'
#' @param match_url FBref match URL
#' @param timeout Request timeout in seconds (default 30)
#'
#' @return Parsed HTML document (rvest xml_document) or NULL on failure
#' @keywords internal
fetch_match_page <- function(match_url, timeout = 30) {
  # Validate URL
if (!grepl("fbref\\.com/en/matches/", match_url)) {
    cli::cli_abort("Invalid FBref match URL: {.val {match_url}}")
  }

  # Make request with session cookies and retry logic

  response <- fetch_with_retry(
    match_url,
    httr::add_headers(.headers = get_fbref_headers()),
    httr::timeout(timeout),
    handle = get_fbref_session()
  )

  # Check for errors returned by fetch_with_retry
  if (is.null(response)) {
    if (isTRUE(attr(response, "rate_limited"))) {
      cli::cli_warn("Rate limited by FBref (429). Stopping.")
    } else if (isTRUE(attr(response, "blocked"))) {
      cli::cli_warn("Blocked by Cloudflare (403). Stopping.")
    } else if (isTRUE(attr(response, "connection_error"))) {
      cli::cli_warn("Connection error: {attr(response, 'error_message')}")
    } else {
      cli::cli_warn("Failed to fetch {match_url}")
    }
    return(response)
  }

  # Parse HTML
  html_content <- httr::content(response, "text", encoding = "UTF-8")
  rvest::read_html(html_content)
}
