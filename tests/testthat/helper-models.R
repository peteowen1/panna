# Fixture models and the published-model guard (panna#244)
#
# Tests must never score with whatever model is currently published. When a
# test passed `xg_model = NULL`, calculate_action_epv() fell through to
# load_xg_model(), which downloads the live pannamodels release; deploying the
# season-aware xG model on 2026-09-12 then turned an assertion about EPV bounds
# into an xG error with no code change. Tests that need a model build a fixture
# here and pass it explicitly, and run under local_no_published_models() so a
# fall-through fails the test instead of downloading.

.fixture_model_cache <- new.env(parent = emptyenv())

#' Tiny xG model fitted on synthetic shots
#'
#' Built once per test run and cached. No season term (`season_feature` is
#' FALSE by default), so callers need not pass a `season`. Uses its own seed and
#' restores the caller's RNG state, so building it mid-test does not shift any
#' random data the test generates afterwards.
fixture_xg_model <- function() {
  testthat::skip_if_not_installed("xgboost")
  if (is.null(.fixture_model_cache$xg)) {
    .fixture_model_cache$xg <- withr::with_seed(244, {
      n <- 200
      shots <- data.frame(
        match_id = "fixture_m1",
        player_id = paste0("p", seq_len(n)),
        player_name = paste0("Player ", seq_len(n)),
        x = stats::runif(n, 70, 100),
        y = stats::runif(n, 20, 80),
        is_goal = stats::rbinom(n, 1, 0.1),
        body_part = sample(c("Head", "Right Foot", "Left Foot"), n, replace = TRUE),
        situation = sample(c("Open Play", "Set Piece"), n, replace = TRUE),
        big_chance = stats::rbinom(n, 1, 0.15)
      )
      suppressMessages(fit_xg_model(prepare_shots_for_xg(shots),
                                    nrounds = 10, nfolds = 2, verbose = 0))
    })
  }
  .fixture_model_cache$xg
}

#' Fail the calling test if it reaches a published-model loader
#'
#' Stubs every panna model loader that can download a published model (and
#' `pannamodels::load_panna_model()` when pannamodels is installed) for the
#' rest of the calling test. Each stub records the call and errors. Recording
#' matters because some callers swallow loader errors -- calculate_action_epv()
#' turns a failed load_xg_model() into a warning and carries on -- so the check
#' runs when the test exits and fails it if any loader was reached.
local_no_published_models <- function(env = parent.frame()) {
  reached <- character()
  stub <- function(name) {
    force(name)
    function(...) {
      reached <<- c(reached, name)
      stop(name, "() reached from a test: pass a fixture model explicitly ",
           "(see tests/testthat/helper-models.R)", call. = FALSE)
    }
  }
  # Registered before the mocks so it runs after they are restored (deferred
  # handlers run last-in first-out). Registered after them, a failing check
  # skipped the restore and the stubs leaked into later tests.
  withr::defer(
    testthat::expect(
      length(reached) == 0,
      sprintf("Test fell through to a published-model loader: %s",
              paste(unique(reached), collapse = ", "))
    ),
    envir = env
  )
  testthat::local_mocked_bindings(
    load_xg_model = stub("load_xg_model"),
    load_xgot_model = stub("load_xgot_model"),
    load_epv_model = stub("load_epv_model"),
    load_wp_model = stub("load_wp_model"),
    load_xpass_model = stub("load_xpass_model"),
    load_duel_model = stub("load_duel_model"),
    .package = "panna",
    .env = env
  )
  if (requireNamespace("pannamodels", quietly = TRUE)) {
    testthat::local_mocked_bindings(
      load_panna_model = stub("pannamodels::load_panna_model"),
      .package = "pannamodels",
      .env = env
    )
  }
  invisible(NULL)
}
