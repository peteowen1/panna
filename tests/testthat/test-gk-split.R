# panna#429: keeper OSR/DSR come from the margin fit, split by what each stat is.

gk_coef <- function() utils::read.csv(system.file("extdata", "gk_psr_coefficients.csv", package = "panna"),
                                      stringsAsFactors = FALSE)
fake_keepers <- function(n = 40, seed = 1) {
  set.seed(seed)
  cf <- gk_coef()
  m <- as.data.frame(lapply(stats::setNames(cf$stat_name, cf$stat_name), function(s) abs(stats::rnorm(n, 1, 0.5))))
  cbind(data.frame(player_id = paste0("k", seq_len(n)), player_name = paste0("K", seq_len(n)),
                   season_end_year = 2026, primary_position = "Goalkeeper", total_minutes = 2000), m)
}

test_that(".gk_split_coefficients puts distribution betas in osr and everything else in dsr", {
  cf <- gk_coef()
  sp <- panna:::.gk_split_coefficients(cf)
  dist <- cf$stat_name %in% panna:::GK_DISTRIBUTION_STATS
  expect_true(any(dist))
  expect_equal(sp$osr$beta + sp$dsr$beta, cf$beta)
  expect_true(all(sp$osr$beta[!dist] == 0))
  expect_true(all(sp$dsr$beta[dist] == 0))
})

test_that("keeper OSR + DSR = PSR with no leftover, and OSR moves only with distribution", {
  sk <- fake_keepers()
  cf <- gk_coef(); sp <- panna:::.gk_split_coefficients(cf)
  r <- calculate_psr_components(sk, cf, sp$osr, sp$dsr, center = TRUE)
  o <- calculate_psr(sk, sp$osr, center = TRUE)$psr
  expect_equal(r$osr + r$dsr, r$psr, tolerance = 1e-12)
  expect_equal(r$osr, o, tolerance = 1e-12)          # leftover is zero: osr is exactly the distribution score
  sk2 <- sk; sk2$goals_conceded_p90 <- sk2$goals_conceded_p90 * 3   # concede far more
  r2 <- calculate_psr_components(sk2, cf, sp$osr, sp$dsr, center = FALSE)
  r1 <- calculate_psr_components(sk, cf, sp$osr, sp$dsr, center = FALSE)
  expect_equal(r2$osr, r1$osr, tolerance = 1e-12)     # conceding does not touch attack
  expect_true(all(r2$dsr < r1$dsr))                   # and lowers defence (beta on conceded is negative)
})

test_that("compute_player_psr scores keepers with the split, not the old gk_osr/gk_dsr fits", {
  sk <- fake_keepers()
  out <- suppressMessages(suppressWarnings(compute_player_psr(sk)))
  out <- data.table::as.data.table(out)
  expect_equal(out$osr + out$dsr, out$psr, tolerance = 1e-9)
  sk2 <- sk; sk2$goals_conceded_p90 <- sk2$goals_conceded_p90 * 3
  out2 <- data.table::as.data.table(suppressMessages(suppressWarnings(compute_player_psr(sk2))))
  m <- merge(out[, .(player_id, osr1 = osr)], out2[, .(player_id, osr2 = osr)], by = "player_id")
  expect_equal(m$osr1, m$osr2, tolerance = 1e-9)
})

test_that("keepers with no distribution columns get OSR 0 and DSR = PSR (no error)", {
  sk <- fake_keepers()
  sk <- sk[, !names(sk) %in% panna:::GK_DISTRIBUTION_STATS]
  out <- data.table::as.data.table(suppressMessages(suppressWarnings(compute_player_psr(sk))))
  expect_true(all(out$osr == 0))
  expect_equal(out$dsr, out$psr)
})
