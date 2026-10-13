test_that(".match_grouped_folds keeps every match in one fold and covers every row", {
  set.seed(1)
  mid <- rep(sprintf("m%02d", 1:23), times = sample(5:40, 23, replace = TRUE))
  folds <- .match_grouped_folds(mid, 5)
  expect_length(folds, 5)
  expect_setequal(unlist(folds), seq_along(mid))
  expect_equal(anyDuplicated(unlist(folds)), 0L)
  fold_of_row <- integer(length(mid))
  for (k in seq_along(folds)) fold_of_row[folds[[k]]] <- k
  expect_true(all(tapply(fold_of_row, mid, function(f) length(unique(f))) == 1L))
  expect_true(all(lengths(folds) > 0))
})

test_that(".match_grouped_folds warns and splits rows when matches < folds", {
  expect_warning(f <- .match_grouped_folds(rep(c("a", "b"), each = 10), 5), "split rows at random")
  expect_length(f, 5)
  expect_setequal(unlist(f), 1:20)
})
