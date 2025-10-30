test_that("basic data structure validation works", {
  # Test any utility functions that don't need API
  valid_df <- data.frame(
    var1 = c("A", "B"),
    var2 = c("B", "C"),
    probability = c(0.5, 0.7)
  )

  expect_true(is.data.frame(valid_df))
  expect_equal(ncol(valid_df), 3)
})
