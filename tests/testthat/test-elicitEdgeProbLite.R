test_that("elicitEdgeProbLite completes with 4 variables (regression: infinite loop)", {
  # 4 variables leave only 2 remaining vars per pair, i.e. only 2 distinct
  # orderings; the old code looped forever trying to find 5 unique ones.
  local_mocked_bindings(callLLM = mock_callLLM_factory("E"), .package = "bgmElicit")

  result <- suppressMessages(elicitEdgeProbLite(
    variable_list    = c("A", "B", "C", "D"),
    LLM_model        = "gpt-4o",
    n_perm           = 5,
    display_progress = FALSE
  ))

  expect_s3_class(result, "elicitEdgeProbLite")
  expect_equal(nrow(result$raw_LLM), 6 * 5)   # 6 pairs x 5 permutations
  expect_true(all(result$relation_df$prob == 0))
})

test_that("elicitEdgeProbLite accepts n_perm = NULL (regression: length-zero crash)", {
  local_mocked_bindings(callLLM = mock_callLLM_factory("I"), .package = "bgmElicit")

  result <- suppressMessages(elicitEdgeProbLite(
    variable_list    = c("A", "B", "C"),
    LLM_model        = "gpt-4o",
    n_perm           = NULL,
    display_progress = FALSE
  ))

  expect_s3_class(result, "elicitEdgeProbLite")
  expect_equal(result$arguments$n_perm, 5)
})

test_that("elicitEdgeProbLite uses logprobs when supported (regression: dead logprobs path)", {
  local_mocked_bindings(
    callLLM = mock_callLLM_factory("I", with_logprobs = TRUE, prob_i = 0.8),
    .package = "bgmElicit"
  )

  result <- suppressMessages(elicitEdgeProbLite(
    variable_list    = c("A", "B", "C"),
    LLM_model        = "gpt-4o",
    n_perm           = 2,
    logprobs         = TRUE,
    display_progress = FALSE
  ))

  expect_equal(result$relation_df$prob, rep(0.8, 3), tolerance = 1e-8)
  expect_true(all(result$raw_LLM$mode_used == "logprobs"))
  expect_true(result$arguments$logprobs_used)
})

test_that("elicitEdgeProbLite validates inputs", {
  expect_error(
    elicitEdgeProbLite(context = "test", variable_list = c("A", "B")),
    "at least three"
  )
  expect_error(
    elicitEdgeProbLite(variable_list = c("A", "B", "C"), n_perm = 0),
    "n_perm"
  )
  expect_error(
    elicitEdgeProbLite(variable_list = c("A", "B", "C"), n_perm = -3),
    "n_perm"
  )
  expect_error(
    elicitEdgeProbLite(variable_list = c("A", "B", "C"), n_perm = 51),
    "maximum"
  )
})

test_that("elicitEdgeProbLite works with the live API", {
  skip_on_cran()
  skip_on_ci()
  skip_if_no_api_key()

  result <- suppressMessages(elicitEdgeProbLite(
    context = "Simple test",
    variable_list = c("X", "Y", "Z"),
    LLM_model = "gpt-4o",
    n_perm = 1
  ))

  expect_s3_class(result, "elicitEdgeProbLite")
  expect_true("relation_df" %in% names(result))
})
