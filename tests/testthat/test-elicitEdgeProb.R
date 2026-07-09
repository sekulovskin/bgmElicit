test_that("elicitEdgeProb works end-to-end with a mocked LLM (hard decisions)", {
  local_mocked_bindings(callLLM = mock_callLLM_factory("I"), .package = "bgmElicit")

  result <- suppressMessages(elicitEdgeProb(
    context          = "Test study",
    variable_list    = c("A", "B", "C"),
    LLM_model        = "gpt-4o",
    n_perm           = 2,
    display_progress = FALSE
  ))

  expect_s3_class(result, "elicitEdgeProb")
  expect_named(result, c("raw_LLM", "diagnostics", "relation_df",
                         "arguments", "inclusion_probability_matrix"))
  expect_equal(nrow(result$relation_df), 3)          # 3 variables -> 3 pairs
  expect_equal(nrow(result$raw_LLM), 3 * 2)          # pairs x permutations
  expect_true(all(result$relation_df$prob == 1))     # all answers were "I"
  # exact 1s are squashed to 0.99 in the symmetric matrix, diagonal is 0
  m <- result$inclusion_probability_matrix
  expect_true(all(diag(m) == 0))
  expect_true(all(m[upper.tri(m)] == 0.99))
})

test_that("elicitEdgeProb uses logprobs when the model supports them", {
  local_mocked_bindings(
    callLLM = mock_callLLM_factory("I", with_logprobs = TRUE, prob_i = 0.9),
    .package = "bgmElicit"
  )

  result <- suppressMessages(elicitEdgeProb(
    variable_list    = c("A", "B", "C"),
    LLM_model        = "gpt-4o",
    n_perm           = 1,
    logprobs         = TRUE,
    display_progress = FALSE
  ))

  expect_equal(result$relation_df$prob, rep(0.9, 3), tolerance = 1e-8)
  expect_true(all(result$raw_LLM$mode_used == "logprobs"))
  expect_true(result$arguments$logprobs_used)
})

test_that("elicitEdgeProb falls back to 0.5 on uninterpretable output", {
  local_mocked_bindings(callLLM = mock_callLLM_factory("banana"), .package = "bgmElicit")

  result <- suppressMessages(elicitEdgeProb(
    variable_list    = c("A", "B", "C"),
    LLM_model        = "gpt-4o",
    n_perm           = 1,
    display_progress = FALSE
  ))

  expect_true(all(result$relation_df$prob == 0.5))
  expect_equal(result$diagnostics$defaults_0p5, 3)
})

test_that("context is optional", {
  local_mocked_bindings(callLLM = mock_callLLM_factory("E"), .package = "bgmElicit")

  expect_no_error(suppressMessages(elicitEdgeProb(
    variable_list    = c("A", "B", "C"),
    LLM_model        = "gpt-4o",
    n_perm           = 1,
    display_progress = FALSE
  )))
})

test_that("elicitEdgeProb restores the caller's RNG state", {
  local_mocked_bindings(callLLM = mock_callLLM_factory("I"), .package = "bgmElicit")

  set.seed(999)
  expected <- runif(1)
  set.seed(999)
  suppressMessages(elicitEdgeProb(
    variable_list = c("A", "B", "C"), LLM_model = "gpt-4o",
    n_perm = 1, display_progress = FALSE
  ))
  expect_identical(runif(1), expected)
})

test_that("elicitEdgeProb validates inputs", {
  expect_error(
    elicitEdgeProb(context = "test", variable_list = c("A")),
    "at least three"
  )
  expect_error(
    elicitEdgeProb(context = "test", variable_list = c("A", "B", "C", "A")),
    "duplicated"
  )
  expect_error(
    elicitEdgeProb(context = 1, variable_list = c("A", "B", "C")),
    "`context`"
  )
  expect_error(
    elicitEdgeProb(variable_list = c("A", "B", "C"), n_perm = 0),
    "n_perm"
  )
  expect_error(
    elicitEdgeProb(variable_list = c("A", "B", "C"), n_perm = 51),
    "maximum"
  )
})

test_that("elicitEdgeProb returns expected structure with the live API", {
  skip_on_cran()
  skip_on_ci()
  skip_if_no_api_key()

  result <- suppressMessages(elicitEdgeProb(
    context = "Test study",
    variable_list = c("Var1", "Var2", "Var3"),
    LLM_model = "gpt-4o",
    n_perm = 1
  ))

  expect_s3_class(result, "elicitEdgeProb")
  expect_true("relation_df" %in% names(result))
  expect_true(is.data.frame(result$relation_df))
})
