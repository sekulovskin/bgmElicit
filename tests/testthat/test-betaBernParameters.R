# Build a decision pattern with enough overdispersion across pairs that both
# MoM and MLE estimation are well-posed (inclusion counts pushed to extremes).
varied_contents <- function(n_pairs, n_perm, decorate = identity) {
  counts <- rep(c(0, n_perm, 1, n_perm - 1), length.out = n_pairs)
  unlist(lapply(counts, function(k) {
    decorate(c(rep("I", k), rep("E", n_perm - k)))
  }))
}

test_that("betaBernParameters returns MLE estimates by default", {
  skip_if_not_installed("trust")
  obj <- make_mock_llmobject(varied_contents(4, 5), n_pairs = 4, n_perm = 5)
  fit <- betaBernParameters(obj)
  expect_named(fit, "mle")
  expect_named(fit$mle, c("alpha", "beta"))
  expect_true(all(fit$mle > 0))
})

test_that("force_mom returns method-of-moments estimates (regression: inverted semantics)", {
  obj <- make_mock_llmobject(varied_contents(4, 5), n_pairs = 4, n_perm = 5)
  fit <- betaBernParameters(obj, method = "mle", force_mom = TRUE)
  expect_named(fit, "mom")   # previously this returned MLE
})

test_that("decorated responses count as inclusions (regression: exact string match)", {
  skip_if_not_installed("trust")
  clean <- make_mock_llmobject(varied_contents(4, 5), n_pairs = 4, n_perm = 5)
  messy <- make_mock_llmobject(
    varied_contents(4, 5, decorate = function(x) {
      ifelse(x == "I", c("I.", " i", "Include")[seq_along(x) %% 3 + 1], "e.")
    }),
    n_pairs = 4, n_perm = 5
  )
  expect_equal(betaBernParameters(clean), betaBernParameters(messy))
})

test_that("wrong-class input errors immediately without a spurious warning", {
  expect_no_warning(
    expect_error(
      betaBernParameters(list(a = 1)),
      "must be of class 'elicitEdgeProb' or 'elicitEdgeProbLite'"
    )
  )
})

test_that("missing raw_LLM gives an informative error", {
  obj <- structure(list(relation_df = data.frame()), class = "elicitEdgeProb")
  expect_error(betaBernParameters(obj), "raw_LLM")
})

test_that("fewer than 5 permutations triggers a warning", {
  obj <- make_mock_llmobject(varied_contents(3, 3), n_pairs = 3, n_perm = 3)
  expect_warning(
    try(betaBernParameters(obj, method = "mle", force_mom = TRUE), silent = TRUE),
    "more permutations"
  )
})

test_that("invalid method is rejected", {
  obj <- make_mock_llmobject(varied_contents(3, 5), n_pairs = 3, n_perm = 5)
  expect_error(betaBernParameters(obj, method = "map"), "either 'mle' or 'mom'")
})
