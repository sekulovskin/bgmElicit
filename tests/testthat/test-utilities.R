test_that("estimate_beta_binomial MoM roughly recovers parameters on overdispersed data", {
  set.seed(42)
  alpha <- 2; beta <- 3; n <- 20
  p <- rbeta(2000, alpha, beta)
  x <- rbinom(2000, n, p)
  fit <- bgmElicit:::estimate_beta_binomial(x, n, method = "mom")
  expect_named(fit, "mom")
  expect_equal(unname(fit$mom["alpha"]), alpha, tolerance = 0.3)
  expect_equal(unname(fit$mom["beta"]), beta, tolerance = 0.3)
})

test_that("estimate_beta_binomial MLE roughly recovers parameters", {
  skip_if_not_installed("trust")
  set.seed(42)
  alpha <- 2; beta <- 3; n <- 20
  p <- rbeta(2000, alpha, beta)
  x <- rbinom(2000, n, p)
  fit <- bgmElicit:::estimate_beta_binomial(x, n, method = "mle")
  expect_named(fit, "mle")
  expect_equal(unname(fit$mle["alpha"]), alpha, tolerance = 0.3)
  expect_equal(unname(fit$mle["beta"]), beta, tolerance = 0.3)
})

test_that("force_mom forces the MoM branch even when method = 'mle'", {
  set.seed(1)
  p <- rbeta(500, 2, 3)
  x <- rbinom(500, 20, p)
  fit <- bgmElicit:::estimate_beta_binomial(x, 20, method = "mle", force_mom = TRUE)
  expect_named(fit, "mom")
})

test_that("estimate_beta_binomial validates the range of x", {
  expect_error(bgmElicit:::estimate_beta_binomial(c(-1, 2), 5), "between 0 and n")
  expect_error(bgmElicit:::estimate_beta_binomial(c(1, 7), 5), "between 0 and n")
})

test_that("MoM warns and returns NA when the data are underdispersed", {
  expect_warning(
    fit <- bgmElicit:::estimate_beta_binomial(rep(10, 10), 20, method = "mom"),
    "Invalid MoM"
  )
  expect_true(all(is.na(fit$mom)))
})

test_that("getLLMLogprobs parses a chat-completions response", {
  mock_resp <- list(choices = list(list(logprobs = list(content = list(
    list(top_logprobs = list(
      list(token = "I", logprob = -0.1),
      list(token = "E", logprob = -2.5)
    ))
  )))))
  parsed <- bgmElicit:::getLLMLogprobs(mock_resp)
  expect_type(parsed, "list")
  expect_length(parsed, 1)
  expect_s3_class(parsed[[1]], "data.frame")
  expect_equal(parsed[[1]]$top5_tokens, c("I", "E"))
  expect_equal(parsed[[1]]$probability, round(exp(c(-0.1, -2.5)), 5))
})

test_that("extractDecisionChar interprets messy responses", {
  expect_equal(bgmElicit:::extractDecisionChar("I"), "i")
  expect_equal(bgmElicit:::extractDecisionChar("  e."), "e")
  expect_equal(bgmElicit:::extractDecisionChar("Include"), "i")
  expect_equal(bgmElicit:::extractDecisionChar("banana"), "?")
  expect_equal(bgmElicit:::extractDecisionChar(""), "?")
  expect_equal(bgmElicit:::extractDecisionChar(NULL), "?")
})

test_that("makePairsDf builds all unordered pairs", {
  pairs <- bgmElicit:::makePairsDf(c("A", "B", "C", "D"))
  expect_equal(nrow(pairs), 6)
  expect_equal(names(pairs), c("var1", "var2"))
  expect_false(any(pairs$var1 == pairs$var2))
})
