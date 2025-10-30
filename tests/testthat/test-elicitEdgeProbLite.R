test_that("elicitEdgeProbLite works with valid inputs", {
  skip_on_cran()
  skip_on_ci()
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")),
              "OpenAI API key not available")

  result <- elicitEdgeProbLite(
    context = "Simple test",
    variable_list = c("X", "Y", "Z"),
    LLM_model = "gpt-4",
    n_perm = 1
  )

  expect_s3_class(result, "elicitEdgeProbLite")
  expect_true("relation_df" %in% names(result))
})

test_that("elicitEdgeProbLite validates inputs", {
  expect_error(
    elicitEdgeProbLite(
      context = "test",
      variable_list = c("A", "B")
    ),
    "length\\(variable_list\\) >= 3"
  )
})
