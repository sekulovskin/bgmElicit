test_that("elicitEdgeProb returns expected structure", {
  skip_on_cran()
  skip_on_ci()  # Skip on continuous integration
  skip_if_not(nzchar(Sys.getenv("OPENAI_API_KEY")),
              "OpenAI API key not available")

  result <- elicitEdgeProb(
    context = "Test study",
    variable_list = c("Var1", "Var2", "Var3"),
    LLM_model = "gpt-4",
    n_perm = 1
  )

  expect_s3_class(result, "elicitEdgeProb")
  expect_true("relation_df" %in% names(result))
  expect_true(is.data.frame(result$relation_df))
})

test_that("elicitEdgeProb validates inputs", {
  # Test requires at least 3 variables
  expect_error(
    elicitEdgeProb(context = "test", variable_list = c("A")),
    "length\\(variable_list\\) >= 3"
  )

  # Test empty context
  expect_error(
    elicitEdgeProb(context = "", variable_list = c("A", "B", "C"))
  )
})
