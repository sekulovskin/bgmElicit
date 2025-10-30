test_that("sbmClusters identifies clusters", {
  # Create proper mock with all required components
  mock_llm <- structure(
    list(
      relation_df = data.frame(
        var1 = c("A", "A", "B"),
        var2 = c("B", "C", "C"),
        probability = c(0.8, 0.7, 0.6),
        stringsAsFactors = FALSE
      ),
      # Add the required inclusion_probability_matrix
      inclusion_probability_matrix = matrix(
        c(0, 0.8, 0.7,
          0.8, 0, 0.6,
          0.7, 0.6, 0),
        nrow = 3,
        dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
      )
    ),
    class = "elicitEdgeProb"
  )

  result <- sbmClusters(
    llmobject = mock_llm,
    algorithm = "louvain",
    threshold = 0.5
  )

  expect_type(result, "list")
  expect_true("elicited_no_clusters" %in% names(result))
})

test_that("sbmClusters validates input class", {
  # Test with wrong class
  wrong_class <- list(relation_df = data.frame())

  expect_error(
    sbmClusters(wrong_class),
    "must be of class 'elicitEdgeProb' or 'elicitEdgeProbLite'"
  )
})
