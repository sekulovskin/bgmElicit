make_sbm_mock <- function(mat, class = "elicitEdgeProb") {
  structure(list(inclusion_probability_matrix = mat), class = class)
}

test_that("sbmClusters identifies clusters", {
  mock_llm <- make_sbm_mock(matrix(
    c(0,   0.8, 0.7,
      0.8, 0,   0.6,
      0.7, 0.6, 0),
    nrow = 3,
    dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
  ))

  result <- sbmClusters(
    llmobject = mock_llm,
    algorithm = "louvain",
    threshold = 0.5
  )

  expect_type(result, "list")
  expect_true("elicited_no_clusters" %in% names(result))
  expect_equal(result$elicited_no_clusters, 1L)  # fully connected -> one cluster
  expect_length(result$details$results_ties0$membership, 3)
})

test_that("sbmClusters detects two separated communities", {
  # A-B strongly connected, C-D strongly connected, nothing across
  m <- matrix(0.1, 4, 4, dimnames = list(LETTERS[1:4], LETTERS[1:4]))
  m["A", "B"] <- m["B", "A"] <- 0.9
  m["C", "D"] <- m["D", "C"] <- 0.9
  diag(m) <- 0

  result <- sbmClusters(make_sbm_mock(m), algorithm = "louvain")
  expect_equal(result$elicited_no_clusters, 2L)
})

test_that("sbmClusters resolves ties in both directions", {
  m <- matrix(0.1, 3, 3, dimnames = list(c("A", "B", "C"), c("A", "B", "C")))
  m["A", "B"] <- m["B", "A"] <- 0.5   # exactly at the threshold
  diag(m) <- 0

  result <- sbmClusters(make_sbm_mock(m), algorithm = "louvain", threshold = 0.5)
  expect_true(result$details$ties_present)
})

test_that("sbmClusters validates input class", {
  wrong_class <- list(relation_df = data.frame())
  expect_error(
    sbmClusters(wrong_class),
    "must be of class 'elicitEdgeProb' or 'elicitEdgeProbLite'"
  )
})

test_that("sbmClusters validates the inclusion probability matrix", {
  expect_error(
    sbmClusters(make_sbm_mock(NULL)),
    "missing"
  )
  asym <- matrix(c(0, 0.9, 0.1, 0, 0.2, 0.3, 0.4, 0.5, 0), 3, 3)
  expect_error(sbmClusters(make_sbm_mock(asym)), "symmetric")
  m <- matrix(0.5, 3, 3); diag(m) <- 0; m[1, 2] <- m[2, 1] <- 1.5
  expect_error(sbmClusters(make_sbm_mock(m)), "values in \\[0, 1\\]")
  expect_error(sbmClusters(make_sbm_mock(matrix(0.5, 3, 2))), "square|symmetric")
})
