#' Community detection on an LLM inclusion-probability matrix (branch-only)
#'
#' Uses the `inclusion_probability_matrix` from an LLM elicitation object to build a
#' binary adjacency matrix (thresholded at `threshold`) and runs community detection twice:
#'   (i) with ties (== threshold) set to 0 and (ii) with ties set to 1.
#' Reports whether the number of clusters differs between the two tie resolutions.
#'
#' @param llmobject An object of class "elicitEdgeProb" or "elicitEdgeProbLite",
#'   containing `inclusion_probability_matrix` (symmetric, values in [0,1], zero diagonal).
#' @param algorithm Community detection algorithm: one of
#'   "louvain", "walktrap", "fast_greedy", "infomap", "label_prop", "edge_betweenness".
#' @param threshold Numeric threshold for binarizing the matrix (default 0.5).
#' @param tol Numerical tolerance for equality to the threshold (default 1e-12).
#' @param return_membership If TRUE, return node→cluster membership (default TRUE).
#' @param remove_isolates If TRUE, drop isolated vertices before clustering and return
#'   their membership as NA in the original order (default FALSE).
#' @param seed Optional integer for reproducibility (affects e.g. Infomap).
#' @param walktrap_steps Steps for Walktrap (default 4).
#' @param infomap_trials Number of trials for Infomap (default 10).
#'
#' @return A list with:
#'   - algorithm, threshold, ties_present
#'   - results_ties0 (k, membership, modularity)
#'   - results_ties1 (k, membership, modularity)
#'   - different_number_of_clusters (logical)
#'   - note (character)
#'
#' @importFrom igraph graph_from_adjacency_matrix cluster_louvain cluster_walktrap
#'   cluster_fast_greedy cluster_infomap cluster_label_prop cluster_edge_betweenness
#'   degree delete_vertices vcount membership modularity components
#'
sbmClusters <- function(
    llmobject,
    algorithm = c("louvain", "walktrap", "fast_greedy",
                  "infomap", "label_prop", "edge_betweenness"),
    threshold = 0.5,
    tol = 1e-12,
    return_membership = TRUE,
    remove_isolates = FALSE,
    seed = NULL,
    walktrap_steps = 4,
    infomap_trials = 10
) {
  if (!requireNamespace("igraph", quietly = TRUE))
    stop("Please install the 'igraph' package.")

  algorithm <- match.arg(algorithm)

  # ---- Class & structure checks ----
  if (!(inherits(llmobject, "elicitEdgeProb") || inherits(llmobject, "elicitEdgeProbLite"))) {
    stop("The input must be of class 'elicitEdgeProb' or 'elicitEdgeProbLite'.")
  }
  if (is.null(llmobject$inclusion_probability_matrix)) {
    stop("`llmobject$inclusion_probability_matrix` is missing.")
  }

  mat <- llmobject$inclusion_probability_matrix

  if (!is.matrix(mat) || !is.numeric(mat))
    stop("`inclusion_probability_matrix` must be a numeric matrix.")
  if (nrow(mat) != ncol(mat))
    stop("`inclusion_probability_matrix` must be square.")
  if (max(abs(mat - t(mat))) > 1e-8)
    stop("`inclusion_probability_matrix` must be symmetric (within tolerance).")
  if (any(!is.finite(mat)))
    stop("`inclusion_probability_matrix` contains non-finite values.")
  if (any(mat < -1e-12 | mat > 1 + 1e-12))
    stop("`inclusion_probability_matrix` must have values in [0,1].")
  if (any(abs(diag(mat)) > 1e-12))
    stop("Diagonal of `inclusion_probability_matrix` must be all zeros.")

  # Optional: warn if few permutations are present
  if (!is.null(llmobject$raw_LLM) && !is.null(llmobject$raw_LLM$permutation)) {
    n_perms <- length(unique(llmobject$raw_LLM$permutation))
    if (n_perms < 10) {
      warning("Few permutations detected in llmobject (", n_perms,
              "); consider increasing for stability.")
    }
  }

  n <- nrow(mat)

  # ---- Threshold: <th -> 0, >th -> 1; ties handled via branching ----
  adj_base <- matrix(0L, n, n)
  adj_base[mat > (threshold + tol)] <- 1L
  tie_mask <- abs(mat - threshold) <= tol

  make_adj <- function(tie_fill) {
    adj <- adj_base
    if (any(tie_mask)) adj[tie_mask] <- tie_fill
    diag(adj) <- 0L
    storage.mode(adj) <- "integer"
    adj
  }

  run_algo <- function(adj) {
    g <- igraph::graph_from_adjacency_matrix(adj, mode = "undirected", diag = FALSE)

    dropped <- integer(0)
    if (remove_isolates) {
      iso <- which(igraph::degree(g) == 0)
      if (length(iso)) {
        dropped <- iso
        g <- igraph::delete_vertices(g, iso)
      }
    }

    if (igraph::vcount(g) == 0) {
      memb <- rep(NA_integer_, n)
      return(list(k = 0L,
                  membership = if (return_membership) memb else NULL,
                  modularity = NA_real_))
    }

    if (!is.null(seed)) set.seed(seed)

    comm <- try(
      switch(algorithm,
             louvain          = igraph::cluster_louvain(g),
             walktrap         = igraph::cluster_walktrap(g, steps = walktrap_steps),
             fast_greedy      = igraph::cluster_fast_greedy(g),
             infomap          = igraph::cluster_infomap(g, nb.trials = infomap_trials),
             label_prop       = igraph::cluster_label_prop(g),
             edge_betweenness = igraph::cluster_edge_betweenness(g, directed = FALSE)
      ),
      silent = TRUE
    )

    if (inherits(comm, "try-error")) {
      comps  <- igraph::components(g)
      memb_g <- comps$membership
      k      <- comps$no
      mod    <- NA_real_
    } else {
      memb_g <- igraph::membership(comm)
      k      <- length(unique(memb_g))
      mod    <- tryCatch(igraph::modularity(comm), error = function(e) NA_real_)
    }

    if (return_membership) {
      memb <- integer(n); memb[] <- NA_integer_
      kept_idx <- setdiff(seq_len(n), dropped)
      memb[kept_idx] <- as.integer(memb_g)
    } else {
      memb <- NULL
    }

    list(k = as.integer(k),
         membership = memb,
         modularity = as.numeric(mod))
  }

  # ---- Branch: ties→0 and ties→1 (if no ties, both identical) ----
  adj0 <- make_adj(0L)
  adj1 <- make_adj(1L)

  if (!any(tie_mask)) {
    res0 <- run_algo(adj0)
    res1 <- res0
    note <- "No values exactly at the threshold; both runs are identical."
  } else {
    res0 <- run_algo(adj0)
    res1 <- run_algo(adj1)
    note <- sprintf(
      "Resolved ties at %.3f in two ways: ties→0 (k=%d) and ties→1 (k=%d).",
      threshold, res0$k, res1$k
    )
  }

  tie_disagreement <- (res0$k != res1$k)
  estimated_k <- if (!tie_disagreement) res0$k else NA_integer_

  list(
    algorithm   = algorithm,
    elicited_no_clusters = estimated_k,
    details = list(
      threshold        = threshold,
      ties_present     = any(tie_mask),
      tie_disagreement = tie_disagreement,
      results_ties0    = list(k = res0$k,
                              membership = res0$membership,
                              modularity = res0$modularity),
      results_ties1    = list(k = res1$k,
                              membership = res1$membership,
                              modularity = res1$modularity),
      note             = note,
      options          = list(
        tol             = tol,
        remove_isolates = remove_isolates,
        return_membership = return_membership,
        seed            = seed,
        walktrap_steps  = walktrap_steps,
        infomap_trials  = infomap_trials
      )
    )
  )
}
