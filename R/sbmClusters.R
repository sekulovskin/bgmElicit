#' Detect Communities from an LLM-Derived Inclusion-Probability Matrix
#'
#' This function estimates the number of clusters (communutues) in the network from the edge
#' inclusion probabilities elicited using the functions `"elicitEdgeProb"` or `"elicitEdgeProbLite"`.
#' The function uses the \code{inclusion_probability_matrix} stored in an object of class
#' \code{"elicitEdgeProb"} or \code{"elicitEdgeProbLite"} to construct a binary
#' adjacency matrix (thresholded at \code{threshold}) and runs community detection algorithm.
#' To assess sensitivity to ties (values exactly at the threshold of 0.5), the procedure
#' is executed twice: once with ties treated as 0 and once with ties treated as 1.
#' The elicited number of cluster can be used for specifying the rate parameter
#' of the Poisson structure prior for the Stochastic Block Model prior in the package \link[easybgm:easybgm]{easybgm}.
#' This rate parameter denotes the expected number of clusters.
#'
#' @details
#' Binarization rule: entries strictly greater than \code{threshold + tol} are set to 1;
#' entries strictly less than \code{threshold} are set to 0; values within \code{tol} of
#' \code{threshold} are considered ties and are resolved in two separate runs (ties->0 and ties->1).
#' Optionally, isolated vertices (degree 0) are removed prior to clustering and their membership is
#' returned as \code{NA} in the original node order. If the elicitation object was generated from
#' only a small number of permutations (e.g., < 5), cluster estimates may be unstable.
#'
#' Supported community detection algorithms: \code{"louvain"}, \code{"walktrap"}, \code{"fast_greedy"},
#' \code{"infomap"}, \code{"label_prop"}, \code{"edge_betweenness"}.
#'
#' @param llmobject An object of class \code{"elicitEdgeProb"} or \code{"elicitEdgeProbLite"}
#'   containing a valid \code{inclusion_probability_matrix} (numeric, symmetric, values in \code{[0,1]},
#'   zero diagonal).
#' @param algorithm Community-detection algorithm. One of
#'   \code{c("louvain","walktrap","fast_greedy","infomap","label_prop","edge_betweenness")}.
#' @param threshold Numeric threshold for binarizing the matrix. Default is \code{0.5}.
#' @param tol Numerical tolerance for equality to the threshold. Default is \code{1e-12}.
#' @param return_membership Logical; if \code{TRUE}, return node-to-cluster membership.
#'   Default is \code{TRUE}.
#' @param remove_isolates Logical; if \code{TRUE}, drop isolated vertices before clustering and set their
#'   membership to \code{NA}. Default is \code{FALSE}.
#' @param seed Optional integer to set the random seed for algorithms with stochastic components (e.g., Infomap).
#' @param walktrap_steps Integer number of steps for Walktrap. Default is \code{4}.
#' @param infomap_trials Integer number of trials for Infomap. Default is \code{10}.
#'
#' @return A list with components:
#' \describe{
#'   \item{algorithm}{The algorithm used.}
#'   \item{elicited_no_clusters}{Estimated number of clusters when the two tie resolutions agree;
#'     otherwise \code{NA}.}
#'   \item{details}{A list with details:
#'     \describe{
#'       \item{threshold}{Threshold used for binarization.}
#'       \item{ties_present}{Logical; whether any entries equaled the threshold (within \code{tol}).}
#'       \item{tie_disagreement}{Logical; whether ties->0 and ties->1 produced different \eqn{k}.}
#'       \item{results_ties0}{List with \code{k}, \code{membership}, and \code{modularity} for ties->0.}
#'       \item{results_ties1}{List with \code{k}, \code{membership}, and \code{modularity} for ties->1.}
#'       \item{note}{Human-readable note describing the tie resolution.}
#'       \item{options}{Echo of key options (\code{tol}, \code{remove_isolates}, \code{return_membership},
#'         \code{seed}, \code{walktrap_steps}, \code{infomap_trials}).}
#'     }}
#' }
#'
#' @examples
#' \dontrun{
#' llm_out <- llmPriorElicitSimple(
#'   context = "Exploring cognitive symptoms and mood in depression",
#'   variable_list = c("Concentration", "Sadness", "Sleep"),
#'   n_rep = 3
#' )
#' cl <- sbmClusters(
#'   llmobject = llm_out,
#'   algorithm = "louvain",
#'   threshold = 0.5
#' )
#' cl$elicited_no_clusters
#' }
#'
#' @importFrom igraph graph_from_adjacency_matrix cluster_louvain cluster_walktrap
#'   cluster_fast_greedy cluster_infomap cluster_label_prop cluster_edge_betweenness
#'   degree delete_vertices vcount membership modularity components
#' @seealso \link[easybgm:easybgm]{easybgm}, \link[igraph:igraph]{igraph}
#' @export

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

  # Optional: warn if few permutations are present
  if (!is.null(llmobject$raw_LLM) && !is.null(llmobject$raw_LLM$permutation)) {
    n_perms <- length(unique(llmobject$raw_LLM$permutation))
    if (n_perms < 5) {
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

  # ---- Branch: ties -> 0 and ties -> 1 (if no ties, both identical) ----
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
      "Resolved ties at %.3f in two ways: ties -> 0 (k=%d) and ties -> 1 (k=%d).",
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
