#' Estimate Beta-Bernoulli Parameters from LLM Output
#'
#' This function estimates the parameters of a Beta-Bernoulli distribution from the edge
#' inclusions elicited using the functions `"elicitEdgeProb"` or `"elicitEdgeProbLite"`.
#' These parameters help in describing the prior probability for the network density.
#' The elicited parameters can be used for specifying the shape parameters
#' of the Beta-Bernoulli structure prior in the package \link[easybgm:easybgm]{easybgm}.
#'
#' @param llmobject An object of class `"elicitEdgeProb"` or `"elicitEdgeProbLite"`,
#'  as returned by the functions `"elicitEdgeProb"` or `"elicitEdgeProbLite"`.
#' @param method Estimation method. One of `"mle"` (maximum likelihood) or `"mom"` (method of moments).
#'   Default is `"mle"`.
#' @param force_mom Logical. If `TRUE`, forces method of moments estimation even
#' if `"mle"` is requested.
#'   Default is `FALSE`.
#'
#' @return A list containing the estimated `alpha` and `beta` parameters of the Beta-Bernoulli distribution.
#'
#' @details
#' The function extracts the number of included edges (`"I"`) for each permutation or repetition
#' from the LLM output and fits a Beta-Bernoulli distribution to these counts. A response is
#' counted as an inclusion when its first non-whitespace character is `"I"` or `"i"`. It estimates
#' the shape parameters `alpha` and `beta` based on the specified method. The available estimation
#' methods are:
#' - `"mle"`: Maximum likelihood estimation.
#' - `"mom"`: Method of moments estimation.
#' A warning is issued if fewer than 5 permutations are detected, as parameter estimation
#' may be unreliable in such cases.
#'
#' @examples
#' \dontrun{
#' llm_out <-  elicitEdgeProb(
#'   context = "Exploring cognitive symptoms and mood in depression",
#'   variable_list = c("Concentration", "Sadness", "Sleep"),
#'   n_perm = 5
#' )
#' beta_params <- betaBernParameters(llm_out)
#' print(beta_params)
#' }
#'
#' @seealso \link[easybgm:easybgm]{easybgm}
#' @export

betaBernParameters <- function(llmobject,
                              method = "mle",
                              force_mom = FALSE) {

  # check the class of the llm object
  if (!(inherits(llmobject, "elicitEdgeProb") ||
        inherits(llmobject, "elicitEdgeProbLite"))) {
    stop("The input object must be of class 'elicitEdgeProb' or 'elicitEdgeProbLite'.")
  }

  # check if method is "mle" or "mom" if not stop the function
  if (!method %in% c("mle", "mom")) {
    stop("Method must be either 'mle' or 'mom'.")
  }

  df <- llmobject$raw_LLM
  if (is.null(df) || !is.data.frame(df) ||
      !all(c("content", "pair_index", "permutation") %in% names(df))) {
    stop("`llmobject$raw_LLM` is missing or incomplete; it must be a data frame ",
         "with columns 'content', 'pair_index', and 'permutation'.")
  }

  # check the number of permutations and give a warning message
  if (length(unique(df$permutation)) < 5) {
    warning("Consider using more permutations in order to be able to properly estimate the parameters of the Beta distribution")
  }

  # Count inclusions per pair: a response counts as "I" based on its first
  # non-whitespace character (case-insensitive), consistent with how the
  # elicitation functions interpret responses.
  decisions <- vapply(df$content, extractDecisionChar, character(1), USE.NAMES = FALSE)
  x <- tapply(X = decisions, INDEX = df$pair_index, function(y) sum(y == "i"))
  n <- max(as.integer(df$permutation))

  # estimate Beta-Bernoulli parameters
  bb <- estimate_beta_binomial(x = x, n = n, method = method, force_mom = force_mom)

  return(bb)
}  # end of function
