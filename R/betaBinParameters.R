#' Estimate Beta-Bernoulli Parameters from LLM Output
#'
#' This function estimates the parameters of a Beta-Bernoulli distribution from the edge
#' inclusion probabilities elicited using the functions `"elicitEdgeProb"` or `"elicitEdgeProbLite"`.
#' These parameters help in describing the prior probability for the network density.
#' The elicited parameters can be used for specifying the shape parameters
#' of the Beta-Bernoulli structure prior in the package \pkg{easybgm}.
#'
#' @param llmobject An object of class `"elicitEdgeProb"` or `"elicitEdgeProbLite"`,
#' "` as returned by LLM-based prior elicitation functions.
#' @param method Estimation method. One of `"mle"` (maximum likelihood) or `"mom"` (method of moments).
#'   Default is `"mle"`.
#' @param force_mom Logical. If `TRUE`, forces method of moments estimation even if `"mle"` is requested.
#'   Default is `FALSE`.
#'
#' @return A list containing the estimated `alpha` and `beta` parameters of the Beta-Bernoulli distribution.
#'
#' @details
#' The function extracts the number of included edges (`"I"`) for each permutation or repetition
#' from the LLM output and fits a Beta-Bernoulli distribution to these counts. It estimates the
#' shape parameters `alpha` and `beta` based on the specified method. The available estimation methods are:
#' - `"mle"`: Maximum likelihood estimation.
#' - `"mom"`: Method of moments estimation.
#' A warning is issued if fewer than 10 permutations are detected, as parameter estimation
#' may be unreliable in such cases.
#'
#' @examples
#' \dontrun{
#' llm_out <- llmPriorElicitSimple(
#'   context = "Exploring cognitive symptoms and mood in depression",
#'   variable_list = c("Concentration", "Sadness", "Sleep"),
#'   n_rep = 3
#' )
#' beta_params <- betaBinParameters(llm_out)
#' print(beta_params)
#' }
#'
#' @import dplyr
#' @import stringr
#'
#' @seealso \link[=easybgm-package]{\pkg{easybgm}}
#' @export
# Calculate the beta-Bernoulli parameters for the llm object

betaBernParameters <- function(llmobject,
                              method = "mle",
                              force_mom = FALSE) {

  # check if method is "mle" or "mom" or both if not stop the function
  if (!method %in% c("mle", "mom")) {
    stop("Method must be either 'mle' or 'mom'.")
  }

  # check the number of permutations and give a warning message
  if (length(unique(llmobject$raw_LLM$permutation)) < 10) { # we should discuss this
    warning("Consider using more permutations in order to be able to properly estimate the parameters of the Beta distribution")
  }

  # check the class of the llm object
  if (inherits(llmobject, "elicitEdgeProb") ||
      inherits(llmobject, "elicitEdgeProbLite")) {
    df <- llmobject$raw_LLM
    x <- tapply(X = df$content, INDEX = df$pair_index, function(y) length(which(y == "I")))
    n <- max(df$permutation)
  }

  else{
   stop("The input object must be of class 'elicitEdgeProb' or 'elicitEdgeProbLite'.")
  }

  # estimate Beta-Bernoulli parameters
  bb <- estimate_beta_binomial(x = x, n = n, method = method, force_mom = force_mom)

  return(bb)
}  # end of function
