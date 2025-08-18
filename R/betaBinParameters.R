#' Estimate Beta-Binomial Parameters from LLM Output
#'
#' This function computes the parameters of a Beta-Binomial distribution from the edge
#' inclusion outputs of an LLM-based prior elicitation object. The estimated parameters
#' describe the distribution of edge counts (network density) across permutations or repetitions.
#'
#' @param llmobject An object of class `"elicitEdgeProb"` or `"elicitEdgeProbLite"`,
#' "` as returned by LLM-based prior elicitation functions.
#' @param method Estimation method. One of `"mle"` (maximum likelihood) or `"mom"` (method of moments).
#'   Default is `"mle"`.
#' @param force_mom Logical. If `TRUE`, forces method of moments estimation even if `"mle"` is requested.
#'   Default is `FALSE`.
#'
#' @return A list containing the estimated `alpha` and `beta` parameters of the Beta-Binomial distribution.
#'
#' @details
#' The function extracts the number of included edges (`"I"`) for each permutation or repetition
#' from the LLM output and fits a Beta-Binomial distribution to these counts. This gives a probabilistic
#' description of edge inclusion uncertainty across network realizations.
#'
#' For `` objects, edge inclusion is determined from
#' the raw content per iteration. For `llmPriorElicitRelations` objects, edge inclusion is extracted
#' from the full I/E sequence in the final output for each permutation.
#'
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
#' @export
#' @name betaBinParameters
# Calculate the beta-binomial parameters for the llm object
library(dplyr)
library(stringr)  # we will have to think about these dependencies when making
# this a package

betaBinParameters <- function(llmobject,
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

  # estimate Beta-Binomial parameters
  bb <- estimate_beta_binomial(x = x, n = n, method = method, force_mom = force_mom)

  return(bb)
}  # end of function
