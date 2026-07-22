#' Print Method for elicitEdgeProb Objects
#'
#' Compact display of the elicited prior edge-inclusion probabilities.
#'
#' @param x An object of class `"elicitEdgeProb"`.
#' @param ... Further arguments passed to or from other methods (unused).
#'
#' @return `x`, invisibly.
#' @export
print.elicitEdgeProb <- function(x, ...) {
  cat("Elicited prior edge-inclusion probabilities (elicitEdgeProb)\n")
  cat("  LLM model:     ", x$arguments$LLM_model %||% "unknown", "\n", sep = "")
  cat("  Permutations:  ", x$arguments$n_perm %||% NA, "\n", sep = "")
  cat("  Logprobs used: ", isTRUE(x$arguments$logprobs_used), "\n", sep = "")
  cat("  Defaults to 0.5 (no usable signal): ", x$diagnostics$defaults_0p5 %||% 0, "\n\n", sep = "")
  print(x$relation_df)
  cat("\nAccess the full output via $raw_LLM, $diagnostics, and $inclusion_probability_matrix.\n")
  invisible(x)
}

#' Print Method for elicitEdgeProbLite Objects
#'
#' Compact display of the elicited prior edge-inclusion probabilities.
#'
#' @param x An object of class `"elicitEdgeProbLite"`.
#' @param ... Further arguments passed to or from other methods (unused).
#'
#' @return `x`, invisibly.
#' @export
print.elicitEdgeProbLite <- function(x, ...) {
  cat("Elicited prior edge-inclusion probabilities (elicitEdgeProbLite)\n")
  cat("  LLM model:     ", x$arguments$LLM_model %||% "unknown", "\n", sep = "")
  cat("  Permutations:  ", x$arguments$n_perm %||% NA, "\n", sep = "")
  cat("  Logprobs used: ", isTRUE(x$arguments$logprobs_used), "\n", sep = "")
  cat("  Defaults to 0.5 (no usable signal): ", x$diagnostics$defaults_0p5 %||% 0, "\n\n", sep = "")
  print(x$relation_df)
  cat("\nAccess the full output via $raw_LLM, $diagnostics, and $inclusion_probability_matrix.\n")
  invisible(x)
}
