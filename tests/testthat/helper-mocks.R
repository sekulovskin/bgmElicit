# Test helpers: a mock for the internal callLLM() so the elicitation pipeline
# can be exercised end-to-end without network access or an API key.

# Returns a function with the same signature/return shape as callLLM().
# - decision: the text the "LLM" answers with (e.g. "I", "E", "I.", "banana")
# - with_logprobs: if TRUE (and the caller requested logprobs), attach a
#   first-token top-5 logprobs data frame with P(I) = prob_i, P(E) = 1 - prob_i
mock_callLLM_factory <- function(decision = "I",
                                 with_logprobs = FALSE,
                                 prob_i = 0.9) {
  function(prompt,
           LLM_model = "gpt-4o",
           max_tokens = 2000,
           temperature = 0,
           top_p = 1,
           logprobs = TRUE,
           top_logprobs = 5,
           timeout_sec = 60,
           system_prompt = NULL,
           raw_output = TRUE,
           update_key = FALSE,
           ...) {
    out <- list(
      raw_content = list(
        LLM_model     = LLM_model,
        content       = decision,
        finish_reason = "stop",
        prompt_tokens = 10L,
        answer_tokens = 1L,
        total_tokens  = 11L,
        error         = NULL
      ),
      output = decision
    )
    if (isTRUE(logprobs) && isTRUE(with_logprobs)) {
      first_token_df <- data.frame(
        top5_tokens = c("I", "E"),
        logprob = log(c(prob_i, 1 - prob_i)),
        probability = c(prob_i, 1 - prob_i)
      )
      # Same nesting as callLLM(): list( list-of-per-token-data.frames )
      out$top5_tokens <- list(list(first_token_df))
    }
    out
  }
}

# Synthetic elicitEdgeProb-shaped object for betaBernParameters()/sbmClusters()
make_mock_llmobject <- function(contents,
                                n_pairs,
                                n_perm,
                                class = "elicitEdgeProb") {
  stopifnot(length(contents) == n_pairs * n_perm)
  structure(
    list(
      raw_LLM = data.frame(
        pair_index  = rep(seq_len(n_pairs), each = n_perm),
        permutation = rep(seq_len(n_perm), times = n_pairs),
        content     = contents,
        stringsAsFactors = FALSE
      )
    ),
    class = class
  )
}

skip_if_no_api_key <- function() {
  testthat::skip_if_not(
    nzchar(Sys.getenv("OPENAI_API_KEY")),
    "OpenAI API key not set. Set OPENAI_API_KEY environment variable."
  )
}
