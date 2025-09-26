#' Elicit Prior Edge-Inclusion Probabilities with a LLM - Lightweight Version
#'
#' This function is a lightweight version of the `elicitEdgeProb` function.
#' In case users are willing to spend more time and budget we recommend using
#' `elicitEdgeProb` instead.
#' This function queries a large language model (LLM) to elicit prior probabilities
#'  for the presence of conditional associations (edges) between pairs of variables
#' (nodes) in a Markov random field (MRF) graphical model, as used for
#' psychological network models within the Bayesian Graphical Modeling (BGM)
#' framework. In this function each variable pair is evaluated while only
#' considering the remaining variables in the network, but without taking into
#'  account the LLM's decision for the variable pairs of those remaining variables.
#' Permutations are applied to the ordering of the remaining variables.
#' The output is useful for specifying prior inclusion probabilities in
#' the package \link[easybgm:easybgm]{easybgm}. The output from this function
#' can also be used toelicit the hyperparameters for the Beta-Bernoulli prior
#' using the function `betaBernParameters`, or the expected number of clusters
#'  for the Stochastic Block prior using the function `sbmClusters`.
#'
#' @details
#' The function iterates over all variable pairs and provides a list of the
#' remaining variables in the network. It allows for different permutations
#' of the remaining variables to be evaluated, which can help in eliciting
#' more robust probabilities.
#' The LLM is constrained to return binary decisions:`"I"` (include)
#' or `"E"` (exclude); to facilitate stable probability elicitation.
#'
#' When supported, token log-probabilities are requested and used for calculating
#' the final elicited prior probabilities. If log-probabilities are not
#' supported by the selected model, or if the log-probabilities for `"I"`
#' and `"E"` are both zero, the function falls back to using the hard decision
#' from the text output. If the text output is not interpretable, a default
#' probability of `0.5` is assigned.
#'
#' @param context Optional character string with study background or domain
#'   context to incorporate into the prompt. Defaults to `NULL`.
#' @param variable_list Character vector of variable (node) names; must contain
#'   at least three variables.
#' @param LLM_model Character string selecting the LLM. Options include
#'   `"gpt-4"`, `"gpt-4o"`, `"gpt-4-turbo"`, `"gpt-3.5-turbo"`, `"gpt-5"`,
#'   `"gpt-5-mini"`, and `"gpt-5-nano"`.
#' @param update_key Logical; if `TRUE`, refreshes the API key prior to the LLM
#'   call. Only the first call uses the updated key. Default is `FALSE`.
#' @param n_perm Integer or `NULL`. Number of random permutations of pair order
#'   to evaluate. If `NULL`, five permutations are used by default. Maximum is
#'   `50`.
#' @param seed Integer random seed for reproducibility of permutations.
#'   Default is `123`.
#' @param main_prompt Optional length-1 character vector used as the LLM system
#'   prompt. If `NULL`, a sensible default is used.
#' @param display_progress Logical; if `TRUE`, show progress messages.
#'   Default is `TRUE`.
#' @param logprobs Logical; if `TRUE`, request token log-probabilities for the
#'   first decision token. Ignored for models that do not support logprobs.
#'   Default is `FALSE`.
#'
#' @return A list of class `"elicitEdgeProbLite"` with components:
#' \describe{
#'  \item{raw_LLM}{Data frame of raw prompts, responses, and (when available)
#'   token log-probabilities.}
#'  \item{diagnostics}{List with counts and summary information about modes and
#'   decision pathways used during elicitation.}
#'  \item{relation_df}{Data frame with columns `var1`, `var2`, and `prob`,
#'   containing elicited prior probabilities of conditional associations.}
#'  \item{arguments}{List of input arguments for reproducibility.}
#'  \item{inclusion_probability_matrix}{Symmetric matrix of edge-inclusion
#'   probabilities suitable for a Bernoulli prior in \link[easybgm:easybgm]{easybgm}.
#'   Exact 0/1 values are squashed to `0.01`/`0.99`.}
#' }
#' @examples
#' \dontrun{
#' result <- elicitEdgeProbLite(
#'   context       = "Study on anxiety, sleep, and concentration.",
#'   variable_list = c("Anxiety", "Sleep", "Concentration"),
#'   LLM_model     = "gpt-4o",
#'   n_perm        = 2
#' )
#' print(result$relation_df)
#' }
#'
#' @seealso \link[easybgm:easybgm]{easybgm}
#' @export

elicitEdgeProbLite <- function(
    context,
    variable_list,
    LLM_model = "gpt-5",
    update_key = FALSE,
    n_perm = NULL,
    seed = 123,
    main_prompt = NULL,
    display_progress = TRUE,
    logprobs = FALSE
) {
  # ---------- helpers ----------
  `%||%` <- function(x, y) if (is.null(x)) y else x

  model_supports_logprobs <- function(m) {
    grepl("^gpt-4o$|^gpt-4-turbo$|^gpt-4$|^gpt-3\\.5-turbo$", m)
  }

  extract_decision_char <- function(txt) {
    ch <- substr(trimws(txt %||% ""), 1, 1)
    if (!nzchar(ch)) return("?")
    ch <- tolower(ch)
    if (ch %in% c("i", "e")) ch else "?"
  }

  # safely fetch the first-token logprobs data.frame if present & non-empty
  safe_first_token_df <- function(topk_list) {
    if (is.null(topk_list) || !is.list(topk_list) || length(topk_list) < 1) return(NULL)
    ft <- topk_list[[1]]
    if (is.null(ft) || !is.data.frame(ft) || NROW(ft) == 0) return(NULL)
    ft
  }

  # NEW: safe accessor to avoid [[idx]] on NULL/short lists
  get_topk_for <- function(lst, idx) {
    if (is.null(lst)) return(NULL)
    if (!is.list(lst)) return(NULL)
    if (length(lst) < idx) return(NULL)
    lst[[idx]]
  }

  # ---------- validation ----------
  stopifnot(is.character(context) | is.null(context))
  stopifnot(is.vector(variable_list) && length(variable_list) >= 3)
  stopifnot(all(sapply(variable_list, is.character)))

  # ---------- all unordered pairs ----------
  pairs_df <- data.frame(var1 = character(), var2 = character())
  for (i in 1:(length(variable_list) - 1)) {
    for (j in (i + 1):length(variable_list)) {
      pairs_df <- rbind(pairs_df, data.frame(var1 = variable_list[[i]], var2 = variable_list[[j]]))
    }
  }
  n_pairs <- nrow(pairs_df)

  # ---------- permutations count ----------
  if (missing(n_perm)) {
    n_perm <- 5
    message("The n_perm argument was not specified. The function will proceed using five permutations of the remaining variables.")
  }
  if (!missing(n_perm) && n_perm == 0) stop("n_perm cannot be zero.")
  if (n_perm > 50) stop("Requested `n_perm` (", n_perm, ") exceeds maximum possible permutations which is set to 50.")

  set.seed(seed)

  # ---------- prompts ----------
  system_prompt <- if (!is.null(main_prompt)) {
    paste(main_prompt, collapse = " ")
  } else {
    "You are an expert in using graphical models to study psychological constructs. \
You will classify whether there is a conditional relationship between pairs of variables in a Markov random field graphical model applied to psychological research. \
If there is a conditional association (edge present), output 'I'. If there is no conditional association (edge absent), output 'E'. \
Only output a single character: 'I' or 'E'. Consider the remaining variables."
  }

  # ---------- config ----------
  use_logprobs <- isTRUE(logprobs) && model_supports_logprobs(LLM_model)

  # ---------- storage ----------
  raw_LLM        <- vector("list", n_pairs)
  logprobs_LLM   <- vector("list", n_pairs)
  mode_used_mat  <- matrix("fallback", nrow = n_pairs, ncol = n_perm)  # "logprobs" or "fallback"

  # ---------- main loop over pairs ----------
  for (i in 1:n_pairs) {
    var1 <- pairs_df[i, 1]
    var2 <- pairs_df[i, 2]
    remaining_vars <- setdiff(variable_list, c(var1, var2))

    # Build list of permutations for remaining variables
    if (length(remaining_vars) >= 2) {
      perms <- matrix(nrow = 0, ncol = length(remaining_vars))
      while (nrow(perms) < n_perm) {
        new_perm <- sample(remaining_vars, length(remaining_vars), replace = FALSE)
        if (nrow(perms) == 0 || !any(apply(perms, 1, function(x) all(x == new_perm)))) {
          perms <- rbind(perms, new_perm)
        }
      }
      remaining_vars_list <- lapply(1:n_perm, function(p) perms[p, ])
    } else if (length(remaining_vars) == 1 && n_perm == 1) {
      remaining_vars_list <- list(remaining_vars)
    } else {
      remaining_vars_list <- replicate(n_perm, remaining_vars, simplify = FALSE)
    }

    raw_LLM_pair      <- vector("list", n_perm)  # pre-allocate
    logprobs_LLM_pair <- vector("list", n_perm)  # pre-allocate

    for (perm in 1:n_perm) {
      remaining_vars_str <- paste(remaining_vars_list[[perm]], collapse = ", ")
      prompt <- paste0(
        if (!is.null(context)) paste0("Context: ", context, "\n") else "",
        "Current pair: '", var1, "' & '", var2, "'\n",
        "Remaining variables: ", remaining_vars_str, "\n",
        "Respond ONLY with 'I' (included) or 'E' (excluded). Do not write anything else."
      )

      if (isTRUE(display_progress)) {
        message(paste0("Processing pair ", i, "/", n_pairs, ", permutation ", perm, ": ", var1, " - ", var2))
      }

      # ---- LLM call ----
      LLM_output <- callLLM(
        prompt        = prompt,
        LLM_model     = LLM_model,
        max_tokens    = 1,                # callLLM handles GPT-5 specifics internally
        temperature   = 0,
        logprobs      = use_logprobs,
        raw_output    = TRUE,
        system_prompt = system_prompt,
        update_key    = update_key
      )
      update_key <- FALSE

      content_txt <- LLM_output$output %||% LLM_output$raw_content$content %||% ""

      raw_LLM_pair[[perm]] <- c(
        pair_index    = i,
        permutation   = perm,
        prompt        = prompt,
        system_prompt = system_prompt,
        LLM_model     = LLM_output$raw_content$LLM_model %||% LLM_model,
        content       = content_txt,
        finish_reason = LLM_output$raw_content$finish_reason %||% NA_character_,
        prompt_tokens = LLM_output$raw_content$prompt_tokens %||% NA_integer_,
        answer_tokens = LLM_output$raw_content$answer_tokens %||% NA_integer_,
        total_tokens  = LLM_output$raw_content$total_tokens  %||% NA_integer_,
        error         = LLM_output$raw_content$error %||% NA
      )

      # Store logprobs only if available AND non-empty
      has_logprobs_df <-
        isTRUE(use_logprobs) &&
        !is.null(LLM_output$top5_tokens) &&
        length(LLM_output$top5_tokens) >= 1 &&
        is.data.frame(LLM_output$top5_tokens[[1]]) &&
        NROW(LLM_output$top5_tokens[[1]]) > 0

      if (has_logprobs_df) {
        logprobs_LLM_pair[[perm]] <- LLM_output$top5_tokens      # list(data.frame)
        mode_used_mat[i, perm] <- "logprobs"
      } else {
        logprobs_LLM_pair[[perm]] <- NULL
        mode_used_mat[i, perm] <- "fallback"
      }
    }

    raw_LLM[[i]]      <- raw_LLM_pair
    logprobs_LLM[[i]] <- logprobs_LLM_pair
  }

  # ---------- probability aggregation (guarded) ----------
  prob_matrix  <- matrix(NA_real_, nrow = n_pairs, ncol = n_perm)
  n_default_05 <- 0

  for (i in 1:n_pairs) {
    for (perm in 1:n_perm) {

      # SAFE: never index [[perm]] on a NULL/short list
      topk_list_for_call <- get_topk_for(logprobs_LLM[[i]], perm)
      first_token_df <- safe_first_token_df(topk_list_for_call)

      if (!is.null(first_token_df)) {
        prob_i <- 0; prob_e <- 0
        for (m in seq_len(nrow(first_token_df))) {
          token <- trimws(tolower(first_token_df$top5_tokens[m]))
          if (token == "i") prob_i <- prob_i + as.numeric(first_token_df$probability[m])
          if (token == "e") prob_e <- prob_e + as.numeric(first_token_df$probability[m])
        }
        if (prob_i + prob_e > 0) {
          prob_matrix[i, perm] <- prob_i / (prob_i + prob_e)
        } else {
          prob_matrix[i, perm] <- 0.5; n_default_05 <- n_default_05 + 1
        }
      } else {
        temp_text <- raw_LLM[[i]][[perm]][["content"]]
        temp_text <- if (is.null(temp_text)) "" else temp_text
        dchr <- tolower(substr(trimws(temp_text), 1, 1))
        if (dchr == "i") {
          prob_matrix[i, perm] <- 1
        } else if (dchr == "e") {
          prob_matrix[i, perm] <- 0
        } else {
          prob_matrix[i, perm] <- 0.5; n_default_05 <- n_default_05 + 1
        }
      }
    }
  }

  message(paste0("Number of edges defaulted to 0.5 (no usable signal): ",
                 n_default_05, " out of ", n_perm * n_pairs, " total."))

  avg_probs <- rowMeans(prob_matrix, na.rm = TRUE)
  prob_relation_df <- data.frame(
    var1 = pairs_df[, 1],
    var2 = pairs_df[, 2],
    prob = avg_probs,
    row.names = NULL
  )

  # ---------- flatten raw_LLM ----------
  output <- list()
  tryCatch({
    flattened_df_raw_LLM <- data.frame(
      pair_index = integer(),
      permutation = integer(),
      var1 = character(),
      var2 = character(),
      LLM_model = character(),
      prompt = character(),
      system_prompt = character(),
      content = character(),
      mode_used = character(),
      finish_reason = character(),
      prompt_tokens = numeric(),
      answer_tokens = numeric(),
      total_tokens = numeric(),
      error = character(),
      stringsAsFactors = FALSE
    )

    for (i in seq_along(raw_LLM)) {
      for (perm in seq_along(raw_LLM[[i]])) {
        temp <- raw_LLM[[i]][[perm]]
        flattened_df_raw_LLM <- rbind(
          flattened_df_raw_LLM,
          data.frame(
            pair_index    = as.integer(temp[["pair_index"]] %||% i),
            permutation   = as.integer(temp[["permutation"]] %||% perm),
            var1          = pairs_df[i, 1],
            var2          = pairs_df[i, 2],
            LLM_model     = temp[["LLM_model"]] %||% LLM_model,
            prompt        = temp[["prompt"]] %||% "",
            system_prompt = temp[["system_prompt"]] %||% "",
            content       = temp[["content"]] %||% "",
            mode_used     = mode_used_mat[i, perm],
            finish_reason = temp[["finish_reason"]] %||% NA_character_,
            prompt_tokens = as.numeric(temp[["prompt_tokens"]] %||% NA),
            answer_tokens = as.numeric(temp[["answer_tokens"]] %||% NA),
            total_tokens  = as.numeric(temp[["total_tokens"]] %||% NA),
            error         = ifelse(is.null(temp[["error"]]), NA, temp[["error"]]),
            stringsAsFactors = FALSE
          )
        )
      }
    }
    output$raw_LLM <- flattened_df_raw_LLM
  }, error = function(e) {
    cat(paste0("Warning: Unable to return raw LLM output -> ", e$message, "."),
        "Only part of the output is returned.", sep = "\n")
  })

  # ---------- diagnostics ----------
  diag_tbl <- table(mode_used_mat)
  output$diagnostics <- list(
    mode_counts = as.list(diag_tbl),
    defaults_0p5 = n_default_05
  )

  # ---------- assemble ----------
  output$relation_df <- prob_relation_df
  message(paste0("Total of LLM prompts: ", n_pairs * n_perm))
  output$arguments <- list(
    context = context,
    variable_list = variable_list,
    LLM_model = LLM_model,
    update_key = update_key,
    n_perm = n_perm,
    seed = seed,
    n_default_05 = n_default_05,
    logprobs_requested = isTRUE(logprobs),
    logprobs_used = isTRUE(use_logprobs)
  )

  # -------- symmetric p x p inclusion-prob matrix ----------
  p <- length(variable_list)
  prob_matrix_sym <- matrix(0, nrow = p, ncol = p,
                            dimnames = list(variable_list, variable_list))
  for (i in 1:n_pairs) {
    v1 <- pairs_df[i, 1]; v2 <- pairs_df[i, 2]
    val <- avg_probs[i]
    prob_matrix_sym[v1, v2] <- val
    prob_matrix_sym[v2, v1] <- val
  }
  prob_matrix_sym[prob_matrix_sym == 1] <- 0.99
  prob_matrix_sym[prob_matrix_sym == 0] <- 0.01
  diag(prob_matrix_sym) <- 0
  output$inclusion_probability_matrix <- prob_matrix_sym

  class(output) <- "elicitEdgeProbLite"
  return(output)
}
