# Utility functions

# Some of these functions are adapted from the theoraizer package
# credits: https://github.com/MeikeWaaijers/theoraizer

# logprob helper function
getLLMLogprobs <- function(raw_content,
                         LLM_model = LLM_model) {

  length <- length(raw_content$choices[[1]]$logprobs$content)
  logprobs_dfs <- vector("list", length = length)

  for (j in seq_along(logprobs_dfs)) {
    logprobs_list <- raw_content$choices[[1]]$logprobs$content[[j]]$top_logprobs

    top5_tokens <- vector("list", length = length(logprobs_list))
    top5_logprobs <- numeric(length(logprobs_list))
    top5_probabilities <- numeric(length(logprobs_list))

    for (i in seq_along(logprobs_list)) {
      top5_tokens[[i]] <- logprobs_list[[i]]$token
      top5_logprobs[i] <- logprobs_list[[i]]$logprob

      top5_probabilities[i] <- round(exp(top5_logprobs[i]), 5)
    }

    logprobs_dfs[[j]] <- data.frame(top5_tokens = unlist(top5_tokens),
                                    logprob = top5_logprobs,
                                    probability = top5_probabilities)
  }

  return(logprobs_dfs)
}

# LLM helper function
getApiKey <- function(service_name, update_key = FALSE) {
  # Check if running on shinyapps.io
  shinyapps <- Sys.getenv("R_CONFIG_ACTIVE") == "shinyapps"
  # Check if running shiny app
  if (shinyapps) {
    # Attempt to retrieve the API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")

    if (nzchar(api_key)) {
      return(api_key)
    }
  }

  # Check if running in a CI environment
  ci <- nzchar(Sys.getenv("CI"))

  if (ci) {
    # Attempt to retrieve the API key from environment variable
    api_key <- Sys.getenv("OPENAI_API_KEY")

    if (nzchar(api_key)) {
      return(api_key)
    }
  }

  # If not found in environment variable and not in CI, attempt to retrieve from keyring
  if (!ci && !shinyapps && (update_key || nrow(keyring::key_list(service = service_name)) == 0)) {
    cat("To use this functionality, an API key needs to be set.\n")
    cat("Please follow these steps to resolve the issue:\n")
    cat("1. Create an API key on https://platform.openai.com/account/api-keys \n")
    cat("2. Please enter your API key below to add/update it.")
    answer <- readline("API key = ")
    keyring::key_set_with_value(service = service_name, username = "user", password = answer)
  }

  return(keyring::key_get(service = service_name, username = "user"))
}

#  v1 responses
`%||%` <- function(x, y) if (is.null(x)) y else x

# Extract assistant text from /v1/responses (GPT-5); robust across shapes
.get_text_from_responses <- function(r) {
  # Preferred path: output[] -> message -> content[] -> text
  if (!is.null(r$output) && length(r$output)) {
    for (out in r$output) {
      if (!is.null(out$type) && out$type == "message" && length(out$content)) {
        for (chunk in out$content) {
          if (!is.null(chunk$text) && is.character(chunk$text) && nzchar(chunk$text)) {
            return(chunk$text)
          }
          # Some deployments expose text in 'output_text'
          if (!is.null(chunk$output_text) && is.character(chunk$output_text) && nzchar(chunk$output_text)) {
            return(chunk$output_text)
          }
        }
      }
    }
  }
  # Fallback shapes seen in some builds
  if (!is.null(r$text) && is.character(r$text) && nzchar(r$text)) return(r$text)
  if (!is.null(r$text) && !is.null(r$text$output) && length(r$text$output)) {
    return(paste0(r$text$output, collapse = ""))
  }
  if (!is.null(r$output_text) && length(r$output_text)) {
    return(paste0(r$output_text, collapse = ""))
  }
  NA_character_
}

# main function to call LLM
callLLM <- function(
    prompt,
    LLM_model = "gpt-4o",
    max_tokens = 2000,
    temperature = 0,
    top_p = 1,
    logprobs = TRUE,
    top_logprobs = 5,
    timeout_sec = 60,
    system_prompt = NULL,
    raw_output = TRUE,
    update_key = FALSE
) {
  # Allowed models
  allowed_models <- c(
    "gpt-5", "gpt-5-mini", "gpt-5-nano",
    "gpt-4o", "gpt-4-turbo", "gpt-4", "gpt-3.5-turbo"
  )
  if (!LLM_model %in% allowed_models) {
    stop("Only the following models are supported: ", paste(allowed_models, collapse = ", "))
  }

  api_key <- getApiKey("openai", update_key = isTRUE(update_key))
  is_gpt5 <- grepl("^gpt-5", LLM_model)

  if (is_gpt5) {
    # ---------- GPT-5 via /v1/responses ----------
    # Minimal & tenant-safe: model + input (no temperature/top_p/logprobs/max_output_tokens)
    endpoint <- "https://api.openai.com/v1/responses"

    # Inline system prompt into input for simplicity/compatibility
    combined_input <- if (!is.null(system_prompt)) {
      paste0(system_prompt, "\n\n", prompt)
    } else {
      prompt
    }

    request_body <- list(
      model = LLM_model,
      input = combined_input
    )

    request <- httr::RETRY(
      "POST", endpoint,
      body = request_body, # pass list; httr drops NULLs
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      encode = "json",
      times = 5,
      httr::timeout(timeout_sec)
    )

    status <- httr::status_code(request)
    resp_text <- httr::content(request, as = "text", encoding = "UTF-8")
    if (status >= 300) stop(paste("HTTP", status, "->", resp_text))
    resp <- jsonlite::fromJSON(resp_text, simplifyVector = FALSE)

    output_text <- .get_text_from_responses(resp)

    if (raw_output) {
      return(list(
        raw_content = list(
          LLM_model     = resp$model %||% LLM_model,
          content       = output_text,
          finish_reason = NULL,
          prompt_tokens = resp$usage$input_tokens %||% NA_integer_,
          answer_tokens = resp$usage$output_tokens %||% NA_integer_,
          total_tokens  = resp$usage$total_tokens %||% NA_integer_,
          error         = resp$error %||% NULL
        ),
        output = output_text
      ))
    } else {
      return(output_text)
    }

  } else {
    # ---------- GPT-4o/4-turbo/4/3.5 via /v1/chat/completions ----------
    endpoint <- "https://api.openai.com/v1/chat/completions"

    messages <- list()
    if (!is.null(system_prompt)) {
      messages <- append(messages, list(list(role = "system", content = system_prompt)))
    }
    messages <- append(messages, list(list(role = "user", content = prompt)))

    request_body <- list(
      model        = LLM_model,
      messages     = messages,
      temperature  = temperature,
      top_p        = top_p,
      max_tokens   = max_tokens,
      logprobs     = isTRUE(logprobs),
      top_logprobs = if (isTRUE(logprobs)) top_logprobs else NULL
    )

    request <- httr::RETRY(
      "POST", endpoint,
      body = jsonlite::toJSON(request_body, auto_unbox = TRUE, null = "null"),
      httr::add_headers(
        Authorization = paste("Bearer", api_key),
        `Content-Type` = "application/json"
      ),
      encode = "raw",
      times = 5,
      httr::timeout(timeout_sec)
    )

    status <- httr::status_code(request)
    resp_text <- httr::content(request, as = "text", encoding = "UTF-8")
    if (status >= 300) stop(paste("HTTP", status, "->", resp_text))
    resp <- jsonlite::fromJSON(resp_text, simplifyVector = FALSE)

    output_text <- resp$choices[[1]]$message$content

    if (raw_output && isTRUE(logprobs)) {
      # Uses your existing chat/completions parser
      top5_logprobs <- getLLMLogprobs(raw_content = resp, LLM_model = LLM_model)
      return(list(
        raw_content = list(
          LLM_model     = resp$model %||% LLM_model,
          content       = output_text,
          finish_reason = resp$choices[[1]]$finish_reason %||% NULL,
          prompt_tokens = resp$usage$prompt_tokens %||% NA_integer_,
          answer_tokens = resp$usage$completion_tokens %||% NA_integer_,
          total_tokens  = resp$usage$total_tokens %||% NA_integer_,
          error         = resp$error %||% NULL
        ),
        top5_tokens = list(top5_logprobs),
        output = output_text
      ))
    }

    if (raw_output && !isTRUE(logprobs)) {
      return(list(
        raw_content = list(
          LLM_model     = resp$model %||% LLM_model,
          content       = output_text,
          finish_reason = resp$choices[[1]]$finish_reason %||% NULL,
          prompt_tokens = resp$usage$prompt_tokens %||% NA_integer_,
          answer_tokens = resp$usage$completion_tokens %||% NA_integer_,
          total_tokens  = resp$usage$total_tokens %||% NA_integer_,
          error         = resp$error %||% NULL
        ),
        output = output_text
      ))
    }

    return(output_text)
  }
}



# Helper function to parse decision from LLM output
parseDecision <- function(content) {
  # Simple parsing - look for "I" or "E" in the output
  content <- tolower(trimws(content))
  if (grepl("i", content)) {
    return("I")
  } else if (grepl("e", content)) {
    return("E")
  } else {
    return(NA)  # Handle cases where output doesn't contain I/E
  }
}



# function to estimate beta-binomial parameters

estimate_beta_binomial <- function(x, n, method = c("mle", "mom"), force_mom = FALSE) {
  method <- match.arg(method)

  # the model is
  # p ~ Beta(alpha,beta) x|p ~ Binomial(n,p)
  # where:
  # - "n" is the number of premutations
  # - "x" is the vector of success (sum of I's) across iterations (or permutations)

  if (any(x < 0) || any(x > n)) {
    stop("All values of x must be between 0 and n.")
  }

  # alpha and beta estimated using method of moments (mom)
  # Mean: E[X] = E[E[X|p]] = E[n * p] = n * E[p] = n * ( alpha/(alpha + beta) )
  # Second factorial moment: E[X(X-1)] = n*(n-1) * [(alpha*(alpha+1))/( (alpha+beta)*(alpha+beta+1) )]

  # Only compute MoM when we actually need to return MoM (and are not forcing MLE)
  if (method == "mom" && !force_mom) {
    m1  <- mean(x)/n
    m2f <- mean(x*(x-1))/(n*(n-1))

    denom <- m2f - m1^2
    if (denom <= 0) {
      alpha <- beta <- NA
    } else {
      s     <- (m1 - m2f)/denom
      alpha <- m1 * s
      beta  <- (1 - m1) * s
    }

    # For MoM return, we don't need optimizer init; just return below.
    init <- c(0, 0) # placeholder, unused in this branch
  } else {
    # Skipping MoM computation; set a reasonable starting point for MLE directly
    alpha <- beta <- NA
    init <- log(c(mean(x) + 1, n - mean(x) + 1))  # reasonable starting point
  }

  # negative loglikelihood
  beta_binom_fun <- function(pars, x, n){
    alpha <- exp(pars[1])
    beta <- exp(pars[2])

    # value of the loglikelihood calculated at pars (excluding log(choose(n,x))
    value <- sum(lgamma(x+alpha)+lgamma(n-x+beta)-lgamma(n+alpha+beta)-lgamma(alpha)-lgamma(beta)+lgamma(alpha+beta))

    # digamma and trigamma functions used in first and second
    # perhaps computing them once here and then use them later?

    # Score function
    gradient <- rep(0.0,2)
    gradient[1] <- sum(digamma(x+alpha) - digamma(n+alpha+beta) - digamma(alpha) + digamma(alpha+beta))*alpha
    gradient[2] <- sum(digamma(n-x+beta) - digamma(n+alpha+beta) - digamma(beta) + digamma(alpha+beta))*beta

    # Hessian matrix
    hessian <- matrix(0.0,2,2)
    hessian[1,1] <- sum(trigamma(x+alpha) - trigamma(n+alpha+beta) - trigamma(alpha) + trigamma(alpha+beta))*alpha^2 + gradient[1]
    hessian[2,2] <- sum(trigamma(n-x+beta) - trigamma(n+alpha+beta) - trigamma(beta) + trigamma(alpha+beta))*beta^2 + gradient[2]
    hessian[1,2] <- sum(trigamma(alpha+beta) - trigamma(n+alpha+beta))*alpha*beta
    hessian[2,1] <- hessian[1,2]

    return(list(value = -value, gradient = -gradient, hessian = -hessian)) # return negative because negative loglikelihood
  }

  # output depending on what method is requested
  if (method == "mle" || (force_mom && method == "mom")) {
    # Run optimizer only in the MLE branch
    fit <- trust::trust(objfun = beta_binom_fun, parinit = init, x = x, n = n, rinit = 0.1, rmax = 10.0)
    alpha_mle <- exp(fit$argument[1])
    beta_mle  <- exp(fit$argument[2])
    return(list(mle = c("alpha" = alpha_mle, "beta" = beta_mle)))
  }
  if (method == "mom") {
    # If MoM invalid, keep your original warning + NA behavior
    if ((alpha <= 0 || beta <= 0) || (is.na(alpha) || is.na(beta))) {
      warning("Invalid MoM estimates (possibly due to low variance).")
    }
    return(list(mom = c("alpha" = alpha, "beta" = beta)))
  }
}
