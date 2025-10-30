mock_openai_response <- function() {
  list(
    choices = list(
      list(
        message = list(
          content = "Variable A and B are related with probability 0.7"
        )
      )
    )
  )
}

skip_if_no_api_key <- function() {
  skip_if_not(
    nzchar(Sys.getenv("OPENAI_API_KEY")),
    "OpenAI API key not set. Set OPENAI_API_KEY environment variable."
  )
}
