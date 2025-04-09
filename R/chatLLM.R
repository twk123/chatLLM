# chatLLM.R
# ------------------------------------------------------------------------------
# A flexible LLM calling utility that supports multiple providers:
#   - OpenAI (e.g., GPT-3.5-turbo, GPT-4)
#   - Groq (e.g., mixtral-8x7b-32768)
#   - Anthropic (e.g., Claude 3)
#
# API Keys must be set in the environment:
#   - Sys.setenv(OPENAI_API_KEY    = "your_openai_key")
#   - Sys.setenv(GROQ_API_KEY      = "your_groq_key")
#   - Sys.setenv(ANTHROPIC_API_KEY = "your_anthropic_key")
# ------------------------------------------------------------------------------
#' @importFrom httr POST add_headers http_error content
#' @importFrom jsonlite toJSON fromJSON
library(httr)
library(jsonlite)

# ------------------------------------------------------------------------------
# Helper: return default model names
# ------------------------------------------------------------------------------
get_default_model <- function(provider) {
  switch(
    provider,
    "openai"    = "gpt-3.5-turbo",
    "groq"      = "mixtral-8x7b-32768",
    "anthropic" = "claude-3-opus-20240229",
    stop("No default model for provider: ", provider)
  )
}

# ------------------------------------------------------------------------------
# Helper: fetch the API key from environment or user input
# ------------------------------------------------------------------------------
get_api_key <- function(provider, api_key = NULL) {
  env_var <- switch(
    provider,
    "openai"    = "OPENAI_API_KEY",
    "groq"      = "GROQ_API_KEY",
    "anthropic" = "ANTHROPIC_API_KEY",
    stop("No environment variable defined for provider: ", provider)
  )
  if (is.null(api_key)) {
    api_key <- Sys.getenv(env_var)
  }
  if (!nzchar(api_key)) {
    stop(sprintf("API key not found. Please set %s or provide `api_key`.", env_var))
  }
  api_key
}

# ------------------------------------------------------------------------------
# Helper: parse the text from different provider responses
# ------------------------------------------------------------------------------
parse_response <- function(provider, content) {
  switch(
    provider,
    "openai"    = content$choices[[1]]$message$content,
    "groq"      = content$choices[[1]]$message$content,
    "anthropic" = content$content[[1]]$text,
    stop("Parsing not implemented for provider: ", provider)
  )
}

# ------------------------------------------------------------------------------
# Main function: call_llm()
# ------------------------------------------------------------------------------
#' Call a Large Language Model with optional retries & extra parameters
#'
#' @param prompt (character) A single user prompt. If `messages` provided, prompt
#'   is appended as a user message (optional).
#' @param messages (list) A list of message objects, each a list(role, content).
#' @param provider (character) One of "openai", "groq", or "anthropic".
#' @param model (character) The model name. Defaults to a known model per provider.
#' @param temperature (numeric) Sampling temperature (0 to ~2).
#' @param max_tokens (integer) Maximum number of tokens to generate.
#' @param api_key (character) An API key. If NULL, uses the environment variable.
#' @param n_tries (integer) Number of attempts for retry on failure.
#' @param backoff (numeric) Seconds to wait between retries.
#' @param endpoint_url (character) Base URL for the provider. If NULL,
#'   uses a default for the given provider.
#' @param ... Additional parameters appended to the request body.
#' @param .post_func (function) (Internal) Function to perform POST requests.
#'   Defaults to \code{httr::POST}.
#'
#' @return (character) The text completion from the chosen model.
#' @examples
#' \dontrun{
#' response <- call_llm(
#'   prompt = "Hello, how are you?",
#'   provider = "openai",
#'   max_tokens = 50,
#'   n_tries = 3,
#'   backoff = 2
#' )
#' cat(response)
#'
#' conv <- list(
#'   list(role = "system", content = "You are a helpful assistant."),
#'   list(role = "user",   content = "Explain recursion in R.")
#' )
#' response <- call_llm(
#'   messages = conv,
#'   provider = "openai",
#'   max_tokens = 200,
#'   presence_penalty = 0.2,
#'   frequency_penalty = 0.1,
#'   top_p = 0.95,
#'   stop = c("###")
#' )
#' cat(response)
#' }
#' @export
call_llm <- function(
    prompt        = NULL,
    messages      = NULL,
    provider      = c("openai", "groq", "anthropic"),
    model         = NULL,
    temperature   = 0.7,
    max_tokens    = 1000,
    api_key       = NULL,
    n_tries       = 3,
    backoff       = 2,
    endpoint_url  = NULL,
    ...,
    .post_func    = httr::POST
) {
  provider <- match.arg(tolower(provider), c("openai", "groq", "anthropic"))

  # Default model if not specified
  if (is.null(model)) {
    model <- get_default_model(provider)
  }

  # Build final messages list
  if (!is.null(messages)) {
    if (!is.null(prompt)) {
      messages <- c(messages, list(list(role = "user", content = prompt)))
    }
  } else {
    if (is.null(prompt)) stop("Must provide either `prompt` or `messages`.")
    messages <- list(list(role = "user", content = prompt))
  }

  # Fetch API key
  api_key <- get_api_key(provider, api_key)

  # Base request body
  req_body <- switch(
    provider,
    "openai" = list(
      model       = model,
      messages    = messages,
      temperature = temperature,
      max_tokens  = max_tokens
    ),
    "groq" = list(
      model       = model,
      messages    = messages,
      temperature = temperature,
      max_tokens  = max_tokens
    ),
    "anthropic" = list(
      model       = model,
      messages    = messages,
      temperature = temperature,
      max_tokens  = max_tokens
    )
  )

  extra_args <- list(...)
  if (length(extra_args) > 0) {
    req_body <- c(req_body, extra_args)
  }

  # Set headers
  req_headers <- switch(
    provider,
    "openai" = httr::add_headers(Authorization = paste("Bearer", api_key)),
    "groq" = httr::add_headers(
      Authorization = paste("Bearer", api_key),
      "Content-Type" = "application/json"
    ),
    "anthropic" = httr::add_headers(
      "x-api-key" = api_key,
      "anthropic-version" = "2023-06-01",
      "Content-Type" = "application/json"
    )
  )

  if (is.null(endpoint_url)) {
    endpoint_url <- switch(
      provider,
      "openai"    = "https://api.openai.com/v1/chat/completions",
      "groq"      = "https://api.groq.com/openai/v1/chat/completions",
      "anthropic" = "https://api.anthropic.com/v1/messages"
    )
  }

  message(sprintf(
    "Calling %s [%s] at %s with %d messages. Temp=%.1f, max_tokens=%d, attempts=%d",
    provider, model, endpoint_url, length(messages), temperature, max_tokens, n_tries
  ))

  # Retry mechanism using .post_func
  res <- NULL
  for (i in seq_len(n_tries)) {
    res <- tryCatch({
      .post_func(url = endpoint_url, encode = "json", body = req_body, req_headers)
    }, error = function(e) {
      if (i == n_tries) {
        stop(sprintf("Error calling %s on attempt %d: %s", provider, i, e$message))
      } else {
        message(sprintf(
          "Attempt %d of %d failed for %s. Retrying in %d seconds...\n%s",
          i, n_tries, provider, backoff, e$message
        ))
        Sys.sleep(backoff)
        return(NULL)
      }
    })

    if (is.null(res)) next

    if (!httr::http_error(res)) {
      break
    } else {
      if (i == n_tries) {
        stop(sprintf("%s API error on attempt %d: %s",
                     provider, i, httr::content(res, "text")))
      } else {
        message(sprintf(
          "HTTP error on attempt %d of %d for %s. Retrying in %d seconds...",
          i, n_tries, provider, backoff
        ))
        Sys.sleep(backoff)
      }
    }
  }

  parsed_content <- httr::content(res, "parsed")
  response_text <- parse_response(provider, parsed_content)
  snippet <- substr(response_text, 1, min(200, nchar(response_text)))
  message(sprintf("Response from %s (truncated):\n%s", provider, snippet))
  return(response_text)
}


# ------------------------------------------------------------------------------
# Example usage (uncomment & ensure environment variables are set properly)
# ------------------------------------------------------------------------------
# # Single prompt, no fancy parameters
# Sys.setenv(OPENAI_API_KEY = "sk-xxxx")
# response <- call_llm(
#   prompt = "Hello, how are you?",
#   provider = "openai",
#   max_tokens = 50,
#   n_tries = 3,
#   backoff = 2
# )
# cat(response)
#
# # Multi-message conversation plus advanced parameters
# conv <- list(
#   list(role = "system", httr::content = "You are a helpful assistant."),
#   list(role = "user",   httr::content = "Explain recursion in R.")
# )
# response <- call_llm(
#   messages    = conv,
#   provider    = "openai",
#   max_tokens  = 200,
#   presence_penalty  = 0.2,
#   frequency_penalty = 0.1,
#   top_p       = 0.95,
#   stop        = c("###")
# )
# cat(response)
