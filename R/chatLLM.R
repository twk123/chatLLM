###############################################################################
# chatLLM helpers + call_llm()                                                #
# Requires: httr (>= 1.4.6), jsonlite                                         #
###############################################################################
# ------------------------------------------------------------------------------
#' @importFrom httr POST add_headers http_error content
#' @importFrom jsonlite toJSON fromJSON
#' @importFrom httr GET timeout status_code
#' @importFrom stats setNames

library(httr)
library(jsonlite)

###############################################################################
# 1. Provider defaults                                                        #
###############################################################################
get_default_model <- function(provider) {
  switch(
    tolower(provider),
    "openai"    = "gpt-3.5-turbo",
    "groq"      = "meta-llama/llama-4-scout-17b-16e-instruct",
    "anthropic" = "claude-3-7-sonnet-20250219",
    "deepseek"  = "deepseek-chat",
    "dashscope" = "qwen-plus-latest",
    "github"    = "openai/gpt-4.1",
    "ollama"    = "llama3",  # Default model for Ollama
    stop("No default model for provider: ", provider)
  )
}

###############################################################################
# 2. API-key helper                                                           #
###############################################################################
get_api_key <- function(provider, api_key = NULL) {
  env_var <- switch(
    tolower(provider),
    "openai"    = "OPENAI_API_KEY",
    "groq"      = "GROQ_API_KEY",
    "anthropic" = "ANTHROPIC_API_KEY",
    "deepseek"  = "DEEPSEEK_API_KEY",
    "dashscope" = "DASHSCOPE_API_KEY",
    "github"    = "GH_MODELS_TOKEN",
    "ollama"    = "OLLAMA_API_KEY",  # Ollama API key (optional)
    stop("Unknown provider: ", provider)
  )
  if (is.null(api_key)) api_key <- Sys.getenv(env_var)
  # Make API key optional for Ollama as it often doesn't require authentication
  if (!nzchar(api_key) && tolower(provider) != "ollama")
    stop(sprintf("API key not found for %s. Set %s or pass `api_key`.",
                 provider, env_var))
  api_key
}

###############################################################################
# 3. Parse chat-completion responses                                          #
###############################################################################
parse_response <- function(provider, parsed, raw_content = NULL) {
  if (tolower(provider) == "ollama" && !is.null(raw_content)) {
    # Special handling for Ollama NDJSON responses
    # Try to parse the raw content directly
    tryCatch({
      # Get text content
      text_content <- content(raw_content, "text", encoding = "UTF-8")
      
      # For NDJSON format, we need to parse each line separately
      # Usually the last line contains the final message
      lines <- strsplit(text_content, "\n")[[1]]
      valid_lines <- lines[nzchar(lines)]
      
      if (length(valid_lines) > 0) {
        # Try to parse the last complete line
        last_json <- fromJSON(valid_lines[length(valid_lines)])
        if (!is.null(last_json$message$content)) {
          return(last_json$message$content)
        }
      }
      
      # If we couldn't parse it properly, extract content with regex as fallback
      content_matches <- gregexpr('"content":\\s*"([^"]*)"', text_content)
      if (length(content_matches) > 0 && content_matches[[1]][1] > 0) {
        content_strs <- regmatches(text_content, content_matches)[[1]]
        if (length(content_strs) > 0) {
          # Get the last content string (most likely to be the final response)
          content_str <- content_strs[length(content_strs)]
          # Extract the actual content part
          content_only <- gsub('^"content":\\s*"|"$', '', content_str)
          return(content_only)
        }
      }
      
      # Last resort - return the raw text if everything else fails
      return(paste("Raw response:", text_content))
      
    }, error = function(e) {
      # If all parsing attempts fail, try the standard method
      if (!is.null(parsed$message$content)) {
        return(parsed$message$content)
      }
      return(paste("Failed to parse Ollama response:", e$message))
    })
  } else {
    # Standard parsing for other providers
    switch(
      tolower(provider),
      "openai"    = parsed$choices[[1]]$message$content,
      "groq"      = parsed$choices[[1]]$message$content,
      "anthropic" = parsed$content[[1]]$text,
      "deepseek"  = parsed$choices[[1]]$message$content,
      "dashscope" = parsed$choices[[1]]$message$content,
      "github"    = parsed$choices[[1]]$message$content,
      "ollama"    = {
        if (!is.null(parsed$message$content)) {
          parsed$message$content
        } else if (!is.null(parsed$response)) {
          # Some Ollama versions use different response format
          parsed$response
        } else {
          stop("Could not parse Ollama response format")
        }
      },
      stop("Parsing not implemented for provider: ", provider)
    )
  }
}

###############################################################################
# 4. Model-catalog helpers (one per provider)                                 #
###############################################################################
get_openai_models <- function(token = Sys.getenv("OPENAI_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.openai.com/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_groq_models <- function(token = Sys.getenv("GROQ_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.groq.com/openai/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_anthropic_models <- function(token = Sys.getenv("ANTHROPIC_API_KEY"),
                                 anthropic_api_version = "2023-06-01",
                                 limit = 1000) {
  if (!nzchar(token)) return(character())
  r <- tryCatch(
    GET("https://api.anthropic.com/v1/models",
        add_headers(`x-api-key` = token,
                    `anthropic-version` = anthropic_api_version),
        query = list(limit = limit),
        timeout(60)),
    error = function(e) NULL)
  if (is.null(r) || http_error(r)) return(character())
  p <- content(r, "parsed")
  if (!is.null(p$data))
    return(vapply(p$data, `[[`, character(1), "id"))
  if (!is.null(p$models))
    return(vapply(p$models, `[[`, character(1), "name"))
  character()
}

get_deepseek_models <- function(token = Sys.getenv("DEEPSEEK_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://api.deepseek.com/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_dashscope_models <- function(token = Sys.getenv("DASHSCOPE_API_KEY")) {
  if (!nzchar(token)) return(character())
  r <- GET("https://dashscope-intl.aliyuncs.com/compatible-mode/v1/models",
           add_headers(Authorization = paste("Bearer", token)),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed")$data, `[[`, character(1), "id")
}

get_all_github_models <- function(token       = Sys.getenv("GH_MODELS_TOKEN"),
                                  api_version = "2022-11-28") {
  if (!nzchar(token)) return(character())
  r <- GET("https://models.github.ai/catalog/models",
           add_headers(Accept = "application/vnd.github+json",
                       Authorization = paste("Bearer", token),
                       `X-GitHub-Api-Version` = api_version),
           timeout(60))
  if (http_error(r)) return(character())
  vapply(content(r, "parsed", simplifyVector = FALSE),
         `[[`, character(1), "id")
}

get_ollama_models <- function(base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")) {
  if (!nzchar(base_url)) base_url <- "http://localhost:11434"
  
  r <- tryCatch(
    GET(paste0(base_url, "/api/tags"),
        timeout(60)),
    error = function(e) NULL)
  
  if (is.null(r) || http_error(r)) return(character())
  
  # Safely parse the response content
  models <- tryCatch({
    # Get the raw content first
    raw_content <- content(r, "text", encoding = "UTF-8")
    parsed <- fromJSON(raw_content)
    
    if (is.list(parsed) && "models" %in% names(parsed)) {
      # Newer Ollama API format
      vapply(parsed$models, function(x) x$name, character(1))
    } else if (is.list(parsed) && length(parsed$name) > 0) {
      # Alternative format where the response is a single model
      c(parsed$name)
    } else if (is.list(parsed) && length(parsed) > 0 && all(vapply(parsed, is.list, logical(1)))) {
      # Format where response is a list of models
      vapply(parsed, function(x) {
        if (is.list(x) && "name" %in% names(x)) {
          return(x$name)
        }
        NA_character_
      }, character(1))
    } else {
      # For older Ollama versions
      if ("models" %in% names(parsed)) {
        vapply(parsed$models, function(x) {
          if (is.character(x)) return(x)
          if (is.list(x) && "name" %in% names(x)) return(x$name)
          NA_character_
        }, character(1))
      } else {
        # Try to handle array of strings directly
        as.character(parsed)
      }
    }
  }, error = function(e) {
    # Fallback: try with a different endpoint
    tryCatch({
      r2 <- GET(paste0(base_url, "/api/models"),
               timeout(60))
      if (http_error(r2)) return(character())
      
      parsed2 <- content(r2, "parsed")
      if (is.list(parsed2) && "models" %in% names(parsed2)) {
        vapply(parsed2$models, function(x) x$name, character(1))
      } else {
        character()
      }
    }, error = function(e2) character())
  })
  
  # Filter out any NA values
  models[!is.na(models)]
}

###############################################################################
# 5. list_models()                                                            #
###############################################################################
#' List Available Models for Supported Providers
#'
#' @name list_models
#'
#' @description
#' Retrieve the catalog of available model IDs for one or all supported
#' chat - completion providers. Useful for discovering active models and
#' avoiding typos or deprecated defaults.
#'
#' Supported providers:
#' \itemize{
#'   \item \code{"openai"}     -  OpenAI Chat Completions API
#'   \item \code{"groq"}       -  Groq OpenAI - compatible endpoint
#'   \item \code{"anthropic"}  -  Anthropic Claude API
#'   \item \code{"deepseek"}   -  DeepSeek chat API
#'   \item \code{"dashscope"}  -  Alibaba DashScope compatible API
#'   \item \code{"github"}     -  GitHub Models OpenAI - compatible API
#'   \item \code{"ollama"}     -  Ollama local or remote server
#'   \item \code{"all"}        -  Fetch catalogs for all of the above
#' }
#'
#' @param provider Character. One of \code{"github"}, \code{"openai"},
#'   \code{"groq"}, \code{"anthropic"}, \code{"deepseek"},
#'   \code{"dashscope"}, \code{"ollama"} or \code{"all"}. Case - insensitive.
#' @param ... Additional arguments passed to the per - provider helper
#'   (e.g. \code{limit} for Anthropic, or \code{api_version} for GitHub).
#' @param github_api_version Character. Header value for
#'   \code{X - GitHub - Api - Version} (GitHub Models). Default \code{"2022 - 11 - 28"}.
#' @param anthropic_api_version Character. Header value for
#'   \code{anthropic - version} (Anthropic). Default \code{"2023 - 06 - 01"}.
#'
#' @return
#' If \code{provider != "all"}, a character vector of model IDs for that
#' single provider. If \code{provider == "all"}, a named list of character
#' vectors, one per provider.
#'
#' @examples
#' \dontrun{
#' Sys.setenv(OPENAI_API_KEY = "sk-...")
#' openai_models <- list_models("openai")
#' head(openai_models)
#'
#' Sys.setenv(ANTHROPIC_API_KEY = "sk-...")
#' anthro_models <- list_models("anthropic", anthropic_api_version = "2023-06-01")
#'
#' Sys.setenv(GH_MODELS_TOKEN = "ghp-...")
#' github_models <- list_models("github", github_api_version = "2022-11-28")
#'
#' Sys.setenv(OLLAMA_BASE_URL = "http://localhost:11434")
#' ollama_models <- list_models("ollama")
#' }
#'
#' @seealso
#'   \code{\link{call_llm}}
#' @export
NULL

list_models <- function(provider = c("github","openai","groq",
                                     "anthropic","deepseek","dashscope","ollama","all"),
                        ...) {
  provider <- match.arg(tolower(provider),
                        c("github","openai","groq",
                          "anthropic","deepseek","dashscope","ollama","all"))

  fetch <- switch(
    provider,
    "openai"    = get_openai_models,
    "groq"      = get_groq_models,
    "anthropic" = get_anthropic_models,
    "deepseek"  = get_deepseek_models,
    "dashscope" = get_dashscope_models,
    "github"    = function(...) get_all_github_models(...),
    "ollama"    = get_ollama_models,
    "all"       = NULL
  )

  if (!is.null(fetch)) {
    mods <- tryCatch(fetch(...), error = function(e) character())
    if (length(mods) == 0)
      message(sprintf("No model catalog returned for '%s'.", provider))
    return(mods)
  }

  provs <- c("openai","groq","anthropic","deepseek","dashscope","github","ollama")
  setNames(lapply(provs, function(p) {
    tryCatch(list_models(p, ...), error = function(e) character())
  }), provs)
}

###############################################################################
# 6. Core chat-completion wrapper                                             #
###############################################################################
#' Core chat - completion wrapper for multiple providers
#'
#' @title Unified chat - completion interface
#' @name call_llm
#' @description
#' A unified wrapper for several "OpenAI‑compatible" chat - completion APIs
#' (OpenAI, Groq, Anthropic, DeepSeek, Alibaba DashScope, GitHub Models,
#' and Ollama).
#' Accepts either a single `prompt` **or** a full `messages` list, adds the
#' correct authentication headers, retries on transient failures, and returns
#' the assistant's text response. You can toggle informational console
#' output with `verbose = TRUE/FALSE`. If the chosen `model` is no longer
#' available, the function stops early and suggests running
#' `list_models("<provider>")`.
#'
#' @section Messages:
#' * `prompt`    -  character scalar treated as a single *user* message.
#' * `messages`  -  list of lists; each element must contain `role` and `content`.
#'                If both arguments are supplied, the `prompt` is appended
#'                as an extra user message.
#'
#' @param prompt   Character. Single user prompt (optional if `messages`).
#' @param messages List. Full chat history; see *Messages*.
#' @param provider Character. One of `"openai"`, `"groq"`, `"anthropic"`,
#'                 `"deepseek"`, `"dashscope"`, `"github"`, or `"ollama"`.
#' @param model    Character. Model ID. If `NULL`, uses the provider default.
#' @param temperature Numeric. Sampling temperature (0 - 2). Default `0.7`.
#' @param max_tokens  Integer. Max tokens to generate. Default `1000`.
#' @param api_key     Character. Override API key; if `NULL`, uses the
#'                    environment variable for that provider.
#' @param n_tries     Integer. Retry attempts on failure. Default `3`.
#' @param backoff     Numeric. Seconds between retries. Default `2`.
#' @param verbose     Logical. Whether to display informational messages
#'                    (`TRUE`) or suppress them (`FALSE`). Default `TRUE`.
#' @param endpoint_url  Character. Custom endpoint; if `NULL`, a sensible
#'                    provider - specific default is used.
#' @param github_api_version     Character. Header `X - GitHub - Api - Version`.
#'                           Default `"2022 - 11 - 28"`.
#' @param anthropic_api_version  Character. Header `anthropic - version`.
#'                             Default `"2023 - 06 - 01"`.
#' @param ollama_base_url  Character. Base URL for Ollama server.
#'                      Default from env var `OLLAMA_BASE_URL` or
#'                      `"http://localhost:11434"`.
#' @param ...         Extra JSON - body fields (e.g. `top_p`, `stop`,
#'                    `presence_penalty`).
#' @param .post_func  Internal. HTTP POST function (default `httr::POST`).
#'
#' @return Character scalar: assistant reply text.
#'
#' @examples
#' \dontrun{
#'
#' ## 1. Listing available models
#' # List all providers at once
#' all_mods <- list_models("all")
#' str(all_mods)
#'
#' # List OpenAI-only, Groq-only, Anthropic-only
#' openai_models   <- list_models("openai")
#' groq_mods     <- list_models("groq")
#' anthropic_mods<- list_models("anthropic", anthropic_api_version = "2023-06-01")
#'
#' # List Ollama models on local or remote server
#' Sys.setenv(OLLAMA_BASE_URL = "http://your-server:11434")
#' ollama_models <- list_models("ollama")
#'
#' ## 2. Single-prompt interface
#'
#' # 2a. Basic usage
#' Sys.setenv(OPENAI_API_KEY = "sk-...")
#' res_basic <- call_llm(
#'   prompt   = "Hello, how are you?",
#'   provider = "openai"
#' )
#' cat(res_basic)
#'
#' # 2b. Using Ollama
#' res_ollama <- call_llm(
#'   prompt    = "Explain neural networks simply",
#'   provider  = "ollama",
#'   model     = "llama3"
#' )
#' cat(res_ollama)
#'
#' # 2c. Adjust sampling and penalties
#' res_sampling <- call_llm(
#'   prompt      = "Write a haiku about winter",
#'   provider    = "openai",
#'   temperature = 1.2,
#'   top_p       = 0.5,
#'   presence_penalty  = 0.6,
#'   frequency_penalty = 0.4
#' )
#' cat(res_sampling)
#'
#' # 2d. Control length and retries
#' res_len <- call_llm(
#'   prompt      = "List 5 uses for R",
#'   provider    = "openai",
#'   max_tokens  = 50,
#'   n_tries     = 5,
#'   backoff     = 0.5
#' )
#' cat(res_len)
#'
#' # 2e. Using stop sequences
#' res_stop <- call_llm(
#'   prompt   = "Count from 1 to 10:",
#'   provider = "openai",
#'   stop     = c("6")
#' )
#' cat(res_stop)
#'
#' # 2f. Factory interface for repeated prompts
#' RemoteOllama <- call_llm(
#'   provider       = "ollama",
#'   model          = "mistral",
#'   ollama_base_url = "http://192.168.1.100:11434",
#'   max_tokens     = 1000,
#'   verbose        = FALSE
#' )
#' # direct invocation
#' story1 <- RemoteOllama("Tell me a short story")
#' cat(story1)
#'
#' ## 3. Multi-message conversation
#'
#' # 3a. Simple system + user
#' convo1 <- list(
#'   list(role = "system",    content = "You are a helpful assistant."),
#'   list(role = "user",      content = "Explain recursion.")
#' )
#' res1 <- call_llm(
#'   messages   = convo1,
#'   provider   = "ollama",
#'   model      = "llama3",
#'   max_tokens = 100
#' )
#' cat(res1)
#' }
#'
#' @export
NULL

call_llm <- function(
    prompt        = NULL,
    messages      = NULL,
    provider      = c("openai","groq","anthropic",
                      "deepseek","dashscope","github","ollama"),
    model         = NULL,
    temperature   = 0.7,
    max_tokens    = 1000,
    api_key       = NULL,
    n_tries       = 3,
    backoff       = 2,
    verbose       = TRUE,
    endpoint_url  = NULL,
    github_api_version     = "2022-11-28",
    anthropic_api_version  = "2023-06-01",
    ollama_base_url        = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),
    ...,
    .post_func    = httr::POST
) {

  ## ------------------------------------------------------------------------- ##
  ## Factory mode: if neither prompt nor messages supplied, return an LLM object
  if (missing(prompt) && missing(messages)) {
      return(
        function(prompt   = NULL,
                 messages = NULL,
                 ...) {
          # Re - invoke call_llm() with stored defaults + whatever the user
          # passes now
          args_main <- list(prompt = prompt, messages = messages)
          opts_main <- list(
            provider              = provider,
            model                 = model,
            temperature           = temperature,
            max_tokens            = max_tokens,
            api_key               = api_key,
            n_tries               = n_tries,
            backoff               = backoff,
            verbose               = verbose,
            endpoint_url          = endpoint_url,
            github_api_version    = github_api_version,
            anthropic_api_version = anthropic_api_version,
            ollama_base_url       = ollama_base_url,
            .post_func            = .post_func
          )
          extra_args <- list(...)
          all_args   <- c(args_main, opts_main, extra_args)
          do.call(call_llm, all_args)
        }
      )
    }

  ##
  ## ------------------------------------------------------------------------- ##

  provider <- match.arg(tolower(provider),
                        c("openai","groq","anthropic",
                          "deepseek","dashscope","github","ollama"))
  if (is.null(model)) model <- get_default_model(provider)

  ## ---------------- assemble messages ----------------------------------- ##
  if (!is.null(messages)) {
    if (!is.null(prompt))
      messages <- c(messages, list(list(role = "user", content = prompt)))
  } else {
    if (is.null(prompt))
      stop("Provide either `prompt` or `messages`.")
    messages <- list(list(role = "user", content = prompt))
  }

  ## ---------------- common request pieces ------------------------------- ##
  api_key <- get_api_key(provider, api_key)

  # Special handling for Ollama request format
  if (provider == "ollama") {
    # Ollama expects a specific format for options
    req_body <- list(
      model = model,
      messages = messages,
      options = list(
        temperature = temperature,
        num_predict = max_tokens
      )
    )
    
    # Add additional parameters to options
    extra_args <- list(...)
    if (length(extra_args) > 0) {
      for (arg in names(extra_args)) {
        # Only add compatible parameters
        if (!arg %in% c("model", "messages", "options")) {
          req_body$options[[arg]] <- extra_args[[arg]]
        }
      }
    }
  } else {
    # Standard format for other providers
    req_body <- c(
      list(
        model       = model,
        messages    = messages,
        temperature = temperature,
        max_tokens  = max_tokens
      ),
      list(...)
    )
  }

  req_headers <- switch(
    provider,
    "openai" = add_headers(Authorization = paste("Bearer", api_key)),
    "groq"   = add_headers(Authorization = paste("Bearer", api_key),
                           "Content-Type" = "application/json"),
    "anthropic" = add_headers(`x-api-key` = api_key,
                              `anthropic-version` = anthropic_api_version,
                              "Content-Type" = "application/json"),
    "deepseek"  = add_headers(Authorization = paste("Bearer", api_key)),
    "dashscope" = add_headers(Authorization = paste("Bearer", api_key)),
    "github"    = add_headers(Accept = "application/vnd.github+json",
                              Authorization = paste("Bearer", api_key),
                              `X-GitHub-Api-Version` = github_api_version,
                              "Content-Type" = "application/json"),
    "ollama"    = {  # Ollama headers
      headers <- add_headers("Content-Type" = "application/json")
      # Add Authorization header only if API key is provided
      if (nzchar(api_key)) {
        headers <- c(headers, add_headers(Authorization = paste("Bearer", api_key)))
      }
      headers
    }
  )

  if (is.null(endpoint_url)) {
    endpoint_url <- switch(
      provider,
      "openai"    = "https://api.openai.com/v1/chat/completions",
      "groq"      = "https://api.groq.com/openai/v1/chat/completions",
      "anthropic" = "https://api.anthropic.com/v1/messages",
      "deepseek"  = "https://api.deepseek.com/v1/chat/completions",
      "dashscope" = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions",
      "github"    = {
        org <- Sys.getenv("GH_MODELS_ORG")
        if (nzchar(org))
          sprintf("https://models.github.ai/orgs/%s/inference/chat/completions", org)
        else
          "https://models.github.ai/inference/chat/completions"
      },
      "ollama"    = paste0(ollama_base_url, "/api/chat")  # Ollama chat endpoint
    )
  }

  if (verbose) {
    message(sprintf("Calling %s [%s] ... attempts=%d", provider, model, n_tries))
    if (provider == "ollama") {
      message(sprintf("Ollama server: %s", ollama_base_url))
    }
  }

  ## ---------------- retry loop ------------------------------------------ ##
  res <- NULL
  for (i in seq_len(n_tries)) {

    res <- tryCatch(
      .post_func(
        url    = endpoint_url,
        encode = "json",
        body   = req_body,
        req_headers
      ),
      error = function(e) {
        # detect network time-outs explicitly
        if (grepl("timeout", e$message, ignore.case = TRUE)) {
          msg <- sprintf("Attempt %d/%d failed (%s: timeout). Retrying in %ds...",
                         i, n_tries, provider, backoff)
        } else {
          msg <- sprintf("Attempt %d/%d failed (%s). Retrying in %ds...",
                         i, n_tries, provider, backoff)
        }
        message(msg)

        if (i == n_tries) {
          # Special error message for Ollama
          if (provider == "ollama") {
            stop(sprintf(
              paste0(
                'The request to Ollama server at "%s" failed after %d attempt(s).\n\n',
                'Ensure Ollama is running and accessible at the specified URL.\n',
                'You can:\n',
                ' . Use ollama_base_url="http://your-server:11434" to specify a different server\n',
                ' . Set the OLLAMA_BASE_URL environment variable\n',
                ' . Check if the Ollama server is running and accessible\n\n',
                'Error message: %s'
              ),
              ollama_base_url, n_tries, e$message
            ), call. = FALSE)
          } else {
            stop(sprintf(
              paste0(
                'The request to provider "%s" timed out after %d attempt(s).\n\n',
                'Tip: the model may be retired or misspelled.\n',
                'Run list_models("%s") (after setting the proper *_API_KEY / ',
                'MODELS_TOKEN) to see current models, e.g.\n',
                '    openai_models <- list_models("openai")\n',
                'Then rerun call_llm(..., model = "<new-model>").\n\n',
                'If the issue is network-related you can also:\n',
                ' . increase `n_tries` or `backoff`,\n',
                ' . provide a longer `timeout()` via `.post_func`, or\n',
                ' . check your network / VPN.\n\n',
                'Internal message: %s'
              ),
              provider, n_tries, provider, e$message
            ), call. = FALSE)
          }
        }

        Sys.sleep(backoff)
        NULL
      }
    )

    if (is.null(res)) next          # go to next iteration
    if (!http_error(res)) break     # success, exit loop

    ## ---- HTTP error branch --------------------------------------------- ##
    err_txt    <- content(res, "text", encoding = "UTF-8")
    err_parsed <- tryCatch(fromJSON(err_txt), error = function(e) NULL)

    not_found <- FALSE
    if (!is.null(err_parsed$error$code))
      not_found <- grepl("model_not_found|invalid_model|404",
                         err_parsed$error$code, ignore.case = TRUE)
    if (!not_found && !is.null(err_parsed$message))
      not_found <- grepl("model.*not.*found|no such model|de.?commiss|deprecated",
                         err_parsed$message, ignore.case = TRUE)
    
    # Special handling for Ollama error formats
    if (provider == "ollama" && !is.null(err_parsed$error)) {
      if (grepl("model.*not.*found|no such model", err_parsed$error, ignore.case = TRUE)) {
        not_found <- TRUE
      }
    }

    if (not_found) {
      if (provider == "ollama") {
        stop(sprintf(
          paste0(
            'The model "%s" is not available on your Ollama server at "%s".\n',
            'Possible solutions:\n',
            ' . Run list_models("ollama") to see available models\n',
            ' . Download the model with "ollama pull %s" on your server\n',
            ' . Check if the model name is correctly spelled'
          ),
          model, ollama_base_url, model
        ), call. = FALSE)
      } else {
        stop(sprintf(
          paste0(
            'The model "%s" is unavailable or de-commissioned for provider "%s".\n',
            'Tip: run list_models("%s") after setting the proper *_API_KEY / ',
            'MODELS_TOKEN to see current models, e.g.\n',
            '    openai_models <- list_models("openai")\n',
            'Then rerun call_llm(..., model = "<new-model>").'
          ),
          model, provider, provider
        ), call. = FALSE)
      }
    }

    ## --- generic HTTP error after final retry --------------------------- ##
    if (i == n_tries) {
      if (provider == "ollama") {
        stop(sprintf(
          paste0(
            'Ollama server at "%s" returned an error after %d attempt(s).\n\n',
            'Raw response from server:\n%s\n\n',
            'Tips:\n',
            ' . Ensure the model "%s" is available on your Ollama server\n',
            ' . Run list_models("ollama") to view available models\n',
            ' . Pull the model with "ollama pull %s" on your server\n',
            ' . Check server logs for more details'
          ),
          ollama_base_url, n_tries, err_txt, model, model
        ), call. = FALSE)
      } else {
        stop(sprintf(
          paste0(
            'Provider "%s" still returned an error after %d attempt(s).\n\n',
            'Raw response from server:\n%s\n\n',
            'Tip: the model may be retired, renamed, or misspelled.\n',
            '. Run list_models("%s") (after setting the proper *_API_KEY / ',
            'MODELS_TOKEN) to view currently available models, e.g.\n',
            '    openai_models <- list_models("openai")\n',
            '. Or visit the provider\'s dashboard / documentation for the ',
            'latest list.\n\n',
            'Then rerun call_llm(..., model = "<new-model>").'
          ),
          provider, n_tries, err_txt, provider
        ), call. = FALSE)
      }
    }

    if (verbose) {
      message(sprintf("HTTP %d on attempt %d/%d. Retrying in %ds...",
                      status_code(res), i, n_tries, backoff))
    }

    Sys.sleep(backoff)
  }

  txt <- if (provider == "ollama") {
    # Pass the raw response for Ollama to handle NDJSON format
    parse_response(provider, content(res, "parsed", "application/json"), res)
  } else {
    parse_response(provider, content(res, "parsed"))
  }

  if (verbose) {
    message(sprintf("Response (truncated):\n%s",
                    substr(txt, 1, min(200, nchar(txt)))))
  }
  txt
}
