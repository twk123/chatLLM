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

# Modified sections of R/chatLLM.R to add Ollama support

###############################################################################
# 1. Provider defaults - Add Ollama                                           #
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
# 2. API-key helper - Add Ollama                                              #
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
# 3. Parse chat-completion responses - Add Ollama                             #
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
      content_match <- regexpr('"content":\\s*"([^"]*)"', text_content)
      if (content_match > 0) {
        content_str <- regmatches(text_content, content_match)
        # Extract the actual content part
        content_only <- gsub('^"content":\\s*"|"

###############################################################################
# 4. Add Ollama model-catalog helper                                          #
###############################################################################
get_ollama_models <- function(base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")) {
  if (!nzchar(base_url)) base_url <- "http://localhost:11434"

  # Check if server is reachable first
  server_check <- tryCatch({
    GET(base_url, timeout(5))
  }, error = function(e) {
    message(sprintf("Cannot connect to Ollama server at %s: %s", base_url, e$message))
    return(NULL)
  })

  if (is.null(server_check) || http_error(server_check)) {
    message(sprintf("Ollama server at %s is not responding or returned an error.", base_url))
    return(character())
  }

  # Try multiple endpoints in sequence
  endpoints <- list(
    list(path = "/api/tags", parser = function(r) {
      parsed <- fromJSON(content(r, "text", encoding = "UTF-8"))
      if (is.list(parsed) && "models" %in% names(parsed)) {
        return(vapply(parsed$models, function(x) x$name, character(1)))
      } else if (is.list(parsed) && !is.null(parsed$models)) {
        return(unlist(parsed$models))
      } else if (is.list(parsed) && length(parsed) > 0) {
        result <- character()
        for (i in seq_along(parsed)) {
          if (is.list(parsed[[i]]) && "name" %in% names(parsed[[i]])) {
            result <- c(result, parsed[[i]]$name)
          } else if (is.character(parsed[[i]])) {
            result <- c(result, parsed[[i]])
          }
        }
        return(result)
      }
      character()
    }),
    list(path = "/api/models", parser = function(r) {
      parsed <- fromJSON(content(r, "text", encoding = "UTF-8"))
      if (is.list(parsed) && "models" %in% names(parsed)) {
        return(vapply(parsed$models, function(x) {
          if (is.list(x) && "name" %in% names(x)) return(x$name)
          if (is.character(x)) return(x)
          NA_character_
        }, character(1)))
      }
      character()
    }),
    list(path = "/api/model", parser = function(r) {
      models <- character()

      # For older Ollama versions, we may need to make a separate request to get models
      # This is a fallback but not optimal as it might be slow
      cmd_resp <- POST(
        url = paste0(base_url, "/api/generate"),
        body = list(
          model = "unknown",
          prompt = "List all available models"
        ),
        encode = "json"
      )

      if (!http_error(cmd_resp)) {
        err_txt <- content(cmd_resp, "text", encoding = "UTF-8")
        # Extract model names from error message
        matches <- gregexpr("'([^']+)'", err_txt)
        if (matches[[1]][1] > 0) {
          model_matches <- regmatches(err_txt, matches)[[1]]
          models <- gsub("'", "", model_matches)
          # Filter out likely non-models
          models <- models[!grepl("unknown|error|not found", models, ignore.case = TRUE)]
        }
      }

      models
    }),
    # Try local Ollama CLI as a last resort
    list(path = "local_cli", parser = function(r) {
      # This tries to execute the Ollama CLI command if all else fails
      if (.Platform$OS.type == "unix") {
        cli_output <- tryCatch({
          system("ollama list", intern = TRUE)
        }, error = function(e) character())

        if (length(cli_output) > 0) {
          # Parse the CLI output - usually in the format "NAME ID"
          models <- character()
          for (line in cli_output) {
            if (grepl("^[a-zA-Z0-9_\\-]+", line)) {
              model_name <- strsplit(line, "\\s+")[[1]][1]
              if (!is.na(model_name) && nzchar(model_name)) {
                models <- c(models, model_name)
              }
            }
          }
          return(models)
        }
      }
      character()
    })
  )

  # Try each endpoint until we get results
  for (endpoint in endpoints) {
    if (endpoint$path == "local_cli") {
      # Special case for local CLI
      models <- endpoint$parser(NULL)
      if (length(models) > 0) {
        message("Retrieved models from local Ollama CLI")
        return(models)
      }
      next
    }

    r <- tryCatch({
      GET(paste0(base_url, endpoint$path), timeout(10))
    }, error = function(e) NULL)

    if (is.null(r)) next

    # Success or partial success
    if (!http_error(r)) {
      models <- tryCatch({
        endpoint$parser(r)
      }, error = function(e) {
        message(sprintf("Failed to parse response from %s: %s",
                      paste0(base_url, endpoint$path), e$message))
        character()
      })

      if (length(models) > 0) {
        # Strip any version tags from model names if present
        models <- gsub(":.*$", "", models)
        # Remove duplicates
        return(unique(models[!is.na(models)]))
      }
    }
  }

  # If we've tried all endpoints and still have no models
                                message("Could not retrieve model list from Ollama server. Please check that:")
                                message(" 1. Ollama is running and accessible at ", base_url)
                                message(" 2. You have models installed (run 'ollama pull <model>' to install)")
                                message(" 3. Your Ollama version is up to date")

                                character()
      }

      ###############################################################################
      # 5. Update list_models() to include Ollama                                   #
      ###############################################################################
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
          "ollama"    = get_ollama_models,  # Add Ollama model getter
          "all"       = NULL
        )

        if (!is.null(fetch)) {
          mods <- tryCatch(fetch(...), error = function(e) character())
          if (length(mods) == 0)
            message(sprintf("No model catalog returned for '%s'.", provider))
          return(mods)
        }

        provs <- c("openai","groq","anthropic","deepseek","dashscope","github","ollama")  # Add Ollama
        setNames(lapply(provs, function(p) {
          tryCatch(list_models(p, ...), error = function(e) character())
        }), provs)
      }

      ###############################################################################
      # 6. Update call_llm() to support Ollama                                      #
      ###############################################################################
      call_llm <- function(
    prompt        = NULL,
    messages      = NULL,
    provider      = c("openai","groq","anthropic",
                      "deepseek","dashscope","github","ollama"),  # Add Ollama
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
    ollama_base_url        = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),  # Add Ollama base URL parameter
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
                ollama_base_url       = ollama_base_url,  # Add to factory mode
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
                                "deepseek","dashscope","github","ollama"))  # Include Ollama
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
      , '', content_str)
    return(content_only)
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
# 4. Add Ollama model-catalog helper                                          #
###############################################################################
get_ollama_models <- function(base_url = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434")) {
  if (!nzchar(base_url)) base_url <- "http://localhost:11434"

  r <- tryCatch(
    GET(paste0(base_url, "/api/tags"),
        timeout(60)),
    error = function(e) NULL)

  if (is.null(r) || http_error(r)) return(character())

  models <- tryCatch({
    parsed <- content(r, "parsed")
    if (is.list(parsed) && "models" %in% names(parsed)) {
      vapply(parsed$models, function(x) x$name, character(1))
    } else {
      # If no 'models' key, try direct list of models
      vapply(parsed, function(x) {
        if (is.list(x) && "name" %in% names(x)) {
          return(x$name)
        }
        # If models are in a different format
        if (is.character(x)) return(x)
        NA_character_
      }, character(1))
    }
  }, error = function(e) character())

  models[!is.na(models)]  # Filter out any NA values
}

###############################################################################
# 5. Update list_models() to include Ollama                                   #
###############################################################################
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
    "ollama"    = get_ollama_models,  # Add Ollama model getter
    "all"       = NULL
  )

  if (!is.null(fetch)) {
    mods <- tryCatch(fetch(...), error = function(e) character())
    if (length(mods) == 0)
      message(sprintf("No model catalog returned for '%s'.", provider))
    return(mods)
  }

  provs <- c("openai","groq","anthropic","deepseek","dashscope","github","ollama")  # Add Ollama
  setNames(lapply(provs, function(p) {
    tryCatch(list_models(p, ...), error = function(e) character())
  }), provs)
}

###############################################################################
# 6. Update call_llm() to support Ollama                                      #
###############################################################################
call_llm <- function(
    prompt        = NULL,
    messages      = NULL,
    provider      = c("openai","groq","anthropic",
                      "deepseek","dashscope","github","ollama"),  # Add Ollama
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
    ollama_base_url        = Sys.getenv("OLLAMA_BASE_URL", "http://localhost:11434"),  # Add Ollama base URL parameter
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
          ollama_base_url       = ollama_base_url,  # Add to factory mode
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
                          "deepseek","dashscope","github","ollama"))  # Include Ollama
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


  txt <- parse_response(provider, content(res, "parsed"))

  if (verbose) {
    message(sprintf("Response (truncated):\n%s",
                    substr(txt, 1, min(200, nchar(txt)))))
  }
  txt
}
