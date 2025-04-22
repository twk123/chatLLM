
<!-- README.md is generated from README.Rmd. Please edit that file -->

# chatLLM <a href="https://cran.r-project.org/package=chatLLM"><img src="man/figures/chatlogo.png" align="right" height="120" /></a>

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/chatLLM)](https://cran.r-project.org/package=chatLLM)
[![Total
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/chatLLM?color=orange)](https://cranlogs.r-pkg.org/badges/grand-total/chatLLM)
[![Codecov test
coverage](https://codecov.io/gh/knowusuboaky/chatLLM/branch/main/graph/badge.svg)](https://app.codecov.io/gh/knowusuboaky/chatLLM?branch=main)
[![Last
Commit](https://img.shields.io/github/last-commit/knowusuboaky/chatLLM.svg)](https://github.com/knowusuboaky/chatLLM/commits/main)
[![Issues](https://img.shields.io/github/issues/knowusuboaky/chatLLM.svg)](https://github.com/knowusuboaky/chatLLM/issues)
<!-- badges: end -->

## Overview

**chatLLM** is an R package that provides a unified and flexible
interface for interacting with popular Large Language Model (LLM)
providers such as **OpenAI**, **Groq**, and **Anthropic**.

Features include:

- 🔁 Seamless provider switching
- 🗣 Multi-message conversations
- 🔄 Retries + backoff
- 🔌 Extendable parameters
- 🛠 Test using `.post_func`

------------------------------------------------------------------------

## Installation

``` r
install.packages("chatLLM")
```

------------------------------------------------------------------------

## Development version

To get the latest features or bug fixes, you can install the development
version of `chatLLM` from GitHub:

``` r
# If needed
install.packages("remotes")

remotes::install_github("knowusuboaky/chatLLM")
```

See the full [function
reference](https://knowusuboaky.github.io/chatLLM/reference) or the
[package website](https://knowusuboaky.github.io/chatLLM/) for more
details.

------------------------------------------------------------------------

## Environment Setup

``` r
Sys.setenv(OPENAI_API_KEY    = "your-openai-api-key")
Sys.setenv(GROQ_API_KEY      = "your-groq-api-key")
Sys.setenv(ANTHROPIC_API_KEY = "your-anthropic-api-key")
```

------------------------------------------------------------------------

## Usage

### 1. Simple Prompt Call

``` r
response <- call_llm(
  prompt = "Who is messi?",
  provider = "openai",
  max_tokens = 500,
  n_tries = 3,
  backoff = 2
)
```

``` r
response
#> [1] "Lionel Messi is an Argentine professional footballer who plays as a forward for Paris Saint-Germain and the Argentina national team.
#> He is widely considered one of the greatest footballers of all time, having won numerous awards and accolades throughout his career.
#> Messi is known for his incredible dribbling skills, vision, and goal-scoring ability."

cat(response)
#> Lionel Messi is an Argentine professional footballer who plays as a forward for Paris Saint-Germain and the Argentina national team.
#> He is widely considered one of the greatest footballers of all time, having won numerous awards and accolades throughout his career.
#> Messi is known for his incredible dribbling skills, vision, and goal-scoring ability.
```

------------------------------------------------------------------------

### 2. Multi-Message Conversation

``` r
conv <- list(
  list(role = "system", content = "You are a helpful assistant."),
  list(role = "user", content = "Explain recursion in R.")
)
response <- call_llm(
  messages = conv,
  provider = "openai",
  max_tokens = 200,
  presence_penalty = 0.2,
  frequency_penalty = 0.1,
  top_p = 0.95
)
```

``` r
response
#> [1] "Recursion is a programming technique where a function calls itself in order to solve a problem.
#> In R, a recursive function is defined similarly to any other function, but within the function body, there is a call to the same function.
#> This continues until a base case is reached, at which point the recursion unwinds and results are returned.
#>
#> Here is an example of a recursive factorial function in R:
#>
#> factorial_recursive <- function(n) {
#>   if (n == 0) return(1)
#>   n * factorial_recursive(n - 1)
#> }
#>
#> factorial_recursive(5)  # 120"

cat(response)
#> Recursion is a programming technique where a function calls itself in order to solve a problem.
#> In R, a recursive function is defined similarly to any other function, but within the function body, there is a call to the same function.
#> This continues until a base case is reached, at which point the recursion unwinds and results are returned.
#>
#> Here is an example of a recursive factorial function in R:
#>
#> factorial_recursive <- function(n) {
#>   if (n == 0) return(1)
#>   n * factorial_recursive(n - 1)
#> }
#>
#> factorial_recursive(5)  # 120
```

------------------------------------------------------------------------

## Issues

Please open an issue on
[GitHub](https://github.com/knowusuboaky/chatLLM/issues) if you run into
bugs or have feature requests.

------------------------------------------------------------------------

## License

MIT © [Kwadwo Daddy Nyame Owusu
Boakye](mailto:kwadwo.owusuboakye@outlook.com)

------------------------------------------------------------------------

## Acknowledgements

Inspired by projects like **RAGFlowChainR** and powered by the R
open-source community. Enjoy chatting with your LLMs!
