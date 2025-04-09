# tests/testthat/test_chatLLM.R

context("Testing call_llm() functionality")

test_that("call_llm() requires either prompt or messages", {
  expect_error(
    call_llm(provider = "openai"),
    "Must provide either `prompt` or `messages`."
  )
})

test_that("call_llm() returns expected response using a fake POST", {
  # Set a dummy API key so get_api_key doesn't fail.
  Sys.setenv(OPENAI_API_KEY = "dummy_key")

  # Fake POST function that returns a realistic httr::response structure
  fake_post_func <- function(url, encode, body, req_headers, ...) {
    structure(
      list(
        status_code = 200L,
        url         = url,
        # Headers must be named character vectors, not lists
        headers     = c("Content-Type" = "application/json"),
        # all_headers is a list of lists with at least status, version, and headers
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
        # Content is raw JSON matching the structure expected by parse_response()
        content     = charToRaw('{"choices": [{"message": {"content": "Hello from fake response"}}]}'),
        date        = Sys.time()
      ),
      class = "response"
    )
  }

  result <- call_llm(
    prompt     = "Test",
    provider   = "openai",
    n_tries    = 1,
    .post_func = fake_post_func
  )

  expect_equal(result, "Hello from fake response")
})
