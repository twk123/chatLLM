# tests/testthat/test_chatLLM.R

context("Testing call_llm() functionality")

test_that("call_llm() returns a factory function when no input is given", {
  factory_fn <- call_llm(provider = "openai")
  expect_type(factory_fn, "closure")  # should return a function
})

test_that("call_llm() returns expected response using a fake POST", {
  # Set a dummy API key so get_api_key doesn't fail
  Sys.setenv(OPENAI_API_KEY = "dummy_key")

  # Fake POST function that mimics a real httr::response
  fake_post_func <- function(url, encode, body, req_headers, ...) {
    structure(
      list(
        status_code = 200L,
        url         = url,
        headers     = c("Content-Type" = "application/json"),
        all_headers = list(list(
          status  = 200L,
          version = "HTTP/1.1",
          headers = c("Content-Type" = "application/json")
        )),
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
