library(testthat)
library(text)
library(tibble)

context("textLLM")

test_that("textLLM validates its input without needing Python or a key", {
  # Needs x and/or prompt
  testthat::expect_error(
    textLLM(api_key = "a-key"),
    "Please provide"
  )

  # Needs an API key
  testthat::expect_error(
    textLLM(prompt = "hi", api_key = ""),
    "No API key"
  )

  # Data frames must have exactly one column
  testthat::expect_error(
    textLLM(
      x = data.frame(a = "x", b = "y"),
      prompt = "p",
      api_key = "a-key"
    ),
    "exactly one column"
  )
})

test_that("textLLM returns a tidy tibble with per-row error status on failed calls", {
  skip_on_cran()
  testthat::skip_if_not(
    reticulate::py_module_available("openai"),
    "openai python module not available"
  )

  # A dummy key makes every call fail (401 or connection error); the function
  # should capture that per row rather than aborting. With verbose = TRUE the
  # third-party data notice must be shown.
  testthat::expect_message(
    result <- textLLM(
      x = c("I feel calm", "I feel worried"),
      prompt = "Rate the degree of worry from 1-9. Respond only with a number.",
      api_key = "dummy-key-123",
      verbose = TRUE
    ),
    regexp = "third-party"
  )

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_equal(nrow(result), 2)
  testthat::expect_named(
    result,
    c(
      "text", "response", "reasoning", "model", "status",
      "input_tokens", "output_tokens", "response_id"
    )
  )
  # Input texts are preserved in order
  testthat::expect_equal(result$text, c("I feel calm", "I feel worried"))
  # All calls failed, and the failure is reported per row
  testthat::expect_true(all(grepl("^error", result$status)))
  testthat::expect_true(all(is.na(result$response)))

  # Settings are stored in the comment, but never the API key
  testthat::expect_true(is.character(comment(result)))
  testthat::expect_false(grepl("dummy-key-123", comment(result), fixed = TRUE))
})

test_that("textLLM live call works when an API key is available", {
  skip_on_cran()
  testthat::skip_if(
    Sys.getenv("OPENAI_API_KEY") == "",
    "OPENAI_API_KEY not set"
  )
  testthat::skip_if_not(
    reticulate::py_module_available("openai"),
    "openai python module not available"
  )

  result <- textLLM(
    prompt = "What is the capital of France? Answer in one word.",
    verbose = FALSE
  )

  testthat::expect_equal(nrow(result), 1)
  testthat::expect_equal(result$status, "completed")
  testthat::expect_true(grepl("Paris", result$response, ignore.case = TRUE))
  testthat::expect_true(result$output_tokens > 0)
})
