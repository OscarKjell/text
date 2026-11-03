#' Send texts to a Large Language Model API (EXPERIMENTAL)
#'
#' textLLM() sends texts to a Large Language Model (LLM) through an API, and returns
#' the model's responses as a tibble. It can be used for annotating, labelling,
#' classifying or transforming language data; the same \code{prompt} is applied to
#' every element of \code{x}. It currently supports OpenAI's responses API and any
#' OpenAI-compatible endpoint (e.g., Azure or a local vLLM server) via \code{api_url}.
#' @param x (character vector) Texts to send to the model, one API call per element.
#' If \code{NULL}, a single call is made using \code{prompt} alone.
#' @param prompt (string) An instruction applied to every element of \code{x}
#' (e.g., "Rate the degree of worry in this text from 1-9. Respond only with a number.").
#' It is prepended to each text, separated by a blank line.
#' @param system_prompt (string) An optional system prompt that sets the model's
#' overall behaviour; it is sent with every call.
#' @param model (string) The model to query (e.g., "gpt-5-mini", "gpt-5").
#' @param api_key (string) Your personal API key (see the Details section for how
#' to get one). Defaults to the OPENAI_API_KEY environment variable, which is the
#' recommended way to supply it (e.g., set it in .Renviron); avoid writing keys
#' into scripts. The key is only passed to the API client and is not stored in
#' the returned object.
#' @param api_url (string) An optional base URL for OpenAI-compatible endpoints;
#' NULL (default) uses the official OpenAI endpoint.
#' @param max_output_tokens (integer) Cap on the number of generated tokens per call
#' (reasoning tokens included). NULL (default) leaves it to the API.
#' @param reasoning_effort (string) Optional reasoning setting for reasoning models:
#' "minimal", "low", "medium" or "high". NULL (default) leaves it to the API.
#' @param aware_third_party (boolean) Set to TRUE to confirm, in your code, that
#' you are aware that your texts are sent to a third-party provider; the
#' interactive yes/no question is then skipped. FALSE (default) asks for
#' confirmation in interactive sessions (when verbose = TRUE).
#' @param verbose (boolean) If TRUE (default), shows the notice that data is sent
#' to a third party, and a progress message for each call.
#' @return A tibble with one row per element of \code{x}, with the columns:
#' \code{text} (the input), \code{response} (the model's reply),
#' \code{reasoning} (reasoning summary, if the model returned one),
#' \code{model} (the model that served the request),
#' \code{status} ("completed", "incomplete", or an error message),
#' \code{input_tokens} and \code{output_tokens} (token usage), and
#' \code{response_id} (the API's id for the call).
#' Settings are stored in the tibble's comment (see \code{comment()}).
#' @details
#' EXPERIMENTAL: the interface may change in future versions.
#'
#' IMPORTANT — your data leaves your computer: unlike the rest of the text-package,
#' which runs language models locally, textLLM() sends your texts over the internet
#' to a third-party provider (OpenAI, or the server in \code{api_url}). Do not send
#' sensitive or identifiable data unless you have the right to share it with that
#' provider, and consider the provider's data-use policy. With the default
#' \code{verbose = TRUE}, interactive sessions therefore ask you to confirm this
#' before any data is sent. To skip the question, state your awareness in code
#' by setting \code{aware_third_party = TRUE}. In non-interactive sessions (such
#' as scripts and CI) the notice is shown but no confirmation is required, and
#' \code{verbose = FALSE} turns both the notice and the question off.
#'
#' Getting an API key: using textLLM() requires your own account with the API
#' provider; the account (and its usage costs) are between you and the provider
#' and are not affiliated with the text-package. For OpenAI: (1) create an
#' account at \url{https://platform.openai.com}; (2) add prepaid credits under
#' Settings -> Billing (the API is paid per use, separately from any ChatGPT
#' subscription); (3) create an API key at
#' \url{https://platform.openai.com/api-keys}; and (4) store it in your
#' .Renviron file as a line reading \code{OPENAI_API_KEY=sk-...} (open the file
#' with \code{usethis::edit_r_environ()} and restart R afterwards). It is
#' recommended to set a monthly spend limit in the provider's billing settings.
#' For other OpenAI-compatible providers, get the key from that provider and
#' point \code{api_url} to their endpoint.
#'
#' Each element of \code{x} is sent as its own, independent API call, so the model
#' does not see the other texts; this is usually what you want when annotating data.
#' Failed calls (e.g., network problems or rate limits) do not stop the run: the row's
#' \code{status} column contains the error message and \code{response} is NA.
#' @examples
#' \dontrun{
#' # Set your key once, e.g. in .Renviron: OPENAI_API_KEY=sk-...
#'
#' # Annotate texts with a rating
#' annotations <- textLLM(
#'   x = Language_based_assessment_data_8$harmonywords[1:3],
#'   prompt = "Rate the degree of harmony in this text from 1 (low) to 9 (high).
#'   Respond only with a number.",
#'   model = "gpt-5-mini"
#' )
#' annotations
#'
#' # A single free-form prompt
#' textLLM(prompt = "What is the capital of France? Answer in one word.")
#' }
#' @seealso see \code{\link{textGeneration}}, \code{\link{textClassify}},
#' \code{\link{textZeroShot}}
#' @importFrom reticulate source_python
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @export
textLLM <- function(x = NULL,
                    prompt = NULL,
                    system_prompt = NULL,
                    model = "gpt-5-mini",
                    api_key = Sys.getenv("OPENAI_API_KEY"),
                    api_url = NULL,
                    max_output_tokens = NULL,
                    reasoning_effort = NULL,
                    aware_third_party = FALSE,
                    verbose = TRUE) {
  T1_textLLM <- Sys.time()

  if (is.null(x) && is.null(prompt)) {
    stop("Please provide `x` (texts to send) and/or `prompt` (an instruction).")
  }
  if (is.null(api_key) || !nzchar(api_key)) {
    stop(
      "No API key found. Supply `api_key`, or set the OPENAI_API_KEY environment ",
      "variable (e.g., in your .Renviron file). See ?textLLM (Details) for how ",
      "to create an account and get a key."
    )
  }
  # Coerce a single-column data frame/tibble to a character vector.
  if (is.data.frame(x)) {
    if (ncol(x) != 1) {
      stop("`x` should be a character vector (or a data frame with exactly one column).")
    }
    x <- x[[1]]
  }
  if (!is.null(x)) x <- as.character(x)

  # Everything else in the text-package runs models locally; textLLM() is the
  # exception, so make sure the user knows their texts leave the machine.
  third_party_notice <- paste0(
    "NOTE: textLLM() sends your texts over the internet to a third-party provider (",
    if (is.null(api_url)) "OpenAI" else api_url,
    "). Unlike other text-package functions, the data is NOT processed locally. ",
    "Do not send sensitive or identifiable data unless you have the right to ",
    "share it with this provider."
  )
  if (verbose) {
    message(colourise(third_party_notice, "orange"))
    if (interactive() && !isTRUE(aware_third_party)) {
      answer <- readline(
        "Are you aware that your data is sent to a third party, and do you want to proceed? (yes/no): "
      )
      if (!tolower(trimws(answer)) %in% c("yes", "y")) {
        stop("Aborted by user: textLLM() did not send any data.")
      }
    }
  }

  # Load the Python-side API communicator.
  reticulate::source_python(system.file("python",
    "llm-inference-api",
    "OpenAIAPICommunicator.py",
    package = "text",
    mustWork = TRUE
  ))

  reasoning_parameters <- NULL
  if (!is.null(reasoning_effort)) {
    reasoning_parameters <- list(effort = reasoning_effort)
  }
  if (!is.null(max_output_tokens)) {
    max_output_tokens <- as.integer(max_output_tokens)
  }

  communicator <- OpenAICommunicator(
    api_key = api_key,
    model_name = model,
    system_prompt = system_prompt,
    max_tokens = max_output_tokens,
    reasoning_parameters = reasoning_parameters,
    base_url = api_url
  )

  # The full user message for each call: the prompt, the text, or both combined.
  if (is.null(x)) {
    input_texts <- prompt
    user_messages <- prompt
  } else {
    input_texts <- x
    user_messages <- if (is.null(prompt)) x else paste(prompt, x, sep = "\n\n")
  }

  not_null <- function(value, default) if (is.null(value)) default else value

  results <- vector("list", length(user_messages))
  for (i in seq_along(user_messages)) {
    api_result <- tryCatch(
      communicator$generate(
        context = list(list(role = "user", content = user_messages[[i]]))
      ),
      error = function(e) {
        list(status = paste0("error: ", conditionMessage(e)))
      }
    )

    usage <- api_result$usage
    results[[i]] <- tibble::tibble(
      text = input_texts[[i]],
      response = not_null(api_result$text, NA_character_),
      reasoning = not_null(api_result$reasoning, NA_character_),
      model = not_null(api_result$model, model),
      status = not_null(api_result$status, NA_character_),
      input_tokens = as.integer(not_null(usage$input_tokens, NA_integer_)),
      output_tokens = as.integer(not_null(usage$output_tokens, NA_integer_)),
      response_id = not_null(api_result$response_id, NA_character_)
    )

    if (verbose) {
      progress_text <- paste0(
        "textLLM: ", i, "/", length(user_messages),
        " (status: ", results[[i]]$status, ")"
      )
      # Failed calls are shown in red; normal progress in orange.
      progress_colour <- if (grepl("^error", results[[i]]$status)) "red" else "orange"
      message(colourise(progress_text, progress_colour))
    }
  }

  output <- dplyr::bind_rows(results)

  T2_textLLM <- Sys.time()
  all_time <- T2_textLLM - T1_textLLM
  all_time <- sprintf("Duration: %f %s", all_time, units(all_time))

  # Note: the api_key is deliberately NOT stored in the comment.
  comment(output) <- paste("Information about the textLLM settings.  ",
    "model: ", model, "; ",
    "api_url: ", not_null(api_url, "default (OpenAI)"), "; ",
    "system_prompt: ", not_null(system_prompt, "NULL"), "; ",
    "prompt: ", not_null(prompt, "NULL"), "; ",
    "reasoning_effort: ", not_null(reasoning_effort, "NULL"), "; ",
    "max_output_tokens: ", not_null(max_output_tokens, "NULL"), "; ",
    "time: ", all_time, "; ",
    "text_version: ", as.character(packageVersion("text")), ".",
    sep = "",
    collapse = "\n"
  )

  return(output)
}
