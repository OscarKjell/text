library(text)
library(tibble)
library(testthat)

context("Installation and Embedding of text and retrieval of word embeddings")

# helper function to skip tests if we don't have the 'foo' module
skip_if_no_transformers <- function() {
  have_transformers <- reticulate::py_module_available("transformers")
  if (!have_transformers) {
    skip("transformers not available for testing")
  }
}
skip_if_no_torch <- function() {
  have_torch <- reticulate::py_module_available("torch")
  if (!have_torch) {
    skip("torch not available for testing")
  }
}


test_that("textEmbedLayerAggregation 'all': layer =  aggregate_tokens = 'mean' produces aggregated word embeddings", {
  skip_on_cran()

  aggregated_embeddings1 <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 0:1,
    aggregate_layers = "mean",
    aggregate_tokens = "normalize"
  )

  expect_is(aggregated_embeddings1$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings1$harmonywords))
  expect_equal(aggregated_embeddings1$harmonywords[1][[1]][1], 0.007959422, tolerance = 0.0001)
  length_dims_mean <- length(aggregated_embeddings1[[1]])

  aggregated_embeddings_con <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 0:1,
    aggregate_layers = "concatenate",
    aggregate_tokens = "mean"
  )
  length_dims_con <- length(aggregated_embeddings_con[[1]])
  expect_true(2 * length_dims_mean == length_dims_con)
  expect_equal(aggregated_embeddings_con$harmonywords[1][[1]][1], -0.07345252, tolerance = 0.0001)

  # Expect error
  expect_error(aggregated_embeddings <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 0:3,
    aggregate_layers = "mean",
    aggregate_tokens = "mean"
  ))
})

test_that("textEmbedLayerAggregation 1:2 'min' tokens_select = '[CLS]' produces aggregated word embeddings", {
  skip_on_cran()

  aggregated_embeddings2 <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = 1:2,
    aggregate_layers = "concatenate",
    aggregate_tokens = "min",
    tokens_select = "[CLS]"
  )

  expect_is(aggregated_embeddings2$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings2$harmonywords))
  expect_equal(aggregated_embeddings2$harmonywords[1][[1]][1], 0.1291003, tolerance = 0.0001)

})

test_that("textEmbedLayerAggregation 1:2 'max' tokens_deselect = '[CLS]' produces aggregated word embeddings", {
  skip_on_cran()

  # skip_on_cran() library(purrr)
  aggregated_embeddings3 <- textEmbedLayerAggregation(embeddings_from_huggingface2$context,
    layers = "all",
    aggregate_tokens = "max",
    tokens_deselect = c("[CLS]")
  )

  expect_is(aggregated_embeddings3$harmonywords[[1]][1], "numeric")
  expect_true(tibble::is_tibble(aggregated_embeddings3$harmonywords))
  expect_equal(aggregated_embeddings3$harmonywords[1][[1]][1], 2.129543, tolerance = 0.0001)

})

test_that("textEmbedStatic with example space", {
  skip_on_cran()

  # Create example space
  words <- c("happy", "joy", "smile")
  Dim1 <- c(0.1, 4, 7)
  Dim2 <- c(2, 5, 8)
  Dim3 <- c(3, 6, 9)
  test_space <- tibble(words, Dim1, Dim2, Dim3)
  test_space
  # Create example data
  word_response <- c("happy", "joy smile", "adfae", NA, "")
  rating_response <- c(25, 30, 30, 2, 10)
  tibble_response <- tibble(word_response, rating_response)
  tibble_response

  # Test function
  test_result <- textEmbedStatic(df = tibble_response, space = test_space, tk_df = "null", aggregate = "mean")
  test_result

  expect_is(test_result$word_response[[1]][[1]], "numeric")
  expect_is(test_result, "list")
  expect_equal(test_result$word_response$Dim1[[1]], 0.1)
})

# Potentially below works on GitHUB but not on Mac?



test_that("textEmbedLayersOutput contexts=TRUE, decontexts = FALSE returns a list", {
  skip_on_cran()

  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings1 <- textEmbedLayersOutput(x,
    model = "bert-base-uncased",
    contexts = TRUE,
    decontexts = FALSE,
    return_tokens = TRUE,
    layers = "all"
  )

  # Is the first value there and numeric
  expect_that(embeddings1[[1]][[1]][[1]][[1]][[1]], is.character)
  # If below line fail it might be because the output in huggingface has changed,
  # so that 770 needs to be something else
  expect_that(ncol(embeddings1[[1]][[1]][[1]]), equals(771))
  expect_equal(embeddings1[[1]][[1]][[1]]$Dim1[1], 0.1685506, tolerance = 0.0001)
})

test_that("textEmbedLayersOutput bert-base-uncased contexts=FALSE, decontexts = TRUE returns a list", {
  skip_on_cran()


  text_to_test_import1 <- c("jag mår bra", "vad händer")
  text_to_test_import2 <- c("ön är vacker", "molnen svävar")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings2 <- textEmbedLayersOutput(x,
    model = "bert-base-uncased",
    contexts = FALSE,
    decontexts = TRUE,
    layers = "all"
  )
  expect_that(embeddings2, is_a("list"))

  # Is the first value there and numeric
  expect_that(embeddings2[[1]][[1]][[1]][[1]][[1]], is.character)
  # If below line fail it might be because the output in huggingface has changed,
  # so that 770 needs to be something else
  expect_that(ncol(embeddings2[[1]][[1]][[1]][[1]]), equals(771))
  expect_equal(embeddings2[[1]][[1]][[1]][[1]]$Dim1[2], 0.4537115, tolerance = 0.0001)

})

test_that("textEmbed", {
  skip_on_cran()

  text_to_test_import1 <- c("test this", "hope it works")
  text_to_test_import2 <- c("I am happy", "Let us go")
  x <- tibble::tibble(text_to_test_import1, text_to_test_import2)

  embeddings_decontextsT <- textEmbed(x,
    model = "bert-base-uncased",
    decontexts = TRUE,
    single_context_embeddings = FALSE
  )

  single_context_embeddingsT <- textEmbed(x,
                                      model = "bert-base-uncased",
                                      decontexts = FALSE,
                                      single_context_embeddings = TRUE
  )

  embeddings_decontextsF <- textEmbed(x,
    model = "bert-base-uncased",
    decontexts = FALSE
  )

  expect_that(embeddings_decontextsT, is_a("list"))
  expect_that(single_context_embeddingsT, is_a("list"))
  expect_that(embeddings_decontextsF, is_a("list"))
  expect_equal(embeddings_decontextsF[[1]][[1]][[1]][[1]], -0.002111321, tolerance = 0.0001)


  long_text_test <- c("Humour (British English) or humor (American English; see spelling differences) is the tendency to experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
 People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
 ")
  long_text_embedding <- textEmbed(long_text_test,
    model = "bert-base-uncased"
  )

  expect_that(long_text_embedding, is_a("list"))
  expect_equal(long_text_embedding[[1]][[1]][[1]][[1]], -0.01866776, tolerance = 0.0001)
})
