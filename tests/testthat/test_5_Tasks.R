
library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("textClassify tests", {
  skip_on_cran()

  # String example help(textClassify)
  sen1 <- textClassify("I like you. I love you",
    model = "distilbert-base-uncased-finetuned-sst-2-english",
    return_incorrect_results = TRUE,
    return_all_scores = FALSE,
    function_to_apply = "none"
  )
  expect_equal(sen1$score_x, 4.67502, tolerance = 0.001)

  #  # Test another model
  sen2 <- textClassify("I like you. I love you",
    model = "cardiffnlp/twitter-roberta-base-sentiment",
    return_incorrect_results = TRUE, # need to set to TRUE to get results
    return_all_scores = TRUE,
    function_to_apply = "none"
  )
  #
})

test_that("textGeneration test", {
  skip_on_cran()

  generated_text <- textGeneration(
    x = "The meaning of life is",
    model = "gpt2",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "warning",
    return_incorrect_results = FALSE,
    return_tensors = FALSE,
    return_text = TRUE,
    return_full_text = TRUE,
    clean_up_tokenization_spaces = FALSE,
    prefix = "",
    handle_long_generation = "hole"
  )

  expect_equal(generated_text$x_generated, "The meaning of life is self-improvement. We do this the right way. It is an exercise in self-improvement. The better part of the day we try to do that is to stop thinking about doing things to the level most people")
  expect_that(generated_text$x_generated, is_a("character"))

  # Return token IDs
  generated_text2 <- text::textGeneration(
    x = "The meaning of life is",
    model = "gpt2",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "warning",
    return_incorrect_results = FALSE,
    return_tensors = TRUE,
    return_text = FALSE,
    return_full_text = FALSE,
    clean_up_tokenization_spaces = FALSE,
    prefix = "",
    handle_long_generation = "hole",
    set_seed = 22L
  )
  expect_equal(generated_text2$generated_token_ids[1], 464)
  expect_that(generated_text2$generated_token_ids[1], is_a("integer"))
})


test_that("textNER test", {
  skip_on_cran()

  ner_example <- textNER("Arnes plays football with Daniel",
    mode = "dslim/bert-base-NER",
    return_incorrect_results = FALSE
  )
  ner_example

  expect_equal(ner_example$x_NER$score[1], 0.9987748, tolerance = 0.001)


  ner_example2 <- textNER(Language_based_assessment_data_8[1:2, 1],
    mode = "dslim/bert-base-NER",
    return_incorrect_results = FALSE
  )
  ner_example2
  expect_equal(ner_example2$satisfactiontexts_NER$score[2], 0.976, tolerance = 0.01)
})

test_that("textSum test", {
  skip_on_cran()

  sum_examples <- textSum(Language_based_assessment_data_8[1:2, 1:2],
    min_length = 2L,
    max_length = 4L
  )
  sum_examples
  expect_that(sum_examples$sum_satisfactiontexts, is_a("character"))
  expect_equal(sum_examples$sum_satisfactiontexts[1], "I am not")
})


test_that("textQA test", {
  skip_on_cran()

  qa_examples <- textQA(
    question = "Which colour have trees?",
    context = "Trees are mostly green and like water"
  )
  expect_equal(qa_examples$answer, "green")
})

Language_based_assessment_data_8[1, 1]
test_that("textTranslate test", {
  skip_on_cran()

  translation_example <- text::textTranslate(
    Language_based_assessment_data_8[1, 1:2],
    source_lang = "en",
    target_lang = "fr",
    model = "t5-base"
  )
  translation_example
  expect_that(translation_example$en_to_fr_satisfactiontexts, is_a("character"))
  expect_equal(translation_example$en_to_fr_satisfactiontexts[1], "Je ne suis pas satisfait de ma vie. Je suis reconnaissante de ce que j'ai et de l'endroit où je suis parce que la situation peut toujours être pire. Je veux une carrière et un diplôme, je veux perdre du poids et je n'ai pas encore atteint ces objectifs. Je ne suis donc pas satisfait de ma vie à l'heure actuelle.")
})
