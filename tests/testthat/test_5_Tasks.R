

library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("textClassify tests", {
  skip_on_cran()

  # String example
  sen1 <- textClassify("I like you. I love you",
                       model = "distilbert-base-uncased-finetuned-sst-2-english",
                       return_incorrect_results = TRUE,
                       return_all_scores = FALSE,
                       function_to_apply = "none")
  expect_equal(sen1$score_x, 4.67502, tolerance = 0.001)

#  # Tibble example
#  sen2 <- textClassify(Language_based_assessment_data_8[1:2,1:2],
#                       model = "distilbert-base-uncased-finetuned-sst-2-english",
#                       return_incorrect_results = TRUE,
#                       return_all_scores = FALSE,
#                       function_to_apply = "softmax")
#  sen2
#
#  # return_incorrect_results
#  sen_err <- textClassify("I like you. I love you",
#                       model = "bert-base-uncased",
#                       return_incorrect_results = FALSE,
#                       return_all_scores = FALSE,
#                       function_to_apply = "softmax")
#
#  # Test another model
  sen2 <- textClassify("I like you. I love you",
                       model = "cardiffnlp/twitter-roberta-base-sentiment",
                       return_incorrect_results = TRUE, # need to set to TRUE to get results
                       return_all_scores = TRUE,
                       function_to_apply = "none")
#
})

test_that("textGeneration test", {
  skip_on_cran()

  generated_text <- textGeneration(x = "The meaning of life is",
                                   model = 'gpt2',
                                   device = 'cpu',
                                   tokenizer_parallelism = FALSE,
                                   logging_level = 'warning',
                                   return_incorrect_results = FALSE,
                                   return_tensors = TRUE,
                                   return_text = TRUE,
                                   return_full_text = TRUE,
                                   clean_up_tokenization_spaces = FALSE,
                                   prefix = '',
                                   handle_long_generation = "hole"
                                   )

  expect_that(generated_text$generatedtext, is_a("character"))

#  # Testing return_incorrect_results = FALSE
#  gen_err <- textGeneration(x = "The meaning of life is",
#                            model = 'bert-base-uncased',
#                            return_incorrect_results = FALSE)
#  gen_err

  })


test_that("textNER test", {
  skip_on_cran()


  ner_example <- textNER("Arnes plays football with Daniel",
                         mode = "dslim/bert-base-NER",
                         return_incorrect_results = FALSE)
  ner_example

  expect_equal(ner_example$score[1], 0.9987748, tolerance = 0.001)

#  ner_err <- textNER("Arnes plays football with Daniel",
#                         mode = "bert-base-uncased",
#                         return_incorrect_results = FALSE)
#  ner_err

})

test_that("textSum test", {
  skip_on_cran()

  sum_examples <- textSum(Language_based_assessment_data_8[1:2,1:2],
                          min_length = 2L,
                          max_length = 4L)
  sum_examples
  expect_that(sum_examples$sum_satisfactiontexts, is_a("character"))

#  sum_examples_err <- textSum(Language_based_assessment_data_8[1,1],
#                          min_length = 2L,
#                          max_length = 4L,
#                          model = "bert-base-uncased",
#                          return_incorrect_results = FALSE)
#  sum_examples_err

})


test_that("textQA test", {
  skip_on_cran()


#  textQA(context="Oh, Yeah, For Now, But The Beeper's Gonna Be Making A Comeback. Technology's Cyclical.",
#  question="Who is Liz Lemon?")
#
#  qa_examples <- textQA(question = "Which colour have trees?",
#                         context = "Trees are mostly green and like water")
#  qa_examples
#  expect_that(sum_examples$sum_satisfactiontexts, is_a("character"))

  #  sum_examples_err <- textSum(Language_based_assessment_data_8[1,1],
  #                          min_length = 2L,
  #                          max_length = 4L,
  #                          model = "bert-base-uncased",
  #                          return_incorrect_results = FALSE)
  #  sum_examples_err

})


test_that("textTranslate test", {
  skip_on_cran()

  translation_example <- text::textTranslate(
    Language_based_assessment_data_8[1,1:2],
    source_lang = "en",
    target_lang = "fr",
    model = "t5-base")
  translation_example
  expect_that(translation_example$en_to_fr_satisfactiontexts, is_a("character"))

#  translation_err <- text::textTranslate(
#    Language_based_assessment_data_8[1,1:2],
#    source_lang = "en",
#    target_lang = "sv",
#    model = "t5-base",
#    return_incorrect_results = FALSE)
#  translation_err

})