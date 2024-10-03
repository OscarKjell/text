library(testthat)
library(tibble)
library(text)

context("Testing tasks")


test_that("textClassify tests", {
  skip_on_cran()

  # String example help(textClassify) help(textClassify)
  sen1 <- textClassify(
    x = "I like you. I love you",
    model = "distilbert-base-uncased-finetuned-sst-2-english",
    force_return_results = TRUE,
    return_all_scores = FALSE,
    function_to_apply = "none"
  )
  expect_equal(sen1$score_x, 4.67502, tolerance = 0.001)
  textModelsRemove("distilbert-base-uncased-finetuned-sst-2-english")

  #  # Test another model
  sen2 <- textClassify("I like you. I love you",
    model = "cardiffnlp/twitter-roberta-base-sentiment",
    force_return_results = TRUE, # need to set to TRUE to get results
    return_all_scores = TRUE,
    function_to_apply = NULL
  )
  textModelsRemove("cardiffnlp/twitter-roberta-base-sentiment")
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
    force_return_results = FALSE,
    return_tensors = FALSE,
    return_full_text = TRUE,
    clean_up_tokenization_spaces = FALSE,
    prefix = "",
    handle_long_generation = "hole",
    set_seed = 22L
  )
  # torch 1.11
  # expect_equal(generated_text$x_generated, "The meaning of life is to live for
  # the sake of one's children, and to take the name or life of another, and not as
  # the child of a dead person. It is a name, a life, and not as the son, daughter")
  expect_that(generated_text$x_generated, is_a("character"))

  # Return token IDs
  generated_text2 <- text::textGeneration(
    x = "The meaning of life is",
    model = "gpt2",
    device = "cpu",
    tokenizer_parallelism = FALSE,
    logging_level = "warning",
    force_return_results = FALSE,
    return_tensors = TRUE,
    return_full_text = FALSE,
    clean_up_tokenization_spaces = FALSE,
    prefix = "",
    handle_long_generation = "hole",
    set_seed = 22L
  )
  textModelsRemove("gpt2")
  expect_equal(generated_text2$generated_token_ids[1], 464)
  expect_that(generated_text2$generated_token_ids[1], is_a("integer"))
})


test_that("textNER test", {
  skip_on_cran()

  ner_example <- textNER("Arnes plays football with Daniel",
    mode = "dslim/bert-base-NER",
    force_return_results = FALSE
  )
  ner_example

  expect_equal(ner_example$x_NER$score[1], 0.9987748, tolerance = 0.001)


  ner_example2 <- textNER(Language_based_assessment_data_8[1:2, 1],
    mode = "dslim/bert-base-NER",
    force_return_results = FALSE
  )
  ner_example2
  expect_equal(ner_example2$satisfactiontexts_NER$score[2], 0.976, tolerance = 0.01)
  textModelsRemove("dslim/bert-base-NER")
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



test_that("textZeroShot test", {
  skip_on_cran()

  ZeroShot_example <- text::textZeroShot(
    sequences = c("I play football", "The forrest is wonderful"),
    candidate_labels = c("sport", "nature", "research"),
    # model = "facebook/bart-large-mnli"
    model = "okho0653/distilbert-base-uncased-zero-shot-sentiment-model"
  )

  testthat::expect_equal(ZeroShot_example$scores_x_1[1], 0.3341856, tolerance = 0.00001)
  textModelsRemove("okho0653/distilbert-base-uncased-zero-shot-sentiment-model")
})

test_that("textTranslate test", {
  skip_on_cran()

  textModels()
  translation_example <- text::textTranslate(
    Language_based_assessment_data_8[1, 1:2],
    source_lang = "en",
    target_lang = "fr",
    model = "t5-small",
    max_length = 400
  )

  testthat::expect_that(translation_example$en_to_fr_satisfactiontexts, testthat::is_a("character"))
  testthat::expect_equal(
    translation_example$en_to_fr_satisfactiontexts[1],
    "Je ne suis pas satisfait de ma vie, je suis reconnaissante de ce que j'ai et de ce que je suis, car la situation peut toujours être pire. Je veux une carrière et un diplôme, je veux perdre de poids et je n'ai pas encore atteint ces objectifs."
  )
  textModelsRemove("t5-small")
})
