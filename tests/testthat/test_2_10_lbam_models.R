
################
#### Automatic unit testing for testing each model included in the L-BAM library
################

library(testthat)
library(tibble)
library(text)

context("Testing L-BAM models")

test_that("Testing L-BAM models", {
  skip_on_cran()

  res <- text::textAssess(model_info =  "depression_select_phq9_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 8.961579, tolerance = 0.001)

  # Checking stop warning
  testthat::expect_error(
    text::textAssess(
      model_info = "depression_select_phq9_roberta23_gu2024",
      word_embeddings = word_embeddings_4$texts["harmonywords"],
      dim_names = FALSE
    ),
    regexp = "[Ww]ord embedding settings do not match"
  )

  res <- text::textAssess(model_info =  "depression_words_phq9_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 10.9472, tolerance = 0.001)

  res <- text::textAssess(model_info =  "depression_phrases_phq9_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 12.56859, tolerance = 0.001)

  res <- text::textAssess(model_info =  "depression_text_phq9_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 10.73265, tolerance = 0.001)

  res <- text::textAssess(model_info =  "depression_select_cesd_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 20.19071, tolerance = 0.001)

  res <- text::textAssess(model_info =  "depression_word_cesd_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 25.76702, tolerance = 0.001)

  res <- text::textAssess(model_info =  "depression_phrase_cesd_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 24.7369, tolerance = 0.001)

  res <- text::textAssess(model_info =  "depression_text_cesd_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 25.04321, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_select_gad7_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 7.463203, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_words_gad7_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 8.142148, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_phrases_gad7_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 9.346749, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_text_gad7_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 8.277918, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_select_pswq_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 35.68946, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_words_pswq_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 38.08564, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_phrases_pswq_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 39.04689, tolerance = 0.001)

  res <- text::textAssess(model_info =  "worry_text_pswq_roberta23_gu2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 39.58207, tolerance = 0.001)

  res <- text::textAssess(model_info =  "suicidalityrisk_suicidalitytext_mixedbread23_gu2025", texts = "hello", dim_names = TRUE)
  testthat::expect_equal(res[[1]], 0.4167996, tolerance = 0.001)

  res <- text::textAssess(model_info =  "selfharmrisk_selfharmtext_mixedbread23_gu2025", texts = "hello", dim_names = TRUE)
  testthat::expect_equal(res[[1]], 0.3332303, tolerance = 0.001)


  res <- text::textAssess(model_info =  "valence_facebook_roberta23_eijsbroek2024", texts = "hello", dim_names = TRUE)
  testthat::expect_equal(res[[1]], 4.704488, tolerance = 0.001)

  res <- text::textAssess(model_info =  "valence_facebook_roberta23_eijsbroek2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 4.704488, tolerance = 0.001)

  res <- textAssess(model_info =  "valence_facebook_mxbai23_eijsbroek2024", texts = "hello", dim_names = TRUE)
  testthat::expect_equal(res[[1]], 5.142306, tolerance = 0.001)


  res <- text::textAssess(model_info =  "implicitpower_roberta23_nilsson2024", texts = tibble(texts = "hello"), dim_names = T)
  testthat::expect_equal(res[[2]], 0.9879577, tolerance = 0.001)
  #
  res <- text::textAssess(model_info =  "implicitpower_roberta_ft_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]],  0.9955019, tolerance = 0.001)

  res <- text::textAssess(model_info =  "implicitachievement_roberta23_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], 0.9768399, tolerance = 0.001)

  res <- text::textAssess(model_info =  "implicitachievement_roberta_ft_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .999077, tolerance = 0.001)

  res <- text::textAssess(model_info =  "implicitaffiliation_roberta_ft_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .997766, tolerance = 0.0001)

  ######
  res <- text::textAssess(model_info =  "implicitpower_germanbert11_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], 0.996004, tolerance = 0.001)
  #
  res <- text::textAssess(model_info =  "implicitpower_roberta23_previoussentence_nilsson2024", texts = tibble(texts = "hello"), dim_names = T)
  testthat::expect_equal(res[[2]], .9854281, tolerance = 0.001)
  #


  res <- text::textAssess(model_info =  "implicitachievement_germanbert11_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], 0.9851559, tolerance = 0.001)

  #
  res <- text::textAssess(model_info =  "implicitachievement_roberta23_previoussentence_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .9623212, tolerance = 0.001)
  #
  res <- text::textAssess(model_info =  "implicitaffiliation_roberta23_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .9269618, tolerance = 0.001)
  #
  res <- text::textAssess(model_info =  "implicitaffiliation_germanbert11_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .9755044, tolerance = 0.001)
  #
  res <- text::textAssess(model_info =  "implicitaffiliation_roberta23_previoussentence_nilsson2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .8238421, tolerance = 0.001)

  ######


  res <- text::textAssess(model_info =  "harmony_words_bert23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 21.3629, tolerance = 0.001)

  res <- text::textAssess(model_info =  "harmony_text_bert23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 23.7938, tolerance = 0.001)

  res <- text::textAssess(model_info =  "harmony_words_roberta23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]],  21.92362, tolerance = 0.001)

  res <- text::textAssess(model_info =  "harmony_text_roberta23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 24.14056, tolerance = 0.001)

  res <- text::textAssess(model_info =  "satisfaction_words_bert23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 24.72859, tolerance = 0.001)

  res <- text::textAssess(model_info =  "satisfaction_text_bert23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 30.30167, tolerance = 0.001)

  res <- text::textAssess(model_info =  "satisfaction_words_roberta23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 18.38246, tolerance = 0.001)

  res <- text::textAssess(model_info =  "satisfaction_text_roberta23_kjell2022", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[1]], 27.26928, tolerance = 0.001)

  res <- text::textAssess(model_info =  "harmony_balance_roberta23_lomas2024", texts = "hello", dim_names = T)
  testthat::expect_equal(res[[2]], .581874, tolerance = 0.001)
  #
  res <- text::textAssess(model_info =  "harmony_balance_is_it_roberta23_lomas2024", texts = "hello", dim_names = FALSE)
  testthat::expect_equal(res[[2]], .5067378, tolerance = 0.001)

  res <- text::textAssess(model_info =  "workengagement_words_mxbai23_nilsson2024", texts = "hello", dim_names = T)
  testthat::expect_equal(res[[1]], 11.22422, tolerance = 0.001)

  res <- text::textAssess(model_info =  "jobsatisfaction_words_mxbai23_nilsson2024", texts =  "hello", dim_names = T)
  testthat::expect_equal(res[[1]], 10.5646, tolerance = 0.001)

  res <- text::textAssess(model_info =  "mental_health_recommendations_mxbai_parsimonious_model_wiebel2024", texts = "hello", dim_names = T)
  testthat::expect_equal(res[[1]], 2.362874, tolerance = 0.001)

  res <- text::textAssess(model_info =  "autonomy_text_bert23_mesquiti2025", texts =  "hello you", dim_names = F)
  testthat::expect_equal(res[[1]], 4.704528, tolerance = 0.001)

  res <- text::textAssess(model_info =  "satisfaction_text_bert23_mesquiti2026", texts = "hello everyone", dim_names = F)
  testthat::expect_equal(res[[1]], 7.404376, tolerance = 0.001)

  res <- text::textAssess(model_info =  "mentalhealth_interview_mxbai23_kjell2025", texts =  "hello you", dim_names = F, check_matching_word_embeddings = F)
  testthat::expect_equal(res[[1]],
                        # 56.10808,
                         50.77206,
                         tolerance = 0.001)

  res <- text::textAssess(model_info =  "physicalhealth_interview_mxbai23_kjell2025", texts =  "hello you", dim_names = F, check_matching_word_embeddings = F)
  testthat::expect_equal(res[[1]],
                       #  29.80874,
                         33.72575,
                         tolerance = 0.001)


  # list.files()
  delete <- c("autonomy_autonomy.RDS",
              "depressionphrase_robertaL23_cesd_Gu2024.rds",
              "depressionphrase_robertaL23_phq9_Gu2024.rds",
              "depressionselect_robertaL23_cesd_Gu2024.rds",
              "depressionselect_robertaL23_phq9_Gu2024.rds",
              "depressiontext_robertaL23_cesd_Gu2024.rds",
               "depressiontext_robertaL23_phq9_Gu2024.rds" ,
               "depressionword_robertaL23_cesd_Gu2024.rds" ,
               "depressionword_robertaL23_phq9_Gu2024.rds",
               "Facebook_mxbai_ValenceModel.rds",
               "Facebook_RoBERTaLarge_ValenceModel.rds",
               "harmony_text_bert_large_uncased_23_HILS_Kjell2022.rds",
               "harmony_text_roberta-large_23_HILS_Kjell2022.rds",
               "harmony_words_bert-large-uncased_23_HILS_Kjell2022.rds",
               "harmony_words_roberta-large_23_HILS_Kjell2022.rds",
               "jobsatisfaction_words_mxbai23_nilsson2024.rds",
               "mixbread_recommendations_parsimonious_model.rds",
               "satisfaction_text_bert-large-uncased_23_SWLS_Kjell2022.rds",
               "satisfaction_text_roberta-large_23_SWLS_Kjell2022.rds",
               "satisfaction_words_bert-large-uncased_23_SWLS_Kjell2022.rds",
               "satisfaction_words_roberta-large_23_SWLS_Kjell2022.rds",
               "selfharmrisk_selfharmtext_mixedbread23_gu2025.rds",
               "suicidalityrisk_suicidalitytext_mixedbread23_gu2025.rds",
               "swls_swls.rds",
               "textPredict_381541617.RDS",
               "textPredict_381544597.RDS",
               "textPredict_435931617.RDS",
               "textPredict_681171617.RDS",
               "textPredict_694261617.RDS",
               "textPredict_828971617.RDS",
               "textPredict_941251617.RDS",
               "textPredict_3815411016.RDS",
               "textPredict_828972152.RDS",
              "textPredict_337104597.RDS",
               "workengagement_words_mxbai23_nilsson2024.rds",
               "worryphrase_robertaL23_gad7_Gu2024.rds",
               "worryphrase_robertaL23_pswq_Gu2024_corrected.rds",
               "worryselect_robertaL23_gad7_Gu2024.rds",
               "worryselect_robertaL23_pswq_Gu2024_corrected.rds",
               "worrytext_robertaL23_gad7_Gu2024.rds",
               "worrytext_robertaL23_pswq_Gu2024_corrected.rds",
               "worryword_robertaL23_gad7_Gu2024.rds",
               "worryword_robertaL23_pswq_Gu2024_corrected.rds",
              "models_mx_bai_MCS.rds",
              "models_mx_bai_PCS.rds")

  file.remove(delete)

})



