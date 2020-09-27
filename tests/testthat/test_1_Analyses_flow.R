
#.rs.restartR()
#library(reticulate)
#install_miniconda(update = TRUE)
#help(conda_install)
conda_install(envname = 'r-reticulate', c('torch==1.5.0'), pip = TRUE, force=TRUE)
#$ conda update -n base -c defaults conda
#miniconda_update()

library(testthat)
library(text)
library(tibble)

context("Big analyses flow")

test_that("Testing textEmbed as well as train", {

  long_text_test <- c("Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
Humour (British English) or humor (American English; see spelling differences) is the tendency of experiences to provoke laughter and provide amusement. The term derives from the humoral medicine of the ancient Greeks, which taught that the balance of fluids in the human body, known as humours (Latin: humor, body fluid), controlled human health and emotion.
People of all ages and cultures respond to humour. Most people are able to experience humour—be amused, smile or laugh at something funny (such as a pun or joke)—and thus are considered to have a sense of humour. The hypothetical person lacking a sense of humour would likely find the behaviour inducing it to be inexplicable, strange, or even irrational. Though ultimately decided by personal taste, the extent to which a person finds something humorous depends on a host of variables, including geographical location, culture, maturity, level of education, intelligence and context. For example, young children may favour slapstick such as Punch and Judy puppet shows or the Tom and Jerry cartoons, whose physical nature makes it accessible to them. By contrast, more sophisticated forms of humour such as satire require an understanding of its social meaning and context, and thus tend to appeal to a more mature audience.
")
  textEmbed(long_text_test)


#Get this warnong Warning message:
#    In list(list(package = "torch", pip = TRUE), list(package = "transformers",  :
#                                                        argument 4 is empty

  short_text <- c("hello how are you. I'm fine thanks.")
  textEmbed(short_text)

  harmony_word_embeddings <- textEmbed(Language_based_assessment_data_8[1],
                                       model = "bert-base-uncased",
                                       layers=11)

  text_train_results <- textTrain(x = harmony_word_embeddings$harmonywords,
                                  y = Language_based_assessment_data_8$hilstotal,
                                  preprocess_PCA = c(0.20),
                                  penalty = 1e-16)

  expect_that(text_train_results$results$estimate[1], is_a("numeric"))
  expect_gt(text_train_results$results$estimate[1], 0.3)


  # Predict
  hils_predicted_scores1 <- textPredict(model_info = text_train_results,
                                       new_data = harmony_word_embeddings$harmonywords)
  #hils_predicted_scores1

  expect_that(hils_predicted_scores1$.pred[1], is_a("numeric"))
})

