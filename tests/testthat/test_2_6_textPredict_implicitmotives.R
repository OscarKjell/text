library(testthat)
library(text)
library(tibble)
library(dplyr)
library(tidyr)
#help(textrpp_initialize)
#textrpp_initialize(refresh_settings = T, save_profile = T)
##.rs.restartR()

context("Prediction")

test_that("textPredict Implicit motives", {
  skip_on_cran()
  set.seed(1)

  # Manually recreate the dataset for 20 participants and 80 stories
  PSE_stories <- tibble(data.frame(
    Unnamed_0 = 1:62,
    Participant_ID = rep(paste0('P', sprintf('%02d', 1:31)), each = 2),  # 31 participants, each contributing 2 stories
    Picture_ID = rep(c('IMG001', 'IMG002'), times = 31),
    Story_Text = c(
      "In the heart of the old forest, there was a place where magic still thrived. The trees whispered ancient secrets, and the ground pulsed with life.",
      "The sun was shining brightly as the family arrived at the park for a picnic. Laughter filled the air as the children ran to play.",
      "Emily loved to paint. One day, she found a magical brush that brought her paintings to life. Her world was suddenly filled with vibrant creatures.",
      "A young boy named Jake let go of his balloon, watching it soar into the sky. He smiled as it drifted away, imagining where it would go.",
      "The sea was calm, and the boat gently rocked as the fisherman cast his net. He hummed an old tune, feeling at peace with the world.",
      "The library was quiet except for the soft sound of turning pages. A girl sat in the corner, lost in a world of dragons and knights.",
      "The mountain path was steep, but they pressed on, determined to reach the summit. Clouds gathered overhead, and a storm was brewing.",
      "The train sped through the countryside, and the passengers stared out at the passing fields. In one car, a young woman scribbled furiously in a notebook.",
      "The city streets were alive with the buzz of activity. People hurried by, oblivious to the man playing a sad tune on his guitar.",
      "The forest was dark and dense, but they felt a sense of wonder as they ventured deeper. Something ancient watched them from the shadows.",
      "The campfire crackled as stories were shared. The group huddled closer, the cold night air nipping at their faces.",
      "The waterfall roared as it plunged into the river below. The mist hung in the air, creating a rainbow in the afternoon light.",
      "The old house creaked with every step. Dust hung in the air, and the furniture was covered in white sheets, as though it had been abandoned for years.",
      "The beach was deserted, except for a lone figure walking along the shoreline. She picked up a shell and held it to her ear, listening for the ocean.",
      "The stars twinkled overhead as the children lay in the grass, pointing out constellations. The night was still, and the world seemed infinite.",
      "The garden was in full bloom, with flowers of every color swaying gently in the breeze. A bumblebee buzzed from one blossom to the next.",
      # Continuing with similar multi-sentence stories
      "The desert stretched out before them, an endless sea of sand. The heat was oppressive, but they knew they had to keep moving.",
      "The market was bustling with vendors selling everything from spices to silk. The air was filled with the scent of exotic foods.",
      "The rain poured down, soaking them to the bone, but they laughed anyway. It was a memory they would cherish forever.",
      "The ship sailed on, its sails full of wind, cutting through the waves. Below deck, the crew prepared for the long journey ahead.",
      "The autumn leaves crunched underfoot as they walked through the park. The crisp air was filled with the scent of pine and earth.",
      "The old library was filled with books of every kind. She wandered through the aisles, running her fingers along the spines.",
      "The clock ticked slowly, marking the minutes as they passed. She stared at the door, waiting for him to return.",
      "The snow fell softly, blanketing the world in white. Everything was still, as though the earth was holding its breath.",
      "The forest floor was covered in a thick layer of fallen leaves, and their footsteps crunched loudly with every step.",
      "The cat stretched lazily in the sun, its tail flicking idly. It watched the birds in the trees with mild interest.",
      "The bakery smelled of fresh bread and cinnamon, making her stomach growl. She couldn't resist buying a warm croissant.",
      "The storm raged outside, but inside the cabin, the fire crackled warmly. They sat together, sipping hot cocoa and watching the flames dance.",
      "The train rumbled over the tracks, and she stared out the window, lost in thought. The scenery blurred as her mind wandered.",
      "The sound of waves crashing against the rocks filled the air. He stood at the edge of the cliff, watching the ocean with awe.",
      "The bustling city square was full of life, with street performers and food stalls at every corner. It was a sensory overload.",
      "The smell of pine filled the air as they hiked through the forest. The trail wound through the trees, leading them deeper into the wilderness.",
      # Continuing for the rest of the stories
      "The airplane soared above the clouds, and the passengers looked out at the endless sky. Below them, the world seemed so small.",
      "The sun set behind the mountains, casting a golden glow over the landscape. They stood in silence, taking in the beauty.",
      "The ancient ruins were overgrown with vines and moss, but they could still see the outlines of old buildings and statues.",
      "The moonlit path led them through the woods, the branches swaying gently in the breeze. The night was calm, but full of mystery.",
      "The ship creaked as it swayed in the harbor. Seagulls called out overhead, their cries echoing in the still air.",
      "The room was filled with the sound of laughter and music as they danced the night away. It was a night to remember.",
      "The train came to a stop, and the passengers began to disembark. She took a deep breath, ready for the adventure ahead.",
      "The firework exploded in a burst of color, lighting up the night sky. The crowd cheered as the next one launched into the air.",
      "In the heart of the old forest, there was a place where magic still thrived. The trees whispered ancient secrets, and the ground pulsed with life.",
      "The sun was shining brightly as the family arrived at the park for a picnic. Laughter filled the air as the children ran to play.",
      "Emily loved to paint. One day, she found a magical brush that brought her paintings to life. Her world was suddenly filled with vibrant creatures.",
      "A young boy named Jake let go of his balloon, watching it soar into the sky. He smiled as it drifted away, imagining where it would go.",
      "The sea was calm, and the boat gently rocked as the fisherman cast his net. He hummed an old tune, feeling at peace with the world.",
      "The library was quiet except for the soft sound of turning pages. A girl sat in the corner, lost in a world of dragons and knights.",
      "The mountain path was steep, but they pressed on, determined to reach the summit. Clouds gathered overhead, and a storm was brewing.",
      "The train sped through the countryside, and the passengers stared out at the passing fields. In one car, a young woman scribbled furiously in a notebook.",
      "The city streets were alive with the buzz of activity. People hurried by, oblivious to the man playing a sad tune on his guitar.",
      "The forest was dark and dense, but they felt a sense of wonder as they ventured deeper. Something ancient watched them from the shadows.",
      "The campfire crackled as stories were shared. The group huddled closer, the cold night air nipping at their faces.",
      "The waterfall roared as it plunged into the river below. The mist hung in the air, creating a rainbow in the afternoon light.",
      "The old house creaked with every step. Dust hung in the air, and the furniture was covered in white sheets, as though it had been abandoned for years.",
      "The beach was deserted, except for a lone figure walking along the shoreline. She picked up a shell and held it to her ear, listening for the ocean.",
      "The stars twinkled overhead as the children lay in the grass, pointing out constellations. The night was still, and the world seemed infinite.",
      "The garden was in full bloom, with flowers of every color swaying gently in the breeze. A bumblebee buzzed from one blossom to the next.",
      # Continuing with similar multi-sentence stories
      "The desert stretched out before them, an endless sea of sand. The heat was oppressive, but they knew they had to keep moving.",
      "The market was bustling with vendors selling everything from spices to silk. The air was filled with the scent of exotic foods.",
      "The rain poured down, soaking them to the bone, but they laughed anyway. It was a memory they would cherish forever.",
      "The ship sailed on, its sails full of wind, cutting through the waves. Below deck, the crew prepared for the long journey ahead.",
      "The autumn leaves crunched underfoot as they walked through the park. The crisp air was filled with the scent of pine and earth.",
      "The old library was filled with books of every kind. She wandered through the aisles, running her fingers along the spines."
    ),
    story_id = 1:62
  ))

  # Create datasets for testing merging feature help(reframe)
  # Participant level data for testing data merge
  PSE_stories_participant_level <- PSE_stories %>%
    group_by(Participant_ID) %>%
    reframe(stories=paste(Story_Text, collapse = " "))

  # Story level data for testing data merge
  PSE_stories_story_level <- PSE_stories

  # Sentence level data for testing data merge
  PSE_stories_sentence_level <- PSE_stories %>%
    filter()

  #split to sentences help(unnest)
  PSE_stories_sentence_level <- PSE_stories %>%
    mutate(Story_Text = strsplit(Story_Text, "[\\.\\!\\?]\\s+")) %>%
    tidyr::unnest(Story_Text)


  ###### Testing fine-tuned models #################

  # help(textClassify)
  implicit_motive <- text::textClassify(
    model_info = "implicitpower_roberta_ft_nilsson2024",
    texts = PSE_stories_participant_level$stories,
    show_texts = T
  )

  testthat::expect_equal(implicit_motive[[1]][[1]], 0)
  testthat::expect_equal(implicit_motive$.pred_0[[1]], 0.9975997, tolerance = 0.0001)
  testthat::expect_equal(implicit_motive$.pred_1[[2]], 0.006339252, tolerance = 0.0001)


  ### Merging Participant level
  predictions_participant_1 <- text::textPredict(
    texts = PSE_stories_participant_level$stories,
    model_info = "implicitpower_roberta23_nilsson2024",
    participant_id = PSE_stories_participant_level$Participant_ID,
    dataset_to_merge_predictions = PSE_stories_participant_level
  )
  testthat::expect_that(predictions_participant_1, testthat::is_a("list"))
  testthat::expect_equal(length(predictions_participant_1), 3)
  testthat::expect_equal(predictions_participant_1$sentence_assessments$.pred_0[[1]], 0.9233226, tolerance = 0.0001)
  testthat::expect_equal(predictions_participant_1$sentence_assessments$.pred_1[[1]], 0.07667742, tolerance = 0.0001)
  testthat::expect_equal(predictions_participant_1$person_assessments$person_prob[[1]], -0.07572437, tolerance = 0.0001)
  testthat::expect_equal(predictions_participant_1$person_assessments$person_class[[2]], -0.1359569, tolerance = 0.0001)
  testthat::expect_equal(predictions_participant_1$person_assessments$person_prob_no_wc_correction[[3]], 0.1402563, tolerance = 0.0001)

  testthat::expect_equal(predictions_participant_1$dataset$Participant_ID[[2]], "P02", tolerance = 0.0001)

  #
  predictions_participant_2 <- text::textPredict(
    texts = PSE_stories_participant_level$stories,
    model_info = "implicitpower_roberta23_nilsson2024",
    show_texts = T
    # participant_id = PSE_stories_participant_level$Participant_ID,
    # dataset_to_merge_predictions = PSE_stories_participant_level
  )
  testthat::expect_that(predictions_participant_2, testthat::is_a("tbl_df"))
  testthat::expect_equal(length(predictions_participant_2), 4)

  ### Merging Story level
  #as expected
  predictions_story_1 <- text::textPredict(
    texts = PSE_stories_story_level$Story_Text,
    model_info = "implicitachievement_roberta23_nilsson2024",
    participant_id = PSE_stories_story_level$Participant_ID,
    story_id = PSE_stories_story_level$story_id,
    dataset_to_merge_predictions = PSE_stories_story_level
  )
  testthat::expect_that(predictions_story_1, testthat::is_a("list"))
  testthat::expect_equal(length(predictions_story_1), 4)
  testthat::expect_equal(predictions_story_1$story_predictions$story_prob[[1]], -0.01077453, tolerance = 0.0001)
  testthat::expect_equal(predictions_story_1$story_predictions$story_prob_no_wc_correction[[2]], 0.01022855, tolerance = 0.0001)

####################################################################
  #as expected
  predictions_story_2 <- text::textPredict(
    texts = PSE_stories_story_level$Story_Text,
    model_info = "implicitachievement_roberta23_nilsson2024",
    #participant_id = PSE_stories_story_level$Participant_ID,
    #story_id = PSE_stories_story_level$story_id,
    dataset_to_merge_predictions = PSE_stories_story_level
  )
  testthat::expect_that(predictions_story_2, testthat::is_a("list"))
  testthat::expect_equal(length(predictions_story_2), 3)

  ### Merging sentence level.
  #this is working as expected
  predictions_sentence_1 <- text::textPredict(
    texts = PSE_stories_sentence_level$Story_Text,
    model_info = "implicitaffiliation_roberta23_nilsson2024",
    participant_id = PSE_stories_sentence_level$Participant_ID,
    story_id = PSE_stories_sentence_level$story_id,
    dataset_to_merge_predictions = PSE_stories_sentence_level
  )
  testthat::expect_that(predictions_sentence_1, testthat::is_a("list"))
  testthat::expect_equal(length(predictions_sentence_1), 4)
  testthat::expect_equal(predictions_sentence_1$story_predictions$story_prob[[1]], 0.2883911, tolerance = 0.0001)
  testthat::expect_equal(predictions_sentence_1$story_predictions$story_prob_no_wc_correction[[2]], 0.8627519, tolerance = 0.0001)

  # This one is now 10/10. Awesome
  predictions_sentence_2 <- text::textPredict(
    texts = PSE_stories_sentence_level$Story_Text,
    model_info = "implicitaffiliation_roberta23_nilsson2024",
    #participant_id = PSE_stories_sentence_level$Participant_ID,
    #story_id = PSE_stories_sentence_level$story_id,
    dataset_to_merge_predictions = PSE_stories_sentence_level
  )
  testthat::expect_that(predictions_sentence_2, testthat::is_a("list"))
  testthat::expect_equal(length(predictions_sentence_2), 3)
  testthat::expect_equal(predictions_sentence_2$sentence_assessments$.pred_0[[1]], 0.9089251, tolerance = 0.0001)
  testthat::expect_equal(predictions_sentence_2$sentence_assessments$.pred_1[[2]], 0.09193248, tolerance = 0.0001)

})
