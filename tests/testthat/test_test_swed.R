


library(text)
library(tibble)
library(testthat)

context("Testing different systems for swedish")



test_that("AÅÖ", {
  skip_on_cran()

  text_swedish <- c("hej jag mår väldigt trött", "Allt bra med mig ska du veta")

  embeddings <- textEmbed(texts = text_swedish,
            model = "KBLab/bert-base-swedish-cased")

  expect_equal(embeddings$texts$texts[[1]][[1]], 0.0832777, tolerance = 0.00001)
  expect_equal(embeddings$texts$texts[[1]][[2]], 0.1389231, tolerance = 0.00001)

  text_swedish <- c("Björnar skiljer sig från de andra familjerna i samma ordning genom att inte ha några egentliga rovtänder, de har i allmänhet en stor kropp, kort svans och raka tår med långa, trubbiga klor, som inte kan dras in.[1] Björnarnas ögon är små och öronen runda och uppåtriktade. De har långsträckt nos och tandformeln är I 3/3 C 1/1 P 3-4/4 M 2/3, alltså 40 till 42 tänder.[2] Björnar är hälgångare och fotsulorna är oftast försedda med hår. Bara några arter som ofta klättrar i träd, till exempel malajbjörnen, har naken fotsula. Vid alla fötter finns fem tår. Svansen är bara en liten stump.[3] Pälsen är oftast lång, hos merparten arter enfärgad och oftast brun eller svart. Undantag är jättepandan med påfallande svartvit teckning och isbjörnen som är vit. Ljusare markeringar på bröstet och i ansiktet finns hos några arter.Vikten varierar mellan 25 kilogram (malajbjörn) och 800 kilogram (isbjörn, upp till 1000 kg), och oavsett art är vuxna hanar alltid tyngre än honorna. Längden är 100 till 280 centimeter.")
  textdescri <- textDescriptives(text_swedish)
  expect_equal(textdescri[2][[1]], 176)
})




