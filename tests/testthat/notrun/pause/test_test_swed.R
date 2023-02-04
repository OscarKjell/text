


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


  swedish_text1 <- Language_based_assessment_data_8[1:20, 1:2]

  satisfactiontext <- c("Jag är inte nöjd med mitt liv. Jag är tacksam för det jag har och var jag är för situationen kan alltid vara värre. Jag vill ha en karriär och en examen, jag vill gå ner i vikt och jag har inte nått dessa mål än. Så jag är inte nöjd med mitt liv just nu. Jag känner att jag kan uppnå mer.",
                        "Jag är definitivt ganska nöjd just nu. Jag har varit mycket besviken i flera månader när jag var arbetslös och nu när jag arbetar är jag mycket nöjd med att veta att jag kan hjälpa till med min familjs börda. Jag är nöjd med min resa inom yoga och med det jag har lärt mig i detta nu hektiska liv. Jag är nöjd med att veta att jag kan ge och känner att jag vet att jag kan ta hand om mitt barn.",
                        "jag är väldigt nöjd. Jag lär mig och gör de saker jag vill göra med mitt liv. Jag klarar mig bra på det området.",
                        "Jag känner mig vilsen. Jag vet inte vad jag ska göra längre. Jag är osäker på mina val och om de kommer att leda mig på rätt väg eller inte. Jag vet inte om jag kan vara lycklig och hitta min plats och mitt syfte i den här världen.",
                        "Generellt är jag väldigt nöjd. Jag skulle gynnas av mer finansiell säkerhet, eftersom det för närvarande är svårt att betala alla mina räkningar medan jag är en heltidsstudent. Även om jag har en del funderingar om pengar är mina grundläggande behov tillgodosedda och livet är mycket tillfredsställande. Jag har gott om tid att njuta av livet och spendera all kvalitetstid med min fru och hundar så det finns inte mycket att klaga på. Jag har kärlek och mina grundläggande behov tillgodoses så jag är nöjd.",
                        "Jag är nöjd på grund av olika positiva aspekter i mitt liv. Jag mår bra med mig själv om jag vet att jag försökte mitt bästa och försökte allt möjligt inom min makt. För att avgöra om jag är nöjd eller inte finns det en standard som är satt och om den inte uppfylls kommer den att göra mig besviken.",
                        "Ja, överlag är jag nöjd med mitt liv. Många upp- och nedgångar, mest nedgångar, men det är så man kommer över nedgångarna och går upp! Jag har inga pengar, men jag är okej med det. Jag har tillräckligt att äta och dricka. Inga pengar för att betala alla räkningar, men jaja. Hade en stor olycka och har varit hemma sedan juli, och har precis börjat förbättras tillräckligt för att sitta vid datorn. Jag är nöjd, för det kunde ha varit mycket värre, eftersom jag hade njursvikt kunde jag ha hamnat i dialys, vilket jag undvek. Jag kunde ha tappat benet, men i stället jobbar jag fortfarande på att gå. Men jag kan gå, långsamt, men bättre än inte. Jag är nöjd med det jag har, inga klagomål.",
                        "Jag är väldigt nöjd i mitt liv. Jag har fått alla saker som jag har bestämt mig för att göra. Jag hittade min själsfrände och vi är lyckligt gifta. Jag har mitt drömjobb som jag tycker om att gå till måndag till fredag. Jag har tillräckligt med pengar att leva och några många att spela med. Jag är väldigt nöjd med mitt liv.",
                        "Jag är helnöjd med mitt liv både hemma och yrkesmässigt. Jag är överlycklig när varje dag kommer och ångrar verkligen ingenting. Jag känner mig trygg, har mycket självkänsla och ser fram emot framtiden. I grund och botten älskar jag livet.",
                        "Jag är nöjd men jag kan inte säga helt än. Det finns fortfarande saker som jag vill åstadkomma och livssituationer som jag ännu inte har upplevt för att få mitt liv att kännas bra. Jag är nöjd med att jag har hittat det viktigaste i livet och det är min andliga relation till Gud. Det är det viktigaste i livet. Jag vill ha fler nära vänner och eventuellt äktenskap och då kommer jag återigen känna att livet är tillfredsställande.",
                        "Jag skulle säga att jag är tacksam men inte nöjd och inte säker på att jag någonsin kommer att bli nöjd. Nöjd skulle vara för mig att det inte finns något kvar att göra, inget kvar att förbättra. Och i den meningen är jag inte det. Jag har många projekt oavslutade. Jag har många önskemål om personlig pedagogisk förbättring såväl som hemförbättring. Jag har önskemål om att förbättra mina barn och mitt dagliga liv.",
                        "JAG HAR ETT LÅNGVARIGT äktenskap som är bra för det mesta, men vi har verkligen haft svåra tider. Han är en bra man och vi kommer överens för det mesta. Jag fostrade två fantastiska barn som är självständiga, utbildade och ansvarsfulla. Jag har ett bra jobb och är ekonomiskt säker. Jag har rest över hela världen och har haft fantastiska möjligheter och upplevelser. Jag är frisk och ganska känslomässigt säker. På det hela taget har mitt liv varit bra.",
                        "Jag är trött. Jag springer i cirklar. Jag kanske kommer framåt men hamnar där jag började. Jag känner att jag är den enda som bryr mig, så det gör jag inte. att inte bry sig känns inte annorlunda, faktiskt värre. kanske jag bara inte förstår. jag känner mig malplacerad. mitt sinne är fortfarande ungt men kroppen är gammal. svårt att hitta någon som känner likadant.",
                        "Jag är överlag nöjd med livet. Jag njuter av mitt kärleksliv. Jag trivs också där jag är just nu i livet. Jag kanske inte är så långt som jag skulle vilja vara men jag kommer alltid ihåg att jag kunde vara sämre. Jag är nöjd med alla välsignelser i mitt liv och min familj.",
                        "Jag är ganska nöjd med mitt liv. Jag har en fantastisk man som älskar mig och skulle göra vad som helst för mig, mina fyra barn är vuxna nu och jag har 13 vackra barnbarn, jag är så välsignad.",
                        "När jag närmar mig de senare åren i mitt liv, finner jag att de saker som gör mig glad och ger mig frid är mycket annorlunda än när jag var yngre. Jag blir inte arg lika lätt eller låter saker störa mig lika mycket. Saker som verkade viktiga verkar inte lika viktiga. Pengar, framgång och den typen av saker spelar inte lika viktig roll som familjen. Jag är mycket gladare med små framgångar och prestationer. Jag är mer ifred.",
                        "Jag får lov att jag faktiskt är nöjd med mitt liv, men det finns alltid utrymme för avancemang. från där jag stod för ett par år sedan i livet för att veta att jag växte ganska mycket. Jag vet att jag har en högskoleexamen och är på väg till flottan. inte illa av någon som arresterades i ett år för att förändra sitt liv.",
                        "Överlag är jag nöjd med mitt liv. Jag är glad och frisk och har en väldigt positiv attityd. Jag tror att en positiv attityd skapar eller bryter varje situation. Jag har en varm plats att sova på på nätterna och mat på bordet. Jag kunde inte begära mer än så här för att få mig att känna mig nöjd med mitt liv.",
                        "Jag är nöjd. Livet kunde vara mycket värre. Jag har varit i sämre tider när det gäller pengar än just nu. Jag är inte rik men jag mår bra. Jag gjorde slut med min flickvän nyligen och även om det var svårt visste jag att vi borde ha gjort slut för länge sedan. Jag mår mycket bättre nu. Jag har några vänner och jag skulle göra allt för dem. Mitt ex som förrådde mig fick mig att känna mig hemsk men jag vet att det var hennes fel och inte mitt.",
                        "Jag är nöjd med livet, varje dag vaknar jag och det är en ny dag. Vad jag gör av den dagen är upp till mig, men tack och lov har jag möjlighet att komma ut och göra något)")

  harmonytext <- c("Jag är inte i harmoni i mitt liv så mycket som jag skulle vilja vara. Jag skulle vilja vara mer fridfull och mer produktiv. Jag vill ha ett balanserat liv. Jag skulle vilja spendera mer tid med min familj och vara mer fokuserad på mina mål och var jag vill vara i livet och vad jag vill uppnå.",
                   "Jag är mer i harmoni nu än jag var förra veckan eftersom jag äntligen har fått ett nytt jobb och har kommit till rätta med det. Jag känner mig lugn och bekväm med att veta att jag kan hjälpa till med räkningar och tjäna pengar. Mina tvångstankar om pengar har lagt sig och nu känner jag mig mer tillfreds med mitt liv. Jag känner mig mindre orolig så att jag kan meditera och yoga för att förbereda mig för hårt arbete igen och vara ifred med att interagera med nya människor och auktoriteter igen.",
                   "jag känner att jag är i harmoni med livet. Jag verkar vara väl kopplad till människorna runt omkring mig. Jag känner att mina kontakter har varit starkare än någonsin. Jag mår bra och är framgångsrik",
                   "Mina tankar känns utspridda, konstruerade och motsägelsefulla. Inget i mitt liv är vettigt tillsammans längre. Jag känner att jag försöker lägga ett pussel, men alla bitar kommer från olika uppsättningar.",
                   "I allmänhet är jag i harmoni. Eftersom mina ekonomiska behov inte riktigt är tillgodosedda och min hälsa är något ansträngd, har jag svårt att säga att jag är i harmoni. Jag skulle kunna använda mer hälsa, mindre smärta och mer pengar för att känna att jag verkligen har ett balanserat liv. På det hela taget är min disharmoni en tillfällig effekt av en långsiktig framgång (att gå i skolan på heltid), och min hälsa är inte dödlig så jag kan känna stor tröst i att veta att en dag kommer mina omständigheter att förändras och saker har potential att bli mer harmonisk.",
                   "Jag tror att jag är i harmoni på grund av olika aspekter i mitt liv. Först och främst känner jag att det är bra att vara rationell och perspektiv inom sig själv och sin omgivning. Om du har en bra känsla för sakerna omkring dig kan du göra en bra bedömning om du är i harmoni eller inte.",
                   "Att vara i harmoni med naturen är viktigt för mig. Jag älskar att titta på fåglarna och alla andra levande varelser som strövar omkring. Jag älskar allt som växer, och det finns ett syfte med allt. Jag känner frid när jag är nära havet eller på en båt. Jag känner mig lugn och i harmoni när jag är på bakgården och ser alla växter, träd, blommor och ogräs växa. Det är tråkigt att så många människor inte bryr sig om naturen.",
                   "Mitt liv är i harmoni. Jag har ett balanserat arbetsliv. Mitt arbete stannar på jobbet. Jag tar aldrig hem den. Mitt hemliv stör inte arbetet. På helgerna kan jag ha roligt och släppa utan att oroa mig för jobbet.",
                   "Det viktigaste i mitt liv är mina barn och min fru. Dessa områden får mig att må väldigt bra och givande. Jag känner mig önskad, älskad och omhuldad hela tiden. Jag har aldrig ett klagomål och njuter av varje dag.",
                   "Jag skulle säga att jag är i harmoni eftersom jag har lärt mig mycket i livet. När jag var i tjugoårsåldern hade jag inte denna visdom. Jag var mindre lugn. Nu på grund av allt jag har lärt mig; inte bara bokkunskap utan livskunskap jag är ifred. Jag vet varför jag känner som jag gör. Jag uppskattar de små sakerna som andra människor, naturen, Gud och jag inser att det är klokt att sätta de små sakerna i livet först.",
                   "Jag känner inte heller att jag är i harmoni. Det finns aspekter av mitt liv som är i harmoni, men de flesta verkar fortfarande vara mer kaotiska än jag skulle vilja. Jag känner att jag har för mycket grejer. Jag tror att eliminering av många saker skulle frigöra mer tid att spendera på att faktiskt leva. Jag har börjat träna och känner att min kropp blir mer i harmoni med sig själv. Maten vi äter är ekologisk och fräsch, jag lagar mycket hemma och den här aspekten känns som att vi är i harmoni med naturen. Men oordningen och kaoset av att ha för mycket saker blir överväldigande.",
                   "Jag är i harmoni eftersom jag är självsäker och har kontroll över de flesta omständigheter. Jag blir sällan upprörd och jag har en positiv syn. Jag är stadig på jobbet och trivs med mitt jobb. Jag kommer överens med mina medarbetare och känner att jag bidrar till världen och hjälper människor. Jag trivs med min familj och trivs med deras sällskap.",
                   "nej jag är inte i harmoni. hur kan du vara i harmoni om du inte är nöjd? varför jag inte är i harmoni? titta på föregående sida. jag förstår inte ibland.",
                   "Jag känner att jag är i harmoni. Jag är en ganska glad person. Jag är alltid väl utvilad och lugn. Jag låter inte saker stressa mig. Jag försöker ta vara på dagen och alltid göra något positivt av den. Jag njuter av mitt liv och försöker göra det bästa av det.",
                   "Mitt liv är nu i fridfull harmoni.Jag låter inte vad andra tänker eller säger eller gör påverka mig längre.Jag har blockerat all negativitet från mitt liv.",
                   "Jag känner mig mycket mer i harmoni i detta skede av mitt liv än någon annan tid i mitt liv. Jag är mer medveten om min omgivning. Jag tycker om att lyssna på musik och se naturen. När jag var yngre tog jag dessa saker för givet. Jag är mycket mer nöjd med mitt liv.",
                   "Harmoni betyder mycket för mig, det är insikten om någon som älskar sig själv. vilket är en sak som jag tror att alla borde ha, för när du gör det verkar livet gå mycket bättre. att älska sig själv är det första steget i livet tror jag. så harmoni är bara den kärlek och frid du har med dig själv.",
                   "Jag är i harmoni med mitt liv. Jag är glad och stark och säker på foten. Jag vet vem jag är i livet och vilka mina styrkor och svagheter är. Jag vet också vad jag strävar efter att bli och vilka steg jag måste ta för att nå dit. Jag är nöjd där jag är men skulle älska att driva mig själv längre för att bli mer. Överlag är jag väldigt nöjd med mitt liv.",
                   "Jag har alltid varit väldigt centrerad i livet. Jag är en våg och även om jag inte tror på sånt känns det som att vågar representerar mig väldigt bra. Jag är balanserad. Jag är svår att reta upp. Samtidigt är det svårt för mig att hitta glädje i saker. Det är ok ändå. Jag är alltid lugn. Jag är en klippa och är svår att flytta.",
                   "Jag är i harmoni, mitt hus är tyst och fridfullt förutom enstaka slagsmål med min fästman, men vi är lyckliga, och det är vår katt också)")

  swe_test2 <- tibble::tibble(satisfactiontext, harmonytext)



  #textModels()
  harmony_word_embeddings <- textEmbed(swe_test2,
                                       model = "KBLab/bert-base-swedish-cased",
                                       dim_name = TRUE,
                                       layers = c(11:12),
                                       aggregation_from_layers_to_tokens = "concatenate",
                                       aggregation_from_tokens_to_texts = "mean",
                                       aggregation_from_tokens_to_word_types = "mean"
  )

  expect_equal(harmony_word_embeddings$texts$satisfactiontext[[1]][1], -0.1405028, tolerance = 0.0001)
  expect_equal(harmony_word_embeddings$texts$satisfactiontext[[1]][2], -0.1395987, tolerance = 0.00001)
  dim1for1 <- harmony_word_embeddings$word_types[[3]][harmony_word_embeddings$word_types[[1]]=="du"]
  expect_equal(dim1for1, 0.04012914, tolerance = 0.00001)

  dim1for2 <- harmony_word_embeddings$word_types[[3]][harmony_word_embeddings$word_types[[1]]=="!"]
  expect_equal(dim1for2, 1.469593, tolerance = 0.00001)
  dim1for3 <- harmony_word_embeddings$word_types[[3]][harmony_word_embeddings$word_types[[1]]=="-"]
  expect_equal(dim1for3, -0.6040312, tolerance = 0.00001)

  text_train_results1 <- textTrainRegression(
    x = harmony_word_embeddings$texts["satisfactiontext"],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = FALSE
  )

  expect_that(text_train_results1$results$estimate[1],
              is_a("numeric"))
  expect_gt(text_train_results1$results$estimate[1],
            .3)
  expect_equal(text_train_results1$results$estimate[[1]],
               0.4069999, tolerance = 0.00001)

  # Train with x_variable
  train_x_append <- text::textTrainRegression(
    x = harmony_word_embeddings$texts["satisfactiontext"],
    x_append = Language_based_assessment_data_8[1:20, 6:7],
    y = Language_based_assessment_data_8["hilstotal"][1:20, ],
    cv_method = "cv_folds",
    outside_folds = 2,
    inside_folds = 2,
    outside_strata_y = NULL,
    inside_strata_y = NULL,
    # preprocess_PCA = c(0.20),
    preprocess_PCA = NA,
    penalty = 1e-16,
    multi_cores = FALSE
  )

  # Predict
  hils_predicted_scores1 <- text::textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings$texts["satisfactiontext"],
    dim_names = TRUE
  )

  expect_that(hils_predicted_scores1[[1]],
              is_a("numeric"))
  expect_equal(hils_predicted_scores1[[1]][1],
               11.39664, tolerance = 0.00001)

  # Same as above with different input
  hils_predicted_scores1b <- textPredict(
    model_info = text_train_results1,
    word_embeddings = harmony_word_embeddings$texts,
    dim_names = TRUE
  )
  expect_that(hils_predicted_scores1b[[1]], is_a("numeric"))
  expect_equal(hils_predicted_scores1b[[1]][1],
               11.39664, tolerance = 0.00001)

  # Including x_append
  hils_predicted_scores1 <- textPredict(
    model_info = train_x_append,
    word_embeddings = harmony_word_embeddings$texts,
    # x_append = Language_based_assessment_data_8[1:20, ], # sending all give same as below: 12.40038
    # x_append = Language_based_assessment_data_8[1:20, 6:7], # sending only swlstotal and age: 12.40038
    x_append = Language_based_assessment_data_8[1:20, c(7, 6)], # sending "wrong" order give: 12.40038
    append_first = FALSE,
    # x_append = Language_based_assessment_data_8[1:20, c(5,6) ], # missing one throws error
    dim_names = TRUE
  )
  expect_equal(hils_predicted_scores1[[1]][1],
               11.70798, tolerance = 0.00001)


#  # Predict ALL
#  models_1_2 <- list(text_train_results1, train_x_append)
#  all_predictions <- textPredictAll(
#    models = models_1_2,
#    word_embeddings = harmony_word_embeddings$texts,
#    x_append = Language_based_assessment_data_8[1:20, 5:8],
#    append_first = FALSE
#  )
#
#  expect_equal(all_predictions[[1]][1], 11.89, tolerance = 0.1)
#  expect_equal(all_predictions[[2]][1], 12.40038, tolerance = 0.01)
#
#
#  proj <- textProjection(
#    words = Language_based_assessment_data_8[1],
#    word_embeddings = harmony_word_embeddings$texts$satisfactiontexts,
#    word_types_embeddings = harmony_word_embeddings$word_type,
#    x = Language_based_assessment_data_8$hilstotal,
#    y = Language_based_assessment_data_8$swlstotal,
#    pca = NULL,
#    aggregation = "mean",
#    split = "quartile",
#    word_weight_power = 1,
#    min_freq_words_test = 0,
#    Npermutations = 10,
#    n_per_split = 5,
#    seed = 1003
#  )
#
#  expect_that(proj[[1]][[1]][[1]][[1]], is_a("numeric"))
#  expect_equal(proj[[1]][[1]][[1]][[1]], 0.2005167, tolerance = 0.0000001)
#
#  # for decontexts = TRUE expect_equal(proj[[1]][[1]][[1]][[1]], -0.402433, tolerance = 0.0000001)
#
#  plot_proj <- textProjectionPlot(
#    word_data = proj,
#    explore_words = c("happy"),
#    y_axes = TRUE
#  )
#  plot_proj$processed_word_data$n[1]
#  expect_that(plot_proj$processed_word_data$n[1], is_a("numeric"))
#  # expect_equal(plot_proj$processed_word_data$n[1], 2)
#  # expect_equal(plot_proj$processed_word_data$n[1], 1)
#
#  one_character <- plot_proj$processed_word_data %>%
#    dplyr::filter(words == "-")
#  expect_equal(one_character$n, 1)
#  #  pred_word <- textWordPrediction(words = Language_based_assessment_data_8[1],
#  #                                  single_word_embeddings = harmony_word_embeddings$singlewords_we,
#  #                                  x = Language_based_assessment_data_8$hilstotal)
#  #
#  #  plot_pred <- textPlot(pred_word,
#  #                        explore_words = c("happy"),
#  #                        y_axes = FALSE)
#  #
#
#
#  text_train_results <- textTrain(
#    x = harmony_word_embeddings$texts$satisfactiontexts,
#    y = Language_based_assessment_data_8$hilstotal[1:20],
#    cv_method = "cv_folds",
#    outside_folds = 2,
#    inside_folds = 2,
#    outside_strata_y = NULL,
#    inside_strata_y = NULL,
#    # preprocess_PCA = c(0.20),
#    preprocess_PCA = NA,
#    penalty = 1e-16,
#    multi_cores = "multi_cores_sys_default"
#  )
#
#  expect_that(text_train_results$results$estimate[1], is_a("numeric"))
#  expect_gt(text_train_results$results$estimate[1], 0.3)
#  expect_equal(text_train_results$results$estimate[[1]], 0.3273128, tolerance = 0.0001)
#
#
#  # Predict
#  hils_predicted_scores1 <- textPredict(
#    model_info = text_train_results,
#    word_embeddings = harmony_word_embeddings$texts
#  )
#
#  expect_that(hils_predicted_scores1[[1]][1], is_a("numeric"))
#  expect_equal(hils_predicted_scores1[[1]][1], 11.89219, tolerance = 0.000001)
#
#


})




