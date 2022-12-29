
### For excluding stopwords OK
tidytext::unnest_tokens(word, message)
text_cleaning_tokens <- text_cleaning_tokens %>% filter(!(nchar(word) == 1))%>%
  anti_join(stop_words[572:745,])
tokens <- text_cleaning_tokens %>% filter(!(word==""))
tokens <- tokens %>% mutate(ind = row_number())
tokens <- tokens %>% group_by(id) %>% mutate(ind = row_number()) %>%
  tidyr::spread(key = ind, value = word)
tokens [is.na(tokens)] <- ""
tokens <- tidyr::unite(tokens, text,-id,sep =" " )
tokens$text <- trimws(tokens$text)

### Creating absolut frequencies

<-function(frequencies)
  df<-data.frame(table(unlist(strsplit(tolower(LoL_S2_Andy$HarmonyMost), " "))))
df$RelFre<-df$Freq/sum(df$Freq)

View(df)
### Unique frequencies. I.e., how many used a specific word
for (i in 1:nrow(HarmonyOnly))
{
  d <- unlist(strsplit(HarmonyOnly$HarmonyMost[i], split=" "))
  Harmonyunique$descriptions[i]<-paste(d[-which(duplicated(d))], collapse = ' ')
  if(Harmonyunique$descriptions[i] != "")
  {
    HarmonyUnique$HarmonyMost[i]<-Harmonyunique$descriptions[i]
  }
}

### If one wants to compare different columns
df_list<-list(harfreUni, Happfre, BestFre)
RawFreTableS2<-Reduce(function(x, y) merge(x, y, all=TRUE), df_list)
