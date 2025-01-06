#Create a search reference for matvaretabellen----
matvaretabellen2024_query <- readRDS("./data-raw/matvaretabellen2024_query_prep.Rds") %>%
  #Individual database_database_ID for crème fraîche and veal_liver, so that they don't share database_ID's with sour cream and
  #beef liver respectively. Otherwise there will be duplicates when mapping to a recipe by ingredient database_ID
  mutate(database_ID = case_when(
    str_detect(Ingredients, 'crème fraîche|veal_liver') ~ database_ID + 300,

    TRUE ~ database_ID)) %>%
  drop_na(Ingredients) %>%

  #Create search words
  separate(., col = Ingredients, into = c('first_word', 'second_word'), sep = '_', remove = FALSE) %>%
  replace_na(list(second_word = '\\')) %>%

  #Fix some
  mutate(first_word = case_when(
    Ingredients == "fish_sauce" ~ 'sauce',
    Ingredients == 'cashew nut' ~ 'cashew',
    TRUE ~ first_word),

    second_word = case_when(
      Ingredients == "fish_sauce" ~ 'fish',
      Ingredients == 'cashew nut' ~ 'nut',
      TRUE ~ second_word)
  ) %>%

  #Arrange by lexogographical order and number of words in the first search term
  mutate(
    number_of_words1 = str_count(first_word, "\\b\\w+\\b"),
    number_of_words2 = str_count(second_word, "\\b\\w+\\b"),
    number_of_words2 = case_when(

      second_word == "\\" ~ 10,
      TRUE ~ number_of_words2

    )) %>%
  arrange(desc(number_of_words1), desc(number_of_words2), first_word, second_word) %>%
  select(-c(Ingredients, starts_with("number_of_words")))

#Save
saveRDS(matvaretabellen2024_query, "./data-raw/matvaretabellen2024_query.Rds")
