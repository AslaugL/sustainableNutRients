#Create a search reference for matvaretabellen----
matvaretabellen2020_query <- readRDS("./data-raw/matvaretabellen2020_query_prep.Rds") %>%
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
    Ingredients == 'alfalfa seed, sprouted, raw' ~ 'alfalfa',
    Ingredients == 'cashew nut, salted' ~ 'cashew',
    Ingredients == 'cashew nut' ~ 'cashew',
    TRUE ~ first_word),

    second_word = case_when(
      Ingredients == 'alfalfa seed, sprouted, raw' ~ 'sprout',
      Ingredients == 'cashew nut, salted' ~ 'salt',
      Ingredients == 'cashew nut' ~ 'nut',
      TRUE ~ second_word)
  ) %>%

  #Arrange in lexogographical order
  arrange(first_word, second_word) %>%
  select(-Ingredients)

#Save
saveRDS(matvaretabellen2020_query, "./data-raw/matvaretabellen2020_query.Rds")

