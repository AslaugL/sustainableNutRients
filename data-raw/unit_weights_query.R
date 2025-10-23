#Use store bought breads and rolls as default

# Pancake dry mix = pancake powder mix

#Create query words to search through a recipe list to link it to unit_weights
unit_weights_query <- readRDS("./data-raw/unit_weights2.Rds") %>% select(-c(grams_per_unit, unit)) %>% unique() %>% #Only keep names, not units
  mutate(Ingredients = str_replace_all(Ingredients, ',', '')) %>%

  #First two words contain the most important information to identify the ingredients
  mutate(

    first_word = case_when(

      # Keep first two words
      str_detect(Ingredients,
                 paste0(
                   c("bean canned", "bean dry", "wheat flour", "chicken skewer", "spice mix",
                     "(winter|summer) squash", "^chili pepper", "^ice cream", "^passion fruit",
                     "pita bread", "egg noodle", "spice mix", "lemon balm fresh", "soup instant",
                     "oat bran", "wheat bran", "sweet pepper grilled", "dinner kit", "bread crumb",
                     "bok choi", "black pepper", "bread brown", "bread rye", "cheese goat",
                     "cream sauce base", "lamb chop", "lamb leg", "soup instant", "cashew nut",
                     "sweet pepper", "pork neck"),
                   collapse = "|")) |
        Ingredients %in% c("bog blueberries", "chocolate glaze mix", "cream sauce base",
                           "lamb chop") ~ str_extract(Ingredients, "^\\w+ \\w+"),
      str_detect(Ingredients, "dairy imitate") ~ "dairy imitate",
      str_detect(Ingredients, "milk chocolate") ~ "milk chocolate",

      # Keep first word
      Ingredients %in% c("lard pork fat", "cabbage spring green", "pork neck chop", "pasta whole grain",
                         "waffel sour cream", "sausage turkey chicken", "chicken hen meat",
                         "chicken with skin", "tomato sun dried") |
        str_detect(Ingredients, paste0(
          c("minced meat", "^sauce", "^paste", "^nugget", "^beef"), collapse = "|")
        ) ~ str_extract(Ingredients, "^\\w+"),

      # Mixes
      str_detect(Ingredients, "powder mix|cake mix") ~ str_replace(Ingredients, "powder mix|cake mix", ""),

      # Longer words
      Ingredients %in% c("cheese semi hard", "sugar snap pea", "soft ripened cheese", "chili sin carne") ~ Ingredients,
      Ingredients %in% c("gluten-free flour wholemeal", "gluten-free flour white") ~ "gluten-free flour",
      TRUE ~ str_extract(Ingredients, '^\\w+')),


    second_word = case_when(

      # Keep last word
      str_detect(Ingredients,
                 paste0(
                   c("bean dry", "wheat flour", "chicken skewer", "spice mix",
                     "(winter|summer) squash", "^chili pepper", "^îce cream", "^passion fruit",
                     "pita bread", "egg noodle", "spice mix", "lemon balm fresh", "soup instant",
                     "oat bran", "wheat bran", "sweet pepper grilled", "dinner kit", "bread crumb",
                     "bread brown", "bread rye", "gluten-free flour", "lamb chop", "lamb leg",
                     "soup instant", "sweet pepper", "pork neck"),
                   " \\w", collapse = "|")) |
        Ingredients %in% c("ice cream cake", "chocolate glaze mix", "cream sauce base") ~ str_extract(Ingredients, "[a-zæøå-]+$"),

      # Keep last two words
      Ingredients %in% c("lard pork fat", "pea sugar snap", "paper spring roll", "sausage pork belly",
                         "pork ham roast", "curd passion fruit", "cabbage spring green", "pork neck chop",
                         "pasta whole grain", "waffel sour cream", "sausage turkey chicken", "beef rib-eye",
                         "cheese goat chevre white", "chicken hen meat", "salad lollo rosso",
                         "tomato sun dried", "tortilla corn flour", "waffle sour cream") |
        str_detect(Ingredients, "minced meat") ~ str_extract(Ingredients, "[a-zæøå-]+ [a-zæøå-]+$"),
      str_detect(Ingredients, "powder mix") ~ "powder mix",
      str_detect(Ingredients, "cake mix") ~ "cake mix",

      # Variable number of words
      str_detect(Ingredients, "dairy imitate ") ~ str_extract(Ingredients, "(?<=dairy imitate )[a-z ]+"),
      str_detect(Ingredients, "^bean canned ") ~ str_extract(Ingredients, "(?<=bean canned )[a-zæøå ]+"),
      str_detect(Ingredients, "^sauce ") ~ str_extract(Ingredients, "(?<=sauce )[a-zæøå ]+"),
      str_detect(Ingredients, "^tomato sun dried ") ~ str_extract(Ingredients, "(?<=tomato )[a-zæøå ]+"),
      str_detect(Ingredients, "^chili pepper ") ~ str_extract(Ingredients, "(?<=chili pepper )[a-z ]+"),
      str_detect(Ingredients, "^base") ~ str_extract(Ingredients, "(?<=base )[a-z ]+"),
      str_detect(Ingredients, "^nugget") ~ str_extract(Ingredients, "(?<=nugget )[a-z -]+"),
      str_detect(Ingredients, "^milk chocolate") ~ str_extract(Ingredients, "(?<=milk chocolate )[a-z ]+"),
      str_detect(Ingredients, "^cashew nut ") ~ str_extract(Ingredients, "(?<=cashew nut )[a-z ]+"),
      str_count(Ingredients, pattern = "\\w+") == 2 & !str_count(first_word, pattern = "\\w+") == 2  ~ str_extract(Ingredients, '\\w+$')
      ),
    second_word = case_when(
      first_word == second_word | str_detect(first_word, paste0(second_word, "$")) ~ NA,
      TRUE ~ second_word
    )
    ) %>%
  distinct() %>%
  replace_na(list(second_word = '\\')) %>%
  dplyr::mutate(
    across(ends_with("word"), ~str_trim(.))
  ) %>%

  #Clean up some of them that's not right, always have the most generic name in the first column, then a specification in second column if present
  # mutate(
  #   first_word = case_when(
  #     Ingredients == 'cured leg of mutton' ~ 'mutton',
  #     TRUE ~ first_word),
  #
  #   second_word = case_when(
  #     Ingredients == 'cured leg of mutton' ~ 'cured leg',
  #     TRUE ~ second_word
  #   )
  # ) %>%
  #Set column order
  select(first_word, second_word, database_ID) %>% arrange(first_word, second_word)


test <- unit_weights_query %>% summarise(.by = c("first_word", "second_word"), n = n(), ids = paste0(database_ID, collapse = ", ")) %>% dplyr::filter(n >1)


#Save
saveRDS(unit_weights_query, "./data-raw/unit_weights_query.Rds")
