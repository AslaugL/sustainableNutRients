#Use store bought breads and rolls as default

#Create query words to search through a recipe list to link it to unit_weights
unit_weights_query <- readRDS("./data-raw/unit_weights2.Rds") %>% select(-c(grams_per_unit, unit_enhet, reference)) %>% unique() %>% #Only keep names, not units
  mutate(Ingredients = str_replace_all(Ingredients, ',', '')) %>%

  #First two words contain the most important information to identify the ingredients
  mutate(first_word = str_extract(Ingredients, '^\\w+'),
         temp = str_extract(Ingredients, '^\\w+\\s\\w+')) %>%
  mutate(second_word = str_remove(temp, '^\\w+\\s')) %>%
  select(-temp) %>% unique() %>%
  replace_na(list(second_word = '\\')) %>%

  #Clean up some of them that's not right, always have the most generic name in the first column, then a specification in second column if present
  mutate(
    first_word = case_when(
      Ingredients == 'white or yellow hard to semi-hard cheese' ~ 'hard to semi-hard cheese',
      Ingredients == 'egg noodle cooked' ~ 'egg noodle',
      Ingredients == 'horse-radish' ~ 'horseradish',
      Ingredients == 'lasagne sheets uncooked' ~ 'lasagna',
      Ingredients == 'pork neck chops' ~ 'neck',
      Ingredients == 'sugar snap peas' ~ 'sugar snap peas',
      Ingredients %in% c('black pepper whole','black pepper grounded') ~ 'pepper',
      Ingredients == 'black chokeberries' ~ 'chokeberries',
      Ingredients == 'chick pea flour' ~ 'chick pea',
      Ingredients == 'chick pea canned' ~ 'chick pea',
      Ingredients == 'sugar snap pea' ~ 'pea',
      Ingredients == 'smoke-cured ham' ~ 'ham',
      Ingredients == 'cured ham' ~ 'ham',
      Ingredients == 'grilled sweet pepper' ~ 'sweet pepper',
      Ingredients == 'ground meat' ~ 'meat',
      Ingredients == 'bog blueberries' ~ 'bog blueberries',
      Ingredients == 'cured leg of mutton' ~ 'mutton',
      Ingredients == 'whole-grain pasta' ~ 'pasta',
      Ingredients == 'grapess' ~ 'grapes',
      Ingredients == 'soft-ripened cheese (brie camembert etc)' ~ 'soft ripened cheese',
      Ingredients == 'bean white large canned' ~ 'bean white',
      Ingredients == 'bean kidney canned' ~ 'bean kidney',
      Ingredients == 'bean black canned' ~ 'bean black',
      Ingredients == 'cream sauce base' ~ 'cream sauce base',
      Ingredients == 'fish soup base' ~ 'fish soup base',
      Ingredients == 'sausage turkey chicken' ~ 'sausage turkey chicken',
      Ingredients == 'ice cream' ~ 'ice cream',
      Ingredients == 'chicken skewer satay' ~ 'chicken skewer',
      Ingredients == 'tomato sun dried' ~ "tomato",
      Ingredients == 'tomato sun dried in oil' ~ "tomato",
      Ingredients == "cloudberries" ~ "cloud",
      Ingredients == 'lemon balm fresh' ~ 'lemon balm',
      Ingredients == 'oatbran' ~ 'oat bran',
      Ingredients == 'wheatbran' ~ 'wheat bran',
      Ingredients == "potato flatbread lompe" ~ "potato flatbread",
      TRUE ~ first_word),

    second_word = case_when(
      Ingredients == 'white or yellow hard to semi-hard cheese' ~ '\\',
      Ingredients == 'chicken with skin' ~ 'whole',
      Ingredients == 'chili pepper red' ~ 'red',
      Ingredients == 'cod traditional Norwegian dish Lutefisk' ~ 'lutefisk',
      str_detect(Ingredients, 'gg noodle') & str_detect(Ingredients, 'cooked') ~ 'cooked',
      str_detect(Ingredients, 'gg noodle') & str_detect(Ingredients, 'uncooked') ~ '\\',
      str_detect(Ingredients, 'capers|kapers') ~ '\\',
      Ingredients == 'lasagne sheets uncooked' ~ 'plate',
      Ingredients == 'oil liquid margarine' ~ '\\',
      Ingredients == 'onion yellow/red' ~ '\\',
      Ingredients == 'pork neck chops' ~ 'chops',
      Ingredients == 'quinoa tørr' ~ '\\',
      Ingredients == 'sugar snap peas' ~ '\\',
      Ingredients == 'mackerel fillet, in tomato sauce, canned' ~ 'tomato',
      Ingredients %in% c('olives black in oil canned', 'oliven, svarte, i olje, hermetisk') ~ 'black',
      Ingredients == 'black pepper whole' ~ 'whole',
      Ingredients == 'black pepper grounded' ~ 'ground',
      Ingredients == 'chick pea flour' ~ 'flour',
      Ingredients == 'chick pea canned' ~ 'canned',
      Ingredients == 'mackerel fillet, in tomato sauce, canned' ~ 'tomato',
      Ingredients == 'smoke-cured ham' ~ 'smoked',
      Ingredients == 'cured ham' ~ 'cured',
      Ingredients == 'grilled sweet pepper' ~ 'grilled',
      Ingredients == 'ground meat' ~ 'ground',
      Ingredients == 'black chokeberries' ~ 'black',
      Ingredients == 'bog blueberries' ~ '\\',
      Ingredients == 'cured leg of mutton' ~ 'cured leg',
      Ingredients == 'lentils dry' ~ '\\',
      Ingredients == 'sugar snap pea' ~ 'sugar snap',
      Ingredients == 'hamburger bun' ~ 'bread',
      Ingredients == 'whole-grain pasta' ~ 'whole grain',
      Ingredients == 'ginger root' ~ '\\',
      Ingredients == 'spinach, raw' ~ '\\',
      Ingredients == 'mushroom common' ~ '\\',
      Ingredients == 'olives black in oil canned' ~ 'black',
      Ingredients == 'bean white large canned' ~ 'canned',
      Ingredients == 'bean white in tomato sauce canned' ~ 'tomato',
      Ingredients == 'bean kidney canned' ~ 'canned',
      Ingredients == 'bean black canned' ~ 'canned',
      Ingredients == 'sausage turkey chicken' ~ '\\',
      Ingredients == 'ice cream' ~ '\\',
      Ingredients == 'chicken skewer satay' ~ 'satay',
      Ingredients == 'egg noodle cooked' ~ 'cooked',
      Ingredients == 'tomato sun dried' ~ "sun dried",
      Ingredients == 'tomato sun dried in oil' ~ "sun dried in oil",
      Ingredients == "cloudberries" ~ "berr",
      Ingredients == 'lemon balm fresh' ~ 'fresh',
      Ingredients == 'oatbran' ~ '\\',
      Ingredients == 'wheatbran' ~ '\\',
      Ingredients == "potato flatbread lompe" ~ "lompe",
      TRUE ~ second_word
    )
  ) %>%
  #Set column order
  select(first_word, second_word, database_ID, language) %>% arrange(first_word, second_word)



#Save
saveRDS(unit_weights_query, "./data-raw/unit_weights_query.Rds")
