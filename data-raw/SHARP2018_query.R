#Dataframe of query words for SHARP database
SHARP2018_query <- readRDS("./data-raw/SHARP2018_query_prep.Rds") %>%

  #Remove plural s
  mutate(Ingredients = case_when(
    Ingredients %in% c('apples', 'pineapples', 'avocados', 'apricots',
                       'beetroots', 'breadcrumbs', 'carrots', 'cauliflowers',
                       'buns', 'chards', 'clams', 'coconuts', 'courgettes', 'crabs',
                       'cucumbers', 'figs', 'grapefruits', 'groupers', 'hazelnuts',
                       'herrings', 'kales', 'lemons', 'limes', 'lettuces',
                       'mandarins', 'melons', 'mussels', 'nectarines', 'olives',
                       'onions', 'oranges', 'oysters', 'papayas', 'peanuts', 'pears',
                       'pistachios', 'plums', 'pumpkins', 'rhubarbs', 'salmons',
                       'shrimps', 'syrups', 'trouts', 'hazelnuts', 'walnuts',
                       'scallops', 'sardines', 'blackcurrants', 'redcurrants',
                       'leeks', 'shallots', 'shrimps', 'prawns', 'almonds', 'olives',
                       'grapes', 'sausages', 'lobsters'
    ) ~ str_replace(Ingredients, 's\\b', ''),
    str_detect(Ingredients, 'nuts') ~ str_replace(Ingredients, 'nuts', 'nut'),
    str_detect(Ingredients, 'seeds') ~ str_replace(Ingredients, 'seeds', 'seed'),
    str_detect(Ingredients, 'peppers') ~ str_replace(Ingredients, 'peppers', 'pepper'),
    str_detect(Ingredients, 'lentils') ~ str_replace(Ingredients, 'lentils', 'lentil'),
    str_detect(Ingredients, 'lettuces') ~ str_replace(Ingredients, 'lettuces', 'lettuce'),
    str_detect(Ingredients, 'brussels sprouts') ~ 'brussel sprout',
    str_detect(Ingredients, 'sardine') ~ 'sardine',

    TRUE ~ Ingredients)) %>%

  #First two words contain the most important information to identify the ingredients
  mutate(Ingredients = str_replace(Ingredients, '_', ' ')) %>% #Composite ingredients has _ between words
  mutate(first_word = str_extract(Ingredients, '^\\w+'),
         temp = str_extract(Ingredients, '^\\w+\\s\\w+')) %>%
  mutate(second_word = str_remove(temp, '^\\w+\\s')) %>%
  select(-temp) %>% unique() %>%
  replace_na(list(second_word = '\\')) %>%

  #Clean up some of them that's not right, always have the most generic name in the first column, then a specification in second column if present
  mutate(
    first_word = case_when(
      Ingredients == 'wheat bread and rolls' ~ 'wheat bread and rolls',
      Ingredients == 'wheat bread and rolls, brown or wholemeal' ~ 'wheat bread and rolls',
      Ingredients == 'boiled egg' ~ 'egg',
      Ingredients == 'chickpeas dry' ~ 'chick pea',
      Ingredients == 'chickpea flour' ~ 'chick pea',
      Ingredients %in% c('mung bean sprouts', 'soy bean', 'beans dry and similar-',
                         'borlotti or other common beans dry', 'kidney bean dry seeds',
                         'navy beans dry seeds', 'beans canned', 'french beans with pods') ~ 'bean',
      Ingredients == 'bean with pods' ~ 'bean with pods',
      Ingredients == 'processed cheese and spreads' ~ 'processed cheese and spreads',
      Ingredients == 'cheese, feta' ~ 'feta',
      Ingredients == 'cheese, chevre frais' ~ 'chevre frais',
      Ingredients == 'soft-ripened cheese' ~ 'soft-ripened cheese',
      Ingredients == 'hard cheese' ~ 'hard cheese',
      Ingredients == 'hard to semi-hard cheese' ~ 'hard to semi-hard cheese',
      Ingredients == 'garden peas dry' ~ 'pea',
      Ingredients == 'sugar snap pea' ~ 'pea',
      Ingredients == 'mangoes' ~ 'mango',
      Ingredients == 'peaches' ~ 'peach',
      Ingredients == 'potatoes' ~ 'potato',
      Ingredients == 'romaines' ~ 'lettuce',
      Ingredients == 'spinaches' ~ 'spinach',
      Ingredients == 'sun-dried tomatoes' ~ 'tomato',
      Ingredients == 'tomatoes' ~ 'tomato',
      Ingredients == 'wheat flour' ~ 'wheat flour',
      Ingredients == 'wheat wholemeal flour' ~ 'wheat flour',
      Ingredients == 'beef minced meat' ~ 'beef',
      Ingredients == 'italian-style sausage' ~ 'sausage',
      Ingredients == 'broad beans without pods' ~ 'broad bean',
      Ingredients == 'broad beans dry' ~ 'bean',
      Ingredients == 'vegetable fats and oil' ~ 'oil',
      Ingredients == 'breadcrumb' ~ 'bread',
      Ingredients == 'fortified and liqueur wines' ~ 'fortified wine',
      Ingredients == 'condensed cream of celery soup' ~ 'condensed cream of celery soup',
      Ingredients == 'condensed cream of mushroom soup' ~ 'condensed cream of mushroom soup',
      Ingredients == 'condensed cream of chicken soup' ~ 'condensed cream of chicken soup',
      Ingredients == 'refrigerated buttermilk biscuit dough' ~ 'refrigerated buttermilk biscuit dough',
      Ingredients == 'fish cakes coarse' ~ 'fish cakes coarse',
      Ingredients == 'vegetables and vegetable products' ~ 'vegetables and vegetable products',
      Ingredients == 'shrimp salad' ~ 'shrimp salad',
      Ingredients == 'watercresses' ~ 'cress',
      Ingredients == 'yellow cake mix' ~ 'yellow cake',
      Ingredients == 'chocolate pudding mix' ~ 'chocolate pudding',
      Ingredients == 'german chocolate cake mix' ~ 'german chocolate cake',
      Ingredients == 'milk powder nonfat' ~ 'milk powder',
      Ingredients == 'milk powder' ~ 'milk powder',
      Ingredients == 'marrow bone beef' ~ 'marrow bone',
      Ingredients == 'sweet green pickle relish' ~ 'sweet green pickle relish',
      Ingredients == 'ice cream' ~ 'ice cream',
      Ingredients == 'wheat flour rye' ~ 'wheat flour',
      Ingredients == 'wheat flour rye wholemeal' ~ 'wheat flour',
      Ingredients == 'wheat flour semolina' ~ 'wheat flour',
      Ingredients == 'white bread mix' ~ 'white bread',
      Ingredients == 'coffee ground, roasted' ~ 'coffee ground',
      TRUE ~ first_word),

    second_word = case_when(
      Ingredients == 'wheat bread and rolls' ~ 'white',
      Ingredients == 'wheat bread and rolls, brown or wholemeal' ~ 'brown',
      Ingredients == 'boiled egg' ~ 'boiled',
      Ingredients == 'chickpeas dry' ~ '\\',
      Ingredients == 'chickpea flour' ~ 'flour',
      Ingredients == 'mung bean sprouts' ~ 'sprout',
      Ingredients == 'soy bean' ~ 'soy',
      Ingredients == 'beans dry and similar-' ~ '\\',
      Ingredients == 'borlotti or other common beans dry' ~ 'borlotti',
      Ingredients == 'kidney bean dry seeds' ~ 'kidney',
      Ingredients == 'navy beans dry seeds' ~ 'navy',
      Ingredients == 'beans canned' ~ 'canned',
      Ingredients == 'french beans with pods' ~ 'french',
      Ingredients == 'beans with pods' ~ '\\',
      Ingredients == 'processed cheese and spreads' ~ '\\',
      Ingredients == 'cheese, feta' ~ 'cheese',
      Ingredients == 'cheese, chevre frais' ~ '\\',
      Ingredients == 'soft-ripened cheese' ~ '\\',
      Ingredients == 'hard cheese' ~ '\\',
      Ingredients == 'hard to semi-hard cheese' ~ '\\',
      Ingredients == 'garden peas dry' ~ 'garden',
      Ingredients == 'sugar snap pea' ~ 'sugar snap',
      Ingredients == 'romaines' ~ 'romaine',
      Ingredients == 'sun-dried tomatoes' ~ 'sun-dried',
      Ingredients == 'cherry tomatoes' ~ 'tomato',
      Ingredients == 'wheat flour' ~ '\\',
      Ingredients == 'wheat wholemeal flour' ~ 'wholemeal',
      Ingredients == 'beef minced meat' ~ '\\', #Use as standard
      Ingredients == 'hard cheese' ~ '\\',
      Ingredients == 'italian-style sausage' ~ 'italian',
      Ingredients == 'rice grain, brown' ~ 'brown',
      Ingredients == 'sweet peppers' ~ 'pepper',
      Ingredients == 'vegetable juices' ~ 'juice',
      Ingredients == 'broad beans without pods' ~ 'without pods',
      Ingredients == 'broad beans dry' ~ 'broad',
      Ingredients == 'sesame seeds' ~ 'seed',
      Ingredients == 'vegetable fats and oil' ~ 'vegetable',
      Ingredients == 'breadcrumb' ~ 'crumb',
      Ingredients == 'jerusalem artichokes' ~ 'artichoke',
      Ingredients == 'fortified and liqueur wines' ~ '\\',
      Ingredients == 'condensed cream of celery soup' ~ '\\',
      Ingredients == 'condensed cream of mushroom soup' ~ '\\',
      Ingredients == 'condensed cream of chicken soup' ~ '\\',
      Ingredients == 'refrigerated buttermilk biscuit dough' ~ '\\',
      Ingredients == 'fish cakes coarse' ~ '\\',
      Ingredients == 'vegetables and vegetable products' ~ '\\',
      Ingredients == 'sunflower seed oil' ~ 'oil',
      Ingredients == 'vegetables, pickled' ~ 'pickled',
      Ingredients == 'cranberry sauce' ~ '\\',
      Ingredients == 'vinegar, wine' ~ 'wine',
      Ingredients == 'chili sauce sweet' ~ 'sweet',
      Ingredients == 'celery stalk' ~ '\\',
      Ingredients == 'hard cheese' ~ '\\',
      Ingredients == 'kiwi fruits green red yellow' ~ '\\',
      Ingredients == 'parnip roots' ~ '\\',
      Ingredients == 'watercresses' ~ 'water',
      Ingredients == 'rice grain' ~ '\\', #Standard,
      Ingredients == 'yellow cake mix' ~ 'mix',
      Ingredients == 'chocolate pudding mix' ~ 'mix',
      Ingredients == 'german chocolate cake mix' ~ 'mix',
      Ingredients == 'milk powder' ~ '\\',
      Ingredients == 'milk powder nonfat' ~ 'nonfat',
      Ingredients == 'marrow bone beef' ~ 'beef',
      Ingredients == 'sweet green pickle relish' ~ '\\',
      Ingredients == 'ice cream' ~ '\\',
      Ingredients == 'wheat flour rye' ~ 'rye',
      Ingredients == 'wheat flour rye wholemeal' ~ 'rye',
      Ingredients == 'wheat flour semolina' ~ 'semolina',
      Ingredients == 'white bread mix' ~ 'mix',
      Ingredients == 'coffee ground, roasted' ~ '\\',
      TRUE ~ second_word
    )
  ) %>%

  filter(!first_word %in% c('juice', 'fresh')) %>% #These may otherwise cause issues when searching
  distinct(., first_word, second_word, .keep_all = TRUE) %>% #Remove duplicates. Due to splitting up rows with '/', certain ingredients occur multiple times
  #Remove unnecessary column
  select(-Ingredients) %>% arrange(first_word, second_word)
#Is the maize flour in SHARP the same as corn starch?

#Save
saveRDS(SHARP2018_query, "./data-raw/SHARP2018_query.Rds")



