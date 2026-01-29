#Build the search database for SHARP ID

#Empty list to fill with various dataframes throughout
various <- list()
#Read sustainability data and change to fit recipes----
SHARP <- read_csv(
  system.file("extdata", "SHARPversion2018.csv", package = "sustainableNutRients")
  ) %>%

  #Rename to fit with the other datasets
  rename(Ingredients = `Food item`) %>%
  #Remove graphical characters
  mutate(Ingredients = stri_trans_tolower(Ingredients)) %>%
  mutate(Ingredients = str_replace_all(Ingredients, '\\(|\\)', ''))

#Ingredients to remove, either because they're duplicates or just not needed
various$sharp_to_remove <- SHARP %>%

  #Whole groups of ingredients
  filter(L1 %in% c('Coffee, cocoa, tea and infusions', 'Composite dishes',
                   'Food products for young population', 'Eggs and egg products',
                   'Sugar and similar, confectionery and water-based sweet desserts',
                   'Grains and grain-based products', 'Milk and dairy products',
                   'Alcoholic beverages', 'Seasoning, sauces and condiments',
                   'Water and water-based beverages') |
           str_detect(Ingredients, 'rice|other wine-like fruit drinks|soft')) %>%
  # The food items listed below are from these categories but will be kept in the db
  filter(!Ingredients %in% tolower(c('White sugar', 'Honey', 'Syrups',

                                     #Cheeses
                                     'Cream cheese', 'Cottage cheese', 'extra hard cheese parmesan, grana type',
                                     'Firm/semi-hard cheese gouda and edam type', 'Hard cheese cheddar, emmental type',
                                     'Cheese, feta', 'Soft - ripened cheese', 'Cheese, chevre frais',  'processed cheese and spreads',
                                     'ricotta', 'mozzarella', 'cheese, pecorino toscano',
                                     #Milk products
                                     'buttermilk', 'coconut milk cocos nucifera liquid', 'cow milk', 'yoghurt, cow milk',
                                     'evaporated milk liquid, unsweetened', 'cream, plain', 'cr�me fraiche and other mild variants of sour cream',
                                     'sour cream, plain', 'milk powder, skimmed', 'milk powder', 'ice cream, milk-based',

                                     #Egg products
                                     'hen eggs', 'boiled eggs', 'hen egg white', 'hen egg yolk', 'fried eggs',
                                     #Grain/grain replacements products
                                     'rice flour', 'rice drink', 'rice grain', 'rice grain, brown', 'noodle, rice', 'couscous',
                                     'puff pastry', 'biscuits', 'maize flour', 'crisp bread, rye wholemeal', 'wheat bread and rolls',
                                     'breadcrumbs', 'buns', 'wheat bread and rolls, brown or wholemeal', 'wheat flour', 'maize flour',
                                     'wheat wholemeal flour', 'dried pasta', 'pasta wholemeal', 'oat grain', 'bulgur', 'barley grain, pearled',
                                     'oat rolled grains', 'processed oat-based flakes', 'rye flour, light', 'rye flour, wholemeal',
                                     'wheat semolina', 'wheat bran', 'common millet grain', "buckwheat flour", "buckwheat",
                                     #Alcoholic beverages
                                     'beer', 'wine', 'whisky', 'fortified and liqueur wines', 'cider', 'brandy', 'vodka and vodka-like spirits',
                                     'rum', 'liqueurs', 'coffee liqueur',
                                     #Conditments, sauces and spices
                                     'soy sauce', 'salt', 'mixed herbs and spices', 'curry powder',
                                     'stock cubes or granulate bouillon base', 'mustard and related sauces', 'vinegar', 'vinegar, wine',
                                     'tomato ketchup and related sauces', 'barbecue or steak sauces', 'mayonnaise sauce',
                                     #Grains
                                     'short pastry doughs pate brisee',

                                     #Chocolate etc
                                     'cocoa powder', 'bitter chocolate', 'milk chocolate', 'white chocolate',

                                     #Coffee and tea
                                     'coffee ground, roasted', 'coffee beverages',

                                     # Div
                                     'potato crisps from potato slices', 'liquorice candies', 'popcorn maize, popped',
                                     'cola-type drinks', 'french fries from cut potato',

                                     #Water
                                     'tap water'))) #All sugars have the same CO2 and land-use footprint, only need one, same with types of cheeses

SHARP <- SHARP %>%
  #Filter ingredients not needed
  filter(!FoodEx2 %in% c(various$sharp_to_remove$FoodEx2,

                         #Potato products
                         "A011S", "A03VG", "A00ZX", "A011D", "A011E",
                         "A011P", "A011R", "A0BYT", "A005A", "A00ZV",
                         #Pasta products
                         "A007G", "A007M", "A007J", "A007V", "A007P",
                         #Soyabean Sprouts
                         "A00SY",
                         #Dairy
                         "A02YE",
                         #Pork products
                         "A026R",
                         #Pigeon
                         "A01TA",
                         #Corned beef
                         "A0B9G",
                         #Div
                         "A026M",
                         #Sausages/salami
                         "A025S", "A024M", "A026C", "A024G", "A024J", "A024H",
                         "A025G", "A025L", "A024R", "A025Q", "A025B", "A025P",
                         "A026A", "A0EYP", "A025N", "A025E", "A024S", "A026D",
                         "A024Y", "A024Z", "A025V",
                         #Ham
                         "A023H", "A023K", "A022Z", "A023A", "A022S",
                         #Beer
                         "A03MG", "A03MF", "A03ME", "A03MD",
                         #Juice
                         "A03BN"


  )) %>%
  filter(!Ingredients %in% c('bovine and pig, minced meat', 'onion bulbs for fresh consumption',
                             'blue mussel', 'butter and margarine/oil blends', 'margarines and similar',
                             'shrimps, common', 'brown trout', 'atlantic salmon', 'herring, atlantic', 'mackerel, atlantic',
                             'carps', 'halibut, greenland', 'compote of fruit / vegetables',
                             'blended frying oil/fats', 'chickpeas without pods', 'non dairy coffee creamer',
                             'mixed vegetable juice', 'mixed fruit and vegetable juices', 'fruit and vegetable juices and nectars',
                             'peas without pods and similar-', 'garden peas without pods', 'small radishes', 'black radishes',
                             'smooth hounds', 'oranges, sweet', 'mixed dried fruits', 'currants black, red and white',
                             'black cherries', 'black mulberries', 'liquid drink bases including concentrates and home-made preparations',
                             'peanuts fresh seeds',
                             #Rakefish might fit within this category?
                             'marinated / pickled fish')) %>%

  #Fix some names and remove duplicate items
  dplyr::mutate(Ingredients = str_replace_all(
    Ingredients, c(
             'extra hard cheese parmesan, grana type' = 'parmesan/hard cheese',
             'hard cheese cheddar, emmental type|firm/semi-hard cheese gouda and edam type' = 'hard to semi-hard cheese',
             'soft - ripened cheese' = 'soft-ripened cheese',
             'yoghurt, cow milk' = 'yoghurt',
             'cream, plain' = 'cream',
             'sour cream, plain' = 'sour cream',
             'cr�me fraiche and other mild variants of sour cream' = 'crème fraîche',
             'cow milk' = 'milk',
             'milk powder, skimmed' = 'milk powder nonfat',
             'ice cream, milk-based' = 'ice cream',
             'cheese, pecorino toscano' = 'cheese pecorino',
             'cream cheese' = 'cheese cream',

             #Plants
             'aubergines' = 'eggplant',
             'textured soy protein' = 'tofu',
             'barley grain, pearled' = 'pearl barley',
             'potato flour' = 'potato starch',
             'podded pea young pods' = 'sugar snap pea',
             'spring onions' = 'scallion',
             'celeriacs' = 'Celariac root',
             'celeries' = 'celery stalk',
             'common mushrooms' = 'mushroom',
             'coconut oil/fat' = 'Coconut oil',
             'white sugar' = 'sugar',
             'olive oil, virgin or extra-virgin' = 'olive oil',
             'olives, processed' = 'olives canned',
             'table olives ready for consumption' = 'olives fresh',
             'kohlrabies' = 'swede',
             'dried pasta' = 'pasta',
             'beans with pods and similar-' = 'bean with pods',
             'canned or jarred common beans' = 'beans canned',
             'unleavened or flat bread and similar' = 'flatbread',
             'shallots and similar-' = 'shallots',
             'soya bean oil, refined' = 'soy oil',
             'oils' = 'oil',
             'tomato ketchup and related sauces' = 'tomato ketchup',
             'mustard and related sauces' = 'mustard',
             'coconut milk cocos nucifera liquid' = 'coconut milk',
             'palm oil/fat' = 'palm oil',
             'soyabeans for consumption dry' = 'soy bean',
             'canned/jarred vegetables' = 'vegetables canned',
             'pickled/marinated vegetables' = 'vegetables, pickled',
             'florence fennels' = 'fennel',
             'sweet potatoes' = 'potato sweet',
             'red cabbages' = 'cabbage red',
             'head cabbages' = 'cabbage', #Default
             'white cabbage' = 'cabbage white',
             'savoy cabbages' = 'cabbage savoy',
             'chinese cabbages' = 'cabbage chinese',
             'dried vine fruits raisins etc.' = 'raisin',
             'dried prunes' = 'prune dried',
             'dried figs' = 'fig dried',
             'dried fruit' = 'fruit dried',
             'dried dates' = 'dates dried',
             'dried bananas' = 'banana dried',
             'dried apricots' = 'apricot dried',
             'dried mushrooms' = 'mushroom dried',
             'dried nuts and related flours and powderes' = 'nutflour',
             'dried vegetables' = 'vegetables dried',
             'jam, sweet cherry' = 'sweet cherry jam',

             'jam, plums' = 'plum jam',
             'jam, strawberries' = 'strawberry jam',
             'jam, apricots' = 'apricot jam',
             'jam, oranges' = 'orange jam',
             'jam of fruit / vegetables' = 'jam',

             'juice, apple' = 'apple juice',
             'juice, lemon' = 'lemon juice',
             'juice, lime' = 'lime juice',
             'juice, orange' = 'orange juice',
             'juice, mango' = 'mango juice',
             'juice, grapefruit' = 'grapefruit juice',
             'juice, grape' = 'grape juice',
             'juice, pineapple' = 'pineapple juice',
             'juice, carrot' = 'carrot juice',
             'juice, tomato' = 'tomato juice',
             'juice, elderberry' = 'elderberry juice',
             'parsley roots' = 'parsley root',
             'vegetable fats and oils, edible' = 'vegetable oil',
             'swiss chards' = 'chard;mangold',
             'roman rocket and similar-' = 'salad rocket', #What other salads are like rockets?
             'summer squashes' = 'summer squash',
             'kales and similar-' = 'kale',
             'globe artichokes' = 'artichoke',
             'dried prunes' = 'prune dried',
             'granate apples' = 'pomegranat',
             'radishes' = 'radish',
             'litchis' = 'lychee',
             'table grapes' = 'grapes',
             "lamb's lettuces" = "lettuce lamb",
             'barley grain, pearled' = 'barley pearl',
             'courgettes' = 'zucchini', #Name used in recipes
             'sprouts, shoots and similar' = 'sprout',
             'maize flour' = 'corn flour',
             'processed oat-based flakes' = 'oatmeal',
             'oat rolled grains' = 'oat rolled',
             'black cherries' = 'cherries black',
             'cherries and similar-' = 'cherries',
             'sour cherries' = 'cherries sour',
             'jam of fruit / vegetables' = 'jam',
             'fruit juices 100% from named source' = 'fruit juice',
             'canned or jarred pineapple' = 'pineapple canned',
             'canned or jarred peach' = 'peach canned',
             'canned or jarred sweet cherry' = 'cherries canned',
             'canned or jarred fruit' = 'fruit canned',
             'wheat semolina' = 'wheat flour semolina',
             'rye flour, light' = 'wheat flour rye',
             'rye flour, wholemeal' = 'wheat flour rye wholemeal',
             'passionfruits' = 'passion fruit',
             'almonds sweet' = 'almond',
             'berries and small fruits' = 'berries',
             'currants black, red and white' = 'currant',
             'common millet grain' = 'millet',
             'popcorn maize, popped' = 'popcorn',

             '\\bkaki\\b' = 'persimmon',
             'french fries from cut potato' = 'french fries',
             "pumpkins" = "winter squash",
             "peaches and similar-" = "peach",
             "watermelons" = "watermelon",

             #Meat
             'beef tallow including processed suet' = 'tallow',
             'bovine, minced meat' = 'beef minced meat', #Used in the norwegian recipes
             'pork lard' = 'lard',
             'bovine tongue' = 'beef tongue',
             'bovine marrowbone' = 'marrow bone beef',
             'pig muscle' = 'pork', #Use as standard
             'pig liver' = 'pork liver',
             'pig kidney' = 'pork kidney',
             'chicken fresh meat' = 'chicken',
             'goose fresh meat' = 'goose',
             'sheep muscle' = 'sheep',
             'salami-type sausage' = 'salami',
             'ham, pork' = 'ham',
             "stock cubes or granulate bouillon base" = "broth cube",

             #Poultry
             'hen ' = '',
             'eggs' = 'egg',
             'turkey fresh meat' = 'turkey',
             'cooked other poultry meat' = 'chicken cooked/turkey cooked',
             'fried egg' = 'omelet', #Only type of fried eggs in the recipe

             #Seafood
             'anglerfish and monkfish' = 'anglerfish/monkfish', #Easier to separate
             'cods, hakes, haddocks' = 'cod/hake/haddock/pollock', #Pollock is in the same fish family
             'canned tunas and similar' = 'tuna canned',
             'canned anchovies' = 'anchovies canned',
             'canned mackerel' = 'mackerel canned',
             'shrimps and prawns' = 'shrimp/prawn',
             'canned/jarred fish' = 'fish canned',
             'scallops, pectens' = 'scallops',
             'edible crab' = 'crab',
             'anchovies' = 'anchovy',
             'smoked salmon' = 'salmon smoked',
             'smoked herring' = 'herring smoked',
             'freshwater crayfishes' = 'crayfish',

             #Div
             'barbecue or steak sauces' = 'barbeque sauce',
             ', common|, edible' = '',
             'p�rigord black truffles' = 'truffle black',
             'common mushrooms' = 'mushroom common',
             'canned mushrooms' = 'mushroom canned',
             'dried mushrooms' = 'mushroom dried',
             'hazelnuts' = 'nuts hazel',
             'maize oil' = 'corn oil',
             'short pastry doughs pate brisee' = 'pastry dough',
             'tap water' = 'water',
             'pasta sauce' = 'tomato sauce', #pasta sauces are tomato sauces
             'vodka and vodka-like spirits' = 'vodka',
             'bitter chocolate' = 'chocolate dark',
             'milk chocolate' = 'chocolate milk',
             'white chocolate' = 'chocolate white',
             'traditional margarine' = 'margarine',
             'jelly candies' = 'candy jelly',
             'potato crisps from potato slices' = 'chips_potato',
             'liquorice candies' = 'liquorice',

             # Beverages
             'coffee liqueur' = 'liqueur coffee',
             'liqueurs' = 'liqueur',
             'dairy imitates other than milks' = 'dairy imitate',
             'coffee beverages' = 'coffee',
             'cola-type drinks' = 'soda',

             # Grains
             "oat grain" = "oat bran",
             "lentil dry" = "lentil dried",
             "chick pea" = "chickpea"
           ))
  ) %>%

  #Change 'and similar' to 'other'
  mutate(Ingredients = Ingredients %>%
           str_replace('and similar-', 'other') %>%
           str_replace('and similar', 'other')) %>%

  #Filter away other unnecessary ingredients
  filter(!(str_detect(Ingredients, 'canned ') |
             str_detect(Ingredients, 'biscuit'))) %>%
  filter(!(str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'dried')))# %>%

#select(-FoodEx2) %>% unique()

#Create a 'shellfish' ingredient in SHARP, using the mean of all the shellfish ingredients and add to SHARP
various$shellfish <- SHARP %>%
  filter(Ingredients %in% c('clams', 'lobster', 'oyster', 'oyster, pacific cupped', 'mussels', 'scallops', 'shrimps/prawns')) %>%
  summarise(`GHGE of 1 kg food as consumed_kgCO2eq` = mean(`GHGE of 1 kg food as consumed_kgCO2eq`),
            `Land use of 1 kg food as consumed_m2/yr` = mean(`Land use of 1 kg food as consumed_m2/yr`)) %>%
  mutate(Ingredients = 'shellfish',
         L1 = 'Fish, seafood, amphibians, reptiles and invertebrates')
SHARP <- full_join(SHARP, various$shellfish)
#Add composite ingredients
various$composite_ingredients_sharp <- readRDS(
  system.file("extdata", "composite_ingredients_sustainability_markers.Rds", package = "sustainableNutRients")
  )
#Use "lettuce and similar" as a default for salad leaves not in SHARP
various$salad <- SHARP %>%
  filter(Ingredients == "lettuces other") %>%
  mutate(Ingredients = str_replace(Ingredients, "lettuces other", "salad"),
         FoodEx2 = NA)
#Add to SHARP
SHARP <- bind_rows(SHARP,various$salad)

# Nuts
various$nuts <- SHARP %>%
  filter(Ingredients %in% c(
    "almond", "cashew nuts", "nuts hazel", "pistachios", "walnuts",
    "peanuts", "brazil nuts"
  )) %>%
  mutate(Ingredients = "nut",
         FoodEx2 = NA) %>%
  summarise(.by = c(Ingredients, L1),
            `GHGE of 1 kg food as consumed_kgCO2eq` = mean(`GHGE of 1 kg food as consumed_kgCO2eq`),
            `Land use of 1 kg food as consumed_m2/yr` = mean(`Land use of 1 kg food as consumed_m2/yr`))
#Add to SHARP
SHARP <- bind_rows(SHARP,various$nuts)

#Add to SHARP
SHARP <- full_join(SHARP, various$composite_ingredients_sharp)
#Create a half-and-half row, half milk and half cream
SHARP <- SHARP %>%
  add_row(Ingredients = 'half-and-half',
          L1 = 'Milk and dairy products',
          `GHGE of 1 kg food as consumed_kgCO2eq` = (10.6822437+1.5858702)/2,
          `Land use of 1 kg food as consumed_m2/yr` = (10.4333868+1.4606742)/2) %>%

  #Split ingredients that share a row, and remove duplicates created
  separate_rows(Ingredients, sep = '/') %>%
  unique() %>%

  #Create unique ID for each item
  mutate(database_ID = seq_along(Ingredients)) %>%
  #Add a FoodEx2 code for the composite ingredients not in SHARP
  replace_na(list(FoodEx2 = 'Composite ingredient not in SHARP'))

# # Add bananas from "Fremtiden i våre henders" environmental impact database,
# # balsamic vinegar from https://doi.org/10.1016/j.jclepro.2016.04.090 (mean values ued)
# SHARP <- SHARP %>%
#   add_row(Ingredients = "banana", L1 = "Fruit and fruit products",  `GHGE of 1 kg food as consumed_kgCO2eq` = 1.3, `Land use of 1 kg food as consumed_m2/yr` = 0.6, database_ID = 300.999) %>%
#   add_row(Ingredients = "vinegar balsamic", `GHGE of 1 kg food as consumed_kgCO2eq` = 2.24, `Land use of 1 kg food as consumed_m2/yr` = 11.53, database_ID = 301.999)
#


#Finalize for saving
SHARP2018 <- SHARP %>%
  select(-c(Ingredients, L1, FoodEx2)) %>%
  pivot_longer(.,
               cols = -database_ID,
               names_to = "environmental_impact_indicator",
               values_to = "environmental_impact_per_kilo") %>%
  #Turn environmental impact from per kilo to per hekto
  mutate(
    environmental_impact_per_hektogram = environmental_impact_per_kilo/10,
    environmental_impact_indicator = environmental_impact_indicator %>%
      str_replace("GHGE of 1 kg food as consumed_kg", "kg ") %>%
      str_replace("Land use of 1 kg food as consumed_", ""))


# tmp$query_words <- tmp$query_words %>%
#   add_row(first_word = "banana", second_word = "\\", database_ID = 300.999) %>%
#   add_row(first_word = "vinegar", second_word = "balsamic", database_ID = 301.999)

#Save
saveRDS(SHARP2018, "./data-raw/SHARP2018.Rds")

#Save foodgroup metainfo
SHARPFoodgroups <- SHARP %>%
  select(database_ID, L1) %>%
  rename(foodgroup = L1)

saveRDS(SHARPFoodgroups, "./data-raw/SHARP2018_foodgroups.Rds")

#Save a dataframe to create query words from
SHARP2018_query_prep <- SHARP %>%
  select(database_ID, Ingredients)

saveRDS(SHARP2018_query_prep, "./data-raw/SHARP2018_query_prep.Rds")

