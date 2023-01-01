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
  #Remove ingredients in these categories that should be kept
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
                                     'wheat semolina',
                                     #Alcoholic beverages
                                     'beer', 'wine', 'whisky', 'fortified and liqueur wines', 'cider', 'brandy', 'vodka and vodka-like spirits',
                                     'rum',
                                     #Conditments, sauces and spices
                                     'soy sauce', 'salt', 'mixed herbs and spices', 'curry powder',
                                     'stock cubes or granulate bouillon base', 'mustard and related sauces', 'vinegar', 'vinegar, wine',
                                     'tomato ketchup and related sauces', 'barbecue or steak sauces', 'mayonnaise sauce',
                                     #Grains
                                     'short pastry doughs pate brisee',

                                     #Chocolate etc
                                     'cocoa powder', 'bitter chocolate', 'milk chocolate', 'white chocolate',

                                     #Coffee and tea
                                     'coffee ground, roasted',

                                     #Water
                                     'tap water'))) #All sugars have the same CO2 and land-use footprint, only need one, same with types of cheeses

SHARP <- SHARP %>%
  #Filter ingredients not needed
  filter(!FoodEx2 %in% c(various$sharp_to_remove$FoodEx2,

                         #Potato products
                         "A011L", "A011S", "A03VG", "A0BYS", "A0BYV", "A00ZX", "A011D",
                         "A011E", "A011P", "A011R", "A0BYT", "A005A", "A00ZV",
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
  mutate(Ingredients = Ingredients %>%

           #Dairy
           str_replace('extra hard cheese parmesan, grana type', 'parmesan/hard cheese') %>%
           str_replace('hard cheese cheddar, emmental type|firm/semi-hard cheese gouda and edam type', 'hard to semi-hard cheese') %>%
           str_replace('soft - ripened cheese', 'soft-ripened cheese') %>%
           str_replace('yoghurt, cow milk', 'yoghurt') %>%
           str_replace('cream, plain', 'cream') %>%
           str_replace('sour cream, plain', 'sour cream') %>%
           str_replace('cr�me fraiche and other mild variants of sour cream', 'crème fraîche') %>%
           str_replace('cow milk', 'milk') %>%
           str_replace('milk powder, skimmed', 'milk powder nonfat') %>%
           str_replace('ice cream, milk-based', 'ice cream') %>%
           str_replace('cheese, pecorino toscano', 'cheese pecorino') %>%

           #Plants
           str_replace('aubergines', 'eggplant') %>%
           str_replace('textured soy protein', 'tofu') %>%
           str_replace('barley grain, pearled', 'pearl barley') %>%
           str_replace('potato flour', 'potato starch') %>%
           str_replace('podded pea young pods', 'sugar snap pea') %>%
           str_replace('spring onions', 'scallion') %>%
           str_replace('celeriacs', 'Celariac root') %>%
           str_replace('celeries', 'celery stalk') %>%
           str_replace('common mushrooms', 'mushroom') %>%
           str_replace('coconut oil/fat', 'Coconut oil') %>%
           str_replace('white sugar', 'sugar') %>%
           str_replace('olive oil, virgin or extra-virgin', 'olive oil') %>%
           str_replace('olives, processed', 'olives canned') %>%
           str_replace('table olives ready for consumption', 'olives fresh') %>%
           str_replace('kohlrabies', 'swede') %>%
           str_replace('dried pasta', 'pasta') %>%
           str_replace('beans with pods and similar-', 'bean with pods') %>%
           str_replace('canned or jarred common beans', 'beans canned') %>%
           str_replace('unleavened or flat bread and similar', 'flatbread') %>%
           str_replace('shallots and similar-', 'shallots') %>%
           str_replace('soya bean oil, refined', 'soy oil') %>%
           str_replace('oils', 'oil') %>%
           str_replace('tomato ketchup and related sauces', 'tomato ketchup') %>%
           str_replace('mustard and related sauces', 'mustard') %>%
           str_replace('coconut milk cocos nucifera liquid', 'coconut milk') %>%
           str_replace('palm oil/fat', 'palm oil') %>%
           str_replace('soyabeans for consumption dry', 'soy bean') %>%
           str_replace('canned/jarred vegetables', 'vegetables canned') %>%
           str_replace('pickled/marinated vegetables', 'vegetables, pickled') %>%
           str_replace('florence fennels', 'fennel') %>%
           str_replace('sweet potatoes', 'potato sweet') %>%
           str_replace('red cabbages', 'cabbage red') %>%
           str_replace('head cabbages', 'cabbage') %>% #Default
           str_replace('white cabbage', 'cabbage white') %>%
           str_replace('savoy cabbages', 'cabbage savoy') %>%
           str_replace('chinese cabbages', 'cabbage chinese') %>%
           str_replace('dried vine fruits raisins etc.', 'raisin') %>%
           str_replace('dried prunes', 'prune dried') %>%
           str_replace('dried figs', 'fig dried') %>%
           str_replace('dried fruit', 'fruit dried') %>%
           str_replace('dried dates', 'date dried') %>%
           str_replace('dried bananas', 'banana dried') %>%
           str_replace('dried apricots', 'apricot dried') %>%
           str_replace('dried mushrooms', 'mushroom dried') %>%
           str_replace('dried nuts and related flours and powderes', 'nutflour') %>%
           str_replace('dried vegetables', 'vegetables dried') %>%
           str_replace('jam, sweet cherry', 'sweet cherry jam') %>%
           str_replace('jam, plums', 'plum jam') %>%
           str_replace('jam, strawberries', 'strawberry jam') %>%
           str_replace('jam, apricots', 'apricot jam') %>%
           str_replace('jam, oranges', 'orange jam') %>%
           str_replace('jam of fruit / vegetables', 'jam') %>%
           str_replace('juice, apple', 'apple juice') %>%
           str_replace('juice, lemon', 'lemon juice') %>%
           str_replace('juice, lime', 'lime juice') %>%
           str_replace('juice, orange', 'orange juice') %>%
           str_replace('juice, mango', 'mango juice') %>%
           str_replace('juice, grapefruit', 'grapefruit juice') %>%
           str_replace('juice, grape', 'grape juice') %>%
           str_replace('juice, pineapple', 'pineapple juice') %>%
           str_replace('juice, carrot', 'carrot juice') %>%
           str_replace('juice, elderberry', 'elderberry juice') %>%
           str_replace('parsley roots', 'parsley root') %>%
           str_replace('vegetable fats and oils, edible', 'vegetable oil') %>%
           str_replace('swiss chards', 'chard') %>%
           str_replace('roman rocket and similar-', 'salad rocket') %>% #What other salads are like rockets?
           str_replace('summer squashes', 'squash summer') %>%
           str_replace('kales and similar-', 'kale') %>%
           str_replace('globe artichokes', 'artichoke') %>%
           str_replace('dried prunes', 'prune dried') %>%
           str_replace('granate apples', 'pomegranat') %>%
           str_replace('radishes', 'radish') %>%
           str_replace('litchis', 'lychee') %>%
           str_replace('table grapes', 'grapes') %>%
           str_replace("lamb's lettuces", "lettuce lamb") %>%
           str_replace('barley grain, pearled', 'barley pearl') %>%
           str_replace('courgettes', 'zucchini') %>% #Name used in recipes
           str_replace('sprouts, shoots and similar', 'sprout') %>%
           str_replace('maize flour', 'corn flour') %>%
           str_replace('processed oat-based flakes', 'oatmeal') %>%
           str_replace('oat rolled grains', 'oat rolled') %>%
           str_replace('black cherries', 'cherries black') %>%
           str_replace('cherries and similar-', 'cherries') %>%
           str_replace('sour cherries', 'cherries sour') %>%
           str_replace('jam of fruit / vegetables', 'jam') %>%
           str_replace('fruit juices 100% from named source', 'fruit juice') %>%
           str_replace('canned or jarred pineapple', 'pineapple canned') %>%
           str_replace('canned or jarred peach', 'peach canned') %>%
           str_replace('canned or jarred sweet cherry', 'cherries canned') %>%
           str_replace('wheat semolina', 'wheat flour semolina') %>%
           str_replace('rye flour, light', 'wheat flour rye') %>%
           str_replace('rye flour, wholemeal', 'wheat flour rye wholemeal') %>%
           str_replace('passionfruits', 'passion fruit') %>%
           str_replace('almonds sweet', 'almond') %>%

           #Meat
           str_replace('beef tallow including processed suet', 'tallow') %>%
           str_replace('bovine, minced meat', 'beef minced meat') %>% #Used in the norwegian recipes
           str_replace('pork lard', 'lard') %>%
           str_replace('bovine tongue', 'beef tongue') %>%
           str_replace('bovine marrowbone', 'marrow bone beef') %>%
           str_replace('pig muscle', 'pork') %>% #Use as standard
           str_replace('pig liver', 'pork liver') %>%
           str_replace('pig kidney', 'pork kidney') %>%
           str_replace('chicken fresh meat', 'chicken') %>%
           str_replace('goose fresh meat', 'goose') %>%
           str_replace('sheep muscle', 'sheep') %>%
           str_replace('salami-type sausage', 'salami') %>%
           str_replace('ham, pork', 'ham') %>%

           #Poultry
           str_replace('hen ', '') %>%
           str_replace('eggs', 'egg') %>%
           str_replace('turkey fresh meat', 'turkey') %>%
           str_replace('cooked other poultry meat', 'chicken cooked/turkey cooked') %>%
           str_replace('fried egg', 'omelet') %>% #Only type of fried eggs in the recipe

           #Seafood
           str_replace('anglerfish and monkfish', 'anglerfish/monkfish') %>% #Easier to separate
           str_replace('cods, hakes, haddocks', 'cod/hake/haddock/pollock') %>% #Pollock is in the same fish family
           str_replace('canned tunas and similar', 'tuna canned') %>%
           str_replace('canned anchovies', 'anchovies canned') %>%
           str_replace('canned mackerel', 'mackerel canned') %>%
           str_replace('shrimps and prawns', 'shrimp/prawn') %>%
           str_replace('canned/jarred fish', 'fish canned') %>%
           str_replace('scallops, pectens', 'scallops') %>%
           str_replace('edible crab', 'crab') %>%
           str_replace('anchovies', 'anchovy') %>%
           str_replace('smoked salmon', 'salmon smoked') %>%
           str_replace('smoked herring', 'herring smoked') %>%

           #Div
           str_replace('barbecue or steak sauces', 'barbeque sauce') %>%
           str_replace(', common|, edible', '') %>%
           str_replace('p�rigord black truffles', 'truffle black') %>%
           str_replace('common mushrooms', 'mushroom common') %>%
           str_replace('canned mushrooms', 'mushroom canned') %>%
           str_replace('dried mushrooms', 'mushroom dried') %>%
           str_replace('hazelnuts', 'nuts hazel') %>%
           str_replace('maize oil', 'corn oil') %>%
           str_replace('short pastry doughs pate brisee', 'pastry dough') %>%
           str_replace('tap water', 'water') %>%
           str_replace('pasta sauce', 'tomato sauce') %>% #pasta sauces are tomato sauces
           str_replace('vodka and vodka-like spirits', 'vodka') %>%
           str_replace('bitter chocolate', 'chocolate dark') %>%
           str_replace('milk chocolate', 'chocolate milk') %>%
           str_replace('white chocolate', 'chocolate white') %>%
           str_replace('traditional margarine', 'margarine') %>%
           str_replace('dairy imitates other than milks', 'dairy imitate')

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
  system.file("extdata", "composite_ingredients_sustainability_markers.Rds", package = "nutRients")
  )

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

