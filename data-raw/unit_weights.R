 #Script to create the database for unit and weights per food item

#Weight and portion size database
#Empty list to fill with various variables
various <- list()

#Load raw data from the Norwegian directorate of health
raw_data <- readxl::read_xlsx(
  system.file("extdata", "weights_portion_sizes_foods_NorwegianDirectorateOfHealth_2015.xlsx", package = "sustainableNutRients"))

#Reformat
unit_weights <- raw_data %>% rename(
  portion = contains('porsjon'),
  tbsp = contains('ss,'),
  tsp = contains('ts,'),
  ms = contains('ms,'),
  dl = contains('dl,'),
  unit_enhet = EnhetUnit,
  g = contains('g/'),
  brutto = `Brutto g`,
  netto = `Netto g`
) %>%

  #Turn into long format, end goal to get one column with ingredient names/food items,
  #one column with units and one column for weight in grams per unit and an unique identifier for each food item
  select(-`Spiselig del % Edible part %`) %>% #Don't need this column
  pivot_wider(., #Turn into long format first to get all the different unit types in the name row
              names_from = unit_enhet,
              values_from = g) %>%
  pivot_longer(.,
               cols = -contains('Matvare'),
               names_to = 'unit_enhet',
               values_to = 'g') %>%
  drop_na() %>%

#Rename for easier use later
rename(Ingredients = `Matvare\r\nFood item`,
       Foodgroup = `Matvaregruppe\r\nFoodgroup`) %>%

  #Remove some not needed foodgroups, but keep some food items in those foodgroups
  filter(
    #Get rid of
    !(str_detect(Foodgroup, regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter')) &
        #Keep
        !str_detect(Ingredients, 'Pancake'))
  ) %>%

  #Rename some ingredients
  mutate(Ingredients = Ingredients %>%
           str_replace('Loff, formstekt', 'Loff') %>%
           str_replace('Bread, white, square-shaped', 'Bread, white') %>%
           str_replace('Grapes, with seeds', 'grapes')) %>%

  #Create unique IDs for each ingredients
  group_by(Ingredients) %>%
  mutate(database_ID = cur_group_id()) %>%
  ungroup() %>%

  #Split into Norwegian and English, first make a language column with language identifier
  mutate(language = 'norwegian\nenglish') %>%
  separate_rows(., c(Ingredients, Foodgroup, unit_enhet, language), sep = '\n') %>%
  #Remove whitespace
  mutate_at(c('Ingredients', 'Foodgroup', 'unit_enhet'), ~str_trim(.)) %>%

  #Split rows of foods with multiple weights
  #First extract the references to a separate column
  separate_rows(., g, sep = '/') %>%
  #The original datatable have weights in grams together with a letter for reference, split into two columns
  mutate(g = g %>%
           str_replace('([:alpha:])', ' \\1') %>%
           str_replace(',', '.')) %>%
  separate(g, into = c('g', 'reference'), sep = ' ') %>%
  mutate_at('g', ~ as.numeric(.)) %>% #Turn g into numeric for future calculations

  #Use the mean weight for the foods with multiple weights and remove duplicates
  group_by(Ingredients, unit_enhet, Foodgroup, language) %>%
  mutate(g = mean(g),
         reference = paste(reference, collapse = ',')) %>%
  ungroup() %>% unique()

#Split the 'Rømme, crème fraîche, kesam' and butter/margarine rows into individual rows, create new IDs for rømme/kesam etc
#Add ghee as a similar type ingredient
temp <- unit_weights %>%
  filter(str_detect(Ingredients, 'margarin|crème fraîche')) %>%
  #Add ghee, similar to butter and oil
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'margarin') & !str_detect(Ingredients, 'iquid|lytende') ~ paste0(Ingredients, ', ghee'),
    TRUE ~ Ingredients
  )) %>%
  separate_rows(., Ingredients, sep = ',') %>% mutate(Ingredients = str_trim(Ingredients)) %>%
  #Remove liquid/flytende margarine
  filter(!str_detect(Ingredients, 'iquid|lytende')) %>%
  #New ID's
  mutate(database_ID = case_when(
    str_detect(Ingredients, 'ømme|our') ~ as.integer(10000),
    str_detect(Ingredients, 'crème') ~ as.integer(10001),
    Ingredients %in% c('kesam', 'quark') ~ as.integer(10002),
    TRUE ~ database_ID
  ))

unit_weights <- unit_weights %>%
  #Remove the old grouped rømme/kesam rows from unit_weights and add the new individual rows
  filter(!str_detect(unit_weights$Ingredients,'crème fraîche|margarin')) %>%
  full_join(., temp) %>%

  #Fill in reference letters that went missing when separating multiple weight ingredients
  group_by(Ingredients, unit_enhet) %>%
  fill(reference, .direction = 'downup') %>%
  ungroup() %>%

  #Change names of the different units so they are in line with the recipes
  mutate(unit_enhet = unit_enhet %>%
           str_replace_all('desiliter|decilitre', 'dl') %>%
           str_replace_all('spiseskje|tablespoon', 'tbsp') %>%
           str_replace_all('one piece|a piece|piece|per item|stk one|one', 'pcs') %>%
           str_replace_all('porsjon', 'portion') %>%
           str_replace_all('fedd \\(med skall\\)|clove \\(with skin\\)', 'clove') %>%
           str_replace_all('handful', 'neve') %>%
           str_replace('rasher', 'slice') %>%
           str_replace('dl \\(in cubes\\)', 'dl') %>%
           str_replace('cm rot|cm of root', 'cm')
  ) %>%
  #Conditional
  mutate(unit_enhet = case_when(
    str_detect(unit_enhet, 'slice|skive') ~ 'slice',
    str_detect(Ingredients, 'Blåmuggost|Blue cheese') ~ str_replace(unit_enhet, 'portion', 'slice'), #One portion of cheese to go with a slice of bread
    TRUE ~ unit_enhet
  )) %>%

  #Remove some ingredients not used
  filter(!(str_detect(Ingredients, 'Celery') & unit_enhet %in% c('brutto', 'netto') ))

#Use the mean values for different types of crisp bread
temp <- unit_weights %>%
  filter(str_detect(tolower(Ingredients), 'crisp bread')) %>%
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'bread') ~ 'crisp bread'
  )) %>%
  group_by(Ingredients, unit_enhet) %>%
  mutate(g = mean(g),
         database_ID = median(database_ID),
         reference = 'i, t, u') %>%
  ungroup() %>% unique()

unit_weights <- unit_weights %>%
  filter(!str_detect(tolower(Ingredients), 'crisp bread')) %>%
  full_join(temp)

#Ingredients from recipes not present in the Norwegian food weight and measurement database----
temp <- list(

  #Meat
  c('Bacon', 'pack', '140', 'Gilde, stjernebacon', 'english'),
  c('Beef tongue', 'pcs', '1200', 'Gilde', 'english'),
  c('calf tail', 'portion', '200', 'f', 'english'),
  c('Turkey whole', 'pcs', '6000', 'https://www.matprat.no/oppdelingsguiden/kalkun/hel-kalkun/', 'english'),
  c('Turkey drumstick', 'pcs', '600', 'https://engrosnett.no/lokal-mat/homlagarden/kalkun-fryst', 'english'),
  c('Duck breast', 'pcs', '250', 'Gårdsand/Holte', 'english'),
  c('Duck leg', 'pcs', '280', 'Gårdsand/Holte', 'english'),
  c('sheep head', 'pcs', '1100', 'Nortura, Eldhus Smalahove', 'english'),
  c('sausage cumberland', 'pcs', '56.75', 'Tesco British Cumberland Sausages', 'english'),
  c('sausage turkey chicken', 'pack', '600', 'PRIOR kylling og kalkun grillpølser', 'english'),
  c('chicken diced', 'dl', '57.14', 'https://www.eatthismuch.com/food/nutrition/chicken-breast,454/', 'english'),
  c('rabbit', 'pcs', '2500', 'http://grillogmat.blogspot.com/2013/04/kanin-med-smak-av-rosmarin-og-lime.html', 'english'),
  c('pork hock', 'pcs', '1000', 'Fjellgris', 'english'),
  c('grouse breast', 'pcs', '100', 'https://ultimateupland.com/skewering-meathunters-the-true-cost-of-a-pound-of-game-bird/', 'english'),
  c('marrow bone', 'pcs', '113.4', 'https://grassrunfarms.com/blog/benefits-of-grass-fed-beef-marrow-bones/', 'english'), #Really per serving
  c('smoke-cured ham', 'dl', '113.9', 'FoodData Central', 'english'),
  c('cured ham', 'dl', '113.9', 'FoodData Central', 'english'),
  c('boiled ham', 'dl', '113.9', 'FoodData Central', 'english'),
  c('chicken skewer satay', 'pcs', '50', 'Ytterøy kylling', 'english'),

  #Bread, pastries and cookies
  c('Naan bread', 'pcs', '130', 'Santa Maria', 'english'),
  c('crisp bread', 'pack', '520', 'WASA', 'english'),
  c('biscuit oreo', 'pcs', '11', 'OREO original', 'english'),
  c('biscuit oreo', 'pack', '154', 'OREO original', 'english'),

  #Vegetables/plant based
  c('Asparagus', 'bunch', '250', 'Meny', 'english'),
  c('asparagus white', 'dl', '35.93', 'FoodData Central', 'english'),
  c('Asparagus beans', 'dl', '80', 'Assumed similar to other beans in database', 'english'),
  c('corn baby', 'pcs', '10', 'BAMA, mais mini', 'english'),
  c('corn baby', 'can', '425', 'Eldorado Meny', 'english'),
  c('bagel', 'pcs', '85', 'Meny, Hatting', 'english'),
  c('bay leaf', 'pcs', '0.2', 'Yahoo Answers', 'english'),
  c('Bean sprout', 'neve/dl', '97.3', 'FoodData Central', 'english'),
  c('Beans, green, raw', 'neve/dl', '40', 'FoodData Central', 'english'),
  c('olives, black, in oil, canned', 'neve', '50', 'Same as dl', 'english'),
  c('oliven, svarte, i olje, hermetisk', 'neve', '50', 'Same as dl', 'norwegian'),
  c('Break beans', 'pcs', '7', 'Assumed twice the weight of a sugar snap pea', 'english'),
  c('brødrasp/griljermel/bread crumb', 'dl', '67.6', 'FoodData Central', 'norwegian/norwegian/english'),
  c('Bread, semi-coarse', 'pcs', '700', 'Meny, assortert utvalg', 'english'), #Rewrite to keep only Bread, coarse and Loff as bread
  c('Bread, white/Loff, formstekt', 'pcs', '715', 'Meny, Gammeldags loff', 'english/norwegian'),
  c('butternut squash', 'pcs', '1360.8', 'River cottage every day', 'english'),
  c('cardamom pod', 'pcs', '0.14', 'thespiceguide.com', 'english'),
  c('cardamom', 'dl', '39.2', 'FoodData Central', 'english'),
  c('Celariac root', 'slice', '40', 'Assume same as one dl', 'english'),
  c('carrot paste', 'dl', '101.4', 'Assumed same density as other pastes', 'english'),
  c('cayenne pepper', 'dl', '35.8', 'FoodData Central', 'english'),
  c('cherry tomato', 'neve', '65', 'Assume one handful is about one dl', 'english'),
  c('Coconut milk', 'pcs', '400', 'Rema 1000, kokosmelk', 'english'),
  c('Coconut milk', 'box', '400', 'Rema 1000, kokosmelk', 'english'),
  c('Coconut milk', 'can', '400', 'Rema 1000, kokosmelk', 'english'),
  c('corn cob', 'pcs', '250', 'Grønn&Frisk, Meny', 'english'),
  c('corn kernel', 'dl', '105.7', 'FoodData Central', 'english'),
  c('corn kernel', 'can', '160', 'Green Giant Meny', 'english'),
  c('chapati', 'pcs', '50', 'Assume weight is the same as a tortilla', 'english'),
  c('chick pea flour', 'dl', '38.9', 'FoodData Central', 'english'),
  c('Chicory', 'pcs', '85', 'CooksInfo.com', 'english'),
  c('Chicory, endive', 'pcs', '85', 'CooksInfo.com', 'english'),
  c('chili flake', 'dl', '48.7', 'FoodData Central', 'english'),
  c('chili paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('chili powder', 'dl', '54.1', 'FoodData Central', 'english'),
  c('chili pepper, red', 'dl', '38', 'FoodData Central', 'english'),
  c('Chili sauce/chilisaus', 'dl', '111.6', 'FoodData Central', 'english/norwegian'),
  c('ciabatta', 'pcs', '62', 'Eldorado, Steinsovnbakt', 'english'),
  c('cardamom pod', 'pcs', '0.14', 'The Spice Guide', 'english'),
  c('cloves', 'pcs', '0.07', 'Charles McGuinnes medium', 'english'),
  c('cinnamon', 'dl', '52.6', 'FoodData Central', 'english'),
  c('cinnamon bar', 'pcs', '4', 'Google', 'english'),
  c('curry powder', 'dl', '42.6', 'FoodData Central', 'english'),
  c('cumin', 'dl', '40.6', 'FoodData Central', 'english'),
  c('crispi salad', 'pcs', '150', 'Meny, Gartner', 'english'),
  c('eplemos/apple sauce', 'dl', '108.2', 'FoodData Central', 'norwegian/english'),
  c('fig', 'pcs', '62.5', 'Kolonial', 'english'),
  c('curry paste', 'dl', '108.2', 'FoodData Central', 'english'),
  c('garam masala', 'dl', '30.4', 'FoodData Central', 'english'),
  c('Garlic', 'tsp', '2.5', 'a', 'english'), #Rule of thumb says one clove is equal to one tsp
  c('Garlic', 'dl', '50', 'Twenty teaspoons', 'english'),
  c('garlic paste', 'dl', '94.7', 'FoodData Central', 'english'),
  c('grilled sweet pepper', 'glass', '290', 'Meny, gaea', 'english'),
  c('ginger paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('Ginger, pickled', 'dl', '101.4', 'FoodData Central', 'english'),
  c('Ginger root', 'dl', '35.2', 'FoodData Central', 'english'), #This is ground, not grated ginger though
  c('heart salad', 'pcs', '150', 'Meny, Gartner', 'english'),
  c('Horse-radish', 'dl', '101.4', 'FoodData Central', 'english'),
  c('jalapeno/jalapeño', 'dl', '101.4', 'FoodData Central', 'english'),
  c('korianderfrø/coriander seed', 'dl', '33.8', 'FoodData Central', 'norwegian/english'),
  c('lasagna plate', 'pcs', '18', 'Barilla', 'english'),
  c('lemon peel', 'dl', '40.6', 'FoodData Central', 'english'),
  c('mango chutney', 'dl', '135.3', 'FoodData Central', 'english'),
  c('mustard seed', 'dl', '61.5', 'FoodData Central', 'english'),
  c('mustard powder', 'dl', '40.6', 'FoodData Central', 'english'),
  c('mint chutney', 'dl', '115', 'FoodData Central', 'english'),
  c('nutmeg', 'dl', '47.3', 'FoodData Central', 'english'),
  c("onion pearl", "pcs", "5", "Melissa's produce", 'english'), #https://www.melissas.com/products/pearl-onions
  c('paprika powder', 'dl', '46', 'FoodData Central', 'english'),
  c('Pumpkin Seeds', 'dl', '50.7', 'FoodData Central', 'english'),
  c('saffron', 'dl', '14.2', 'FoodData Central', 'english'),
  c('salad mix', 'leaf', '15', 'Recipeland', 'english'),
  c('lettuce/lollo rosso', 'leaf', '15', 'Recipeland', 'english'),
  c('salad', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset', 'english'),
  c('salad mix', 'pcs', '175', 'Kolonial, Baby leaf salad mix', 'english'),
  c('salad mix', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset', 'english'),
  c('scallion, spring onion', 'bunch', '150', 'Meny, Gartner', 'english'),
  c('Pearl barley', 'portion', '65', 'same as other grains used for dinner', 'english'),
  c('pickled pepper', 'dl', '101.4', 'FoodData Central', 'english'),
  c('puff pastry', 'pcs', '75', 'Meny, Bakeverket', 'english'),
  c('rhubarb', 'twig', '50', 'Assume same as dl', 'english'),
  c('ruccola', 'dl/neve', '8.5', 'FoodData Central', 'english'),
  c('ruccola', 'pack', '65', 'Grønn og frisk', 'english'),
  c('Taco sauce', 'glass', '230', 'Rema1000 and Old El Paso', 'english'),
  c('taco spice mix', 'pack', '28', 'Santa Maria', 'english'),
  c('Tomatoes, canned', 'box', '400', 'Mutti, Eldorado', 'english'),
  c('Tomatoes, canned', 'can', '400', 'Mutti, Eldorado', 'english'),
  c('Tomato Paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('Tomato purée', 'glass', '140', 'Mutti, Eldorado', 'english'),
  c('Tomato purée', 'pcs', '200', 'Petti, Kolonial', 'english'),
  c('tomato salsa', 'dl', '101.4', 'FoodData Central', 'english'),
  c('tomato salsa', 'glass', '240', 'Kolonial Supermat', 'english'),
  c('tomato beef', 'pcs', '250', 'frukt.no', 'english'),
  c('Tomatoes, sun-dried', 'dl', '101.4', 'FoodData Central', 'english'), #In oil, a bit less without oil
  c('Beans, white, in tomato sauce, canned', 'box', '410', 'Eldorado', 'english'),
  c('cherry tomato', 'bunch', '250', 'Kolonial, cherrytomat', 'english'),
  c('turmeric', 'dl', '63.6', 'FoodData Central', 'english'),
  c('Worcestershire sauce', 'dl', '116.2', 'FoodData Central', 'english'),
  c('zedoary', 'dl', '63.6', 'Assume same as turmeric, zedoary also known as "white turmeric"', 'english'),
  c('fennel seed', 'dl', '39.22', 'FoodData Central', 'english'),
  c('garlic oil', 'dl', '94.68', 'As other oils', 'english'),
  c('mint dried', 'dl', '10.82', 'FoodData Central', 'english'),
  c('mint sauce', 'dl', '101.4', 'FoodData Central', 'english'),
  c('Mushroom, shiitake', 'pcs', '19', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
  c('Mushroom, portebello', 'pcs', '160.75', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
  c('champignon', 'pcs', '18', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
  c('hazelnut oil', 'dl', '94.68', 'As other oils', 'english'),
  c('peanut oil', 'dl', '94.68', 'As other oils', 'english'),
  c('pesto', 'glass', '185', 'Meny, Eldorado', 'english'),
  c('peas, dry', 'neve', '80', 'A handful is about 1 dl', 'english'),
  c('bean broad', 'dl', '109.9', 'FoodData Central', 'english'),
  c('Beans, white, large, canned', 'pcs', '290', 'Øko kolonial', 'english'), #Kolonial/Oda recipes uses "pcs" for these
  c('Beans, red kidney, canned', 'pcs', '290', 'Øko kolonial', 'english'), #Kolonial/Oda recipes uses "pcs" for these
  c('Beans, red kidney, canned', 'box', '380', 'Meny', 'english'), #3/4 kidney bean boxes/cans at Meny are 380g
  c('bok choi', 'pcs', '125', 'Meny', 'english'),
  c('Mushroom, chestnut', 'pcs', '10', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
  c('caraway seed', 'dl', '45.31', 'FoodData Central', 'english'),
  c('allspice', 'dl', '40.58', 'FoodData Central', 'english'),
  c('pine nuts', 'neve', '20', 'As other nuts', 'english'),
  c('radish', 'bunch', '130', 'Kolonial', 'english'),
  c('peas, frozen', 'neve', '60', 'As dl', 'english'),
  c('sugar snap peas', 'dl/neve', '26.95', 'FoodData Central', 'english'),
  c('lemongrass/sitrongressrot', 'pcs', '50', 'Meny', 'english/norwegian'),
  c('Chick peas, canned', 'box', '290', 'Øko kolonial', 'english'),
  c('cucumber, pickled', 'pcs', '70', 'Produce converter Howmuchisin', 'english'),
  c('chunky salsa', 'glass', '230', 'Santa Maria', 'english'),
  c('orange zest', 'dl', '40.58', 'FoodData Central', 'english'),
  c('lemon zest', 'dl', '40.58', 'FoodData Central', 'english'),
  c('lime zest', 'dl', '40.58', 'FoodData Central', 'english'), #Assume the same as orange and lemon
  c('grapes', 'bunch', '500', 'Kolonial', 'english'),
  c('grapes', 'neve', '70', 'As dl', 'english'),
  c('nutmeg', 'pcs', '7.5', 'Wikipedia', 'english'),
  c('cloves', 'pcs', '0.09', 'https:\\/\\/forum.norbrygg.no\\/threads\\/traden-for-dumme-sporsmal.23452\\/page-62', 'english'),
  c('celery', 'pcs', '400', 'Meny', 'english'),
  c('cashew nuts', 'pcs', '1.6', 'https://www.verywellfit.com/cashew-nutrition-facts-4586608', 'english'),
  c('rice parboiled', 'pack', '400', 'Toro', 'english'),
  c('broccolini', 'pack', '200', 'Kolonial', 'english'),
  c('lingonberry jam', 'pcs', '200', 'Løiten Gourmet', 'english'), #Used in the online recipe cart for recipes from Oda
  c('lentils, canned', 'box', '180', 'GoEco', 'english'),
  c('swede', 'slice', '55', 'same as dl', 'english'),
  c('squash, zucchini', 'pack', '250', 'https://www.shoprite.co.za/All-Departments/Food/Fresh-Food/Fresh-Vegetables/Courgettes%2C-Aubergines-and-Squash/Mixed-Patty-Pans-Pack-250g/p/10145280EA', 'english'),
  c('apricot nectar', 'dl', '104.82', 'FoodData Central', 'english'),
  c('apricot preserve', 'dl', '135.26', 'FoodData Central', 'english'),
  c('flaxseed meal', 'dl', '439.48', 'FoodData Central', 'english'),
  c('chia seed', 'dl', '101.44', 'FoodData Central', 'english'),
  c('hemp seed', 'dl', '67.63', 'FoodData Central', 'english'),
  c('onion powder', 'dl', '46.66', 'FoodData Central', 'english'),
  c('poppy seed', 'dl', '59.51', 'FoodData Central', 'english'),
  c('toenjang', 'dl', '202.88', 'FoodData Central', 'english'),
  c('broccoli', 'dl', '47.9', 'FoodData Central', 'english'),
  c('vanilla extract', 'dl', '87.92', 'FoodData Central', 'english'),
  c('kale', 'bunch', '150', 'Meny', 'english'),
  c('spinach, raw', 'neve', '12', 'same as dl', 'english'),

  #Seafood
  c('Anchovies, canned', 'box', '55', 'Abba', 'english'),
  c('fish sauce/fiskesaus', 'dl', '121.7', 'FoodData Central', 'english/norwegian'),
  c('crab', 'pcs', '500', 'Meny, portion sizes seafood', 'english'),
  c('crab shell', 'pcs', '150', 'Meny, Lerøy seafood', 'english'),
  c('crab claw', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
  c('crab claw', 'pcs', '400', 'Kolonial', 'english'),
  c('fish burger', 'pcs', '125', 'Lofoten fiskeprodukter', 'english'),
  c('fish cake', 'pcs', '65', 'Lofoten fiskeprodukter', 'english'),
  c('lobster', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
  c('lobster', 'pcs', '300', 'Meny Atlantic star', 'english'),
  c('Mackerel fillet, in tomato sauce, canned', 'pcs', '170', 'Stabburet', 'english'),
  c('scampi', 'portion', '200', 'Protein rich seafood', 'english'),
  c('shrimp paste', 'dl', '101.4', 'FoodData Central', 'english'),
  c('tuna oil', 'box', '185', 'Eldorado', 'english'),
  c('tuna water', 'box', '185', 'Eldorado', 'english'),
  c('tuna oil', 'pcs', '185', 'Eldorado', 'english'),
  c('tuna water', 'pcs', '185', 'Eldorado', 'english'),
  c('tuna oil', 'can', '185', 'Eldorado', 'english'),
  c('tuna water', 'can', '185', 'Eldorado', 'english'),
  c('squid baby', 'pcs', '85.05', 'http://www.clovegarden.com/ingred/seasquidc.html', 'english'),
  c('herring smoked', 'pcs', '300', 'DOMSTEIN Sjømat', 'english'),
  c('prawn', 'pcs', '17', 'Kostholdsplanleggeren//Matvaretabellen', 'english'),
  c('arctic char', 'pcs', '650', 'Mat i Bergen', 'english'),

  #Dairy
  c('Blue cheese', 'dl', '47.34', 'FoodData Central', 'english'),
  c('Cream cheese', 'box', '125', 'Tine', 'english'),
  c('Cream cheese', 'dl', '108.2', 'FoodData Central', 'english'),
  c('ricotta salata', 'dl', '104.8', 'FoodData Central', 'english'),
  c('Feta cheese', 'pcs', '200', 'Meny, Apetina/Kolios', 'english'),
  c('Parmesan', 'neve', '40', 'Assume about the same as one dl', 'english'),
  c('chevre', 'pcs', '190', 'Tine', 'english'),
  c('butter spice', 'pack', '125', 'Tine Grillsmør', 'english'),
  c('soft-ripened cheese (brie, camembert etc)', 'pcs', '150', 'Kolonial', 'english'),
  c('soft-ripened cheese (brie, camembert etc)', 'dl', '95', 'FoodData Central', 'english'), #Mascarpone
  c('yoghurt skyr', 'pcs', '160', 'Skyr', 'english'),
  c('yoghurt', 'pcs', '500', 'Tine', 'english'),

  #Div
  c('aioli', 'box', '202.4', 'Oda Mills Aioli', 'english'), #Calcuated by using the same density as mayo
  c('aioli', 'dl', '101.4', 'FoodData Central', 'english'),
  c('caviar', 'tube', '245', 'Mills kaviar', 'english'),
  c('broth cube', 'pcs', '10', 'TORO klar kjøttbuljong', 'english'),
  c('tabasco', 'dl', '101.4', 'FoodData Central', 'english'),
  c('oyster sauce', 'dl', '304.3', 'FoodData Central', 'english'),
  c('Walnuts', 'pcs', '7', 'Google', 'english'),
  c('wine', 'glass', '110', 'vinmonopolet', 'english'),
  c('molasses', 'dl', '142.44', 'FoodData Central', 'english'),
  c('tamarind paste', 'dl', '101.44', 'FoodData Central', 'english'),
  c('horseradish prepared', 'dl', '101.44', 'FoodData Central', 'english'),
  c('mayonnaise', 'dl', '87.92', 'FoodData Central', 'english'),
  c('cream sauce base', 'pcs', '50', 'Toro', 'english'), #Used in the online recipe cart
  c('fish soup base', 'pcs', '81', 'Toro Bergensk fiskesuppe', 'english'), #Used in online recipe cart
  c('hollandaise base', 'pcs', '26', 'Toro', 'english'),
  c('bolognese base', 'pcs', '45', 'Toro Kjøttdeigsaus', 'english'), #Used in online recipe cart
  c('nacho', 'pack', '185', 'Olde El Paso', 'english'),
  c('sauce white', 'pack', '38', 'Toro Hvit Saus', 'english'),
  c('sauce teriyaki', 'dl', '94.7', 'FoodData Central', 'english'),
  c('sauce hoisin', 'dl', '111.6', 'FoodData Central', 'english'),
  c('sauce', 'hp', '114.97', 'FoodData Central', 'english'),
  c('salsa', 'dl', '94.7', 'FoodData Central', 'english'),
  c('dip mix', 'pack', '22', 'Maarud', 'english'),
  c('tandoori spice', 'dl', '106.8', 'FoodData Central', 'english'),
  c('fajita spice', 'dl', '81.2', 'FoodData Central', 'english'),
  c('wasabi', 'dl', '101.4', 'FoodData Central', 'english'),
  c('yeast dry', 'dl', '81.2', 'FoodData Central', 'english'),
  c('anise star', 'pcs', '0.7', 'https://www.chowhound.com/post/mass-star-anise-pod-969821', 'english'),
  c('kimchi', 'dl', '63.4', 'FoodData Central', 'english'),
  c('miso', 'dl', '116.2', 'FoodData Central', 'english'),
  c('oil chili', 'dl', '101.44', 'FoodData Central', 'english'),
  c('chocolate', 'dl', '101.44', 'FoodData Central', 'english'),
  c('Einebær/juniper berry', 'pcs', '1', '', 'norwegian/english'),
  c('pickled onion', 'dl', '55', 'Assume same as reguler onions', 'english'),
  c('salmon roe', 'dl', '94.68', 'FoodData Central', 'english')

)

#Add new ingredients to unit_weights
#' @param vector A vector containing Ingredient name, unit name, gram per unit, reference for where the knowledge is from and the language used
createIngredientRow <- function(vector){

  row <- tibble(
    'Ingredients' = vector[1],
    'unit_enhet' = vector[2],
    'g' = as.numeric(vector[3]),
    'reference' = vector[4],
    'language' = vector[5]
  )

}
#Run createIngredientRow on temp to create a new dataframe of ingredients to be added to unit_weights
new_ingredients <- lapply(temp, createIngredientRow) %>%
  bind_rows() %>%
  #Separate languages
  separate_rows(., c(Ingredients, language), sep = '/')

#Herbs
herbs <- tibble(
  'Ingredients' = c('rosemary fresh', 'tarragon fresh', 'Basil fresh/Basilikum', 'Chives fresh',
                    'Parsley fresh/Kruspersille/Persille', 'thyme fresh', 'Coriander fresh/Koriander',
                    'Oregano fresh', 'Chervil fresh/Kjørvel', 'Dill fresh', 'mint fresh', 'sorrel', 'cilantro fresh',
                    'cress fresh/watercress fresh', 'sage fresh')) %>%
  mutate(unit_enhet = 'bunch/dl/neve',
         g = 20,
         reference = case_when(
           !Ingredients %in% c('sorrel', 'cilantro', 'salvie fresh') ~ 'Fersk i Pose, Netfresh/Kolonial',
           TRUE ~ 'As other herbs'),
         language = c('english', 'english', 'english/norwegian', 'english', 'english/norwegian/norwegian', 'english',
                      'english/norwegian', 'english/norwegian', 'english/norwegian', 'english/norwegian', 'english', 'english', 'english',
                      'english', 'english')) %>%
  separate_rows(.,c(Ingredients, language), sep = '/') %>%
  separate_rows(unit_enhet, sep = '/')
#Add sprigs/twigs of herbs
#Amounts taken from thespicetrain
herbs_sprigs <- tibble(
  'Ingredients' = c('rosemary fresh', 'tarragon fresh', 'Basil fresh/Basilikum', 'Chives fresh',
                    'Parsley fresh/Kruspersille/Persille', 'thyme fresh', 'Oregano fresh',
                    'Dill fresh', 'mint fresh', 'sage fresh', 'coriander fresh')) %>%
  mutate(unit_enhet = c('tsp', 'tsp', 'tbsp/tbsp', 'tsp',
                        'tsp/tsp/tsp', 'tsp', 'tsp',
                        'tsp', 'tsp', 'tbsp', 'tsp'),
         g = c('1', '1', '1/1', '0.375',
               '1.5/1.5/1.5', '0.125', '1',
               '1', '1', '1', '0.25'),
         language = c('english', 'english', 'english/norwegian', 'english',
                      'english/norwegian/norwegian', 'english', 'english/norwegian',
                      'english/norwegian', 'english', 'english', 'english')) %>%
  separate_rows(., c(Ingredients, language), sep = '/') %>%
  separate_rows(unit_enhet, sep = '/') %>%
  separate_rows(g, sep = '/') %>%
  mutate_at('g', ~as.numeric(.)) %>%
  #Turn tsp/tbsp into dl and then to gram
  mutate(
    g = case_when(
      unit_enhet == 'tsp' ~ g*0.05,
      unit_enhet == 'tbsp' ~ g*0.15),
    unit_enhet = case_when(
      unit_enhet %in% c('tsp', 'tbsp') ~ 'dl'
    )) %>%
  mutate_at('g', ~as.numeric(.)) %>%
  inner_join(herbs, by = c('Ingredients', 'unit_enhet', 'language')) %>%
  unique() %>%

  #clean up
  mutate(unit_enhet = str_replace(unit_enhet, 'dl', 'twig'),
         g = g.x * g.y) %>%
  select(-c(g.x, g.y))
#Dry herbs
herbs_dry <- tibble(
  'Ingredients' = c('oregano dried', 'basil dried', 'thyme dried', 'tarragon dried', 'fenugreek dried', 'sage dried'),
  'g' = c(20.3, 30.4, 29.1, 32.5, 75.1, 13.52)) %>%
  mutate(unit_enhet = 'dl',
         reference = 'FoodData Central',
         language = 'english')
#Per leaf
herbs_leaf <- tibble(
  'Ingredients' = c('Basil fresh', 'sage fresh'),
  'g' = c(0.3, 0.5)) %>%
  mutate(unit_enhet = 'leaf',
         reference = 'The Spice Train',
         language = 'english')

#Fats and oils
oils <- tibble(
  'Ingredients' = c('olive oil/olivenolje', 'coconut oil', 'soy oil/soybean oil',
                    'canola oil', 'sunflower oil', 'sesame oil/sesamolje', 'corn oil', 'salad oil',
                    'rapeseed oil', 'truffle oil', 'vegetable oil/vegetabilsk olje', 'sunflower oil'),
  'language' = c('english/norwegian', 'english', 'english/english', 'english', 'english', 'english/norwegian',
                 'english', 'english', 'english', 'english', 'english/norwegian', 'english')) %>%
  mutate(unit_enhet = 'dl',
         'g' = 94.7,
         reference = 'FoodData Central') %>%
  separate_rows(., c(Ingredients, language), sep = '/')

new_ingredients <- full_join(new_ingredients, herbs) %>%
  full_join(herbs_sprigs) %>%
  full_join(herbs_dry) %>%
  full_join(herbs_leaf) %>%
  full_join(., oils) %>%
  mutate(Ingredients = tolower(Ingredients))

#Add all new ingredients back to unit_weight
unit_weights <- full_join(unit_weights, new_ingredients) %>%

  #Turn everything to small letters
  mutate(Ingredients = tolower(Ingredients)) %>%

  #Unique ID
  group_by(Ingredients) %>% #For ingredients already found the the dataset that jsut didn't have a weight measurement
  fill(database_ID, .direction = 'downup') %>%
  ungroup()

#Create unique id's for the new ingredients
temp <- unit_weights %>%
  filter(is.na(database_ID)) %>%
  group_by(Ingredients) %>%
  mutate(database_ID = cur_group_id()) %>%
  mutate(database_ID = database_ID + 1000) %>% #can't use max(database_ID) from unit_weights as it contains NA
  ungroup() %>%
  separate_rows(Ingredients, sep = '/')

#Add the ingredients back to unit_weights with their new id's
unit_weights <- unit_weights %>%
  filter(!is.na(database_ID)) %>%
  full_join(., temp) %>%
  replace_na(list(Foodgroup = '')) %>%
  separate_rows(unit_enhet, sep = '/') %>% unique() %>% #Separate rows with multiple units keeping the same ID

  #Use mean unit measurement
  group_by(Ingredients, unit_enhet) %>%
  mutate(g = mean(g)) %>%
  ungroup() %>%
  unique()

#Ingredients not needed in the final database
various$not_needed <- unit_weights %>%

  #Whole foodgroups not needed
  filter(str_detect(Foodgroup,
                    regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter|cakes|candy|Desert|Dressing|hjemmelaget|homemade|Grain breakfast cereal|knekkebrød|biscuits|pølser|cold cuts|pålegg|Fish spread|Fish products|snacks|candy|Meat products')) |

           #Use raw eggs as the standard for eggs, bread semi coarse as standard for bread
           ((str_detect(Ingredients, 'egg|Egg') & str_detect(Ingredients, 'store|large|små|small|mellomstore|medium|pan-fried|stekt|rambled'))) |

           #Ingredients not needed
           ((str_detect(Ingredients, 'aby') & str_detect(Ingredients, 'carrot|gulrot'))) |
           str_detect(Ingredients, 'fried|without skin|uten skinn|hicken and turkey|resh pasta|baguettes, fine, half fried|drink|drikke|fløteerstatning|milkshake|prim') |
           (str_detect(Ingredients, 'asagn|ulgur|uinoa|ouscous|akaroni|asta|getti|otet|otato|ønner|inser|eans|egumes|ris|Ris|Rice|rice') &
              str_detect(Ingredients, 'boiled|kokt|\\bcooked')) |
           Ingredients %in% c('Oliven, grønne, pickles', 'Olives, green, pickled',
                              'Pepper, hel', 'Pepper, malt', 'Gingersnaps', 'Ginger nuts',
                              'Almond sticks', 'Swedish almond cake macaroon tea cake', 'Loff, spiral',
                              'cream substitute, vegetable fat', 'whipped cream, canned', 'instant coffee creamer', 'coffee creamer',
                              'kaffemelk', 'krem/topping på boks', 'melkepulver', 'syrnet melk, kulturmelk, kefir', 'cultured milk, kefir milk',
                              'gammelost', 'traditional norwegian cheese, matured, gammelost', 'gomme', 'traditional norwegian cheese, gomme',
                              'bread, white, spirally shaped', 'loff, spiral', 'grapes, without seeds', 'beans, green, frozen'
           )) %>%

  #Any ingredient of the above that should be kept
  filter(!Ingredients %in% c('crisp bread', 'flatbread, hard','smoke-cured ham', 'cured ham', 'spekeskinke', 'boiled ham', 'honning', 'sukker, brunt',
                             'sukker hvitt', 'anchovies, canned', 'anchovy fillets, canned', 'salmon, smoked, dry salted',
                             'mackerel fillet, in tomato sauce, canned', 'cod roe', 'tuna canned', 'ground meat, raw', 'bread, semi-coarse', 'bread, white',
                             'cream cracker', 'salami', 'rice parboiled', 'caramels', 'marshmallows', 'ice cream', 'pancakes',
                             'biscuit, with oats, digestive', 'biscuit, marie', 'biscuit, for childen', 'muesli'))

#Remove the not needed ingredients
unit_weights <- unit_weights %>%
  filter(!Ingredients %in% various$not_needed$Ingredients) %>%

  #Change some ingredient names to better fit with the recipes
  mutate(Ingredients = Ingredients %>%
           str_replace(', rå|, raw|, uncooked|, tørr', '') %>%
           str_replace('red kidney', 'kidney') %>%
           str_replace('Tortilla, hvetelefse, fullkorn', 'tortilla, fullkorn, hvete, whole, wheat') %>%
           str_replace('tortilla, hvetelefse, fin', 'tortilla, fin, hvete, wheat') %>%
           str_replace('soft norwegian flatbread, ', '') %>%
           str_replace('\\bcelery\\b', 'celery stalk') %>%
           str_replace('with rye', 'rye') %>%
           str_replace(', spring onion', '') %>%
           str_replace(', hvetelefse,', '') %>%
           str_replace('leaf beet, mangold', 'chard') %>%
           str_replace('aubergine', 'eggplant') %>%
           str_replace('bread, semi-coarse', 'bread') %>%
           str_replace('cream cracker', 'cracker cream') %>%
           str_replace('linseeds, flax seeds', 'flax seed') %>%
           str_replace('sesame paste, tahini', 'tahini') %>%
           str_replace('parmesan', 'parmesan cheese') %>%
           str_replace('biscuit, with oats, digestive', 'biscuit digestive') %>%
           str_replace('biscuit, marie', 'biscuit marie') %>%
           str_replace('biscuit, for childen', 'biscuit children') %>%
           str_replace('mashed potatoes', 'potato mash') %>%
           str_replace('tortilla, hvetelefse, fin|tortilla, wheat flour', 'tortilla') %>% #Standard
           str_replace('tortilla, hvetelefse, fullkorn', 'tortilla fullkorn') %>%
           str_replace('tortilla, wheat flour, wholemeal', 'tortilla coarse') %>%
           str_replace('tomatoes, sun-dried, in oil', 'tomato sun dried in oil') %>%
           str_replace('tomatoes, sun-dried', 'tomato sun dried') %>%
           str_replace('desiccated coconut', 'coconut mass') %>%

           #Change all plural forms to singular
           str_replace('anchovies', 'anchovy') %>%
           str_replace('fillets', 'fillet') %>%
           str_replace('beans', 'bean') %>%
           str_replace('peas', 'pea') %>%
           str_replace('seeds', 'seed') %>%
           str_replace('nuts', 'nut') %>%
           str_replace('peppers', 'pepper') %>%
           str_replace('chops', 'chop') %>%
           str_replace('olives', 'olive') %>%
           str_replace('tomatoes', 'tomato') %>%
           str_replace('purée', 'puree') %>%
           str_replace('grapes', 'grape') %>%
           str_replace('plums', 'plum') %>%
           str_replace('apricots', 'apricot') %>%
           str_replace('almonds', 'almond') %>%
           str_replace('prunes', 'prune') %>%
           str_replace('lentils', 'lentil') %>%
           str_replace('raisins', 'raisin') %>%
           str_replace('caramels', 'caramel') %>%
           str_replace('marshmallows', 'marshmallow') %>%
           str_replace('pancakes', 'pancake') %>%
           str_replace('oats', 'oat') %>%
           str_replace("noodles", "noodle")
  ) %>%
  #Conditionals
  mutate(Ingredients = case_when(
    str_detect(Ingredients, 'chop') & str_detect(Foodgroup, 'lamb') ~ paste0('lamb ', Ingredients),
    TRUE ~ Ingredients
  )) %>%
  #Remove some duplicates (f.ex both storebought and homemade bread, or where the english and norwegian name is the same)
  select(-Foodgroup) %>% unique()

#Create one ID each for butter, margarine and ghee
unit_weights <- unit_weights %>%
  mutate(database_ID = case_when(
    Ingredients %in% c('smør', 'butter') ~ database_ID + 2000,
    Ingredients %in% c('margarin', 'margarine') ~ database_ID + 2001,
    Ingredients == 'ghee' ~ database_ID + 2002,
    TRUE ~ database_ID
  ))

#Rename "g" column to be more understandable
unit_weights <- unit_weights %>%
  rename(grams_per_unit = g)

#Remove some duplicates and add all reference rows for each food into one row
unit_weights <- unit_weights %>%
  filter(!(str_detect(Ingredients, 'basil') & database_ID == 28)) %>%
  rename(tmp = reference) %>%
  group_by(Ingredients, unit_enhet, grams_per_unit, database_ID, language) %>%
  summarise(reference = str_c(unique(tmp), collapse = ", ")) %>%
  ungroup()

#Only keep dl for volume
unit_weights

tmp <- unit_weights %>%
  group_by(Ingredients) %>%
  #Find the ingredients with spoon units
  filter(any(unit_enhet %in% c("tbsp", "tsp", "ms", "ts", "ss"))) %>%
  #Remove those that have dl already
  filter(!any(unit_enhet == "dl")) %>%
  ungroup() %>%
  #Only keep spoon units
  filter(unit_enhet %in% c("ts", "tsp", "tbsp", "ms", "ss")) %>%
  #Turn tbsp and tsp units into dl
  mutate(
    grams_per_unit = case_when(
      unit_enhet %in% c("tsp", "ts") ~ grams_per_unit*20,
      unit_enhet %in% c("tbsp", "ss", "ms") ~ grams_per_unit*(6.67)),
    unit_enhet = "dl") %>%
  #Use the mean value
  group_by(Ingredients, unit_enhet, database_ID, language, reference) %>%
  summarise(tmp = mean(grams_per_unit)) %>% ungroup() %>%
  rename(grams_per_unit = tmp)

#Add back to unit_weights and remove all spoon units
unit_weights <- full_join(unit_weights, tmp) %>%
  filter(!unit_enhet %in% c("ts", "tsp", "ss", "tbsp", "ms"))

#Save
saveRDS(unit_weights, "./data-raw/unit_weights.Rds")

#Simplify way to add new food items
#Read in already created db
old <- readRDS("./data-raw/unit_weights.Rds")

#Ingredients with IDs already
new <- tibble(
  temporary = c(

    "chocolate spread, nut spread;dl;100;",
    "gelatin;pcs;2;", #Same as leaf
    "sauce satay;dl;108.2;FoodData Central",
    "sauce ponzu;dl;111.59;FoodData Central",
    "sauce tomato;dl;103.55;FoodData Central",
    "sauce barbeque;dl;120.89;FoodData Central",
    "granola;dl;50;Same as muesli from Helsedir",
    "granola;portion;100;Same as muesli from Helsedir",
    "lemon balm fresh;dl_bunch_neve;20;Lemon balm is in the mint family, so same as mint"
  )
) %>%
  separate(., col = temporary, into = c("Ingredients", "unit_enhet", "grams_per_unit", "reference"), sep = ";") %>%
  separate_rows(., unit_enhet, sep = "_") %>%
  #Add IDs
  left_join(., old %>% select(Ingredients, database_ID) %>% unique()) %>%
  mutate(language = "english",
         grams_per_unit = as.numeric(as.character(grams_per_unit)))

#Save both old and new
all <- bind_rows(old, new) %>%
  #Create new ID's
  group_by(Ingredients) %>%
  mutate(database_ID = case_when(
    is.na(database_ID) ~ cur_group_id() + 0.77,
    TRUE ~ database_ID
  )) %>% ungroup()

saveRDS(all, "./data-raw/unit_weights2.Rds")
