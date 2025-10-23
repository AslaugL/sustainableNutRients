#'  #Script to create the database for unit and weights per food item
#'
#' #Weight and portion size database
#' #Empty list to fill with various variables
#' various <- list()
#'
#' #Load raw data from the Norwegian directorate of health
#' raw_data <- readxl::read_xlsx(
#'   system.file("extdata", "weights_portion_sizes_foods_NorwegianDirectorateOfHealth_2015.xlsx", package = "sustainableNutRients"))
#'
#' #Reformat
#' unit_weights <- raw_data %>% rename(
#'   portion = contains('porsjon'),
#'   tbsp = contains('ss,'),
#'   tsp = contains('ts,'),
#'   ms = contains('ms,'),
#'   dl = contains('dl,'),
#'   unit_enhet = EnhetUnit,
#'   g = contains('g/'),
#'   brutto = `Brutto g`,
#'   netto = `Netto g`
#' ) %>%
#'
#'   #Turn into long format, end goal to get one column with ingredient names/food items,
#'   #one column with units and one column for weight in grams per unit and an unique identifier for each food item
#'   select(-`Spiselig del % Edible part %`) %>% #Don't need this column
#'   pivot_wider(., #Turn into long format first to get all the different unit types in the name row
#'               names_from = unit_enhet,
#'               values_from = g) %>%
#'   pivot_longer(.,
#'                cols = -contains('Matvare'),
#'                names_to = 'unit_enhet',
#'                values_to = 'g') %>%
#'   drop_na() %>%
#'
#' #Rename for easier use later
#' rename(Ingredients = `Matvare\r\nFood item`,
#'        Foodgroup = `Matvaregruppe\r\nFoodgroup`) %>%
#'
#'   #Remove some not needed foodgroups, but keep some food items in those foodgroups
#'   filter(
#'     #Get rid of
#'     !(str_detect(Foodgroup, regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter')) &
#'         #Keep
#'         !str_detect(Ingredients, 'Pancake'))
#'   ) %>%
#'
#'   #Rename some ingredients
#'   mutate(Ingredients = Ingredients %>%
#'            str_replace('Loff, formstekt', 'Loff') %>%
#'            str_replace('Bread, white, square-shaped', 'Bread, white') %>%
#'            str_replace('Grapes, with seeds', 'grapes')) %>%
#'
#'   #Create unique IDs for each ingredients
#'   group_by(Ingredients) %>%
#'   mutate(database_ID = cur_group_id()) %>%
#'   ungroup() %>%
#'
#'   #Split into Norwegian and English, first make a language column with language identifier
#'   mutate(language = 'norwegian\nenglish') %>%
#'   separate_rows(., c(Ingredients, Foodgroup, unit_enhet, language), sep = '\n') %>%
#'   #Remove whitespace
#'   mutate_at(c('Ingredients', 'Foodgroup', 'unit_enhet'), ~str_trim(.)) %>%
#'
#'   #Split rows of foods with multiple weights
#'   #First extract the references to a separate column
#'   separate_rows(., g, sep = '/') %>%
#'   #The original datatable have weights in grams together with a letter for reference, split into two columns
#'   mutate(g = g %>%
#'            str_replace('([:alpha:])', ' \\1') %>%
#'            str_replace(',', '.')) %>%
#'   separate(g, into = c('g', 'reference'), sep = ' ') %>%
#'   mutate_at('g', ~ as.numeric(.)) %>% #Turn g into numeric for future calculations
#'
#'   #Use the mean weight for the foods with multiple weights and remove duplicates
#'   group_by(Ingredients, unit_enhet, Foodgroup, language) %>%
#'   mutate(g = mean(g),
#'          reference = paste(reference, collapse = ',')) %>%
#'   ungroup() %>% unique()
#'
#' #Split the 'Rømme, crème fraîche, kesam' and butter/margarine rows into individual rows, create new IDs for rømme/kesam etc
#' #Add ghee as a similar type ingredient
#' temp <- unit_weights %>%
#'   filter(str_detect(Ingredients, 'margarin|crème fraîche')) %>%
#'   #Add ghee, similar to butter and oil
#'   mutate(Ingredients = case_when(
#'     str_detect(Ingredients, 'margarin') & !str_detect(Ingredients, 'iquid|lytende') ~ paste0(Ingredients, ', ghee'),
#'     TRUE ~ Ingredients
#'   )) %>%
#'   separate_rows(., Ingredients, sep = ',') %>% mutate(Ingredients = str_trim(Ingredients)) %>%
#'   #Remove liquid/flytende margarine
#'   filter(!str_detect(Ingredients, 'iquid|lytende')) %>%
#'   #New ID's
#'   mutate(database_ID = case_when(
#'     str_detect(Ingredients, 'ømme|our') ~ as.integer(10000),
#'     str_detect(Ingredients, 'crème') ~ as.integer(10001),
#'     Ingredients %in% c('kesam', 'quark') ~ as.integer(10002),
#'     TRUE ~ database_ID
#'   ))
#'
#' unit_weights <- unit_weights %>%
#'   #Remove the old grouped rømme/kesam rows from unit_weights and add the new individual rows
#'   filter(!str_detect(unit_weights$Ingredients,'crème fraîche|margarin')) %>%
#'   full_join(., temp) %>%
#'
#'   #Fill in reference letters that went missing when separating multiple weight ingredients
#'   group_by(Ingredients, unit_enhet) %>%
#'   fill(reference, .direction = 'downup') %>%
#'   ungroup() %>%
#'
#'   #Change names of the different units so they are in line with the recipes
#'   mutate(unit_enhet = unit_enhet %>%
#'            str_replace_all('desiliter|decilitre', 'dl') %>%
#'            str_replace_all('spiseskje|tablespoon', 'tbsp') %>%
#'            str_replace_all('one piece|a piece|piece|per item|stk one|one', 'pcs') %>%
#'            str_replace_all('porsjon', 'portion') %>%
#'            str_replace_all('fedd \\(med skall\\)|clove \\(with skin\\)', 'clove') %>%
#'            str_replace_all('handful', 'neve') %>%
#'            str_replace('rasher', 'slice') %>%
#'            str_replace('dl \\(in cubes\\)', 'dl') %>%
#'            str_replace('cm rot|cm of root', 'cm')
#'   ) %>%
#'   #Conditional
#'   mutate(unit_enhet = case_when(
#'     str_detect(unit_enhet, 'slice|skive') ~ 'slice',
#'     str_detect(Ingredients, 'Blåmuggost|Blue cheese') ~ str_replace(unit_enhet, 'portion', 'slice'), #One portion of cheese to go with a slice of bread
#'     TRUE ~ unit_enhet
#'   )) %>%
#'
#'   #Remove some ingredients not used
#'   filter(!(str_detect(Ingredients, 'Celery') & unit_enhet %in% c('brutto', 'netto') ))
#'
#' #Use the mean values for different types of crisp bread
#' temp <- unit_weights %>%
#'   filter(str_detect(tolower(Ingredients), 'crisp bread')) %>%
#'   mutate(Ingredients = case_when(
#'     str_detect(Ingredients, 'bread') ~ 'crisp bread'
#'   )) %>%
#'   group_by(Ingredients, unit_enhet) %>%
#'   mutate(g = mean(g),
#'          database_ID = median(database_ID),
#'          reference = 'i, t, u') %>%
#'   ungroup() %>% unique()
#'
#' unit_weights <- unit_weights %>%
#'   filter(!str_detect(tolower(Ingredients), 'crisp bread')) %>%
#'   full_join(temp)
#'
#' #Ingredients from recipes not present in the Norwegian food weight and measurement database----
#' temp <- list(
#'
#'   #Meat
#'   c('Bacon', 'pack', '140', 'Gilde, stjernebacon', 'english'),
#'   c('Beef tongue', 'pcs', '1200', 'Gilde', 'english'),
#'   c('calf tail', 'portion', '200', 'f', 'english'),
#'   c('Turkey whole', 'pcs', '6000', 'https://www.matprat.no/oppdelingsguiden/kalkun/hel-kalkun/', 'english'),
#'   c('Turkey drumstick', 'pcs', '600', 'https://engrosnett.no/lokal-mat/homlagarden/kalkun-fryst', 'english'),
#'   c('Duck breast', 'pcs', '250', 'Gårdsand/Holte', 'english'),
#'   c('Duck leg', 'pcs', '280', 'Gårdsand/Holte', 'english'),
#'   c('sheep head', 'pcs', '1100', 'Nortura, Eldhus Smalahove', 'english'),
#'   c('sausage cumberland', 'pcs', '56.75', 'Tesco British Cumberland Sausages', 'english'),
#'   c('sausage turkey chicken', 'pack', '600', 'PRIOR kylling og kalkun grillpølser', 'english'),
#'   c('chicken diced', 'dl', '57.14', 'https://www.eatthismuch.com/food/nutrition/chicken-breast,454/', 'english'),
#'   c('rabbit', 'pcs', '2500', 'http://grillogmat.blogspot.com/2013/04/kanin-med-smak-av-rosmarin-og-lime.html', 'english'),
#'   c('pork hock', 'pcs', '1000', 'Fjellgris', 'english'),
#'   c('grouse breast', 'pcs', '100', 'https://ultimateupland.com/skewering-meathunters-the-true-cost-of-a-pound-of-game-bird/', 'english'),
#'   c('marrow bone', 'pcs', '113.4', 'https://grassrunfarms.com/blog/benefits-of-grass-fed-beef-marrow-bones/', 'english'), #Really per serving
#'   c('smoke-cured ham', 'dl', '113.9', 'FoodData Central', 'english'),
#'   c('cured ham', 'dl', '113.9', 'FoodData Central', 'english'),
#'   c('boiled ham', 'dl', '113.9', 'FoodData Central', 'english'),
#'   c('chicken skewer satay', 'pcs', '50', 'Ytterøy kylling', 'english'),
#'
#'   #Bread, pastries and cookies
#'   c('Naan bread', 'pcs', '130', 'Santa Maria', 'english'),
#'   c('crisp bread', 'pack', '520', 'WASA', 'english'),
#'   c('biscuit oreo', 'pcs', '11', 'OREO original', 'english'),
#'   c('biscuit oreo', 'pack', '154', 'OREO original', 'english'),
#'
#'   #Vegetables/plant based
#'   c('almond flour', 'dl', '47.34', 'FoodData Central', 'english'),
#'   c('Asparagus', 'bunch', '250', 'Meny', 'english'),
#'   c('asparagus white', 'dl', '35.93', 'FoodData Central', 'english'),
#'   c('Asparagus beans', 'dl', '80', 'Assumed similar to other beans in database', 'english'),
#'   c('corn baby', 'pcs', '10', 'BAMA, mais mini', 'english'),
#'   c('corn baby', 'can', '425', 'Eldorado Meny', 'english'),
#'   c('bagel', 'pcs', '85', 'Meny, Hatting', 'english'),
#'   c('bay leaf', 'pcs', '0.2', 'Yahoo Answers', 'english'),
#'   c('Bean sprouts', 'neve/dl', '97.3', 'FoodData Central', 'english'),
#'   c('Beans, green, raw', 'neve/dl', '40', 'FoodData Central', 'english'),
#'   c('olives, black, in oil, canned', 'neve', '50', 'Same as dl', 'english'),
#'   c('oliven, svarte, i olje, hermetisk', 'neve', '50', 'Same as dl', 'norwegian'),
#'   c('Break beans', 'pcs', '7', 'Assumed twice the weight of a sugar snap pea', 'english'),
#'   c('brødrasp/griljermel/bread crumb', 'dl', '67.6', 'FoodData Central', 'norwegian/norwegian/english'),
#'   c('Bread, semi-coarse', 'pcs', '700', 'Meny, assortert utvalg', 'english'), #Rewrite to keep only Bread, coarse and Loff as bread
#'   c('Bread, white/Loff, formstekt', 'pcs', '715', 'Meny, Gammeldags loff', 'english/norwegian'),
#'   c('butternut squash', 'pcs', '1360.8', 'River cottage every day', 'english'),
#'   c('cardamom pod', 'pcs', '0.14', 'thespiceguide.com', 'english'),
#'   c('cardamom', 'dl', '39.2', 'FoodData Central', 'english'),
#'   c('Celariac root', 'slice', '40', 'Assume same as one dl', 'english'),
#'   c('carrot paste', 'dl', '101.4', 'Assumed same density as other pastes', 'english'),
#'   c('cayenne pepper', 'dl', '35.8', 'FoodData Central', 'english'),
#'   c('cherry tomato', 'neve', '65', 'Assume one handful is about one dl', 'english'),
#'   c('Coconut milk', 'pcs', '400', 'Rema 1000, kokosmelk', 'english'),
#'   c('Coconut milk', 'box', '400', 'Rema 1000, kokosmelk', 'english'),
#'   c('Coconut milk', 'can', '400', 'Rema 1000, kokosmelk', 'english'),
#'   c('corn cob', 'pcs', '250', 'Grønn&Frisk, Meny', 'english'),
#'   c('corn kernel', 'dl', '105.7', 'FoodData Central', 'english'),
#'   c('corn kernel', 'can', '160', 'Green Giant Meny', 'english'),
#'   c('chapati', 'pcs', '50', 'Assume weight is the same as a tortilla', 'english'),
#'   c('chick pea flour', 'dl', '38.9', 'FoodData Central', 'english'),
#'   c('Chicory', 'pcs', '85', 'CooksInfo.com', 'english'),
#'   c('Chicory, endive', 'pcs', '85', 'CooksInfo.com', 'english'),
#'   c('chili flake', 'dl', '48.7', 'FoodData Central', 'english'),
#'   c('chili paste', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('chili powder', 'dl', '54.1', 'FoodData Central', 'english'),
#'   c('chili pepper, red', 'dl', '38', 'FoodData Central', 'english'),
#'   c('Chili sauce/chilisaus', 'dl', '111.6', 'FoodData Central', 'english/norwegian'),
#'   c('ciabatta', 'pcs', '62', 'Eldorado, Steinsovnbakt', 'english'),
#'   c('cardamom pod', 'pcs', '0.14', 'The Spice Guide', 'english'),
#'   c('cloves', 'pcs', '0.07', 'Charles McGuinnes medium', 'english'),
#'   c('cinnamon', 'dl', '52.6', 'FoodData Central', 'english'),
#'   c('cinnamon bar', 'pcs', '4', 'Google', 'english'),
#'   c('curry powder', 'dl', '42.6', 'FoodData Central', 'english'),
#'   c('cumin', 'dl', '40.6', 'FoodData Central', 'english'),
#'   c('crispi salad', 'pcs', '150', 'Meny, Gartner', 'english'),
#'   c('eplemos/apple sauce', 'dl', '108.2', 'FoodData Central', 'norwegian/english'),
#'   c('fig', 'pcs', '62.5', 'Kolonial', 'english'),
#'   c('curry paste', 'dl', '108.2', 'FoodData Central', 'english'),
#'   c('garam masala', 'dl', '30.4', 'FoodData Central', 'english'),
#'   c('Garlic', 'tsp', '2.5', 'a', 'english'), #Rule of thumb says one clove is equal to one tsp
#'   c('Garlic', 'dl', '50', 'Twenty teaspoons', 'english'),
#'   c('garlic paste', 'dl', '94.7', 'FoodData Central', 'english'),
#'   c('grilled sweet pepper', 'glass', '290', 'Meny, gaea', 'english'),
#'   c('ginger paste', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('Ginger, pickled', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('Ginger root', 'dl', '35.2', 'FoodData Central', 'english'), #This is ground, not grated ginger though
#'   c('heart salad', 'pcs', '150', 'Meny, Gartner', 'english'),
#'   c('Horse-radish', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('jalapeno/jalapeño', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('korianderfrø/coriander seed', 'dl', '33.8', 'FoodData Central', 'norwegian/english'),
#'   c('lasagna plate', 'pcs', '18', 'Barilla', 'english'),
#'   c('lemon peel', 'dl', '40.6', 'FoodData Central', 'english'),
#'   c('mango chutney', 'dl', '135.3', 'FoodData Central', 'english'),
#'   c('mustard seed', 'dl', '61.5', 'FoodData Central', 'english'),
#'   c('mustard powder', 'dl', '40.6', 'FoodData Central', 'english'),
#'   c('mint chutney', 'dl', '115', 'FoodData Central', 'english'),
#'   c('nutmeg', 'dl', '47.3', 'FoodData Central', 'english'),
#'   c("onion pearl", "pcs", "5", "Melissa's produce", 'english'), #https://www.melissas.com/products/pearl-onions
#'   c('paprika powder', 'dl', '46', 'FoodData Central', 'english'),
#'   c('saffron', 'dl', '14.2', 'FoodData Central', 'english'),
#'   c('salad mix', 'leaf', '15', 'Recipeland', 'english'),
#'   c('lettuce/lollo rosso', 'leaf', '15', 'Recipeland', 'english'),
#'   c('salad', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset', 'english'),
#'   c('salad mix', 'pcs', '175', 'Kolonial, Baby leaf salad mix', 'english'),
#'   c('salad mix', 'dl', '21.5', 'Mean between the two types of salad in the Norwegian dataset', 'english'),
#'   c('scallion, spring onion', 'bunch', '150', 'Meny, Gartner', 'english'),
#'   c('Pearl barley', 'portion', '65', 'same as other grains used for dinner', 'english'),
#'   c('pickled pepper', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('puff pastry', 'pcs', '75', 'Meny, Bakeverket', 'english'),
#'   c('rhubarb', 'twig', '50', 'Assume same as dl', 'english'),
#'   c('ruccola', 'dl/neve', '8.5', 'FoodData Central', 'english'),
#'   c('ruccola', 'pack', '65', 'Grønn og frisk', 'english'),
#'   c('Taco sauce', 'glass', '230', 'Rema1000 and Old El Paso', 'english'),
#'   c('spice mix taco', 'pack', '28', 'Santa Maria', 'english'),
#'   c('Tomatoes, canned', 'box', '400', 'Mutti, Eldorado', 'english'),
#'   c('Tomatoes, canned', 'can', '400', 'Mutti, Eldorado', 'english'),
#'   c('Tomato Paste', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('Tomato purée', 'glass', '140', 'Mutti, Eldorado', 'english'),
#'   c('Tomato purée', 'pcs', '200', 'Petti, Kolonial', 'english'),
#'   c('tomato salsa', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('tomato salsa', 'glass', '240', 'Kolonial Supermat', 'english'),
#'   c('tomato beef', 'pcs', '250', 'frukt.no', 'english'),
#'   c('Tomatoes, sun-dried', 'dl', '101.4', 'FoodData Central', 'english'), #In oil, a bit less without oil
#'   c('Beans, white, in tomato sauce, canned', 'box', '410', 'Eldorado', 'english'),
#'   c('cherry tomato', 'bunch', '250', 'Kolonial, cherrytomat', 'english'),
#'   c('turmeric', 'dl', '63.6', 'FoodData Central', 'english'),
#'   c('Worcestershire sauce', 'dl', '116.2', 'FoodData Central', 'english'),
#'   c('zedoary', 'dl', '63.6', 'Assume same as turmeric, zedoary also known as "white turmeric"', 'english'),
#'   c('fennel seed', 'dl', '39.22', 'FoodData Central', 'english'),
#'   c('garlic oil', 'dl', '94.68', 'As other oils', 'english'),
#'   c('mint dried', 'dl', '10.82', 'FoodData Central', 'english'),
#'   c('mint sauce', 'dl', '101.4', 'FoodData Central', 'english'),
#'   #c('Mushroom, shiitake', 'pcs', '19', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
#'   c('Mushroom, portebello', 'pcs', '160.75', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
#'   c('champignon', 'pcs', '18', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
#'   c('hazelnut oil', 'dl', '94.68', 'As other oils', 'english'),
#'   c('peanut oil', 'dl', '94.68', 'As other oils', 'english'),
#'   c('pesto', 'glass', '185', 'Meny, Eldorado', 'english'),
#'   c('peas, dry', 'neve', '80', 'A handful is about 1 dl', 'english'),
#'   c('bean broad', 'dl', '109.9', 'FoodData Central', 'english'),
#'   c('Beans, white, large, canned', 'pcs', '290', 'Øko kolonial', 'english'), #Kolonial/Oda recipes uses "pcs" for these
#'   c('Beans, red kidney, canned', 'pcs', '290', 'Øko kolonial', 'english'), #Kolonial/Oda recipes uses "pcs" for these
#'   c('Beans, red kidney, canned', 'box', '380', 'Meny', 'english'), #3/4 kidney bean boxes/cans at Meny are 380g
#'   c('bok choi', 'pcs', '125', 'Meny', 'english'),
#'   c('Mushroom, chestnut', 'pcs', '10', 'https://hannaone.com/Recipe/weightmushrooms.html', 'english'),
#'   c('caraway seed', 'dl', '45.31', 'FoodData Central', 'english'),
#'   c('allspice', 'dl', '40.58', 'FoodData Central', 'english'),
#'   c('pine nuts', 'neve', '20', 'As other nuts', 'english'),
#'   c('radish', 'bunch', '130', 'Kolonial', 'english'),
#'   c('peas, frozen', 'neve', '60', 'As dl', 'english'),
#'   c('sugar snap peas', 'dl/neve', '26.95', 'FoodData Central', 'english'),
#'   c('lemongrass/sitrongressrot', 'pcs', '50', 'Meny', 'english/norwegian'),
#'   c('Chick peas, canned', 'box', '290', 'Øko kolonial', 'english'),
#'   c('cucumber, pickled', 'pcs', '70', 'Produce converter Howmuchisin', 'english'),
#'   c('chunky salsa', 'glass', '230', 'Santa Maria', 'english'),
#'   c('orange zest', 'dl', '40.58', 'FoodData Central', 'english'),
#'   c('lemon zest', 'dl', '40.58', 'FoodData Central', 'english'),
#'   c('lime zest', 'dl', '40.58', 'FoodData Central', 'english'), #Assume the same as orange and lemon
#'   c('grapes', 'bunch', '500', 'Kolonial', 'english'),
#'   c('grapes', 'neve', '70', 'As dl', 'english'),
#'   c('nutmeg', 'pcs', '7.5', 'Wikipedia', 'english'),
#'   c('cloves', 'pcs', '0.09', 'https:\\/\\/forum.norbrygg.no\\/threads\\/traden-for-dumme-sporsmal.23452\\/page-62', 'english'),
#'   c('celery', 'pcs', '400', 'Meny', 'english'),
#'   c('cashew nuts', 'pcs', '1.6', 'https://www.verywellfit.com/cashew-nutrition-facts-4586608', 'english'),
#'   c('rice parboiled', 'pack', '400', 'Toro', 'english'),
#'   c('broccolini', 'pack', '200', 'Kolonial', 'english'),
#'   c('lingonberry jam', 'pcs', '200', 'Løiten Gourmet', 'english'), #Used in the online recipe cart for recipes from Oda
#'   c('lentils, canned', 'box', '180', 'GoEco', 'english'),
#'   c('swede', 'slice', '55', 'same as dl', 'english'),
#'   c('squash, zucchini', 'pack', '250', 'https://www.shoprite.co.za/All-Departments/Food/Fresh-Food/Fresh-Vegetables/Courgettes%2C-Aubergines-and-Squash/Mixed-Patty-Pans-Pack-250g/p/10145280EA', 'english'),
#'   c('apricot nectar', 'dl', '104.82', 'FoodData Central', 'english'),
#'   c('apricot preserve', 'dl', '135.26', 'FoodData Central', 'english'),
#'   c('flaxseed meal', 'dl', '439.48', 'FoodData Central', 'english'),
#'   c('chia seed', 'dl', '101.44', 'FoodData Central', 'english'),
#'   c('hemp seed', 'dl', '67.63', 'FoodData Central', 'english'),
#'   c('onion powder', 'dl', '46.66', 'FoodData Central', 'english'),
#'   c('poppy seed', 'dl', '59.51', 'FoodData Central', 'english'),
#'   c('toenjang', 'dl', '202.88', 'FoodData Central', 'english'),
#'   c('broccoli', 'dl', '47.9', 'FoodData Central', 'english'),
#'   c('vanilla extract', 'dl', '87.92', 'FoodData Central', 'english'),
#'   c('vanilla powder', 'dl', '89.23', 'FoodData Central', 'english'),
#'   c('kale', 'bunch', '150', 'Meny', 'english'),
#'   c('spinach, raw', 'neve', '12', 'same as dl', 'english'),
#'
#'   #Seafood
#'   c('Anchovies, canned', 'box', '55', 'Abba', 'english'),
#'   c('fish sauce/fiskesaus', 'dl', '121.7', 'FoodData Central', 'english/norwegian'),
#'   c('crab', 'pcs', '500', 'Meny, portion sizes seafood', 'english'),
#'   c('crab shell', 'pcs', '150', 'Meny, Lerøy seafood', 'english'),
#'   c('crab claw', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
#'   c('crab claw', 'pcs', '83', 'Domstein', 'english'),
#'   c('fish burger', 'pcs', '125', 'Lofoten fiskeprodukter', 'english'),
#'   c('fish cake', 'pcs', '65', 'Lofoten fiskeprodukter', 'english'),
#'   c('lobster', 'portion', '500', 'Meny, portion sizes seafood', 'english'),
#'   c('lobster', 'pcs', '300', 'Meny Atlantic star', 'english'),
#'   c('Mackerel fillet, in tomato sauce, canned', 'pcs', '170', 'Stabburet', 'english'),
#'   c('scampi', 'portion', '200', 'Protein rich seafood', 'english'),
#'   c('shrimp paste', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('tuna oil', 'box', '185', 'Eldorado', 'english'),
#'   c('tuna water', 'box', '185', 'Eldorado', 'english'),
#'   c('tuna oil', 'pcs', '185', 'Eldorado', 'english'),
#'   c('tuna water', 'pcs', '185', 'Eldorado', 'english'),
#'   c('tuna oil', 'can', '185', 'Eldorado', 'english'),
#'   c('tuna water', 'can', '185', 'Eldorado', 'english'),
#'   c('squid baby', 'pcs', '85.05', 'http://www.clovegarden.com/ingred/seasquidc.html', 'english'),
#'   c('herring smoked', 'pcs', '300', 'DOMSTEIN Sjømat', 'english'),
#'   c('prawn', 'pcs', '17', 'Kostholdsplanleggeren//Matvaretabellen', 'english'),
#'   c('arctic char', 'pcs', '650', 'Mat i Bergen', 'english'),
#'
#'   #Dairy
#'   c('Blue cheese', 'dl', '47.34', 'FoodData Central', 'english'),
#'   c('Cream cheese', 'box', '125', 'Tine', 'english'),
#'   c('Cream cheese', 'dl', '108.2', 'FoodData Central', 'english'),
#'   c('ricotta salata', 'dl', '104.8', 'FoodData Central', 'english'),
#'   c('Feta cheese', 'pcs', '200', 'Meny, Apetina/Kolios', 'english'),
#'   c('Parmesan', 'neve', '40', 'Assume about the same as one dl', 'english'),
#'   c('chevre', 'pcs', '190', 'Tine', 'english'),
#'   c('butter', 'pack', '125', 'Tine Grillsmør', 'english'),
#'   c('soft-ripened cheese (brie, camembert etc)', 'pcs', '150', 'Kolonial', 'english'),
#'   c('soft-ripened cheese (brie, camembert etc)', 'dl', '95', 'FoodData Central', 'english'), #Mascarpone
#'   c('yoghurt skyr', 'pcs', '160', 'Skyr', 'english'),
#'   c('yoghurt', 'pcs', '500', 'Tine', 'english'),
#'
#'   #Div
#'   c('aioli', 'box', '202.4', 'Oda Mills Aioli', 'english'), #Calcuated by using the same density as mayo
#'   c('aioli', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('caviar', 'tube', '245', 'Mills kaviar', 'english'),
#'   c('broth cube', 'pcs', '10', 'TORO klar kjøttbuljong', 'english'),
#'   c('tabasco', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('oyster sauce', 'dl', '304.3', 'FoodData Central', 'english'),
#'   c('Walnuts', 'pcs', '7', 'Google', 'english'),
#'   c('wine', 'glass', '110', 'vinmonopolet', 'english'),
#'   c('molasses', 'dl', '142.44', 'FoodData Central', 'english'),
#'   c('tamarind paste', 'dl', '101.44', 'FoodData Central', 'english'),
#'   c('horseradish prepared', 'dl', '101.44', 'FoodData Central', 'english'),
#'   c('mayonnaise', 'dl', '87.92', 'FoodData Central', 'english'),
#'   c('cream sauce base', 'pcs', '50', 'Toro', 'english'), #Used in the online recipe cart
#'   c('fish soup base', 'pcs', '81', 'Toro Bergensk fiskesuppe', 'english'), #Used in online recipe cart
#'   c('hollandaise base', 'pcs', '26', 'Toro', 'english'),
#'   c('bolognese base', 'pcs', '45', 'Toro Kjøttdeigsaus', 'english'), #Used in online recipe cart
#'   c('nacho', 'pack', '185', 'Olde El Paso', 'english'),
#'   c('sauce white', 'pack', '38', 'Toro Hvit Saus', 'english'),
#'   c('sauce teriyaki', 'dl', '94.7', 'FoodData Central', 'english'),
#'   c('sauce hoisin', 'dl', '111.6', 'FoodData Central', 'english'),
#'   c('sauce hp', 'dl', '114.97', 'FoodData Central', 'english'),
#'   c('salsa', 'dl', '94.7', 'FoodData Central', 'english'),
#'   c('dip mix', 'pack', '22', 'Maarud', 'english'),
#'   c('spice mix tandoori', 'dl', '106.8', 'FoodData Central', 'english'),
#'   c('spice mix fajita', 'dl', '81.2', 'FoodData Central', 'english'),
#'   c('wasabi', 'dl', '101.4', 'FoodData Central', 'english'),
#'   c('anise star', 'pcs', '0.7', 'https://www.chowhound.com/post/mass-star-anise-pod-969821', 'english'),
#'   c('kimchi', 'dl', '63.4', 'FoodData Central', 'english'),
#'   c('miso', 'dl', '116.2', 'FoodData Central', 'english'),
#'   c('oil chili', 'dl', '101.44', 'FoodData Central', 'english'),
#'   c('chocolate', 'dl', '101.44', 'FoodData Central', 'english'),
#'   c('Einebær/juniper berry', 'pcs', '1', '', 'norwegian/english'),
#'   c('pickled onion', 'dl', '55', 'Assume same as reguler onions', 'english'),
#'   c('salmon roe', 'dl', '94.68', 'FoodData Central', 'english')
#'
#' )
#'
#' #Add new ingredients to unit_weights
#' #' @param vector A vector containing Ingredient name, unit name, gram per unit, reference for where the knowledge is from and the language used
#' createIngredientRow <- function(vector){
#'
#'   row <- tibble(
#'     'Ingredients' = vector[1],
#'     'unit_enhet' = vector[2],
#'     'g' = as.numeric(vector[3]),
#'     'reference' = vector[4],
#'     'language' = vector[5]
#'   )
#'
#' }
#' #Run createIngredientRow on temp to create a new dataframe of ingredients to be added to unit_weights
#' new_ingredients <- lapply(temp, createIngredientRow) %>%
#'   bind_rows() %>%
#'   #Separate languages
#'   separate_rows(., c(Ingredients, language), sep = '/')
#'
#' #Herbs
#' herbs <- tibble(
#'   'Ingredients' = c('rosemary fresh', 'tarragon fresh', 'Basil fresh/Basilikum', 'Chives fresh',
#'                     'Parsley fresh/Kruspersille/Persille', 'thyme fresh', 'Coriander fresh/Koriander',
#'                     'Oregano fresh', 'Chervil fresh/Kjørvel', 'Dill fresh', 'mint fresh', 'sorrel', 'cilantro fresh',
#'                     'cress fresh/watercress fresh', 'sage fresh')) %>%
#'   mutate(unit_enhet = 'bunch/dl/neve',
#'          g = 20,
#'          reference = case_when(
#'            !Ingredients %in% c('sorrel', 'cilantro', 'salvie fresh') ~ 'Fersk i Pose, Netfresh/Kolonial',
#'            TRUE ~ 'As other herbs'),
#'          language = c('english', 'english', 'english/norwegian', 'english', 'english/norwegian/norwegian', 'english',
#'                       'english/norwegian', 'english/norwegian', 'english/norwegian', 'english/norwegian', 'english', 'english', 'english',
#'                       'english', 'english')) %>%
#'   separate_rows(.,c(Ingredients, language), sep = '/') %>%
#'   separate_rows(unit_enhet, sep = '/')
#' #Add sprigs/twigs of herbs
#' #Amounts taken from thespicetrain
#' herbs_sprigs <- tibble(
#'   'Ingredients' = c('rosemary fresh', 'tarragon fresh', 'Basil fresh/Basilikum', 'Chives fresh',
#'                     'Parsley fresh/Kruspersille/Persille', 'thyme fresh', 'Oregano fresh',
#'                     'Dill fresh', 'mint fresh', 'sage fresh', 'coriander fresh')) %>%
#'   mutate(unit_enhet = c('tsp', 'tsp', 'tbsp/tbsp', 'tsp',
#'                         'tsp/tsp/tsp', 'tsp', 'tsp',
#'                         'tsp', 'tsp', 'tbsp', 'tsp'),
#'          g = c('1', '1', '1/1', '0.375',
#'                '1.5/1.5/1.5', '0.125', '1',
#'                '1', '1', '1', '0.25'),
#'          language = c('english', 'english', 'english/norwegian', 'english',
#'                       'english/norwegian/norwegian', 'english', 'english/norwegian',
#'                       'english/norwegian', 'english', 'english', 'english')) %>%
#'   separate_rows(., c(Ingredients, language), sep = '/') %>%
#'   separate_rows(unit_enhet, sep = '/') %>%
#'   separate_rows(g, sep = '/') %>%
#'   mutate_at('g', ~as.numeric(.)) %>%
#'   #Turn tsp/tbsp into dl and then to gram
#'   mutate(
#'     g = case_when(
#'       unit_enhet == 'tsp' ~ g*0.05,
#'       unit_enhet == 'tbsp' ~ g*0.15),
#'     unit_enhet = case_when(
#'       unit_enhet %in% c('tsp', 'tbsp') ~ 'dl'
#'     )) %>%
#'   mutate_at('g', ~as.numeric(.)) %>%
#'   inner_join(herbs, by = c('Ingredients', 'unit_enhet', 'language')) %>%
#'   unique() %>%
#'
#'   #clean up
#'   mutate(unit_enhet = str_replace(unit_enhet, 'dl', 'twig'),
#'          g = g.x * g.y) %>%
#'   select(-c(g.x, g.y))
#' #Dry herbs
#' herbs_dry <- tibble(
#'   'Ingredients' = c('oregano dried', 'basil dried', 'thyme dried', 'tarragon dried', 'fenugreek dried', 'sage dried'),
#'   'g' = c(20.3, 30.4, 29.1, 32.5, 75.1, 13.52)) %>%
#'   mutate(unit_enhet = 'dl',
#'          reference = 'FoodData Central',
#'          language = 'english')
#' #Per leaf
#' herbs_leaf <- tibble(
#'   'Ingredients' = c('Basil fresh', 'sage fresh'),
#'   'g' = c(0.3, 0.5)) %>%
#'   mutate(unit_enhet = 'leaf',
#'          reference = 'The Spice Train',
#'          language = 'english')
#'
#' #Fats and oils
#' oils <- tibble(
#'   'Ingredients' = c('olive oil/olivenolje', 'coconut oil', 'soy oil',
#'                     'canola oil', 'sunflower oil', 'sesame oil/sesamolje', 'corn oil', 'salad oil',
#'                     'rapeseed oil', 'truffle oil', 'vegetable oil/vegetabilsk olje', 'sunflower oil'),
#'   'language' = c('english/norwegian', 'english', 'english', 'english', 'english', 'english/norwegian',
#'                  'english', 'english', 'english', 'english', 'english/norwegian', 'english')) %>%
#'   mutate(unit_enhet = 'dl',
#'          'g' = 94.7,
#'          reference = 'FoodData Central') %>%
#'   separate_rows(., c(Ingredients, language), sep = '/')
#'
#' new_ingredients <- full_join(new_ingredients, herbs) %>%
#'   full_join(herbs_sprigs) %>%
#'   full_join(herbs_dry) %>%
#'   full_join(herbs_leaf) %>%
#'   full_join(., oils) %>%
#'   mutate(Ingredients = tolower(Ingredients))
#'
#' #Add all new ingredients back to unit_weight
#' unit_weights <- full_join(unit_weights, new_ingredients) %>%
#'
#'   #Turn everything to small letters
#'   mutate(Ingredients = tolower(Ingredients)) %>%
#'
#'   #Unique ID
#'   group_by(Ingredients) %>% #For ingredients already found the the dataset that jsut didn't have a weight measurement
#'   fill(database_ID, .direction = 'downup') %>%
#'   ungroup()
#'
#' #Create unique id's for the new ingredients
#' temp <- unit_weights %>%
#'   filter(is.na(database_ID)) %>%
#'   group_by(Ingredients) %>%
#'   mutate(database_ID = cur_group_id()) %>%
#'   mutate(database_ID = database_ID + 1000) %>% #can't use max(database_ID) from unit_weights as it contains NA
#'   ungroup() %>%
#'   separate_rows(Ingredients, sep = '/')
#'
#' #Add the ingredients back to unit_weights with their new id's
#' unit_weights <- unit_weights %>%
#'   filter(!is.na(database_ID)) %>%
#'   full_join(., temp) %>%
#'   replace_na(list(Foodgroup = '')) %>%
#'   separate_rows(unit_enhet, sep = '/') %>% unique() %>% #Separate rows with multiple units keeping the same ID
#'
#'   #Use mean unit measurement
#'   group_by(Ingredients, unit_enhet) %>%
#'   mutate(g = mean(g)) %>%
#'   ungroup() %>%
#'   unique()
#'
#' #Ingredients not needed in the final database
#' various$not_needed <- unit_weights %>%
#'
#'   #Whole foodgroups not needed
#'   filter(str_detect(Foodgroup,
#'                     regex('prepared|tilberedt|Spedbarnsmat|Baby food|dishes|Retter|cakes|candy|Desert|Dressing|hjemmelaget|homemade|Grain breakfast cereal|knekkebrød|biscuits|pølser|cold cuts|pålegg|Fish spread|Fish products|snacks|candy|Meat products')) |
#'
#'            #Use raw eggs as the standard for eggs, bread semi coarse as standard for bread
#'            ((str_detect(Ingredients, 'egg|Egg') & str_detect(Ingredients, 'store|large|små|small|mellomstore|medium|pan-fried|stekt|rambled'))) |
#'
#'            #Ingredients not needed
#'            ((str_detect(Ingredients, 'aby') & str_detect(Ingredients, 'carrot|gulrot'))) |
#'            str_detect(Ingredients, 'fried|without skin|uten skinn|hicken and turkey|resh pasta|baguettes, fine, half fried|drink|drikke|fløteerstatning|milkshake|prim') |
#'            (str_detect(Ingredients, 'asagn|ulgur|uinoa|ouscous|akaroni|asta|getti|otet|otato|ønner|inser|eans|egumes|ris|Ris|Rice|rice') &
#'               str_detect(Ingredients, 'boiled|kokt|\\bcooked')) |
#'            Ingredients %in% c('Oliven, grønne, pickles', 'Olives, green, pickled',
#'                               'Pepper, hel', 'Pepper, malt', 'Gingersnaps', 'Ginger nuts',
#'                               'Almond sticks', 'Swedish almond cake macaroon tea cake', 'Loff, spiral',
#'                               'cream substitute, vegetable fat', 'whipped cream, canned', 'instant coffee creamer', 'coffee creamer',
#'                               'kaffemelk', 'krem/topping på boks', 'melkepulver', 'syrnet melk, kulturmelk, kefir', 'cultured milk, kefir milk',
#'                               'gammelost', 'traditional norwegian cheese, matured, gammelost', 'gomme', 'traditional norwegian cheese, gomme',
#'                               'bread, white, spirally shaped', 'loff, spiral', 'grapes, without seeds', 'beans, green, frozen',
#'                               'grill sausage, lean', 'grill sausage, with cheese', 'meat sausage, lean'
#'            )) %>%
#'
#'   #Any ingredient of the above that should be kept
#'   filter(!Ingredients %in% c('crisp bread', 'flatbread, hard','smoke-cured ham', 'cured ham', 'spekeskinke', 'boiled ham', 'honning', 'sukker, brunt',
#'                              'sukker hvitt', 'anchovies, canned', 'anchovy fillets, canned', 'salmon, smoked, dry salted',
#'                              'mackerel fillet, in tomato sauce, canned', 'cod roe', 'tuna canned', 'ground meat, raw', 'bread, semi-coarse', 'bread, white',
#'                              'cream cracker', 'salami', 'rice parboiled', 'caramels', 'marshmallows', 'ice cream', 'pancakes', 'waffles, made with sour cream', 'waffles',
#'                              'biscuit, with oats, digestive', 'biscuit, marie', 'biscuit, for childen', 'muesli', 'roast beef'))
#'
#' #Remove the not needed ingredients
#' unit_weights <- unit_weights %>%
#'   filter(!Ingredients %in% various$not_needed$Ingredients) %>%
#'
#'   #Change some ingredient names to better fit with the recipes
#'   mutate(Ingredients = Ingredients %>%
#'            str_replace(', rå|, raw|, uncooked|, tørr', '') %>%
#'            str_replace('red kidney', 'kidney') %>%
#'            str_replace('Tortilla, hvetelefse, fullkorn', 'tortilla, fullkorn, hvete, whole, wheat') %>%
#'            str_replace('tortilla, hvetelefse, fin', 'tortilla, fin, hvete, wheat') %>%
#'            str_replace('soft norwegian flatbread, ', '') %>%
#'            str_replace('\\bcelery\\b', 'celery stalk') %>%
#'            str_replace('with rye', 'rye') %>%
#'            str_replace(', spring onion', '') %>%
#'            str_replace(', hvetelefse,', '') %>%
#'            str_replace('leaf beet, mangold', 'chard') %>%
#'            str_replace('aubergine', 'eggplant') %>%
#'            str_replace('bread, semi-coarse', 'bread') %>%
#'            str_replace('cream cracker', 'cracker cream') %>%
#'            str_replace('linseeds, flax seeds', 'flax seed') %>%
#'            str_replace('sesame paste, tahini', 'tahini') %>%
#'            str_replace('parmesan', 'parmesan cheese') %>%
#'            str_replace('biscuit, with oats, digestive', 'biscuit digestive') %>%
#'            str_replace('biscuit, marie', 'biscuit marie') %>%
#'            str_replace('biscuit, for childen', 'biscuit children') %>%
#'            str_replace('mashed potatoes', 'potato mash') %>%
#'            str_replace('tortilla, hvetelefse, fin|tortilla, wheat flour', 'tortilla') %>% #Standard
#'            str_replace('tortilla, hvetelefse, fullkorn', 'tortilla fullkorn') %>%
#'            str_replace('tortilla, wheat flour, wholemeal', 'tortilla coarse') %>%
#'            str_replace('tomatoes, sun-dried, in oil', 'tomato sun dried in oil') %>%
#'            str_replace('tomatoes, sun-dried', 'tomato sun dried') %>%
#'            str_replace('desiccated coconut', 'coconut mass') %>%
#'            str_replace('grill sausage, hot dogs', 'sausage grill') %>%
#'            str_replace("potato soft flatbread", "potato flatbread lompe") %>%
#'
#'            #Change all plural forms to singular
#'            str_replace('anchovies', 'anchovy') %>%
#'            str_replace('fillets', 'fillet') %>%
#'            str_replace('beans', 'bean') %>%
#'            str_replace('peas', 'pea') %>%
#'            str_replace('seeds', 'seed') %>%
#'            str_replace('nuts', 'nut') %>%
#'            str_replace('peppers', 'pepper') %>%
#'            str_replace('chops', 'chop') %>%
#'            str_replace('olives', 'olive') %>%
#'            str_replace('tomatoes', 'tomato') %>%
#'            str_replace('purée', 'puree') %>%
#'            str_replace('grapes', 'grape') %>%
#'            str_replace('plums', 'plum') %>%
#'            str_replace('apricots', 'apricot') %>%
#'            str_replace('almonds', 'almond') %>%
#'            str_replace('prunes', 'prune') %>%
#'            str_replace('lentils', 'lentil') %>%
#'            str_replace('raisins', 'raisin') %>%
#'            str_replace('caramels', 'caramel') %>%
#'            str_replace('marshmallows', 'marshmallow') %>%
#'            str_replace('pancakes', 'pancake') %>%
#'            str_replace('waffles, made with sour cream', 'waffle sour cream') %>%
#'            str_replace('waffles', 'waffle') %>%
#'            str_replace('oats', 'oat') %>%
#'            str_replace("noodles", "noodle") %>%
#'
#'            #NEW
#'            str_replace_all(., c(
#'              "shrimps, in brine" = "shrimp brine",
#'              "shrimps, boiled" = "shrimp boiled"
#'            ))
#'   ) %>%
#'   #Conditionals
#'   mutate(Ingredients = case_when(
#'     str_detect(Ingredients, 'chop') & str_detect(Foodgroup, 'lamb') ~ paste0('lamb ', Ingredients),
#'     TRUE ~ Ingredients
#'   )) %>%
#'   #Remove some duplicates (f.ex both storebought and homemade bread, or where the english and norwegian name is the same)
#'   select(-Foodgroup) %>% unique() %>%
#'   # Change some units
#'   dplyr::mutate(
#'     unit_enhet = case_when(
#'       Ingredients %in% c("waffle", "waffle sour cream") & unit_enhet == "pcs" ~ "heart",
#'       Ingredients %in% c("waffle", "waffle sour cream") & unit_enhet == "waffel" ~ "pcs", # Whole waffle
#'       TRUE ~ unit_enhet
#'     )
#'   )
#'
#' #Create one ID each for butter, margarine and ghee
#' unit_weights <- unit_weights %>%
#'   mutate(database_ID = case_when(
#'     Ingredients %in% c('smør', 'butter') ~ database_ID + 2000,
#'     Ingredients %in% c('margarin', 'margarine') ~ database_ID + 2001,
#'     Ingredients == 'ghee' ~ database_ID + 2002,
#'     TRUE ~ database_ID
#'   ))
#'
#' #Rename "g" column to be more understandable
#' unit_weights <- unit_weights %>%
#'   rename(grams_per_unit = g)
#'
#' #Remove some duplicates and add all reference rows for each food into one row
#' unit_weights <- unit_weights %>%
#'   filter(!(str_detect(Ingredients, 'basil') & database_ID == 28)) %>%
#'   rename(tmp = reference) %>%
#'   group_by(Ingredients, unit_enhet, grams_per_unit, database_ID, language) %>%
#'   summarise(reference = str_c(unique(tmp), collapse = ", ")) %>%
#'   ungroup()
#'
#' #Only keep dl for volume
#' tmp <- unit_weights %>%
#'   group_by(Ingredients) %>%
#'   #Find the ingredients with spoon units
#'   filter(any(unit_enhet %in% c("tbsp", "tsp", "ms", "ts", "ss"))) %>%
#'   #Remove those that have dl already
#'   filter(!any(unit_enhet == "dl")) %>%
#'   ungroup() %>%
#'   #Only keep spoon units
#'   filter(unit_enhet %in% c("ts", "tsp", "tbsp", "ms", "ss")) %>%
#'   #Turn tbsp and tsp units into dl
#'   mutate(
#'     grams_per_unit = case_when(
#'       unit_enhet %in% c("tsp", "ts") ~ grams_per_unit*20,
#'       unit_enhet %in% c("tbsp", "ss", "ms") ~ grams_per_unit*(6.67)),
#'     unit_enhet = "dl") %>%
#'   #Use the mean value
#'   group_by(Ingredients, unit_enhet, database_ID, language, reference) %>%
#'   summarise(tmp = mean(grams_per_unit)) %>% ungroup() %>%
#'   rename(grams_per_unit = tmp)
#'
#' #Add back to unit_weights and remove all spoon units
#' unit_weights <- full_join(unit_weights, tmp) %>%
#'   filter(!unit_enhet %in% c("ts", "tsp", "ss", "tbsp", "ms")) %>%
#'   dplyr::filter(language == 'english') %>%
#'   rename(unit = unit_enhet) %>%
#'   dplyr::mutate(unit = unit %>%
#'                   str_replace('neve', 'handful')) %>%
#'   #Use brutto weight when possible
#'   group_by(Ingredients) %>%
#'   dplyr::mutate(unit = case_when(
#'     any(unit == "brutto") ~ str_replace(unit, "brutto", "pcs"),
#'     TRUE ~ str_replace(unit, "netto", "pcs")
#'   )) %>% ungroup() %>%
#'   select(-c(language, reference))
#'
#' #Save
#' saveRDS(unit_weights, "./data-raw/unit_weights.Rds")

#Simplify way to add new food items----
#Read in already created db
old <- readRDS("./data-raw/unit_weights.Rds") %>%
  # Remove a wrong value
  dplyr::filter(!(Ingredients == "oil" & grams_per_unit == 66.7)) %>%
  # Rename som ingredients names
  dplyr::mutate(Ingredients = str_replace_all(Ingredients, c(
    "brussel sprouts" = "brussel sprout",
    "rolls, fine" = "rolls white",
    "onion, yellow\\/red" = "onion",
    "brown cheese" = "cheese brown",
    "sugar, white"= "sugar",
    "baguette, half-baked" = "baguette",
    "bakers yeast, compressed" = "yeast fresh",
    "bakers yeast, active, dry" = "yeast dry",
    "bean, black, canned" = "bean canned black",
    "bean, kidney, canned" = "bean canned kidney",
    "bean, white, large, canned" = "bean canned white",
    "bean, brown, dry" = "bean dry brown",
    "bean, kidney, dry" = "bean dry kidney",
    "bean, mung, dry" = "bean dry mung",
    "bean, soya, dry" = "bean dry soy",
    "bean, white, dry" = "bean dry white",
    "soybean curd, tofu" = "tofu",
    "wheat flour, cake flour" = "wheat flour cake",
    "wheat flour, wholemeal" = "wheat flour whole",
    "beetroot, pickled, canned" = "beetroot pickled",
    "butternut squash" = "winter squash butternut",
    "carambola, starfruit" = "starfruit",
    "chick pea" = "chickpea",
    "cod, traditional norwegian dish, lutefisk" = "lutefisk",
    "ginger root" = "ginger",
    "horse-radish" = "horseradish",
    "kiwi fruit" = "kiwi",
    "lingonberries, cowberries" = "lingonberry",
    "noodle, without egg" = "noodle",
    "noodle, without egg, cooked" = "noodle cooked",
    "persimmon, kaki fruit" = "persimmon",
    "pork chop, loin with bones" = "pork chop",
    "semolina wheat meal" = "semolina",
    "squash, zucchini" = "summer squash zucchini",
    "barley flour" = "wheat flour barley",
    "rowan-berries" = "rowan berry",
    "fruit coctail, canned" = "coctail fruit",
    'horse-radish' = 'horseradish',
    "pumkpin seed, squash seeds" = "seed pumpkin",
    "anchovy fillet, canned" = "anchovy fillet",
    "anchovy, canned" = "anchovy",
    "bamboo shoots, canned" = "bamboo shoots",
    "chicken, with skin" = "chicken whole",
    "chicken breast fillet" = "chicken breast",
    "chicken, thigh and drumstick, with skin" = "chicken thigh",
    "mushroom, button, canned" = "mushroom button",
    "noodle, without egg" = "noodle",
    "nori seaweed dried" = "nori seaweed",
    "onion yellow/red" = "onion",
    "salmon, smoked, dry salted" = "salmon smoked",
    'white or yellow hard to semi-hard cheese' = 'hard to semi-hard cheese',
    "whole-grain pasta" = "pasta whole grain",
    "black chokeberries" = "chokeberries black",
    'soft-ripened cheese \\(brie, camembert etc\\)' = 'soft ripened cheese',
    "taco spice mix" = "spice mix taco",
    "hamburger bun" = "hamburger bread",
    "sugar snap pea" = "pea sugar snap",
    'oatbran' = 'oat bran',
    'wheatbran' = 'wheat bran',
    "smoke-cured ham" = "ham smoked",
    " with skin" = "",
    "mushroom, common" = "mushroom",
    "grilled sweet pepper" = "sweet pepper grilled",
    "potato flatbread lompe" = "flatbread lompe",
    "cloudberries" = "cloudberr",
    "water chestnut, canned" = "chestnut water",
    "mackerel fillet, in tomato sauce, canned" = "mackerel tomato",
    "black pepper, grounded" = "pepper ground",
    "black pepper, whole" = "pepper whole",
    "olive, black, in oil, canned" = "olive black",
    "olive, green, pickled" = "olive green",
    "bean, white, in tomato sauce, canned" = "bean canned white tomato",
    "chocolate spread, nut spread" = "chocolate spread",
    "lamb chop with fat" = "lamb chop",
    "icing sugar" = "sugar confectioners",
    "," = ""
  )))

new <- tibble(
  temporary = c(

    #Ingredients with IDs already
    "nut spread;dl;100;",
    "chocolate spread;dl;100;",
    "gelatin;pcs;2;", #Same as leaf
    "sauce satay;dl;108.2;FoodData Central",
    "sauce ponzu;dl;111.59;FoodData Central",
    "sauce tomato;dl;103.55;FoodData Central",
    "sauce barbeque;dl;120.89;FoodData Central",
    "granola;dl;50;Same as muesli from Helsedir",
    "granola;portion;100;Same as muesli from Helsedir",
    "lemon balm fresh;dl_bunch_neve;20;Lemon balm is in the mint family, so same as mint",
    "vanilla pod;pcs;3;Oda online store",
    "sausage wiener;pcs;65;Gilde wiener sausages 8 per 520g",
    #"coconut mass;pcs;200;Meny",
    "chips;pcs;200;Meny",
    "tea;pcs;2;Standard portion bag",
    "bread crumb;slice;42.5", # Same as bread
    "bread crumb white;slice;42.5", # Same as bread
    "bread crumb;pcs;700", # Same as bread
    "bread crumb white;pcs;700", # Same as bread
    "cheese goat chevre white;pcs;150", #Soft ripened cheese
    "scampi;pcs;17", # Same as prawn
    "chicken breast;dl;57.14", # Same as chicken diced
    "bread brown chapati;pcs;50", # Same as chapati
    "corn cob;dl;105.7", #Corn kernels
    "lettuce;dl;25", #As iceberg
    "salad lettuce;dl;25",
    "shrimp boiled;dl;70",
    "shrimp brine;portion;150",
    "sweet pepper grilled;brutto;170",
    "sweet pepper grilled;netto;145",
    "salad lollo rosso;leaf;15",
    "heart salad;leaf;15",
    "chili pepper jalapeno pickled;brutto;15",
    "chili pepper jalapeno pickled;netto;14",
    "plantain;dl;75", #Banan
    "chunky salsa;dl;97.14", #Tomatsalsa
    "salsa;glass;240",
    "salad;pcs;150",

     #create english db entry
    "lefse tykk;pcs;87.5;Helsedir",
    "lefse vestland;pcs;35;Helsedir",
    "meatball;pcs;55;Helsedir",
    "meatball;portion;150;Helsedir",
    "lunch cake;pcs;100;Same as karbonade Helsedir",

    # new ingredients
    "soup cauliflower instant;pcs;65;Toro",
    "paper spring roll;pcs;12;Blue Dragon"
  )
) %>%
  separate(., col = temporary, into = c("Ingredients", "unit", "grams_per_unit", "reference"), sep = ";") %>%
  dplyr::select(-reference) %>%
  separate_rows(., unit, sep = "_") %>%

  # Add from latest recipe project
  bind_rows(.,
            tibble(
              #Packs
              Ingredients = c(
                "asparagus_bean_150",
                "almond_250", "basil_fresh_20",
                "blue_cheese_125", "bread_700",
                "blueberries_150", "cottage_cheese_250", "cream_cheese_125",
                "feta_cheese_200", "mozzarella_125", "ricotta_salata_250",
                "hard to semi-hard cheese_450",
                "chives_fresh_20", "mango_chutney_250",
                "tomato_ketchup_400", "coriander_fresh_20",
                "cream_300",
                "dill_fresh_20", "egg_noodle_250","kale_150", "lentil_canned_200",
                "parsley_fresh_20", "pea_frozen_150",
                "jam_marmalade_330", "mustard_400",
                #"olive_black_340",
                "pasta_400", "pine_nut_50",
                "pineapple_canned_565",
                "potato_mash_450", "potato_salad_300",
                "rice_125", #Legg til boil in bag i stadardise names
                "crispi_salad_150",
                "sour_cream_300", "tomato_canned_400", "thyme_fresh_20",
                "spinach_125", "sugar_500", "heart_salad_150",
                "salmon_smoked dry salted_100",
                "salad_150", "salt_150",
                "corn_kernel_380",
                "gingerbread_house_380", "cherries_compote_455",
                "nuts_mixed_260", "fish_ball_800", "brownie_mix_552", "chocolate_glaze mix_350",
                "coleslaw_250", "frozen_vegetable mix_500", "mediterranean_vegan salad_300",
                "onion_fried_100", "pancake_powder mix_196", "liver_pate_100",
                "pie_dough_275", "potato_chip_250", "candy filled_chocolate balls_100",
                "cheese_halloumi_200", "marmelade_blueberry_250", "milk chocolate_non-stop_180",
                "tart_shell_210", "nugget_plant-based_300", "pizza_base_370", "sauce_pizza red_400",
                "pizza_filling_50", "soup fish_1000", "soup fish_instant_37", "rocket_salad_75",
                "mediterranean_vegan salad_300", "jelly_instant powder_125", "soup_pea instant_146",
                "jelly_candy_350", "muffin_powder mix_330", "sauce_sandefjord_29", "sauce_tikka masala_360",
                "soup_spinach instant_79", "soup_thai instant_84", "soup tomato_instant_91",
                "toro_jegergryte_106", "carbonara_powder mix_29",
                "chickpea_canned_560", "salad_chicken_200", "ham_325", "chicken_ham_325",
                "greek moussaka_powder mix_136",
                "salt_stick_250",
                "mayonnaise_215", "potato_boat_600", "chocolate_cake mix_1100",
                "yeast_dry_15", "bearnaise_powder mix_29", "alfalfa_sprouts_100",
                "bali stew_powder mix_91", "remulade_160", "currant_150", "sausage_vossa_400",
                "salad_mix_175", "carrot_cake mix_396"
              )
            ) %>%
              #Create units of packs/pcs
              mutate(tmp = "pack_pcs") %>%
              separate(., tmp, into = c("unit1", "unit2"), sep = "_") %>%
              pivot_longer(., cols = c("unit1", "unit2"), names_to = "tmp", values_to = "unit") %>%
              select(-tmp) %>%
              #Add new items with pcs unit
              bind_rows(.,
                tibble(
                  #From Oda webstore
                  Ingredients = c(
                    "barbecue_marinade_100", "barbecue_rub_155",
                    "basil_fresh_20", "beetroot_pickled_580", "berries_250",
                    "pizza_base_370", "sauce_pizza red_400", "bread_rye_800",
                    "brown sauce_powder mix_44", "caviar_185", "cheese_blue_150",
                    "cheese_burrata mozzarella_125", "cream_cheese_125", "cheese_halloumi_200",
                    "chili_sin carne_370", "chives_fresh_20", "chocolate_cake mix_1100",
                    "chocolate mousse_powder mix_100", "chocolate_sauce_360", "coconut_flake_100",
                    "coconut_mass_250", "coleslaw_250", "coriander_fresh_20",
                    "crème_fraîche_300", "decorative glaze_125", "dill_fresh_20",
                    "egg_noodle_250", "fondant_250", "gel_top_100", "gingerbread_dough_500",
                    "greek moussaka_powder mix_136", "ham_325", "chicken_ham_325", "ham_cured_120",

                    "hollandaise_powder mix_26", "hummus_165",
                    "margarine_540", "butter_540", #Soft flora in the recipe, add to butter as margarine is mapped as butter
                    "gelatin_sheet_6", "vanilla_pod_3", "rosemary_fresh_20",
                    "taco_shell_13", #Olde el paso¨
                    "tomato_salsa_240", #The one mentioned in the Oda recipes
                    "sauce_pad thai_150", #Santa Maria in the recipe
                    "chili_sauce_781", #700 ml on the recipe site, using density from unit weight database
                    "pearl_barley_300",
                    "ginger_30", #One small piece
                    "pesto_190", "taco_sauce_230", "pork_belly_2000",
                    "pork_spare rib_800", "sauce_barbeque_540", "sauce_teriyaki_345", #Assume same density as soy sauce
                    "parmesan_cheese_200", "nacho_185", "porridge_rice_148",
                    "soup beta_instant_112", "sauce pizza_white_140", "sauce_white_140", "wok_mix_200",
                    "brown sauce_powder mix_44", "wine_750", "cheese_brown_500", "pork_shoulder_150",
                    "pork_tenderloin_600", "milk_1000", "juniper_berry_1", #Small number
                    "pak_choi_125", #Assume same as bok choi
                    "spice mix_raita_8", "spring roll_wrap_11", #Blue dragon spring rolls, 134g/12pcs
                    "stir fry_assorted vegetables_200",
                    "pollock_150", #Based on recipe site
                    "mexican stew_powder mix_284",
                    "sprinkles_chocolate_120",

                    "sauce_wok_161", #Chop suey brand used on recipe site, assume same density as soy sauce

                    "pizza_filling_50", "pavlova_powder mix_299",
                    "soup fish_1000", "soup fish_instant_37", "rocket_salad_75",
                    "mediterranean_vegan salad_300", "jelly_instant powder_125", "soup_pea instant_146",
                    "jelly_candy_350", "muffin_powder mix_330", "sauce_sandefjord_29", "sauce_tikka masala_360",
                    "soup_spinach instant_79", "soup_thai instant_84", "soup tomato_instant_91",
                    "toro_jegergryte_106", "carbonara_powder mix_29",
                    "sausage_chorizo_67.5", #Jacobs utvalgte
                    "bean_mixed_220",

                    "broccolini_200"
                  )) %>%
                  mutate(unit = "pcs")
              ) %>%
              # other units
              bind_rows(.,
                        tibble(

                          #From oda webstore recipe ingredient sugestions
                          tmp = c(
                            "lefse_200;pack", # Birger's beste lefser
                            "lefse_40;pcs",
                            "salad_175;portion", #Suggested ingredient size at Oda
                            "salad_mix_175;portion",
                            "plum_500;pack",
                            "marmelade_blueberry_125;dl", #Assume same as jam
                            "bean_canned_kidney_400;can",
                            "bean_canned_black_400;can",
                            "bean_canned_white_400;can",
                            "brussel_sprout_400;pack",
                            "chickpea canned_400;can",
                            "bean_edamame_80;dl", # Same as other beans
                            "coconut_flake_35;dl", #Assume same as per dl
                            "smoothie_mix_60;dl", #Assume same as strawberries
                            "smoothie_mix_400;pack",
                            "tzatziki_100;dl", #Assume same as yoghurt
                            "marshmallow_40.58;dl", #FoodData Central
                            "lemon_balm_20;dl", #Same as mint, lemon balm is in the mint family
                            "mint_fresh_1;stalk", #A stalk is the same as a twig
                            "rosemary_fresh_1;stalk",
                            "parsley_fresh_1;stalk",
                            #"tomato_salsa_101.44;dl", #FoodData Central
                            "sauce_tomato_350;pcs", #Oda
                            "sauce_tomato_350;box",
                            "vegetable_spread_113.39;dl", #Assume same a nut butter
                            "salt_128;dl",
                            #"tomato_sun dried_96.37;dl", #FoodData Central
                            "sausage_chorizo_78.2;dl", #FoodData Central
                            "nuts_mixed_55;dl", #Mean of all nuts in database
                            "onion_fried_23.67;dl", #FoodData central
                            "gel_top_100;dl", #Recipe says 100g
                            "jelly_candy_100;dl",
                            "currant_jelly_400;pcs",
                            "biscuit_gingerbread_11;pcs",
                            "biscuit_sourdough_11;pcs", #Same as cream crackers
                            "currant_jelly_115;dl", #Same as jam and marmelade
                            "roe_50;dl", #Size of container in recipe
                            "sausage_wiener_520;pack", #Gilde
                            "sausage_grill_600;pack", #Gilde
                            "bread_sausage_396;pack", #Prima sausage
                            "bread_sausage_33;pcs", #Prima sausage
                            "potato_flatbread lompe_290;pack",
                            "potato_flatbread lompe_26;pcs",
                            "pita bread_480;pack",
                            "pita bread_coarse_75;pcs",
                            "pita bread_white_75;pcs",
                            "salmon_fillet_500;pack",
                            "hamburger_bun_480;pack",
                            "tortilla_570;pack", #Fiks tortilla i findFoodInDatabase, ha hvete som standard
                            "pork_chop_1000;pack",
                            "naan_bread_260;pack",
                            "pork_tenderloin_200;portion",
                            "carrot_750;pack",
                            "scallion_150;pack",
                            "tomato_400;pack",
                            "radish_130;pack",
                            "celery_stalk_350;pack",
                            "bearnaise_sauce_150;box",
                            "sauce_herb_150;box",
                            "fish_ball_800;box",
                            "tomato_juice_100;dl",
                            "corn_cob_250;box",
                            "cocoa_mass_92.1;dl",
                            "hen_850;pcs", # Holte
                            "bulgur_150;box",
                            "yoghurt_skyr_125;pack",
                            "broccoli_460;pack",
                            "cucumber_pickled_580;pack",
                            "biscuit_children_175;pack",
                            "biscuit_children_3;pcs",
                            "biscuit_sweet_3;pcs",
                            "potato_salad_300;box",
                            "reindeer_1000;pcs", # without bone, Meny
                            "chestnut_water_227;box",
                            "cod_fillet_400;pack",
                            "cherry_tomato_250;pack",
                            "asparagus_250;pack",
                            "raspberries_125;pack",
                            "cracker_cream_400;pack",
                            "anchovy_200;pack",
                            "tart_50;pcs",
                            "rolls_white_560;pack",
                            "corn_baby_410;pack",
                            "soup_carrot_530;pcs",
                            "soup_cauliflower_530;pcs",
                            "soup_paprika_100;dl",
                            "soup_tomato_530;pcs",
                            #"sauce_taco_113.4;dl",
                            "sauce_wok_111.6;dl",
                            "sauce_hoisin_132.8;glass",
                            "sauce_hoisin_132.8;pcs",
                            "broccoli_400;bunch",
                            #"spice mix_taco_61;dl",
                            "spice mix_chicken_61;dl", #Same as taco spice
                            "spice mix_fish_61;dl",
                            "spice mix_mexican_61;dl",
                            "carrot_750;bunch",
                            "lemon_curd_100;dl", # Egg yolks and butter or cream
                            "lemongrass_50;box",
                            "lemongrass_5;twig", # Assume a bit more than basil
                            "tomato_sun dried_280;box", # Same as pcs
                            "tomato_sun dried_280;glass",
                            "tomato_sun dried_280;pack",
                            "apricot_dried_250;pack",
                            "bean_green_400;pack",
                            "biscuit_marie_200;pack",
                            "cashew_nut_250;pack",
                            "flatbread_hard_275;pack",
                            "pea sugar snap_250;pack",
                            "pistachio_nut_175;pack",
                            "walnut_175;pack",
                            "sausage_cumberland_400;pack",
                            "beef_minced meat_100;dl",
                            "beef_minced meat_200;portion",
                            "cod_breaded_125;pcs",
                            "parata_flat bread_50;pcs", #Tortilla
                            "bread_foccacia_380;pcs", #Meny
                            "sauce_butterchicken_100;dl",
                            "sauce_butterchicken_360;pcs",
                            "sauce_caramel_100;dl",
                            "icing_chocolate_113;dl",
                            "syrup_chocolate_140;dl",
                            "celariac_root_200;portion",
                            "chickpea_flour_38.9;dl",
                            "cheese_schnitzel_200;pcs",
                            "chicken_skewer_50;pcs",
                            "sauce_chicken_250;pcs",
                            "sauce_curry_108.2;dl",
                            "sauce_curry_350;glass",
                            "coriander_paste_100;dl",
                            "asparagus_white_250;bunch",
                            "cheese_spread_87.92;dl", # Mayo
                            "corn_baby_125;box",
                            "firkorn_40;dl",
                            "bean_canned_chili_400;box",
                            "chili_canned_38;dl",
                            "chili pepper_dried_15.6;dl",
                            "chili_glaze_111.6;dl", #Mostly chili sauce
                            "chili_marinade_98.7;dl", #Mainly vegetable oils
                            "onion_340;bunch", #Two large three small
                            "fish_stick_25;pcs",
                            "cucumber_pickled_500;glass",
                            "salsa_fruit_94.7;dl",
                            "salsa_fruit_240;box",
                            "chili_sauce_340;box",
                            "dragon_fruit_475;pcs", #Seeds del mundo
                            "dressing_garlic_100;dl",
                            "hamburger_plant-based_120;pcs", #Naturli' burger
                            "hamburger_lamb_130;pcs",
                            "hamburger_lamb_130;portion",
                            "pork_ham roast_1500;pcs",
                            "strawberries_400;box",
                            "hazelnut_60;dl", #Mål vekt og porsjonsstørrelser
                            "broccoli_salad_260;glass",
                            "sauce_vanilla_100;dl",
                            "sauce_strawberries_100;dl",
                            "chocolate_sauce_100;dl",
                            "salad_mediterranean_200;pcs",
                            "salad_potato_500;pcs",
                            "salad_shellfish_200;pcs",
                            "salad_chicken_200;pcs",
                            "bread_polar_37.5;pcs", #Findal og krogh as
                            "honey_crunch_50;dl", #Granola
                            "hummus_104;dl",
                            "hummus_150;box", # Used in the meny recipes
                            "ice cream_540;box", #Inspera 9dl
                            "ice cream_cake_540;pcs", # Daim 9dl ice cream
                            "lingonberry_jam_115;dl",
                            "lingonberry_jam_400;box", # Same as per pcs
                            "jam_marmalade_400;box",
                            "jam_marmalade_400;glass",
                            "lamb_leg roast_3000;pcs", #Range from 2.5-4.5kg Gilde
                            "lamb_shank_300;pcs", # Gilde sells in twopacks of 600g
                            "lamb_sausage_65;pcs", #Jacobs utvalgte
                            "lamb_chop_145;pcs",
                            "lamb_chop_1000;pack",
                            "lamb_rib_700;pcs", # Gourmetlam
                            #"lingonberry_65;dl", # common for berries
                            "lingonberry_40;box",
                            "mackerel_tomato_170;box",
                            "mackerel_80;box", # Pepper mackerel as in recipe
                            "mayonnaise_160;glass",
                            "mayonnaise_140;box",
                            "pork_minced meat_400;pcs",
                            "spice mix_meat_61;dl",
                            "milk_evaporated_400;box",
                            "milk chocolate_non-stop_50;dl",
                            "mozzarella_25;slice", #Google
                            "sauce_mushroom_250;box", # REMA / Stange
                            "noodle_rice_85;portion", # Pasta
                            "noodle_rice_85;pcs",
                            "halibut_600;pcs", #Godehav
                            "sour cream_300;box",
                            "falafel_8.3;pcs", # Sold in bags at Meny
                            "dressing_caesar_100;dl",
                            "tofu_300;pcs", #Oda
                            #"waffle_88;pcs", # Sold at Meny
                            "physalis_7;pcs", #Googe
                            "meringue_3;pcs",
                            "curry_paste_100;glass",
                            "ice_cube_50;dl",
                            "nøttesjoko_113.9;dl", # Same as nut butters
                            "nugatti_113.9;dl",
                            "nøttesjoko_400;glass",
                            "tomato_paste_140;box",
                            "tomato_paste_140;glass",
                            "tikka masala_paste_104;dl",
                            "sauce_tikka masala_360;glass",
                            "pineapple_200;box",
                            "pineapple_canned_200;box",
                            "sauce_hp_255;pcs",
                            "sauce_hollandaise_100;dl",
                            "currant_50;dl", # Like other berries
                            "coleslaw_100;dl",
                            "coleslaw_240;box",
                            "sauce_white_100;dl",
                            "sauce_white_280;box",
                            "couscous_400;box",
                            "cream_300;box",
                            "vanilla_sugar_175;box",
                            "ginger_100;pack",
                            "ginger_100;box",
                            "ginger_3;slice",
                            "herbs_20;dl",
                            "lemongrass_50;pack",
                            "lentil_dried_80;dl",
                            "nacho_50;dl",
                            "cottage_cheese_300;box",
                            "sausage_pork belly_125;pcs",
                            "pork_neck_220;slice", # same as portion size
                            "ham_325;box",
                            "chicken_ham_325;box",
                            "apple_17.5;slice", # One apple a 140g sliced into 8s
                            "chicken_with skin_100;dl",
                            "bean_mixed_380;box",
                            "sauce_white_45;portion", # Standardised sauce portion is three tablespoons
                            #"lunch_cake_255;pcs",
                            "mackerel_600;pcs",
                            "mackerel_filet_250;pcs",
                            "crayfish_150;pcs", # Meny
                            "shrimp_14.3;pcs", #70 shrimps per kilo
                            "ritz crackers_5;pcs",
                            "guacamole_94.7;dl",
                            "guacamole_150;portion", # Recipe uses portion to mean for the whole recipe
                            "raw vegetables_assorted_150;pcs", #Pack of råkost
                            "marzipan_50;pcs", # Recipes uses "Gullbrød" marzipan
                            "marzipan_50;dl", # Similar to non-stop, they used marzipan balls
                            "liquorice_150;box",
                            "liquorice_100;dl",
                            "bread_brioche_50;pcs", # Meny burger bread
                            "burger_bacon_140;pcs",
                            "bacon_140;pcs",
                            "black pepper grounded_42;glass",
                            "grape_fruit_300;pcs",
                            "spice mix_fajita_30;pcs",
                            "potato_boiled_73;pcs",
                            "lasagna_800;pcs",
                            "spice mix_pasta_65;pcs",
                            "spice mix_pasta_61;dl",
                            "sauce_pepper_100;dl",
                            "winter squash hokkaido_1500;pcs", #Hokkaido
                            "winter squash pumpkin_1500;pcs", #Hokkaido
                            "pre-cut vegetables mixed vegetables_200;pcs",
                            "sauce_pizza red_103.55;dl", # Same as tomato sauce
                            "sauce_pizza red_200;glass",
                            "coriander_fresh_0.3;leaf",
                            "coriander_fresh_3;twig",
                            "mint_fresh_0.3;leaf",
                            "jackfruit_400;box",
                            "salad_15;leaf", # Other salads
                            "crispi_salad_15;leaf",
                            "sour_cream_300;box",
                            "beans_400;box",
                            "bean_canned_white_400;box",
                            "bean_canned_white_400;box",
                            "bean_canned_black_400;box",
                            "olive_green_300;glass",
                            "olive_black_300;glass",
                            "shrimp_250;box",
                            "aioli_150;pack",
                            "capers canned_100;glass",
                            "grissini_5;pcs", # Google
                            "water_1500;pcs", # Large bottle of water
                            "water_carbonated_1000;pcs",
                            "berries_50;dl",
                            "cornflakes_50;dl",
                            "char_900;pcs", # Vulkanfisk
                            "potato_300;pack",
                            #"taco_sauce_103;dl",
                            "turnip_100;slice",
                            "salmon_spread_90;pcs",
                            "sausage_vegetarian_60;pcs",
                            "biscuit_amaretti_26;pcs",  #Google
                            "tomato_salsa_230;box",
                            "baguette_17.5;slice", #Assume they used one baguette and sliced it
                            "ham_12;slice", # Mål vekt og porsjonsstørrelser
                            "chicken_ham_12;slice",
                            "bread_white_67.5;dl", # Assume same as breadcrumbs
                            "bread_67.5;dl", # Assume same as breadcrumbs
                            "cream_vanilla_500;box",
                            "tomato_beef_15;slice", # A bit bigger than regular tomato
                            "tzatziki_180;box",
                            "cake_wreath_900;pcs",
                            "cake_yellow_370;pcs",
                            "sauce_red wine_250;box",
                            "prune_400;box",
                            "honey_350;glass",
                            "cheese_brown_40;dl",
                            "kavring_25;slice", #kavring
                            "kavring_25;pcs",
                            "beetroot_12;slice",
                            "beetroot_520;glass",
                            "cloves_47.3;dl", # Assume same as nutmeg
                            "anise_star_47.3;dl",
                            "bean_sprouts_425;box",
                            "bean_sprouts_425;glass",
                            "sauce_sweet and sour_100;dl",
                            "macaroon_12;pcs",
                            "relish_55;dl", # Same as pickled cucumbers
                            "relish_165;box", # Heinz is 3dl
                            "sauce_venison_100;dl",
                            "wakame_35;dl", # Assume similar to cabbage
                            "dates_dried_80;dl", # Same as figs
                            "apricot_80;dl",
                            "oregano_dried_10;glass",
                            "oregano_dried_1;twig",
                            "sage_dried_3;twig",
                            "sage_dried_0.3;leaf",
                            "sage_dried_20;pcs",
                            "tarragon_dried_1;twig",
                            "ham_smoked_280;pcs",
                            "sweet_pepper_295;glass",
                            "pickled_pepper_295;glass",
                            "psyllium_husk_40;dl", # Similar to oats
                            "cinnamon_4;pcs", #bar of cinnamon
                            "passion fruit_curd_100;dl",
                            "passion fruit_curd_340;glass", # Lemon curd in oda
                            "passion fruit_curd_340;box",
                            "beer_300;box",
                            "beer_300;pcs",
                            "reduction_100;dl",
                            "cider_apple_500;pcs",
                            "daim_58;pcs",
                            "food_coloring_100;dl",
                            "food_coloring_15;pcs",
                            "chocolate_50;pcs",
                            "roe_75;glass",
                            "dinner kit_taco_365;pcs",
                            "tomato_sun dried_101.4;handful",
                            "tomato_sun dried in oil_101.4;dl", #Same as regular sun dried
                            "soda_500;pcs",
                            "orange_18;slice", # A bit larger than lemon
                            "roast_beef_100;pcs",
                            "dairy imitate_oatmilk full fat_250;pack",
                            # Hard to semi hard cheeses
                            "cheese cheddar_14.33;slice",
                            "cheese norvegia_14.33;slice",
                            "cheese jarlsberg_14.33;slice",
                            "cheese romano_14.33;slice",
                            "cheese semi hard_14.33;slice",
                            "cheese goat_14.33;slice",
                            # Spice mix, taco as reference
                            "spice mix_28;pack",
                            "catfish_400;pack"
                          )) %>%
                          separate(tmp, into=c("Ingredients", "unit"), sep = ";")
              ) %>%
              unique() %>%
              separate(., "Ingredients", into = c("Ingredients", "grams_per_unit"), sep = "_(?=\\d+)") %>%
              select(Ingredients, unit, grams_per_unit)  %>%
              dplyr::mutate(Ingredients = str_replace_all(Ingredients, "_", " "))
            ) %>%

  #Add IDs
  left_join(., old %>% select(Ingredients, database_ID) %>% unique(),
            relationship = "many-to-many") %>%
  group_by(Ingredients) %>% fill(database_ID, .direction = "updown") %>%
  ungroup() %>%
  mutate(grams_per_unit = as.numeric(as.character(grams_per_unit)))

#Save both old and new
all <- bind_rows(old, new) %>%
  #Create new ID's
  group_by(Ingredients) %>%
  mutate(database_ID = case_when(
    is.na(database_ID) ~ cur_group_id() + 0.77,
    TRUE ~ database_ID
  )) %>% ungroup() %>%

  ## SStandardiser chickpea, er de tørre eller canned on intet annet er nevnt?
  # Change corn meal to polenta?
  # pear marmelade
## Substitutions----
dplyr::mutate(
  Ingredients = str_replace_all(Ingredients, c(
    # Substitutions
    "peanut butter" = paste0(c("peanut", "almond", "walnut", "cashew nut", "hazelnut"), " butter",
                             collapse = ";"),
    "baking powder" = "baking powder;baking soda",
    "beef minced meat" = "beef minced meat;chicken minced meat;lamb minced meat;pork minced meat",
    "corn starch" = "corn starch;corn flour",
    "onion powder" = "onion powder;garlic powder",
    "yeast dry" = "yeast dry;yeast nutritional",
    "herbs" = "herbs;italian seasoning",
    "pea frozen" = "pea frozen;pea green",
    "jam marmalade" = "jam;marmalade",

    # Simplify
    "potato flatbread lompe" = "lompe",
    "rib-eye steak fillet steak" = "beef rib-eye",
    "rib with loin" = "pork belly"
    ))
  ) %>%
  dplyr::mutate(Ingredients = case_when(
    Ingredients == "butter" ~ "butter;lard pork fat",
    Ingredients == "milk" ~ paste0(c("milk", "buttermilk", "dairy imitate almond milk",
                       "dairy imitate oatmilk", "dairy imitate rice milk",
                       "kefir"), collapse = ";"),
    TRUE ~ Ingredients
  )
  ) %>%
  separate_longer_delim(., Ingredients, delim = ";") %>%
  # Remove some items
  dplyr::filter(!Ingredients %in% c(
    "cocoa instant powder"
  )) %>%
  # Create new IDs again
  dplyr::mutate(
    .by = Ingredients,
    database_ID = cur_group_id()
  )

  # Only keep English, decide on brutto/netto weight for "per pcs"
  # dplyr::filter(language == 'english') %>%
  # rename(unit = unit_enhet) %>%
  # dplyr::mutate(unit = unit %>%
  #                 str_replace('neve', 'handful')) %>%
  # #Use brutto weight when possible
  # dplyr::mutate(.by = Ingredients,
  #               unit = case_when(
  #                 any(unit == "brutto") ~ str_replace(unit, "brutto", "pcs"),
  #                 TRUE ~ str_replace(unit, "netto", "pcs")
  #                 )
  #               ) %>%
  # dplyr::select(-language)

## Change order of words----
switch_word_order <- c(
  "asparagus bean", "alfalfa sprouts", "bean sprouts", "break bean",
  "black chokeberries", "boiled ham", "cured ham", "swedish sausage",
  "cocktail sausage", "lamb sausage", "meat sausage", "lemon curd",
  "passion fruit curd", "ground meat", "lingonberry jam",
  #"smoke-cured ham",
  paste0(c("vegetable", "truffle", "sunflower", "olive", "soy", "sesame",
           "salad", "rapeseed", "peanut", "hazelnut", "garlic", "corn",
           "coconut", "canola"), " oil"),
  paste0(c(
    "bearnaise", "brown", "béchamel", "fish", "cream", "gravy",
    "mint", "oyster", "soy", "taco", "worcestershire", "bbq",
    "chocolate", "apple"
  ), " sauce"),
  paste0(
    c("carrot", "chili", "curry", "garlic", "ginger", "shrimp", "tamarind",
      "tomato", "coriander", "tikka masala"), " paste"),
  paste0(
    c("caraway", "chia", "coriander", "fennel", "flax", "hemp", "mustard",
      "poppy", "pumpkin", "sesame", "sunflower"
    ), " seed"),
  paste0(
    c("pizza", "fish soup", "cream sauce", "bolognese", "hollandaise"), "base"),
  paste0(
    c("carrot", "chocolate", "gluten-free"), "cake mix"),
  paste0(c(
    "bearnaise", "carbonara", "muffin", "greek moussaka", "bali stew"
  ), " powder mix")
  )

# Do the word switching
all <- all %>%
  dplyr::mutate(Ingredients = case_when(
    Ingredients == "gluten-free cake mix" ~ "cake mix gluten-free",
    Ingredients %in% switch_word_order &
      str_count(Ingredients, "\\w+") == 3 & str_detect(Ingredients, "powder mix$|cake mix$") ~ str_replace_all(Ingredients, "([a-zæøå-]+) ([a-zæøå-]+ [a-zæøå-]+)", "\\2 \\1"),
    Ingredients %in% switch_word_order &
      str_count(Ingredients, "\\w+") == 4 & str_detect(Ingredients, "powder mix$|cake mix$") ~ str_replace_all(Ingredients, "(([a-zæøå-]+ [a-zæøå-]+)|([a-zæøå]+-[a-zæøå]+)) ([a-zæøå-]+ [a-zæøå-]+)", "\\2 \\1"),
    Ingredients %in% switch_word_order &
      str_count(Ingredients, "\\w+") == 3 & !str_detect(Ingredients, "powder mix$|cake mix$") ~ str_replace_all(Ingredients, "([a-zæøå-]+ [a-zæøå-]+) ([a-zæøå-]+)", "\\2 \\1"),
    Ingredients %in% switch_word_order &
      str_count(Ingredients, "\\w+") == 2 ~ str_replace_all(Ingredients, "(\\w+) (\\w+)", "\\2 \\1"),
    str_detect(Ingredients, "soup [a-zæøå-]+ instant") ~ str_replace_all(Ingredients, c(" instant" = "", "soup" = "soup instant")),
    TRUE ~ Ingredients)
    ) %>%
  distinct()

test <- all %>% dplyr::filter(str_count(Ingredients, "\\w+") >= 2)
test <- all %>%
  summarise(.by = c(Ingredients, unit), n = n()) %>%
  dplyr::filter(n >1)

# Create a mean of groups of ingredients to use when a specific item is not found
grouped_ingredients <- all %>%
  dplyr::mutate(
    Ingredients = case_when(
      str_detect(Ingredients,
                 paste0(c("bean canned", "bean dry", "winter squash",
                          "summer squash"),
                        collapse = "|")) ~ str_extract(Ingredients, "^\\w+ \\w+"),
      str_detect(Ingredients,
                 paste0(c("^sauce", "^yeast", "^cabbage", "^sausage",
                          "^biscuit"),
                        collapse = "|")) ~ str_extract(Ingredients, "^\\w+")
      )
    ) %>%
  dplyr::select(Ingredients) %>% distinct() %>%
  dplyr::mutate(number_of_words = str_count(Ingredients, "\\w+")) #%>%
  dplyr::filter(number_of_words != 1) %>%
  dplyr::filter(
    !str_detect(Ingredients, paste0("^", c(
      "bean canned", "bean dry"
    ), collapse = "|"))
  )
# rename to make names for generalible



saveRDS(all, "./data-raw/unit_weights2.Rds")

