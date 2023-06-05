#Nutrient content database----
#List to fill with various items to keep environment clean
various <- list()

#Load raw data from Matvaretabellen
raw_data <- read_xlsx(
  system.file("extdata", "matvaretabellen2020.xlsx", package = "sustainableNutRients")
)

#Clean it up and use means for items with more than one addition (such as vegetables with both Norwegian and Imported values)----
clean_nutrients <- raw_data %>%

  #Remove empty rows
  drop_na(Foodgroup) %>%
  #Remove ref columns
  select(!contains('ref')) %>%

  #Rename column and remove unnecessary columns
  rename(food_item = `Food Item`,
         database_ID = FoodID) %>%
  select(database_ID, Foodgroup, food_item) %>%

  #Remove unnecessary food items, first turn to lowercase----
  mutate(food_item = str_to_lower(food_item),
         Foodgroup = str_to_lower(Foodgroup)) %>%
  #Keep some ingredients
  filter((food_item %in% c(
    'chocolate bar, milk', 'chocolate, white', 'chocolate, cooking, plain, minimum 35 % cocoa',
    'chocolate, snickers', 'ice cream, dairy', 'chocolate, dark, 70 % cocoa')) |
           !str_detect(Foodgroup,
                       'dessert|other meat products, prepared|other meats, minced, offal, prepared|egg, prepared|cookies|cod liver oil|homemade|chocolate|instant|cake|breakfast cereals|porridge|pizza')) %>%

  #Rename some ingredients----
  mutate(Ingredients = case_when(

  #Beef----
  str_detect(food_item, 'beef') & str_detect(food_item, 'bottom round') ~ 'beef_bottom round',
  str_detect(food_item, 'beef') & str_detect(food_item, 'chuck roll') ~ 'beef_chuck roll',
  str_detect(food_item, 'beef') & str_detect(food_item, 'shoulder') & str_detect(food_item, 'roast') ~ 'beef_shoulder',
  str_detect(food_item, 'beef') & str_detect(food_item, 'brisket') ~ 'beef_brisket',
  str_detect(food_item, 'beef') & str_detect(food_item, 'tenderloin') ~ 'beef_tenderloin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'striploin') ~ 'beef_striploin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'sirloin') & str_detect(food_item, 'butt') ~ 'beef_sirloin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'rib-eye steak, raw') ~ 'beef_rib-eye steak',
  str_detect(food_item, 'beef') & str_detect(food_item, 'liver') ~ 'beef_liver/veal_liver',
  str_detect(food_item, 'beef') & str_detect(food_item, 'minced meat') & str_detect(food_item, '6 %') ~ 'beef minced meat_6',
  food_item == 'beef, minced meat, without salt and water, raw' ~ 'beef_minced meat',
  food_item == 'veal, for roast, raw' ~ 'beef_veal for roast',
  food_item == 'beef, roast of nuckle, raw' ~ 'beef_roast of knuckle',
  food_item == 'veal, chops, raw' ~ 'beef_veal chops',

  #lamb----
  food_item == 'lamb, for stewing, raw' ~ 'lamb_stew meat',
  food_item == 'lamb, breast and skirt, with bone, raw' ~ 'lamb_breast skirt',
  food_item == 'lamb, shoulder, for roast, raw' ~ 'lamb_shoulder',
  food_item == 'lamb, for mutton and cabbage stew (fårikål), raw' ~ 'lamb_cabbage stew meat',
  food_item == 'lamb, chops, with fat, raw' ~ 'lamb_chop',
  food_item == 'lamb, leg, for roast, raw' ~ 'lamb_leg roast',
  food_item == 'lamb, leg, cured, dried, smoked' ~ 'lamb_leg smoked',
  food_item == 'lamb, rib, cured, dried, smoked, raw' ~ 'lamb_cured rib',
  food_item == 'lamb, chops, cutlet, hind saddle, lean, fat trimmed, raw' ~ 'lamb_hind saddle',

  #Pork----
  food_item == 'pork, belly, with rind, raw' ~ 'pork_belly',
  food_item == 'pork, chops, loin with bones, raw' ~ 'pork_chop',
  food_item == 'pork, ham, boneless, with fat, for roast, without rind, raw' ~ 'pork_ham roast',
  food_item == 'pork, rib with loin, raw' ~ 'pork_rib roast',
  food_item == 'pork, hocks, raw' ~ 'pork_hock',
  food_item == 'pork, inside round, raw' ~ 'pork_inside round',
  food_item == 'pork, neck chops, raw' ~ 'pork_neck chop',
  food_item == 'liver, pork, raw' ~ 'pork_liver',
  food_item == 'pork, minced meat, max 23 % fat, raw' ~ 'pork_minced meat',
  food_item == 'pork, bacon, with rind, raw' ~ 'bacon',
  food_item == 'pork, shoulder, with fat, for roast, raw' ~ 'pork_shoulder',
  food_item == 'pork, grillbones, spare ribs, raw' ~ 'pork_spare rib',
  food_item == 'pork, tenderloin, raw' ~ 'pork_tenderloin',
  food_item == 'pork, trimmed fat, raw' ~ 'pork_lard',
  food_item == 'ham, cured' ~ 'ham_cured',
  food_item == 'ham, smoke-cured' ~ 'ham_smoked',
  food_item == 'ham, boiled' ~ 'ham',
  food_item == 'sausage, salami' ~ 'salami',
  food_item == 'sausage, chorizo' ~ 'sausage_chorizo',
  food_item == 'sausage, swedish, falukorv' ~ 'sausage_vossa', #Similar in nutrients
  food_item == 'sausage, meat, gilde' ~ 'sausage', #Standard

  #Poultry----
  food_item == 'chicken, leg (thigh and drumstick), with skin, raw' ~ 'chicken_thigh',
  food_item == 'chicken, with skin, raw' ~ 'chicken_whole',
  food_item == 'chicken, fillet, without skin, raw' ~ 'chicken_breast',
  food_item == 'chicken, drumstick, with skin, raw' ~ 'chicken_drumstick',
  food_item == 'chicken, minced meat, raw' ~ 'chicken_minced meat',
  food_item == 'egg white' ~ 'egg_white',
  food_item == 'egg yolk' ~ 'egg_yolk',
  food_item == 'sausage, grill, turkey and chicken' ~ 'sausage_turkey chicken',
  food_item == 'duck, meat and skin, raw' ~ 'duck',
  food_item == 'hen, fillet, raw' ~ 'hen_fillet',
  food_item == 'turkey, breast, without skin, raw' ~ 'turkey_breast',
  food_item == 'ham, turkey, smoked' ~ 'turkey_ham',
  food_item == 'turkey, meat and skin, raw' ~ 'turkey_meat',
  food_item == 'grouse, breast, without skin, raw' ~ 'grouse_breast',

  #Game meat----
  food_item == 'moose, roasting, raw' ~ 'elk moose',
  food_item == 'reindeer, roasting, raw' ~ 'reindeer',
  food_item == 'roe deer, meat, raw' ~ 'roe deer',

  #Seafood----
  food_item == 'anchovies, canned' ~ 'anchovy_canned',
  food_item == 'anchovy fillets, canned' ~ 'anchovy_fillet',
  food_item == 'angler fish, raw' ~ 'anglerfish',
  food_item == 'alaska pollock, raw' ~ 'pollock',
  food_item == 'halibut, atlantic, unspecified, raw' ~ 'halibut',
  food_item == 'herring, summer, raw' ~ 'herring',
  food_item == 'king prawns, raw' ~ 'prawn',
  food_item == 'caviar, capelin roe' ~ 'caviar',
  food_item == 'crab, boiled' ~ 'crab',
  food_item == 'cod, split, salted and dried' ~ 'cod_clipfish',
  food_item == 'stockfish, dried fish' ~ 'cod_dried',
  food_item == 'fish, alkaline cured, dried, lutefisk' ~ 'lutefisk',
  food_item == 'crab, boiled' ~ 'crab',
  food_item == 'salmon fillet, dry salted, with sugar and spices' ~ 'trout_cured', #Similar nutritional profile
  food_item == 'lobster, boiled' ~ 'lobster',
  food_item == 'mackerel, may-june, raw' ~ 'mackerel',
  food_item == 'mussel, blue, raw' ~ 'mussel',
  food_item == 'nori, seaweed, dried' ~ 'nori_seaweed',
  food_item == 'salmon, farmed, raw' ~ 'salmon',
  food_item == 'salmon, smoked' ~ 'salmon_smoked',
  food_item == 'sprat, raw' ~ 'sardine',
  food_item == 'scallion, spring onion, raw' ~ 'scallion',
  food_item == 'oyster, common, raw' ~ 'oyster',
  food_item == 'shrimps, in brine, drained' ~ 'shrimp_in brine',
  food_item == 'shrimps, northern, boiled' ~ 'shrimp',
  food_item == 'trout, farmed, raw' ~ 'trout',
  food_item == 'tuna in water, drained, canned' ~ 'tuna_in water canned',
  food_item == 'tuna, in oil, canned' ~ 'tuna_in oil canned',
  food_item == 'halibut, atlantic' ~ 'halibut',
  food_item == 'mackerel fillet, in tomato sauce, 60 % mackerel, canned' ~ 'mackerel_tomato canned',
  food_item == 'cusk, tusk, raw' ~ 'cusk_tusk',
  food_item == "dover sole, raw" ~ 'sole_dover',

  #Herbs and spices----
  food_item == 'anise seeds' ~ 'anise',
  food_item == 'bay leaf, dried' ~ 'bay_leaf',
  food_item == 'pepper, cayenne, red' ~ 'pepper_cayenne',
  food_item == 'cardamom, ground' ~ 'cardamom',
  food_item == 'cinnamon, ground' ~ 'cinnamon',
  food_item == 'ginger, root, raw' ~ 'ginger',
  food_item == 'ginger, ground' ~ 'ginger_dried',
  food_item == 'water-cress, raw' ~ 'cress',
  food_item == 'nutmeg, ground' ~ 'nutmeg',
  food_item == 'sweet pepper, paprika, powder' ~ 'paprika_powder',
  food_item == 'parsley, herb, norwegian, raw' ~ 'parsley_fresh',
  food_item %in% c('basil, dried', 'parsley, dried', 'rosemary, dried', 'tarragon, dried',
                   'thyme, dried', 'pepper, white', 'pepper, black', 'basil, dried', 'basil, fresh',
                   'oregano, dried') ~ str_replace(food_item, ', ', '_'),
  food_item %in% c('coriander, raw', 'dill, raw', 'rosemary, raw', 'thyme, raw') ~ str_replace(food_item, ', raw', '_fresh'),
  food_item == 'saffron, dried' ~ 'saffron',
  food_item == 'turmeric, ground' ~ 'turmeric',
  food_item %in% c('coriander seeds', 'fennel seeds', 'mustard seeds', 'caraway seeds') ~ str_replace(food_item, ' seeds', '_seed'),
  food_item == 'chili powder' ~ 'chili_powder',
  food_item == 'curry powder' ~ 'curry_powder',

  #Fruit and vegetables----
  database_ID == '06.114' ~ 'sprout_alfalfa',
  food_item == 'apricots, dried' ~ 'apricot_dried',
  food_item == 'beans, green, french, raw' ~ 'bean_green',
  food_item == 'beetroot, norwegian, raw' ~ 'beetroot',
  food_item == 'blackcurrants, raw' ~ 'currant_black',
  food_item == 'brussel sprouts, norwegian, raw' ~ 'brussel_sprout',

  food_item == 'cabbage, white, raw' ~ 'cabbage', #Standard
  food_item == 'cabbage, pak-choi, bok choy, raw' ~ 'cabbage_bok choi',
  food_item == 'cabbage, red, raw' ~ 'cabbage_red',
  food_item == 'cabbage, chinese, norwegian, raw' ~ 'cabbage_chinese',

  food_item == 'cauliflower, norwegian, raw' ~ 'cauliflower',
  food_item == 'celariac root, norwegian, raw' ~ 'celariac_root',
  food_item == 'celery stalk or stem, norwegian, raw' ~ 'celery_stalk',
  food_item == 'chicory, raddichio, raw' ~ 'chicory_red',
  food_item == 'chicory, endive, raw' ~ 'chicory_white',

  food_item == 'dates, dried' ~ 'dates_dried',

  food_item == 'pepper, chili, green, raw' ~ 'chili pepper_green',
  food_item == 'pepper, chili, red, raw' ~ 'chili pepper_red',
  food_item == 'jalapeño, raw' ~ 'chili pepper_jalapeno',

  food_item == 'aubergine, raw' ~ 'eggplant',

  food_item == 'horse-radish, raw' ~ 'horseradish',

  food_item == 'kiwi fruit, raw' ~ 'kiwi',

  food_item == 'kohlrabi, raw' ~ 'swede',

  food_item == 'baby corn, canned' ~ 'corn_baby',
  food_item == 'sweet corn, norwegian, raw' ~ 'corn_cob',
  food_item == 'cucumber, norwegian, raw' ~ 'cucumber',
  food_item == 'cucumber, pickled' ~ 'cucumber_pickled',

  food_item == 'figs, dried' ~ 'fig_dried',

  food_item == 'tomato purée' ~ 'tomato_puree',
  food_item == 'tomato, small, cherry, imported, raw' ~ 'cherry_tomato',
  food_item == 'beans, white, in tomato sauce, canned' ~ 'bean_tomato', #White beans
  food_item == 'tomatoes, sun-dried' ~ 'tomato_sun dried',
  food_item == 'tomato, canned' ~ 'tomato_canned',

  food_item == 'leek, norwegian, raw' ~ 'leek',
  food_item == 'lemon juice, bottled' ~ 'lemon_juice',
  food_item == 'lemon peel' ~ 'lemon_zest',
  food_item == 'leaf beet, mangold, raw' ~ 'mangold',
  food_item == 'olives, black, in oil, canned' ~ 'olive_black',
  food_item == 'olives, green, pickled' ~ 'olive_green', #Standard
  food_item == 'onion, norwegian, raw' ~ 'onion',
  food_item == 'orange peel, raw' ~ 'orange_zest',
  food_item == 'parsley root, norwegian, raw' ~ 'parsley_root',
  food_item == 'peas, sugar-snap, norwegian, raw' ~ 'pea_sugar snap',
  food_item == 'peas, frozen' ~ 'pea',
  food_item == 'pineapple, canned, in natural juice' ~ 'pineapple_canned',
  food_item == 'potato flatbread, soft, lompe' ~ 'potato flatbread lompe',
  food_item == 'potatoes, storage, raw' ~ 'potato',
  food_item == 'prunes' ~ 'prune',
  food_item == 'radish, norwegian, raw' ~ 'radish',
  food_item == 'raisins' ~ 'raisin',
  food_item == 'salad, rocket, raw' ~ 'salad_rocket',
  food_item == 'romaine lettuce' ~ 'salad_romaine',
  food_item == 'lettuce leaves, norwegian, raw' ~ 'salad_lettuce',
  food_item == 'squash, zucchini, raw' ~ 'summer squash_zucchini',
  food_item == 'sweet pepper, red, raw' ~ 'sweet pepper_red',
  food_item == 'sweet pepper, yellow/orange, raw' ~ 'sweet pepper_yellow',
  food_item == 'sweet pepper, green, raw' ~ 'sweet pepper_green',
  food_item == 'turnip, norwegian, raw' ~ 'turnip',
  food_item == 'water chestnut, raw' ~ 'chestnut_water',
  food_item == 'melon, water, raw' ~ 'watermelon',
  food_item == 'pumpkin, raw' ~ 'winter squash_pumpkin',
  food_item %in% c('apple juice', 'pineapple juice', 'cranberry juice', 'grape juice',
                   'grapefruit juice', 'orange juice', 'tomato juice', 'tomato ketchup') ~ str_replace(food_item, ' ', '_'),
  food_item == 'sweet corn, canned' ~ 'sweet corn_canned',
  food_item == 'grapes, unspecified, raw' ~ 'grape',
  food_item == 'peaches, canned, in syrup' ~ 'peach_canned',

  food_item == 'melon, cantaloupe, raw' ~ 'melon_cantaloupe',
  food_item == 'melon, honeydew, raw' ~ 'melon_honeydew',
  food_item == 'melon, galia, raw' ~ 'melon_galia',

  #Grains----
  food_item == 'almonds' ~ 'almond',
  food_item == 'pearled barley' ~ 'pearl barley',
  food_item == 'broad beans, uncooked' ~ 'bean_broad',
  food_item == 'beans, black, canned'  ~ 'bean_black canned',
  food_item == 'beans, red (kidney), canned' ~ 'bean_kidney canned',
  food_item == 'beans, red (kidney), uncooked' ~ 'bean_kidney',
  food_item == 'mung beans, sprouted, raw' ~ 'bean_sprout', #Standard
  food_item == 'beans, white, large, canned' ~ 'bean_white canned',
  food_item == 'beans, white, uncooked' ~ 'bean_white',
  food_item == 'beans, soya, uncooked' ~ 'bean_soya',
  food_item == 'flatbread, hard' ~ 'bread flat hard',
  food_item == 'bulgur, uncooked' ~ 'bulgur_wheat',
  food_item == 'cashew nuts, salted' ~ 'cashew nut salt',
  food_item == 'peanuts, raw' ~ 'peanut', #These seem to be shelled
  food_item == 'peanuts, roasted, salted' ~ 'peanut_salt',
  food_item == 'peas, chick peas, uncooked' ~ 'chick pea',
  food_item == 'peas, chick peas, canned' ~ 'chick pea_canned',
  food_item == 'corn starch' ~ 'corn_starch',
  food_item == 'cornmeal, polenta' ~ 'corn flour_polenta',
  food_item == 'couscous, uncooked' ~ 'couscous',
  food_item == 'cracker, cream cracker' ~ 'cracker_cream',
  food_item == 'crisp bread, wholemeal flour, rye, husman' ~ 'crisp bread_coarse',
  food_item == 'noodles, with egg, uncooked' ~ 'noodle_egg',
  food_item == 'pasta, plain, macaroni, spaghetti etc., uncooked' ~ 'pasta',
  food_item == 'pasta, whole-grain, uncooked' ~ 'pasta_whole grain',
  food_item == 'rice, jasmin, uncooked' ~ 'rice_jasmin',
  food_item == 'lentils, green and brown, uncooked' ~ 'lentil_green',
  food_item == 'lentils, red/pink, uncooked' ~ 'lentil_red',
  food_item == 'lentils, green, canned' ~ 'lentil_canned',
  food_item == 'squash seeds, pumpkin seeds' ~ 'pumpkin_seed',
  food_item == 'quinoa, white, uncooked' ~ 'quinoa',
  food_item == 'noodles, rice, uncooked' ~ 'noodle_rice',
  food_item == 'rice, basmati, uncooked' ~ 'rice_basmati',
  food_item == 'rice, brown, long-grain, uncooked' ~ 'rice brown long grain',
  food_item == 'rice, arborio, risotto rice, uncooked' ~ 'rice_risotto',
  food_item == 'rice, white, long-grain, uncooked' ~ 'rice white long grain',
  food_item == 'rice, white, pre-boiled, uncooked' ~ 'rice parboiled',
  food_item == 'rolls, white, industrially made' ~ 'rolls white',
  food_item == 'sesame seeds, without shell' ~ 'sesame_seed',
  food_item == 'tortilla chips' ~ 'nacho',
  food_item == 'wheat flour, 80 % extraction' ~ 'wheat flour',
  food_item == 'wheat flour, wholemeal' ~ 'wheat flour_wholemeal',
  food_item == 'rye flour' ~ 'wheat flour_rye',
  food_item == 'rye flour, wholemeal' ~ 'wheat flour rye_wholemeal',
  food_item == 'semolina, wheat meal' ~ 'wheat flour_semolina',
  food_item %in% c('hamburger bun', 'peanut butter', 'potato starch') ~ str_replace(food_item, ' ', '_'),
  food_item == 'oatmeal' ~ 'oatmeal',
  food_item == 'rolled oats' ~ 'oat_rolled',
  food_item == 'sunflower seeds' ~ 'sunflower_seed',
  food_item == 'linseeds, flax seeds, crushed' ~ 'flax_seed',
  food_item == 'pecan nuts' ~ 'pecan_nut',
  food_item == 'brazil nuts' ~ 'brazil_nut',
  food_item == 'hazelnuts' ~ 'hazel_nut',
  food_item == 'wheatbran' ~ 'wheat bran',

  str_detect(food_item, 'bread, semi-coarse') & str_detect(food_item, '25-50') & str_detect(food_item, 'industrially made') ~ 'bread',
  str_detect(food_item, 'bread, white') & str_detect(food_item, '0-25') & str_detect(food_item, 'industrially made') & !str_detect(food_item, 'spiral|square') ~ 'bread_white',
  str_detect(food_item, 'bread, coarse') & str_detect(food_item, '50-75') & str_detect(food_item, 'industrially made') ~ 'bread_coarse',
  food_item == 'rolls, white, industrially made' ~ 'roll_white',
  food_item == 'tortilla, wheat flour' ~ 'tortilla',
  food_item == 'pizza crust, no filling' ~ 'pizza_crust',

  #Oils----
  food_item == 'oil, peanut' ~ 'peanut_oil',
  food_item == 'mayonnaise, full fat, 80 % fat' ~ 'mayonnaise',
  food_item == 'oil, olive, extra virgin' ~ 'olive_oil',
  food_item == 'oil, rapeseed' ~ 'rapeseed_oil',
  food_item == 'oil, sesame' ~ 'sesame_oil',
  food_item == 'oil, soy' ~ 'soybean_oil',
  food_item == 'oil, sunflower' ~ 'sunflower_oil',
  food_item == 'oil, rapeseed, cold pressed, odelia' ~ 'vegetable_oil', #Standard
  food_item == 'oil, walnut' ~ 'walnut_oil',

  #Dairy and substitutes----
  food_item == 'butter' ~ 'butter',
  food_item == 'butter, unsalted' ~ 'butter_unsalted',
  food_item == 'cheese, blue mold, norzola' ~ 'norzola_blue cheese',
  food_item == 'cheese, blue mold, normanna' ~ 'normanna_blue cheese',
  food_item == 'cheese, blue mold, gorgonzola' ~ 'gorgonzola_blue cheese',
  food_item == 'cheese, blue mold, roquefort' ~ 'roquefort_blue cheese',
  food_item == 'cheese, ripened, brie' ~ 'brie',
  food_item == 'cheese, whey, goat milk' ~ 'goat cheese brown',
  food_item == 'cheese, ripened, camembert' ~ 'camembert',
  food_item == 'cheese, hard, cheddar' ~ 'cheddar',
  food_item == 'cottage cheese' ~ 'cottage cheese',
  food_item == 'cream cheese, plain' ~ 'cream cheese',
  food_item == 'cheese, goat milk, feta' ~ 'feta_cheese',
  food_item == 'goat cheese, chevre, naturell' ~ 'chevre',
  food_item == 'goat cheese, hard, white, balsfjord' ~ 'hard goat cheese_balsfjord',
  food_item == 'goat cheese, hard, white, kvitlin' ~ 'hard goat cheese_kvitlin',
  food_item == 'cream cheese, goat milk, snøfrisk' ~ 'snøfrisk_goat cream cheese',
  food_item == 'halloumi, cheese' ~ 'halloumi_cheese',
  food_item == 'cheese, hard, jarlsberg' ~ 'jarlsberg',
  food_item == 'cheese, mascarpone' ~ 'mascarpone',
  food_item == 'cheese, mozarella' ~ 'mozzarella',
  food_item == 'cheese, hard, norvegia' ~ 'norvegia',
  food_item == 'cheese, hard, parmesan' ~ 'parmesan',
  food_item == 'cheese, semihard, port salut' ~ 'port salut',
  food_item == 'cheese, ricotta' ~ 'ricotta salata',
  food_item == 'cheese, hard, sveitser' ~ 'swiss_cheese',
  food_item == 'cheese, white, unspecified' ~ 'semi-hard to hard cheese',
  food_item == 'cream, household, 18 % fat' ~ 'cream household_18',
  food_item == 'cream, whipping, 37 % fat' ~ 'cream whipped_37',
  food_item == 'cream, sour, 35 % fat, crème fraîche' ~ 'sour cream_35/crème fraîche_35',
  food_item == 'cream, sour, low-fat, 18 % fat' ~ 'sour cream_18/crème fraîche_18',
  food_item == 'cream, sour, extra low-fat, 10 % fat' ~ 'sour cream_10/crème fraîche_10',
  food_item == 'cheese, whey, unspecified' ~ 'cheese_brown',
  food_item == 'milk, cultured, whole, kefir' ~ 'kefir',
  food_item == 'margarine, hard' ~ 'margarine',
  food_item == 'milk, skimmed, tine' ~ 'milk_0.1',
  food_item == 'milk, semi-skimmed, unspecified' ~ 'milk_1',
  food_item == 'milk, whole, unspecified' ~ 'whole milk_3.5',
  food_item == 'coconut milk, canned' ~ 'milk_coconut',
  food_item == 'milk beverage, with chocolate flavour, litago' ~ 'milk beverage_chocolate',
  food_item == 'quark, 1 % fat' ~ 'quark_1',
  food_item == 'quark, 7 % fat' ~ 'quark_7',
  food_item == 'milk, cultured, skimmed, skummet kulturmelk' ~ 'buttermilk', #Similar in nutrient content
  food_item == 'yoghurt, whole milk, plain' ~ 'yoghurt plain',
  food_item == 'milk, condensed, sweetened' ~ 'milk evaporated',
  food_item == 'soy beverage, unsweetened' ~ 'milk_soy',
  food_item == 'oat beverage, with calcium and vitamins' ~ 'dairy imitate_oatmilk',
  food_item == 'ice cream, dairy' ~ 'ice cream',
  food_item == 'cultured milk, with flavour, skyr' ~ 'skyr_flavored',
  food_item == 'cultured milk, plain, skyr' ~ 'skyr',


  #Mushrooms----
  food_item == 'mushroom, chantherelle, raw' ~ 'mushroom_chanterelle',
  food_item == 'mushroom, common, norwegian, raw' ~ 'mushroom',
  food_item == 'mushroom, oyster, raw' ~ 'mushroom_oyster',
  food_item == 'mushroom, portabello' ~ 'mushroom_portebello',
  food_item == 'mushroom, shiitake, raw' ~ 'mushroom_shiitake',
  food_item == 'mushroom, common, canned, drained' ~ 'mushroom_canned',

  #Div----
  food_item == 'beer, dark, 4,5 - 4,7 vol-% alcohol, bayer and christmas beers' ~ 'beer_dark',
  food_item == 'capers, canned' ~ 'caper',
  food_item == 'spirits, 40 vol-% alcohol' ~ 'spirits 40 vol-% alcohol',
  food_item == 'cider, sweet, 4,5 vol-% alcohol' ~ 'cider',
  food_item == 'sugar, brown' ~ 'sugar_brown',
  food_item == 'sugar, white, caster sugar, cube sugar' ~ 'sugar',
  food_item == 'fortified wines, sweet vermouth, 15 vol-% alcohol' ~ 'fortified wine 15 vol-% alcohol',
  food_item == 'fortified wines, sweet, port, 20 vol-% alcohol' ~ 'fortified wine 20 vol-% alcohol',
  food_item == 'sesame paste, tahini' ~ 'tahini',
  food_item == 'vinegar, 7 %' ~ 'vinegar',
  food_item == 'wasabi, root, raw' ~ 'wasabi',
  food_item == 'water, tap' ~ 'water',
  food_item == 'bakers yeast, active, dry' ~ 'yeast_dry',
  food_item == 'bakers yeast, compressed' ~ 'yeast',
  food_item == 'wine, red, unspecified' ~ 'wine_red',
  food_item == 'wine, white, rosé, sparkling, unspecified' ~ 'wine_white',
  food_item == 'bouillon powder' ~ 'broth_cube',
  food_item == 'capers, canned' ~ 'caper',
  food_item == 'cocoa powder' ~ 'cocoa_powder',
  food_item == 'honey' ~ 'honey',
  food_item == 'salt, table' ~ 'salt',
  food_item == 'soy sauce' ~ 'soy_sauce',
  food_item == 'tofu, soy bean curd' ~ 'tofu',
  food_item == 'jam, 45 % berries, 25 % sugar' ~ 'jam',
  food_item == 'chocolate bar, milk' ~ 'chocolate_milk',
  food_item == 'chocolate, white' ~ 'chocolate_white',
  food_item == 'chocolate, cooking, plain, minimum 35 % cocoa' ~ 'chocolate_semi-sweet',
  food_item == 'chocolate, snickers' ~ 'chocolate_candy bar',
  food_item == 'chocolate, dark, 70 % cocoa' ~ 'chocolate_dark', #Not really, but darkest they have
  food_item == 'popcorn, air popped, industrially made' ~ 'popcorn',
  food_item == 'gelatin' ~ 'gelatin',
  food_item == 'cloudberries, raw' ~ 'cloud_berr',

  #Keep the unspecified ingredients
  str_detect(food_item, ', unspecified, raw') ~ str_replace(food_item, ', unspecified, raw', ''),

  #Remove 'raw' from certain ingredients----
  food_item %in% c('pineapple, raw', 'asparagus, raw', 'avocado, raw', 'banana, raw',
                   'blueberries, raw', 'catfish, raw', 'char, raw',
                   'cherries, raw', 'chives, raw', 'clementine, raw',
                   'coconut, raw', 'cranberries, raw', 'peanuts, raw',
                   'egg, raw', 'fennel, raw', 'garlic, raw', 'grapefruit, raw', 'haddock, raw',
                   'hare, raw', 'kale, raw', 'jerusalem artichoke, raw', 'cranberries, raw',
                   'egg, raw', 'lemon, raw', 'lime, raw', 'lychee, raw', 'orange, raw',
                   'parsnip, raw', 'pomegranate, raw', 'sweet potato, raw', 'rabbit, raw',
                   'raspberries, raw', 'redfish, raw', 'rhubarb, raw', 'scallop, raw',
                   'shallot, raw', 'strawberries, raw', 'spinach, raw', 'squid, raw',
                   'tuna, raw') ~ str_replace(food_item, ', raw', ''),

  #Some single food items to keep
  food_item %in% c('ghee') ~ food_item

)) %>%

  mutate(Ingredients = case_when(
    #Turn seeds and nuts and some fruits into singular form
    str_detect(food_item, 'nuts') & !str_detect(Ingredients, 'pecan|brazil|hazel') ~ str_replace(food_item, 'nuts', 'nut'),
    #str_detect(food_item, 'seeds') ~ str_replace(food_item, 'seeds', 'seed'),

    #Some other fruits and vegetable
    Foodgroup %in% c('vegetables, raw and frozen', 'fruit and berries, raw/fresh') & str_detect(food_item, ', raw') & is.na(Ingredients) ~ str_replace(food_item, ', raw', ''),
    Foodgroup %in% c('vegetables, raw and frozen', 'fruit and berries, raw/fresh') & str_detect(food_item, ', ') & is.na(Ingredients) ~ str_replace(food_item, ', ', '_'),

    TRUE ~ Ingredients
  )) %>%

  mutate(Ingredients = Ingredients %>%
           str_replace('alfalfa seed, sprouted, raw', 'alfalfa_sprout') %>%
           str_replace('plums', 'plum') %>%
           str_replace('apricots', 'apricot') %>%
           str_replace('peanut, roasted, salted', 'peanut_salt') %>%
           str_replace('peanut, raw', 'peanut')) %>%
  #Separate rows with multiple food items
  separate_rows(., Ingredients, sep = '/') %>%

  #Remove some resulting errors
  filter(!(food_item %in% c('sweet pepper, yellow/orange, raw') & Ingredients %in% c('orange'))) %>%

  mutate(database_ID = as.numeric(database_ID))

#Add food items from FoodData Central----
#FDC uses different files for food item names, nutrient content and nutrient names
#Nutrients
fromFoodDataCentral_nutrients <- read_csv(
  system.file("extdata", "food_nutrient_FoodDataCentral_april2021.csv", package = "sustainableNutRients"))
fromFoodDataCentral_nutrient_names <- read_csv(
  system.file("extdata", "nutrient_incoming_name_FoodDataCentral_april2021.csv", package = "sustainableNutRients"))
#Food items
fromFoodDataCentral_foods <- read_csv(
  system.file("extdata", "food_FoodDataCentral_april2021.csv", package = "sustainableNutRients")) %>%

  #Select foods of interest
  filter(description %in% c(

    #Grains
    'Tempeh', 'Seeds, chia seeds, dried', 'Buckwheat', 'Edamame, frozen, unprepared',

    #Meat products
    'Beef, variety meats and by-products, tongue, raw', 'Pork, fresh, variety meats and by-products, kidneys, raw',
    'Beef, New Zealand, imported, flank, separable lean and fat, raw',

    #Fruit and veg
    'Jams and preserves, apricot', 'Artichokes, (globe or french), raw', 'Plantains, yellow, raw', 'Sauerkraut, canned, solids and liquids',
    "Tomato products, canned, paste, without salt added (Includes foods for USDA's Food Distribution Program)",
    'Lime juice, raw', 'Figs, raw', 'Cabbage, chinese (pak-choi), raw',
    #Sorrel
    'Sourdock, young leaves (Alaska Native)',

    #Herbs and spices
    'Tamarind nectar, canned', 'Spices, chervil, dried', 'Spices, allspice, ground', 'Coriander (cilantro) leaves, raw',
    'Spices, cloves, ground', 'Spices, cumin seed', 'Spices, fenugreek seed', 'Lemon grass (citronella), raw',
    'Spearmint, fresh', 'Spearmint, dried', 'Mustard, prepared, yellow', 'Spices, onion powder',
    'Spices, sage, ground', 'Seasoning mix, dry, sazon, coriander & annatto', 'Spices, garlic powder',

    #Dairy
    'Cheese, cottage, lowfat, 2% milkfat', 'Cheese spread, pasteurized process, American',
    'Cheese, monterey', 'Cheese, neufchatel', 'Cheese, provolone', 'Cheese, romano',
    'Yogurt, Greek, plain, whole milk',

    #Seafood
    'Mollusks, clam, mixed species, raw', 'Fish, grouper, mixed species, raw', 'Fish, sea bass, mixed species, raw',
    'Seaweed, wakame, raw',

    #Div
    'Seaweed, agar, dried', 'Soup, onion, dry, mix', 'Alcoholic beverage, rice (sake)',
    'Shortening, vegetable, household, composite', 'Pickle relish, sweet', 'Syrups, maple',
    'Sauce, ready-to-serve, pepper, TABASCO', 'Tapioca, pearl, dry', 'Molasses', 'Vital wheat gluten',
    'Horseradish, prepared', 'Oil, coconut', 'Fat, goose', 'Frostings, glaze, prepared-from-recipe',
    'Vanilla extract', 'Leavening agents, baking soda', 'Leavening agents, baking powder, low-sodium')) %>%

  #Rename to fit ingredient names
  mutate(description = description %>%
           str_replace('Beef, variety meats and by-products, tongue, raw', 'beef_tongue') %>%
           str_replace('Pork, fresh, variety meats and by-products, kidneys, raw', 'pork_kidney') %>%
           str_replace('Beef, New Zealand, imported, flank, separable lean and fat, raw', 'beef_flank') %>%
           str_replace('Jams and preserves, apricot', 'apricot_jam') %>%
           str_replace('Artichokes, \\(globe or french\\), raw', 'artichoke_heart') %>%
           str_replace('Plantains, yellow, raw', 'plantain') %>%
           str_replace('Sauerkraut, canned, solids and liquids', 'sauerkraut') %>%
           str_replace('Sourdock, young leaves \\(Alaska Native\\)', 'sorrel') %>%
           str_replace('Spices, chervil, dried', 'chervil_dried') %>%
           str_replace('Spices, allspice, ground', 'allspice') %>%
           str_replace('Coriander \\(cilantro\\) leaves, raw', 'coriander_fresh') %>%
           str_replace('Lemon grass \\(citronella\\), raw', 'lemongrass') %>%
           str_replace('Spices, cloves, ground', 'cloves') %>%
           str_replace('Spices, cumin seed', 'cumin') %>%
           str_replace('Spices, fenugreek seed', 'fenugreek_seed') %>%
           str_replace('Spearmint, dried', 'mint_dried') %>%
           str_replace('Spearmint, fresh', 'mint_fresh') %>%
           str_replace('Mustard, prepared, yellow', 'mustard') %>%
           str_replace('Spices, onion powder', 'onion_powder') %>%
           str_replace('Spices, sage, ground', 'sage_dried') %>%
           str_replace('Seasoning mix, dry, sazon, coriander & annatto', 'sazon seasoning') %>%
           str_replace('Cheese, cottage, lowfat, 2% milkfat', 'cottage cheese_low fat') %>%
           str_replace('Cheese spread, pasteurized process, American', 'cheese_american') %>%
           str_replace('Cheese, monterey', 'monterey_cheese') %>%
           str_replace('Cheese, neufchatel', 'cheese_neufchatel') %>%
           str_replace('Cheese, provolone', 'cheese_provolone') %>%
           str_replace('Cheese, romano', 'cheese_romano') %>%
           str_replace('Yogurt, Greek, plain, whole milk', 'yogurt_greek') %>%
           str_replace('Mollusks, clam, mixed species, raw', 'clam') %>%
           str_replace('Fish, grouper, mixed species, raw', 'grouper') %>%
           str_replace('Fish, sea bass, mixed species, raw', 'sea bass') %>%
           str_replace('Seaweed, agar, dried', 'agar') %>%
           str_replace('Soup, onion, dry, mix', 'onion soup mix') %>%
           str_replace('Alcoholic beverage, rice \\(sake\\)', 'sake') %>%
           str_replace('Shortening, vegetable, household, composite', 'shortening') %>%
           str_replace('Pickle relish, sweet', 'sweet green pickle relish') %>%
           str_replace('Syrups, maple', 'syrup_maple') %>%
           str_replace('Sauce, ready-to-serve, pepper, TABASCO', 'tabasco') %>%
           str_replace('Tapioca, pearl, dry', 'tapioca') %>%
           str_replace('Molasses', 'molasses') %>%
           str_replace("Tomato products, canned, paste, without salt added \\(Includes foods for USDA's Food Distribution Program\\)", 'tomato_paste') %>%
           str_replace('Lime juice, raw', 'lime_juice') %>%
           str_replace('Vital wheat gluten', 'gluten') %>%
           str_replace('Horseradish, prepared', 'horseradish_prepared') %>%
           str_replace('Oil, coconut', 'coconut_oil') %>%
           str_replace('Fat, goose', 'goose_fat') %>%
           str_replace('Seeds, chia seeds, dried', 'seed_chia') %>%
           str_replace('Frostings, glaze, prepared-from-recipe', 'decorative glaze') %>%
           str_replace('Edamame, frozen, unprepared', 'bean_edamame') %>%
           str_replace('Spices, garlic powder', 'garlic_powder') %>%
           str_replace('Figs, raw', 'fig') %>%
           str_replace('Seaweed, wakame, raw', 'wakame') %>%
           str_replace('Vanilla extract', 'vanilla_extract') %>%
           str_replace('Cabbage, chinese \\(pak-choi\\), raw', 'cabbage_pak choi') %>%
           str_replace('Leavening agents, baking soda', 'baking_soda') %>%
           str_replace('Leavening agents, baking powder, low-sodium', 'baking_powder') %>%
           str_replace('Buckwheat', 'buckwheat')
         #'Tamarind nectar, canned'
  ) %>%

  #Get nutrient content from db
  #First get nutrient id's
  inner_join(., fromFoodDataCentral_nutrients, by = 'fdc_id') %>%
  #Select columns
  select(description, nutrient_id, fdc_id, amount) %>%
  #Get nutrient names
  inner_join(fromFoodDataCentral_nutrient_names, by = 'nutrient_id') %>%

  #Filter out the nutrients of interest
  mutate(name = str_to_lower(name)) %>%
  filter(name %in% c(
    #Energy nutrients
    'calories', 'fat', 'saturated fat', 'monounsaturated fat', 'polyunsaturated fat', 'protein', 'c20:5 ecosapentaenoic',
    'c22:5 docosapentaenoic', 'c22:6 docosahexaenoic', 'carbohydrates', 'starch', 'sucrose', 'lactose', 'galactose',
    'maltose', 'glucose', 'dextrose', 'dietary  fiber', 'alcohol v/v',

    #Minerals
    'sodium', 'calcium', 'iron', 'zinc', 'potassium', 'magnesium', 'selenium', 'copper', 'phosphorus', 'iodine',

    #Vitamins
    'vitamin a', 'beta-carotene', 'retinol', 'vitamin d', 'alpha-tocopherol', 'beta-tocopherol', 'delta-tocopherol', 'gamma-tocopherol',
    'thiamin', 'riboflavin', 'niacin', 'vitamin b6', 'folate', 'vitamin b12', 'vitamin c',

    #div
    'cholesterol')) %>%

  #Turn wide and rename nutrients to fit the names in Matvaretabellen
  #remove unneccesery columns and rename others
  select(-c(nutrient_id, id)) %>%

  pivot_wider(.,
              names_from = name,
              values_from = amount) %>%

  rename(Ingredients = description,
         Kilocalories = calories,
         Fat = fat,
         SatFa = `saturated fat`,
         MuFa = `monounsaturated fat`,
         PuFa = `polyunsaturated fat`,
         DPA = `c22:5 docosapentaenoic`,
         EPA = `c20:5 ecosapentaenoic`,
         DHA = `c22:6 docosahexaenoic`,
         Protein = protein,
         Carbo = carbohydrates,
         Starch = starch,
         Cholesterol = cholesterol,
         `Dietary fibre` = `dietary  fiber`,
         Alcohol = `alcohol v/v`,
         `Vitamin A` = `vitamin a`,
         `Beta-carotene` = `beta-carotene`,
         Retinol = retinol,
         `Vitamin D` = `vitamin d`,
         Thiamin = thiamin,
         Riboflavin = riboflavin,
         Niacin = niacin,
         `Vitamin B6` = `vitamin b6`,
         Folate = folate,
         `Vitamin B12` = `vitamin b12`,
         `Vitamin C` = `vitamin c`,
         Calcium = calcium,
         Iron = iron,
         Sodium = sodium,
         Potassium = potassium,
         Magnesium = magnesium,
         Zinc = zinc,
         Selenium = selenium,
         Phosphorus = phosphorus,
         #Iodine = iodine,
         Copper = copper
  ) %>%

  #Add mono- and di-saccharaides together, and the different types of vitamin E, create a Kilojoules and salt column
  mutate(`Mono+Di` = glucose + galactose + dextrose + maltose + sucrose + lactose,
         Sugar = sucrose,
         `Vitamin E` = `alpha-tocopherol` + `gamma-tocopherol` + `beta-tocopherol` + `delta-tocopherol`,
         Kilojoules = Kilocalories*4.184,
         Salt = Sodium*2.5/1000 #Unit is in mg Salt in Matvaretabellen is in g
  ) %>%
  #Remove the columns
  select(-c(glucose, galactose, dextrose, maltose, sucrose, lactose, `alpha-tocopherol`, `gamma-tocopherol`, `beta-tocopherol`, `delta-tocopherol`)) %>%
  #Add zero for NA
  replace(is.na(.), 0) %>%

  #rename database_ID column
  rename(database_ID = fdc_id)

#Add to clean_nutrients df, add where the foods are from
clean_nutrients <- bind_rows(clean_nutrients %>% mutate(from = 'Matvaretabellen_database_ID'), fromFoodDataCentral_foods %>% select(database_ID, Ingredients) %>% mutate(from = 'FoodData Central_fdc_id'))

#Add composite ingredients----
various$component_ingredients_nutrients <- readRDS(
  system.file("extdata", "composite_ingredients_nutrient_content.Rds", package = "nutRients")) %>%
  #Create an database_ID column
  group_by(Ingredients) %>%
  mutate(database_ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(database_ID = database_ID + 200) %>% mutate(from = 'Composite ingredient not in database')

clean_nutrients <- bind_rows(clean_nutrients, various$component_ingredients_nutrients %>% select(database_ID, Ingredients, from)) %>%
  #Add a shellfish row
  add_row(database_ID = 10000, Ingredients = 'shellfish', from = 'Shellfish in Matvaretabellen')

#Create the nutrient content of the shellfish ingredient by taking the mean of the shellfish in the db
various$shellfish <- raw_data %>%

  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', 'Water', ':0', ' sum', '2n', '3n', '4n', 'Foodgroup'))) %>%
  #Filter out shellfish
  filter(`Food Item` %in% c('Crab, plain, canned', 'King prawns, raw', 'Lobster, boiled',
                            'Mussel, blue, raw', 'Oyster, common, raw', 'Scallop, raw')) %>%
  #Rename to fit
  rename(
    database_ID = FoodID,
    EPA = `C20:5n-3 (EPA)`,
    DPA = `C22:5n-3 (DPA)`,
    DHA = `C22:6n-3 (DHA)`,
    Ingredients = `Food Item`) %>%
  #Turn everything to numeric
  pivot_longer(.,
               cols = -c(database_ID, Ingredients),
               names_to = 'feature',
               values_to = 'value') %>%
  mutate_at(c('database_ID', 'value'), ~as.numeric(.)) %>%
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>%
  #Add clams
  bind_rows(., fromFoodDataCentral_foods %>% filter(Ingredients == 'clam')) %>%

  #Get the mean values o each nutrient
  pivot_longer(.,
               cols = -c(database_ID, Ingredients),
               names_to = 'feature',
               values_to = 'value') %>%
  group_by(feature) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  #Add name and database_ID
  mutate(Ingredients = 'shellfish',
         database_ID = 10000) %>%
  #Turn wide
  pivot_wider(.,
              names_from = 'feature',
              values_from = 'value')

#Create a full nutrient database----
#Nutrients of interest
nutrients_to_use <- raw_data %>%

  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', 'Water', ':0', ' sum', '2n', '3n', '4n'))) %>%

  #Rename to fit
  rename(
    database_ID = FoodID,
    EPA = `C20:5n-3 (EPA)`,
    DPA = `C22:5n-3 (DPA)`,
    DHA = `C22:6n-3 (DHA)`) %>%

  #Remove columns and rows
  select(-c(`Food Item`, Foodgroup)) %>%
  drop_na(database_ID) %>%
  drop_na(Kilocalories) %>%

  #Turn everything to numeric
  pivot_longer(.,
               cols = -database_ID,
               names_to = 'feature',
               values_to = 'value') %>%
  mutate_at(c('database_ID', 'value'), ~as.numeric(.)) %>%
  pivot_wider(.,
              names_from = feature,
              values_from = value) %>% mutate(from = 'Matvaretabellen_database_ID')

#Add the foodDataCentral and composite ingredients
nutrients_to_use <- nutrients_to_use %>%
  bind_rows(., fromFoodDataCentral_foods) %>%
  bind_rows(., various$component_ingredients_nutrients) %>%
  bind_rows(., various$shellfish) %>%
  full_join(., clean_nutrients, by = 'database_ID') %>%
  #Give sour cream, crème fraîche and veal liver individual database_IDs, while keeping the nutrition information rows
  mutate(database_ID = case_when(
    str_detect(Ingredients.y, 'crème fraîche|veal_liver') ~ database_ID + 300,

    TRUE ~ database_ID
  )) %>%
  #Remove and rename some columns
  select(-c(Ingredients.x, Ingredients.y, from.x)) %>%
  rename(from = from.y)

#Turn long
matvaretabellen2020 <- nutrients_to_use %>% select(-c(food_item, Foodgroup)) %>%
  pivot_longer(.,
               cols = -c(database_ID, from),
               names_to = 'nutrient',
               values_to = 'nutrient_content_per_hektogram')

#Save matvaretabellen dataframe
saveRDS(matvaretabellen2020, "./data-raw/matvaretabellen2020.Rds")

#Save foodgroups info
matvaretabellenFoodgroups <- nutrients_to_use %>%
  select(database_ID, Foodgroup) %>%
  rename(foodgroup = Foodgroup)

saveRDS(matvaretabellenFoodgroups, "./data-raw/matvaretabellen2020_foodgroups.Rds")

#Save database_ID and food_item columns to create a query dataframe
matvaretabellen2020_query_prep <- clean_nutrients %>%
  select(Ingredients, database_ID)

#Save the dataframe to use to create the queries
saveRDS(matvaretabellen2020_query_prep, "./data-raw/matvaretabellen2020_query_prep.Rds")

