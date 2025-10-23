# Fixes for findFoodInDatabase function

## Volume weight ####
databaseHitFixes <- list(
  volumeWeight = tibble(
    tmp = c(
      "broth cube chicken_broth cube",
      "broth cube vegetable_broth cube",
      "bean white tomato_bean canned white tomato",
      "vinegar apple cider_vinegar",
      "buttermilk_milk",
      'butter clarified ghee_ghee',
      'eggplant_eggplant',
      'bread flat hard_flatbread_hard',
      'caper_capers_canned',
      'cheese mozzarella_mozzarella',
      'chili pepper green_chili pepper_red',
      'chili pepper jalapeno_chili pepper_red', #Same in volume
      'mackerel tomato canned_mackerel_tomato',
      'pork neck chop_pork_neck',
      'sweet pepper grilled_sweet pepper_grilled',
      'turkey chicken drumstick_turkey_drumstick',
      'lemongrass_lemongrass',
      'fig_fig',
      'bean white canned_bean canned_white',
      'bean kidney canned_bean canned_kidney',
      'bean black canned_bean canned_black',
      'chick pea canned_chick pea_canned',
      'mustard powder_mustard_powder',
      'parsley_parsley_fresh',
      'bean canned_bean canned_black',
      'bean black_bean canned_black',
      'rice brown long grain_rice',
      'cranberries jam_jam',
      'jam apricot_jam',
      'hamburger bun_hamburger_bread',
      'mustard honey_mustard',
      'bread rye_bread',
      'watermelon_melon_water',
      'broccolini_broccolini',
      'sausage turkey chicken_sausage turkey chicken',
      'salad rocket_ruccola',
      'lime leaf_bay leaf', #Assume similar
      'chili peppers_chili pepper_red',
      'chili_chili pepper_red',
      'strong chili_chili pepper_red',
      'corn flour_cornmeal_polenta',
      'oat quick_rolled_oat',
      'peach canned_peach canned',
      'pimiento chili pepper_chili pepper_red', #Small pepper
      'garlic wild_scallion',
      'agave nectar_honey',
      'graham cracker_cracker_cream',
      'marjoram fresh herbs_oregano_fresh',
      'harissa mild_paste_chili',
      "harissa_paste_chili",
      'spread speculaas_peanut_butter',
      'onion pickled_beetroot_pickled',
      'pizza sauce red_tomato_canned',
      "rolls white baguette garlic_baguette",
      "egg yolk boiled_egg_yolk",
      "o'boy pulver_cocoa_powder",
      "hamburger plant-based_meat_ground",
      "cheese pizza_cheese semi hard",
      'cheese manchego_cheese semi hard',
      'cheese brie_soft ripened cheese',
      'cheese mascarpone_soft ripened cheese',
      'cheese camembert_soft ripened cheese',
      'cheese le crémier de chaumes_soft ripened cheese',
      'cheese goat chevre white_soft ripened cheese',
      #'cheese goat_soft ripened cheese',
      'shortening_margarine",
      "shortening vegetable_margarine',
      'onion seed_poppy_seed',
      'chia seeds_poppy_seed',
      "margarine_butter",
      "lard pork fat_butter",
      "bacon fat_butter",
      "granola_granola",
      'polenta_cornmeal_polenta',
      "grape fruit_grapefruit",
      'mangold_celery_stalk', #Similar
      "quark_cottage_cheese",
      "garlic powder_onion_powder",
      "remulade_mayonnaise",
      "cheese plant-based_cheese semi hard",
      "almond butter_peanut_butter",
      "barbecue seasoning_taco_spice",
      "syrup chocolate_syrup",
      "syrup currant_syrup",
      "syrup blackcurrant_syrup",
      'syrup glucose_syrup',
      "erythriol_sugar",
      'pearl sugar_sugar',
      "butter spice_butter",
      "pollock_cod_fillet",
      "rolls coarse_rolls_white",
      "pie crust_pie_dough",
      paste0("ice cream ", c("caramel", "chocolate", "vanilla", "plant-based"), "_ice cream")
  )),
  ## Nutrition----
  nutrients = tibble(
    tmp = c(
      #Fruit and veg
      'vinegar raspberries_vinegar',
      'lime, the zest_lemon_zest',
      'eggplant_eggplant',
      'peach_peach',
      'sweet corn kernels_sweet corn_canned',
      'sweet potato_sweet potato',
      'jerusalem artichoke_artichoke_jerusalem',
      'chili pepper dried_chili pepper_red',
      'mangold_mangold',

      'watermelon_watermelon',
      'salsa_salsa_chunky',
      'pear_pear',
      'jam blueberries_jam',
      'tomato beef_tomato',
      "grape fruit_grapefruit",

      'pimiento chili pepper_sweet pepper_red',
      'sweet pepper grilled_sweet pepper_red',
      'sweet pepper canned_sweet pepper_red',
      'sweet pepper pickled_chili pepper_pickled',
      'chili pepper jalapeno pickled_chili pepper_pickled',
      'potato_potato',
      'potato boiled_potato', ## Dobbeltsjekk denne om ikke matvaretabellen har den

      #Dairy
      'parmesan cheese_cheese_parmesan',
      #Can be substituted for eachother in recipes
      paste0(c('cheese asiago', 'cheese cotjia', 'cheese pecorino',
               'cheese romano', 'cheese gruyere', 'cheese parmigiano reggiano'),
             "_cheese_parmesan"),
      'butter clarified ghee_ghee',
      'cheese cottage low fat_cheese cottage_low fat',
      'cheese cottage_cheese cottage',
      'cheese goat chevre white_cheese_chevre',
      'cheese goat_cheese_chevre',
      'cheese soft_cheese cream',
      'cheese le crémier de chaumes_cheese cream',
      'cheese cream goat sn\u00f8frisk_cheese cream_goat sn\u00f8frisk',
      'cheese semi hard_cheese_norvegia',
      'cheese emmentaler_cheese_norvegia',
      'cheese garlic_cheese_norvegia',
      'cheese pizza_cheese_norvegia',
      'cheese hard goat_cheese hard goat_kvitlin', #Use as standard for time being
      'cheese jarlsberg_cheese_jarlsberg',
      'cheese manchego_cheese_cheddar', #Can be substituted in recipes
      'cheese mozzarella_cheese_mozzarella',
      'cheese norvegia_cheese_norvegia',
      'cheese ricotta salata_cheese_ricotta salata',
      'cheese port salut_cheese_port salut',
      'cheese burrata mozzarella_cheese_mozzarella',
      'goat brown cheese_cheese brown_goat',
      'cheese mascarpone_cheese_mascarpone',
      'tine light 2 \u0025 a good alternative to sour cream_quark_1', #Closest in nutritional value
      'milk evaporated_milk evaporated',
      'buttermilk_buttermilk',
      'cheese blue_cheese blue',
      'cheese blue castello_cheese blue',

      #Div
      'mushroom_mushroom',
      'mushroom chestnut_mushroom',
      'sugar_sugar',
      'pearl sugar_sugar',
      'sesame seed oil_sesame_oil',
      'condensed cream of celery soup_condensed cream of celery soup',
      'condensed cream of chicken soup_condensed cream of chicken soup',
      'condensed cream of mushroom soup_condensed cream of mushroom soup',
      'oil corn_vegetable_oil',
      'soup onion instant_onion soup mix',
      'sauce hot pepper_hot pepper sauce',
      'sauce pasta_tomato_sauce', #Use as substitute for time being
      'sauce hot_hot pepper sauce',
      'olive paste tapenade_olive paste tapenade',
      'homemade beef gravy_beef gravy',
      'sweet chili sauce_chili sauce_sweet',
      'refrigerated buttermilk biscuit dough_refrigerated buttermilk biscuit dough',
      'beef gravy_beef_gravy',
      'sauce piri-piri_sauce piri-piri',
      'sauce tikka masala_sauce tikka masala',
      'sauce pad thai_sauce pad thai',
      'ice cube_water',

      #Grains, seeds nuts
      'chick pea_chick pea',
      'rice white long grain_rice white long grain',
      'dried soybeans_bean_soya',
      'bean canned_bean_kidney canned', #Standard
      'peanut_peanut',
      'peanut salt_peanut_salt',
      'rice parboiled_rice parboiled',
      'rice brown long grain_rice brown long grain',
      'white bread mix_white bread_mix',
      'cookies amarettini_amaretti cookie',
      'bean salad_bean salad',
      'taco shell_nacho',
      'lasagna plate pasta_pasta',

      #Seafood
      'cod lutefisk_lutefisk',
      'mackerel tomato canned_mackerel_tomato canned',
      'fish cakes coarse_fish cakes_coarse',

      #Herbs spices and condiments
      'parsley_parsley_fresh',
      'dry mustard_mustard',
      'mayonnaise sauce_mayonnaise',
      'chili flake dried_chili_powder',
      'mustard honey_mustard',
      'lemongrass_lemongrass',
      'spice mix taco_taco spice mix',
      'lemon balm_mint_fresh',

      #Meat
      'pork neck chop_pork_neck chop',
      'sausage_sausage',
      'chicken_chicken_whole',
      'whole turkey_turkey_meat',
      'pork neck_pork_neck chop',
      "meatballs in tomato sauce_meatball_tomato sauce",
      "bacon fat_pork_lard", #Closest match
      'chicken wing_chicken_drumstick',
      'hamburger beef patty_beef_minced meat',
      'lamb ribs_lamb_chop',


      #Substitutions or ingredients not found in Matvaretabellen
      'hazelnut oil_walnut_oil', #Another nut oil
      'bean canned_bean black_canned',
      'scampi_shrimp',
      'ciabatta_bread_white',
      'elk shoulder_elk moose',
      'elk tenderloin_beef_tenderloin',
      'lime, the zest_lemon_zest',
      'sugar vanilla_sugar',
      'cream double 48 \u0025_cream whipped_37', #Highest in the database
      'chocolate unsweetened_chocolate_dark', #Highes cocoa percentage in database
      'aioli_mayonnaise', #Similar
      'trout smoked_salmon_smoked',
      'fresh herbs ginger_ginger',
      'butter plant-based_margarine',
      'apple cider_cider',
      'currant juice_black currant_juice',
      'cashew nut salt_cashew nut',
      'cashew nut roasted_cashew nut',
      'bread crumb_bread',
      'bread_bread',
      'breadstick_bread',
      'crisp bread_crisp bread_coarse',
      'crisp bread coarse_crisp bread_coarse',
      'rolls white baguette garlic_bread_white',
      'bread sausage_bread_white',
      'hamburger bread_bread_white',
      'bread brown chapati_bread_coarse',
      'tortilla coarse_bread_coarse',
      'rolls coarse_bread_coarse',
      'rolls coarse baguette_bread_coarse',
      'red chili_chili pepper_red',
      'strong chili_chili pepper_red',
      'chili peppers_chili pepper_red',
      'beef shank_beef_veal chops',
      'beef oxtail_beef_veal chops',
      'syrup apple_syrup_maple',
      'syrup pear_syrup_maple',
      'syrup currant_syrup_maple',
      'syrup_syrup_maple',
      'agave syrup_syrup_maple',
      'syrup ginger_syrup_maple',
      'syrup chocolate_syrup_maple',
      'syrup caramel_syrup_maple',
      'glucose_syrup_maple',
      'salsa_salsa_chunky',
      'salsa tomato_salsa_chunky',
      'syrup apple_syrup_maple',
      'syrup pear_syrup_maple',
      'garlic oil_olive_oil',
      'oil truffle_olive_oil',
      'frying oil_vegetable_oil',
      'oil_vegetable_oil',
      'flaxseed meal_flax_seed',
      'seed flax_flax_seed',
      'chicken ham_turkey_ham',
      "chips_chips_potato",
      'cream cheese goat_cheese cream_goat snøfrisk',
      "elk minced meat_beef_minced meat lean",
      "pancake powder mix_pancake_mix",
      "peanut butter unsalted" = "peanut_butter",
      "pear marmalade" = "marmelade_pear",
      "rib roll pork_pork belly",
      "rice sushi_rice porridge",
      "smoothie mix_smoothie mix_tropical",
      "mexican salad_mexican salad"
    )
  ),
  ## Sustainability----
  sustainability = tibble(
    tmp = c(
      "broth cube chicken_broth_cube",
      paste0(c('espresso bean coffee ground', 'coffee beans'), '_coffee ground'),
      'hazelnut_nut_hazel',
      'lentils dried_lentil_dry',
      paste0(c('peas green', 'stew peas'), '_pea_garden'),
      paste0(c('bean green asparagus', 'bean green', 'bean broad'), '_bean with pods_with'),
      paste0(c("bean white canned", "bean black canned", "bean kidney canned","bean canned",
               "bean pinto canned", "bean white tomato", "refried beans", "bean mixed",
               "chick pea canned", "chili beans",
               "lentils canned green", "lentils canned red", "lentils canned"), "_bean_canned"),
      paste0(c("rice noodle", "egg noodle", "glass noodle"), "_noodle"),
      'pistachio nut_pistachio',
      'dried soybeans_bean_soy',
      'pecan_tree_nut',
      'tahini_sesame_seed',
      paste0(c('seed flax', 'flaxseed meal'), '_linseed'),
      'corn starch_corn_flour', #Use as substitute
      'chick pea flour_chick pea_flour',
      'bean salad_bean salad',
      'lasagna plate pasta_pasta',

      #Veggies and fruit
      paste0(c("chili pepper jalapeno pickled", "onion pickled", "cucumber pickled",
               "ginger pickled", "beetroot pickled", "sweet pepper pickled"), "_vegetables_pickled"),
      paste0(c("chili canned", "sweet corn canned", "sweet corn kernels", "vegetables_canned")),
      "chicory_curly_endives",
      'peach_peach',
      'sorrel_lettuce_other',
      paste0(c("winter squash butternut", "winter squash pumpkin", "winter squash hokkaido"), "_pumpkin"),
      "eggplant_eggplant",
      'garlic chinese_garlic',
      paste0(c('corn baby', 'corn cob'), '_sweet_corn'),
      #'mangold_chard',
      'olive black_olives_canned',
      paste0(c('olive green', 'of olives'), '_olives_fresh'),
      'olive black_olives_canned',
      paste0(c('red chili', 'strong chili', 'chili peppers'), '_chili_pepper'),
      paste0(c('tomato bunch', 'tomato beef'), '_tomato'),
      paste0(c('salad', 'salad heart', 'salad lollo rosso'), '_head_lettuce'),
      'salad crispi_crisp_lettuce',
      'salsa tomato_chunky_salsa', #Standard
      paste0(c('tomato sun dried', 'tomato sun dried in oil'), '_tomato_sun-dried'),
      "tomato canned_preserved_tomato",
      paste0(c("lemon, the zest", "orange, the zest", "lime, the zest"), "_citrus_fruit"),
      'clementine_mandarin',
      'black currant_blackcurrant',
      'currant_redcurrant',
      'tamarind juice_fruit_juice',
      'salsa_chunky_salsa',
      paste0(c('syrup apple', 'syrup pear', 'syrup ginger', 'syrup currant',
               'syrup chocolate', 'syrup caramel', 'glucose'), '_syrup'),
      'apricot nectar_fruit_nectars',
      paste0(c(
        "cloudberry", "lingonberry", "goji berry", "physalis", "rowan berry"), '_berries'),
      paste0(c("grape fruit"), '_grapefruit'),
      'melon honeydew_melon',
      'sprouts radish_sprout',
      paste0(c("morel"), '_cherries_sour'),
      paste0(c("jackfruit", "nectarine"), '_fig'), #In the same family
      paste0(c("tangerine canned"), '_fruit_canned'),
      paste0(c('starfruit', 'dragon fruit'), '_fruit_used'),
      paste0(c('blueberries dried', 'cranberries dried'), '_fruit_dried'),
      paste0(c('horseradish'), '_horseradish_roots'),

      #Red meat
      paste0(c("reindeer", "elk tenderloin", "elk minced meat", "game beef venison shoulder"), '_mammals_meat'),
      paste0(c('lamb sheep cabbage stew meat', 'lamb sheep head', 'lamb minced meat', "lamb leg roast",
               "lamb shank", "lamb chop", "lamb shoulder", "lamb ribs", "hamburger lamb patty", "lamb sausage"), '_lamb_fresh'),
      "meatballs in tomato sauce_meatball_tomato sauce",
      paste0(c("beef minced meat", "beef minced meat lean", "beef rib-eye steak", "beef sirloin",
               "beef chuck roll", "beef tongue", "beef tenderloin", "beef brisket", "beef oxtail",
               "beef bottom round roast beef", "beef patty lean", "beef patty"), "_beef"),
      paste0(c("pork shoulder", "pork ham roast", "pork minced meat", "pork belly", "pork inside round",
               "pork neck chop", "pork tenderloin", "rib roll pork", "pork chop", "sausage pork belly"), '_pork'),

      # Ham
      paste0(c("pork head cheese", "ham smoked", "ham cured", "ham", "chicken ham", "turkey ham canned"), "_ham"),

      #Poultry
      paste0(c("sausage turkey chicken", "whole turkey", "turkey breast", "turkey offal"), "_turkey"),
      paste0(c('hen breast fillet grouse', 'hen', 'hen grouse'), '_poultry_fresh'), #All poultry meats have the same CO2 and landuse in the db

      #Seafood
      'scampi_prawn',
      paste0(c('arctic char', 'char'), '_trout'), #Look up other alternatives
      paste0(c('catfish', 'flounder'), '_miscellaneous_demersal'), #Steinbit
      paste0(c('salmon roe', 'roe', 'roe salmon'), '_fish_roe'),
      'fish cakes coarse_fish cakes coarse',
      'cockles_scallop',

      # Beverages
      'apple cider_cider',
      paste0("water broth", "ice cube"), '_water',

      paste0(c(
        "cranberry juice", "pomegranate juice", 'strawberry juice',
        "black currant juice", "currant juice", "pineapple juice",
        "apple juice"), '_fruit_juice'),
      'aquavit_fortified wine',
      'coffee liqueur_liqueur_coffee',
      paste0(c("vinegar white wine", "vinegar red wine", "vinegar sweet wine"), "_vinegar_wine"),
      paste0(c('vinegar rice', 'vinegar apple cider',
               'vinegar sherry', 'vinegar brown', 'vinegar raspberries'), '_vinegar'),
      'condensed cream of celery soup_condensed cream of celery soup',
      'condensed cream of chicken soup_condensed cream of chicken soup',
      'condensed cream of mushroom soup_condensed cream of mushroom soup',
      "mushroom_mushroom",
      paste0("mushroom ",
             c("aroma champignon", "chanterelle",
               "champignon", "portebello", "shiitake"),
             "_mushroom"),
      paste0(c('garlic oil', 'oil truffle'), '_olive_oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
      'sauce hot_hot_pepper',
      'sesame oil_seed_oil',
      'hazelnut oil_walnut_oil',
      'sauce pasta_tomato_sauce',
      'sweet chili sauce_chili_sweet',
      paste0(c('cognac', 'kirsch')),  '_brandy',

      #str_detect(Ingredients, 'broth cube') ~ fixFoodMappingError(database = reference, 'stock_cubes',
      paste0(c('nacho', 'taco shell'), '_tortilla_corn'), #Similar ingredients just with more salt
      'mango chutney_mango chutney',
      'soybean oil_soy_oil',
      'mustard honey_mustard',
      'refrigerated buttermilk biscuit dough_refrigerated buttermilk biscuit dough',
      'corn meal mix_corn flour_polenta',
      paste0(c('jam blueberries', 'jam currant', 'cranberries jam'), '_jam'),
      'sweet green pickle relish_sweet green pickle relish',
      'goose fat_fats',
      'remulade_mayonnaise_sauce',
      'tabasco_chili_sauce',
      paste0(c("aioli", "mayonnaise"), '_mayonnaise_sauce'),
      paste0(c("aioli plant-based", "mayonnaise plant-based"), '_mayonnaise_vegan'),
      paste0(c("chocolate semi-sweet"), '_chocolate_dark'),

      #Dairy
      'buttermilk_buttermilk',
      paste0(c("parmesan cheese", "cheese cheddar", "cheese parmigiano reggiano",
               "cheese gruyere", "cheese cotjia", "cheese romano", "cheese hard goat"), '_hard cheese'),
      paste0("cheese ", c("pizza", "halloumi", "manchego", "havarti", "swiss", "monterey jack",
                          "pepperjack", "asiago", "mozzarella", "jarlsberg", "semi hard", "provolone",
                          "norvegia", "emmentaler", "garlic", "brown"), "_hard to semi-hard cheese"),
      "goat brown cheese_hard to semi-hard cheese",
      paste0("cheese ", c("ricotta salata", "blue", "blue selbu", "camembert", "neufchatel", "port salut",
                          "brie", "mascarpone", "gorgonzola", "soft", "le crémier de chaumes", "goat",
                          "goat chevre white", "blue roquefort"), '_soft-ripened cheese'),
      paste0(c('cheese american', 'cheese spread'), '_processed cheese and spreads'),
      paste0(c('yoghurt plain greek', 'yoghurt plain', 'yoghurt plain skyr', 'kefir',
               "quark 7 %", "quark 1", "biola"), "_yoghurt"),

      ##Milk with cocoa powder
      'milk beverage chocolate_milk',
      paste0(c("cheese plant-based", 'yoghurt plant-based'), "_dairy_imitate"),


      #Bread and rolls
      paste0(c(
        'bread', 'bread coarse', 'tortilla coarse', 'crisp bread coarse','bread crumb', 'bread crouton',
        'bread rye', 'bread polar', 'hamburger bread coarse', 'rolls coarse baguette garlic',
        'bread brown chapati', 'rolls coarse', 'rolls coarse baguette',
        "pita bread coarse"), '_wheat bread and rolls_brown'),
      paste0(c(
        'hamburger bun', 'bread white', 'tortilla', 'crisp bread', 'breadstick', 'ciabatta',
        'hamburger bread', 'bread white foccacia',
        'rolls white', 'cracker cream', 'bread naan', 'bread flat hard', 'pita bread white',
        'bread sausage', 'bread paratha', "parata flat bread",
        'pizza crust', 'pizza crust italian'), '_wheat bread and rolls_white'),
      paste0(c('rolls white', "rolls white baguette", "rolls white", "rolls white baguette garlic"),
             '_wheat bread and rolls_white'),

      #Herbs and spices
      paste0(c('herbs', 'different spices', 'spices', 'soup seasoning', 'vanilla extract', 'vanilla pod',
               'vanilla essence', 'vanilla powder', 'saffron', 'fenugreek seed', 'mint fresh herbs',
               'mint dried', 'lemon balm', 'turmeric', 'anise', 'marjoram', 'sazon seasoning', 'caraway seed',
               'lemongrass', "basil fresh herbs", "basil dried", "rosemary fresh herbs", "thyme fresh herbs",
               "thyme dried", "tarragon fresh herbs", "tarragon dried", "oregano fresh herbs", "oregano dried",
               "black pepper", "cayenne pepper", "sage fresh herbs", "sage dried", "garam masala", "nutmeg",
               "cloves", "coriander fresh herbs", "coriander seed", "cumin", "dill fresh herbs", "chili powder",
               "fenugreek leaf", "juniper berry", "cinnamon", "chives", "cardamom", "caper", "allspice",
               "bay leaf", "paprika powder", "fennel seed", "garlic powder", "chervil fresh herbs", "chili flake dried"), '_mixed_herbs'),
      paste0("spice mix ", c("taco", "provence", "raita", "tandoori", "meat", "fajita", "chicken", "guacamole"), "_mixed herbs"),
      paste0(c("piri piri", "five spice", "seafood", "pasta", "pad thai", "fish", "mexican"), " spice mix_mixed_herbs")
  )

)) %>% lapply(., function(fixList) {

  fixList %>%
    tidyr::separate_wider_delim(.,
                         cols = tmp,
                         names = c("Ingredients", "database_referenceword1", "database_referenceword2"),
                         delim = "_",
                         too_few = "align_start") %>%
    replace_na(., list(database_referenceword2 = "\\"))
})




## Not in database Ingredients ----
not_in_database <- list(
  volumeWeight = tibble(
    tmp = c(
      # Ingredient names
      paste0(c(
        'mustard powder', 'chinese five spice', 'dip mix', 'asafoetida powder', 'lemon gel',
        'sauce browning', 'trout caviar', 'whip it stabilizer', 'vanillin', 'turkey offal',
        'sugar color', 'coffee beans', 'prune marinade'), "_IngredientsEqual"))#,
      # Str detect
      #paste0(c('powder mix'), "_strDetectIngredients"))
    ),
  nutrients = tibble(
    tmp = c(
      # Ingredient names
      paste0(c(
      'duck or goose fat for confit', 'lime leaf', "beans'n'pork canned", 'onion seed',
      'cooking spray', 'red food coloring', 'beef fund', 'fish scraps for broth', 'chili bean paste sichuan',
      'pack high quality charcoal briquettes', 'pomegranate kernel', 'condensed tomato soup',
      'salmon roe', 'spice seasoning pepper', 'toro greek moussaka', 'paste chili', 'carbonated beverage lemon-lime',
      'fish soup base', 'spice mix guacamole', 'lamb sheep head', 'can tomato soup', 'sauce white',
      'marrow bone', 'rhubarb juice', 'beef bones', 'whip it stabilizer', 'toenjang soybean paste',
      '20 pound pack high quality charcoal briquettes', 'wine rice', 'trout caviar', 'vanillin', 'cream sauce base',
      'vanilla pod', 'butter-vanilla aroma', 'paste vanilla bean', 'blueberries pie filling', 'almond essence', 'vanilla essence',
      'milk powder nonfat', 'apricot nectar', 'apricot preserve', 'apple sauce', 'oil chili sichuan',
      'frozen vegetable mix', 'sauce curry', 'gingerbread house', 'lemonade', 'sugar candy', 'sugar color',
      'sweet potato fries', 'coriander paste', 'vanilla essence', 'vanilla pod',
      'cheese schnitzel', 'cheese pancakes', 'honey crunch', 'lemon gel', 'mint jelly',
      'prune marinade', 'roe salmon', 'vanilla pod', 'vanilla powder', 'vanilla essence',
      'quick lunch', 'prune marinade', 'orange liqueur', 'turkey offal', 'bones for broth',
      "beer", "horn salt"), "_IngredientsEqual"),
      # Str detect
      paste0(c("spice mix(?! taco)", "powder mix", "soup instant"), "_strDetectIngredients"))
      ),
  sustainability = tibble(
    tmp = c(
      # Ingredient names
      paste0(c(
      'yeast nutritional', 'paste chili', 'agar', 'gluten', 'corn meal mix', 'blueberries pie filling', 'onion seed',
      'nori seaweed','salmon roe', "beans'n'pork canned", 'apricot preserve', 'apple sauce', 'chili bean paste sichuan',
      'plantain', 'tabasco', 'tapioca', 'sake', 'wine rice', 'liquid smoke flavoring', 'carbonated beverage lemon-lime', 'sauce cranberry',
      'pack high quality charcoal briquettes', 'cooking spray', 'quinoa', 'paste carrot', 'seed hemp', 'onion powder', 'rhubarb juice',
      'red food coloring', 'toro greek moussaka', 'banana', 'can tomato soup', 'paste vanilla bean', 'lime leaf', 'sugar color',
      'fish scraps for broth', 'fish soup base', 'paste garlic', 'vanillin', 'vanilla extract', 'toenjang soybean paste',
      'pomegranate kernel', 'sauce white', 'celery seed', 'trout caviar', 'vanilla pod', 'condensed tomato soup', 'cream sauce base',
      'sauce bearnaise', 'wine rice', 'soup onion instant', 'whip it stabilizer', 'butter-vanilla aroma', 'shake mixed spice',
      'vanilla bean', 'sauce curry', 'gingerbread house', 'rice puffed', 'candy mixed', 'horn salt',
      'decorative glaze', 'lemon gel', 'sauce mushroom', 'lemon gel', 'prune marinade', 'roe salmon',
      'chia seed', 'cornflakes', 'erythriol', 'flowers', 'firkorn', 'food coloring', 'food coloring', 'baking soda',
      'gelatin', 'pavlova powder mix', 'quick lunch', 'psyllium husk', 'raspberry jelly', 'rice puffed', 'seltzer',
      'spring roll paper', 'sprinkles', 'wakame', 'wasabi', 'weetabix', 'yeast', 'yeast dry', 'almond essence', 'lemonade'),
      "_IngredientsEqual"),
      # Str detect
      paste0(c("spice mix(?! taco)", "powder mix", "soup instant", "refried beans"), "_strDetectIngredients")
  )

)) %>% lapply(., function(fixList) {

  fixList %>%
    tidyr::separate_wider_delim(.,
                         cols = tmp,
                         names = c("Ingredients", "expressionHelp"),
                         delim = "_",
                         too_few = "align_start")
}) %>%
  bind_rows(.id = "database") %>%
  dplyr::mutate(.by = c(database, expressionHelp),
                expression = case_when(
                  expressionHelp == "IngredientsEqual" ~ paste0(
                    'Ingredients %in% c("', paste0(Ingredients, collapse = '", "'), '")'),
                  expressionHelp == "strDetectIngredients" ~ paste0(
                    'str_detect(Ingredients, "', paste0(Ingredients, collapse = "|"),
                                                                    '")')
                )
  ) %>%
  dplyr::select(-Ingredients) %>% dplyr::distinct() %>%
  dplyr::summarise(.by = database,
                expression = paste0(
                  "(", paste0(expression, collapse = " | "), ") & !str_detect(database_ID, '.999') ~ 0"))


