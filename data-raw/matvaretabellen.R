#Nutrient content database----
#List to fill with various items to keep environment clean
various <- list()

# my.foods is the default name of data downloaded from matvaretabellen selections
# temp <- my.foods %>%
#   rename(
#   Kilojoules = `Energy......Font.Awesome.Free.6.4.2.by..fontawesome...https...fontawesome.com.License...https...fontawesome.com.license.free..Icons..CC.BY.4.0..Fonts..SIL.OFL.1.1..Code..MIT.License..Copyright.2023.Fonticons..Inc.......kJ.`,
#   Kilocalories = `Energy......Font.Awesome.Free.6.4.2.by..fontawesome...https...fontawesome.com.License...https...fontawesome.com.license.free..Icons..CC.BY.4.0..Fonts..SIL.OFL.1.1..Code..MIT.License..Copyright.2023.Fonticons..Inc.......kcal.`) %>%
#   mutate(across(everything(), ~as.character(.))) %>%
#   pivot_longer(.,
#                cols = everything(),
#                names_to = "names_to_change",
#                values_to = "value") %>%
#   # Remove font awesome
#   mutate(names_to_change = str_replace_all(names_to_change, c(
#     "Font.Awesome[a-z0-9\\.A-Z]+" = "",
#     "\\.\\.\\.\\.\\.\\." = ""
#   ))
#   ) %>%
#   # Split names if there are two different naming conventions
#   separate_wider_delim(., cols = names_to_change, delim = stringr::regex("\\.\\."),
#                        names = c("name", "name2", "name3"), too_few = "align_start") %>%
#   # Reformat names similar to the other Matvaretabeller
#   mutate(
#     nutrient = str_replace_all(
#     name, c(
#       "n\\." = "n-",
#       "Saturated.fatty.acids" = "SatFa",
#       "Trans.fatty.acids" = "TransFa",
#       "Monounsaturated.fatty.acids" = "MuFa",
#       "Polyunsaturated.fatty.acids" = "PuFa",
#       "Carbohydrate" = "Carbo",
#       "-(?=(equiv|A|D|E|B|C))" = " ",
#       "Omega.3" = "Omega-3",
#       "Omega.6" = "Omega-6",
#       "Beta.carotene" = "Beta-carotene",
#       "C20:5n-3" = "C20:5n-3 (EPA)",
#       "C22:5n-3" = "C22:5n-3 (DPA)",
#       "C22:6n-3" = "C22:6n-3 (DHA)",
#       "(?<=\\d{2})\\." = ":",
#       "Vitamin-B1" = "Thiamine",
#       "Vitamin-B2" = "Riboflavin",
#       "Vitamin-B3" = "Niacin",
#       "Vitamin-B9" = "Folate",
#       "\\." = " "),
#     nutrient = case_when(
#       name2 == "added" ~ paste0(nutrient, ", added"),
#       name2 == "free" ~ "Mono+Di",
#       TRUE ~ nutrient
#     )
#     ))
#

#
#   %>%
#   rename_with(., ~str_replace_all(., c(
#     "Font.Awesome[a-z0-9\\.A-Z]+" = ""),
#     "......" = ""
#     )

# Get the English food item names
matvaretabellenJSON <- fromJSON(file =  "./inst/extdata/matvaretabellen2024.json", simplify = FALSE)
# Format like the xlsx files
tmp <- matvaretabellenJSON[[1]] %>%
  lapply(., function(x) {

    tibble(
      FoodID = x$foodId,
      `Food Item English` = x$foodName
    )

  } ) %>% bind_rows()

#Load raw data from Matvaretabellen and do somme easy data cleaning
raw_data <- sapply(
  # path to matvaretabellen files from different years
  list.files(path = "./inst/extdata", pattern = "matvaretabellen\\d{4}.xlsx"),
  # Read in data
  function(raw_dataframe) {
    read_xlsx(
      path = paste0("./inst/extdata/", raw_dataframe)
      ) %>%
      # Rename
      # Remove empty rows
      drop_na(contains("Foodgroup")) %>%
      #Remove ref columns from earliest dataframes
      select(!contains('ref'))

  }, USE.NAMES = TRUE, simplify = FALSE)

# Save Foodgroup and foodID
various$foodgroupID <- bind_rows(
  raw_data$matvaretabellen2020.xlsx, raw_data$matvaretabellen2022.xlsx
) %>%
  dplyr::select(FoodID, Foodgroup) %>% distinct()

# Change Norwegian food item names for English for 2024
raw_data$matvaretabellen2024.xlsx <- raw_data$matvaretabellen2024.xlsx %>%
  rename(FoodID = `Matvare ID`) %>%
  left_join(., tmp) %>%
  dplyr::mutate(Matvare = `Food Item English`) %>%
  dplyr::select(-`Food Item English`) %>%
  rename(`Food Item` = Matvare) %>%
  # Change in how foodgroups are added to the datatable, remove the rows from df
  drop_na(`Food Item`)

names(raw_data$matvaretabellen2024.xlsx) <- c(
  "FoodID", "Food Item", "Edible part", "Water",
  "Kilojoules", "Kilocalories", "Fat",
  "SatFa",
  "C12:0", "C14:0", "C16:0", "C18:0",

  "TransFa",
  "MuFa",
  "C16:1 sum", "C18:1 sum",
  "PuFa",
  "C18:2n-6", "C18:3n-3", "C20:3n-3", "C20:3n-6", "C20:4n-3", "C20:4n-6", "C20:5n-3 (EPA)", "C22:5n-3 (DPA)", "C22:6n-3 (DHA)", "Omega-3", "Omega-6",
  "Cholesterol",

  "Carbo", "Starch", "Sugar", "Sugar, added", "Mono+Di", "Dietary fibre",

  "Protein",

  "Alcohol",

  "Vitamin A", "Vitamin A RE", "Beta-carotene", "Retinol", "Vitamin D", "Vitamin E",

  "Thiamin", "Riboflavin", "Niacin", "Niacin equivalents", "Vitamin B6", "Folate", "Vitamin B12",
  "Vitamin C",

  "Calcium", "Potassium", "Sodium", "Salt",  "Phosphorus", "Magnesium",  "Iron",
  "Copper", "Zinc", "Selenium", "Iodine"
)

#Foods removed from the 2022 edition
various$removed_from_2022 <- raw_data$matvaretabellen2020.xlsx %>%
  filter(!FoodID %in% raw_data$matvaretabellen2022.xlsx$FoodID)%>%
  filter(!FoodID %in% c(raw_data$matvaretabellen2024.xlsx$FoodID))
# Foods removed from 2024 edition
various$removed_from_2024 <- raw_data$matvaretabellen2022.xlsx %>%
  filter(!FoodID %in% c(raw_data$matvaretabellen2024.xlsx$FoodID, raw_data$matvaretabellen2020.xlsx))

#Join the Matvaretabellen-dataframes together, use the latest edition but with the added foods that have been dropped from previous versions
raw_data <- bind_rows(
  raw_data$matvaretabellen2024.xlsx,
  various$removed_from_2022,
  various$removed_from_2024) %>%
  distinct() %>%
  # Add foodgrouås
  dplyr::select(-Foodgroup) %>%
  left_join(., various$foodgroupID) %>%
  # Add fodgroups to foods that don't have them
  mutate(Foodgroup = case_when(

    is.na(Foodgroup) & str_detect(`Food Item`,"^Yoghurt") ~ "Yoghurt",
    is.na(Foodgroup) & str_detect(`Food Item`,"Grana padano") ~ "Cheese, extra fat",
    is.na(Foodgroup) & str_detect(`Food Item`,"^Cocoa, prepared from|^Hot chocolate") ~ "Milk and milk based beverages",

    # Predominantly grains
    is.na(Foodgroup) & str_detect(`Food Item`,"porridge") ~ "Porridge",
    (is.na(Foodgroup) & str_detect(`Food Item`,"flour mix|Rice flour|Oatmeal|Gram flour")) |
      `Food Item` %in% c("Quinoa flour")
      ~ "Flour",
    (is.na(Foodgroup) & str_detect(`Food Item`,"flakes|Muesli|Breakfast cereal")) ~ "Breakfast cereals, muesli",
    is.na(Foodgroup) & str_detect(`Food Item`,"Pasta, fresh|Noodles") &
      str_detect(`Food Item`, "uncooked") ~ "Grain, rice, pasta, raw",
    is.na(Foodgroup) & str_detect(`Food Item`,"Pasta, fresh|Noodles|sushi rice") &
      str_detect(`Food Item`, "\\bcooked") ~ "Grain, rice, pasta, prepared",
    (is.na(Foodgroup) & str_detect(`Food Item`,"Baguette|Borek|Calzone|Croissant|Muesli (bread|roll)|Panini|^Roll") &
      str_detect(`Food Item`, "from cafe/bakery")) |
      `Food Item` %in% c(
        "Swiss roll, with chocolate filling", "Bruschetta, with tomato and basil"
        ) ~ "Bread, rolls etc, industry made",
    is.na(Foodgroup) & str_detect(`Food Item`,"Beans|Lentils, green|Lentils, black") ~ "Legumes",
    is.na(Foodgroup) & str_detect(`Food Item`,"Mixed nut") ~ "Nuts, almonds and seeds",

    # Protein
    is.na(Foodgroup) & str_detect(`Food Item`,"Moose|Reindeer") &
      str_detect(`Food Item`, "raw") ~ "Other meats, minced, offal, raw",
    is.na(Foodgroup) & str_detect(`Food Item`,"Moose|Reindeer|Kebab") &
      str_detect(`Food Item`, "roasted|smoked|prepared") ~ "Other meats, minced, offal, prepared",
    is.na(Foodgroup) & str_detect(`Food Item`,"Lamb") &
      str_detect(`Food Item`, "raw") ~ "Lamb, mutton, raw",
    is.na(Foodgroup) & str_detect(`Food Item`,"Lamb") &
      str_detect(`Food Item`, "roasted|fried") ~ "Lamb, mutton, prepared",
    is.na(Foodgroup) & str_detect(`Food Item`,"Pork|Svin") &
      str_detect(`Food Item`, "raw") ~ "Pork, raw",
    is.na(Foodgroup) & str_detect(`Food Item`,"Pork") &
      str_detect(`Food Item`, "roasted|fried|boiled") ~ "Pork, prepared",
    is.na(Foodgroup) & str_detect(`Food Item`,"Chicken") &
      str_detect(`Food Item`, "roasted|fried") ~ "Poultry, prepared",
    `Food Item` == "Salad, with chicken and feta cheese, from cafe/bakery" ~ "Dishes with poultry or meat",
    is.na(Foodgroup) & str_detect(`Food Item`,"Mussel|Lobster|Shrimp|Snow crab|King crab") ~ "Shellfish, fish offal",
    `Food Item` %in% c("Soup, sauce/gravy, stew base") |
      str_detect(`Food Item`, "Ceviche") ~ "Fish products, prepared",

    is.na(Foodgroup) & str_detect(`Food Item`,"Cusk") & str_detect(`Food Item`, "simmer|roast") ~ "Lean fish, prepared",
    is.na(Foodgroup) & str_detect(`Food Item`,"Herring") & str_detect(`Food Item`, "raw") ~ "Fatty fish, raw",
    is.na(Foodgroup) & str_detect(`Food Item`,"Herring") & str_detect(`Food Item`, "simmer|roast|fried") ~ "Fatty fish, prepared",

    # Predominantly sweets
    is.na(Foodgroup) & str_detect(`Food Item`,"Ice cream|mousse|Frozen ice dessert, plant-based") ~ "Dessert, ice cream etc",
    is.na(Foodgroup) & str_detect(`Food Item`,"Cookie") ~ "Cookies, sweet biscuits, rusks",
    is.na(Foodgroup) & str_detect(`Food Item`,"^Pie,|Pizza, white|Pancakes") ~ "Pizza, pie, taco etc",

    # Fruit veg
    is.na(Foodgroup) & str_detect(`Food Item`,"jam") ~ "Fruit and berry products",
    is.na(Foodgroup) & str_detect(`Food Item`,"Avocado|Pineapple|berries|Berries|Pomegranate|Mango|Apple|currants") ~ "Fruit and berries, raw/fresh",
    is.na(Foodgroup) & str_detect(`Food Item`,"Kale|Parsnip|Celeriac|Mushroom|Spinach|Nettle") &
      str_detect(`Food Item`, "steamed|boiled|cooked") ~ "Vegetables, prepared",
    is.na(Foodgroup) & str_detect(`Food Item`,"Leek, frozen|Cauliflower|Kale|Parsnip|Celeriac|Mushroom|Spinach|Nettle|Wakame salad|Purslane") ~ "Vegetables, raw and frozen",
    is.na(Foodgroup) & str_detect(`Food Item`,"Spinach|Pea|Parsley") &
      str_detect(`Food Item`, "cream|puree") ~ "Vegetable products",
    is.na(Foodgroup) & str_detect(`Food Item`,"Potatoes") ~ "Potatoes",

        # Combined dishes
    is.na(Foodgroup) & str_detect(`Food Item`,"Soup|soup|sauce|Pesto") ~ "Soup, sauce/gravy, stew base",
    is.na(Foodgroup) & str_detect(`Food Item`,"Plant-based product") ~ "Vegetarian products and dishes",

    # Div
    is.na(Foodgroup) & str_detect(`Food Item`,"Delfia|Margarine") ~ "Oil, frying fat etc",
    is.na(Foodgroup) & str_detect(`Food Item`,"Juice|juice|Lemonade|Smoothie|Slush") ~ "Juice, fruit drink, soda etc",
    is.na(Foodgroup) & str_detect(`Food Item`,"Americano|Frappe") ~ "Juice, fruit drink, soda etc",
    is.na(Foodgroup) & str_detect(`Food Item`,"Mint|spice mix|Cumin seed") ~ "Vegetarian products and dishes",
    is.na(Foodgroup) & str_detect(`Food Item`,"syrup|Cocoa bean|Nutritional yeast") ~ "Miscellaneous ingredients",
    is.na(Foodgroup) & str_detect(`Food Item`,"Lentil crisps|Cheeze doodles") ~ "Snacks",
    is.na(Foodgroup) & str_detect(`Food Item`,"Chocolate, (dark|filled|milk)") ~ "Snacks",

    TRUE ~ Foodgroup
  ))


#Clean it up and use means for items with more than one addition (such as vegetables with both Norwegian and Imported values)----
clean_nutrients <- raw_data %>%

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
    'pizza, industrially made', 'breakfast cereal, wheat, barley, rye, oat, no sugar, 4-korn',
    'chocolate, snickers', 'ice cream, dairy', 'chocolate, dark, 70 % cocoa', 'tart shell, no filling',
    'puffed oats', 'puffed wheat', 'puffed rice', 'corn flakes, kelloggs')) |
           !str_detect(Foodgroup,
                       'dessert|other meat products, prepared|other meats, minced, offal, prepared|egg, prepared|cookies|cod liver oil|homemade|chocolate|instant|cake|breakfast cereals|porridge|pizza')) %>%

  #Rename some ingredients----
  mutate(Ingredients = case_when(

  #Beef----
  str_detect(food_item, 'beef') & str_detect(food_item, 'bottom round') ~ 'beef_bottom round',
  str_detect(food_item, 'beef') & str_detect(food_item, 'inside round') ~ 'beef_inside round',
  str_detect(food_item, 'beef') & str_detect(food_item, 'blood') ~ 'beef_blood',
  str_detect(food_item, 'beef') & str_detect(food_item, 'chuck roll') ~ 'beef_chuck roll',
  (str_detect(food_item, 'beef') & str_detect(food_item, 'shoulder') & str_detect(food_item, 'roast')) | food_item == "beef, clod (shoulder), foreshank removed, raw" ~ 'beef_shoulder',
  str_detect(food_item, 'beef') & str_detect(food_item, 'brisket') ~ 'beef_brisket',
  str_detect(food_item, 'beef') & str_detect(food_item, 'tenderloin') ~ 'beef_tenderloin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'striploin') ~ 'beef_striploin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'sirloin') & str_detect(food_item, 'butt') ~ 'beef_sirloin',
  str_detect(food_item, 'beef') & str_detect(food_item, 'rib-eye steak, raw') ~ 'beef_rib-eye steak',
  str_detect(food_item, 'beef') & str_detect(food_item, 'liver') ~ 'beef_liver/veal_liver',
  str_detect(food_item, 'beef, trimmed fat, raw') ~ 'tallow_beef',
  (str_detect(food_item, 'beef') & str_detect(food_item, 'minced meat') & str_detect(food_item, '6 %')) |
    food_item %in% c('beef, minced meat, 4,5 % fat, raw') ~ 'beef_minced meat lean',
  str_detect(food_item, 'beef') & str_detect(food_item, 'minced meat') &
    str_detect(food_item, '14 %|13 %') & str_detect(food_item, 'without') ~ 'beef_minced meat', # Default

  #food_item == 'beef, minced meat, without salt and water, raw' ~ 'beef minced meat',
  food_item == 'veal, for roast, raw' ~ 'beef_veal for roast',
  food_item == 'beef, roast of nuckle, raw' ~ 'beef_roast of knuckle',
  food_item == 'veal, chops, raw' ~ 'beef_veal chops',

  #lamb----
  food_item == 'lamb, for stewing, raw' ~ 'lamb_stew meat',
  food_item == 'lamb, breast and skirt, with bone, raw' ~ 'lamb_breast skirt',
  food_item == 'lamb, shoulder, for roast, raw' ~ 'lamb_shoulder',
  food_item == 'lamb, for mutton and cabbage stew (fårikål), raw' ~ 'lamb_cabbage stew meat',
  food_item == 'mutton, for stewing, cured, raw' ~ 'mutton_cabbage stew meat',
  food_item %in% c('lamb, chops, with fat, raw', 'lamb, chops, raw') ~ 'lamb_chop',
  food_item == 'lamb, leg, for roast, raw' ~ 'lamb_leg roast',
  food_item == 'lamb, leg, cured, dried, smoked' ~ 'lamb_leg smoked/lamb_cured leg',
  food_item == 'lamb, rib, cured, dried, smoked, raw' ~ 'lamb_cured rib',
  food_item == 'lamb, rib, raw' ~ 'lamb rib',
  food_item == 'lamb, tenderloin, raw' ~ 'lamb_tenderloin',
  food_item == 'lamb, striploin, raw' ~ 'lamb_striploin',
  food_item == 'lamb, sirloin, raw' ~ 'lamb_sirloin',
  food_item == "lamb, trimmed fat, raw" ~ "tallow_lamb",
  food_item == "lamb, minced meat, 14 % fat, raw" ~ "lamb_minced meat",
  food_item == "lamb, inside round, raw" ~ "lamb_inside round",
  food_item == 'liver, lamb, raw' ~ 'lamb_liver',
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
  food_item %in% c('pork, minced meat, max 23 % fat, raw', 'pork, minced meat, 9 % fat, raw') ~ 'pork_minced meat',
  food_item == 'pork, bacon, with rind, raw' ~ 'bacon',
  food_item == 'pork, shoulder, with fat, for roast, raw' ~ 'pork_shoulder',
  food_item == 'svin, bottom round, raw' ~ 'pork_bottom round',
  food_item == 'svin, silverside, raw' ~ 'pork_silverside',
  food_item == 'pork, grillbones, spare ribs, raw' ~ 'pork_spare rib',
  food_item == 'pork, tenderloin, raw' ~ 'pork_tenderloin',
  food_item == 'pork, striploin, raw' ~ 'pork_striploin',
  food_item == 'pork scratchings' ~ 'bacon_crisp',
  food_item == 'ham, cured' ~ 'ham_cured',
  food_item == 'ham, smoke-cured' ~ 'ham_smoked',
  food_item == 'ham, boiled' ~ 'ham',
  food_item == 'sausage, salami' ~ 'salami',
  food_item == 'sausage, grill' ~ 'sausage_grill',
  food_item == 'sausage, chorizo' ~ 'sausage_chorizo',
  food_item == 'sausage, swedish, falukorv' ~ 'sausage_vossa', #Similar in nutrients
  food_item == 'sausage, meat, gilde' ~ 'sausage', #Standard
  food_item == 'sausage, frankfurter/wiener' ~ 'sausage_wiener',
  food_item == 'pepperoni' ~ 'sausage_pepperoni',
  food_item == 'plant-based minced' ~ 'minced meat_plant-based',
  food_item == 'plant-based burger' ~ 'hamburger_plant-based',
  food_item == 'plant-based balls' ~ 'meatball_plant-based',
  food_item == 'plant-based grill sausage' ~ 'sausage_plant-based',

  #Poultry----
  food_item == 'chicken, leg (thigh and drumstick), with skin, raw' ~ 'chicken_thigh',
  food_item == 'chicken, with skin, raw' ~ 'chicken_whole',
  food_item == 'chicken, fillet, without skin, raw' ~ 'chicken_breast',
  food_item == 'chicken, drumstick, with skin, raw' ~ 'chicken_drumstick',
  food_item == 'liver, chicken, raw' ~ 'chicken_liver',
  food_item %in% c('chicken, minced meat, raw', 'chicken, minced meat, 8 % fat, raw') ~ 'chicken_minced meat',
  food_item == 'egg white, raw' ~ 'egg_white',
  food_item == 'egg yolk, raw' ~ 'egg_yolk',
  food_item == 'sausage, grill, turkey and chicken' ~ 'sausage_turkey chicken',
  food_item == 'duck, meat and skin, raw' ~ 'duck',
  food_item == 'goose, meat and skin, raw' ~ 'goose',
  food_item == 'hen, fillet, raw' ~ 'hen_fillet',
  food_item == 'turkey, breast, without skin, raw' ~ 'turkey_breast',
  food_item == 'ham, turkey, smoked' ~ 'turkey_ham',
  food_item == 'turkey, meat and skin, raw' ~ 'turkey_meat',
  food_item == 'grouse, breast, without skin, raw' ~ 'grouse_breast',
  food_item == "grouse, meat, raw" ~ "grouse",

  #Game meat----
  food_item == 'moose, roasting, raw' ~ 'elk moose',
  food_item == 'moose, chuck, raw' ~ 'elk moose_chuck',
  food_item == 'moose, minced, raw' ~ 'elk moose_minced meat',
  food_item == 'moose, sirloin, raw' ~ 'elk moose_sirloin',
  food_item == 'reindeer, roasting, raw' ~ 'reindeer',
  food_item == 'liver, reindeer, raw' ~ 'reindeer_liver',
  food_item == 'reindeer, shoulder, roasting, raw' ~ 'reindeer_shoulder',
  food_item == 'roe deer, meat, raw' ~ 'roe deer',
  food_item == 'whale, raw' ~ 'whale',

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
  food_item == 'snow crab, boiled' ~ 'crab_snow',
  food_item == 'cod, split, salted and dried' ~ 'cod_clipfish',
  food_item == 'stockfish, dried fish' ~ 'cod_dried',
  food_item == 'fish, alkaline cured, dried, lutefisk' ~ 'lutefisk',
  food_item == 'crab, boiled' ~ 'crab',
  food_item == 'saithe, raw' ~ 'saithe',
  food_item == 'powan, raw' ~ 'powan',
  food_item == 'salmon fillet, dry salted, with sugar and spices' ~ 'trout_cured', #Similar nutritional profile
  food_item == 'lobster, boiled' ~ 'lobster',
  food_item %in% c('mackerel, may-june, raw', 'mackerel, february - june, raw') ~ 'mackerel',
  food_item == 'mussel, blue, raw' ~ 'mussel',
  food_item == 'nori, seaweed, dried' ~ 'nori_seaweed',
  food_item == 'salmon, farmed, raw' ~ 'salmon',
  food_item == 'salmon, smoked' ~ 'salmon_smoked',
  food_item == 'sprat, raw' ~ 'sardine',
  food_item == 'sprat in oil, drained, canned' ~ 'sardine_in oil',
  food_item == 'sprat in tomato sauce, canned' ~ 'sardine_in tomato',
  food_item == 'scallion, spring onion, raw' ~ 'scallion',
  food_item == 'oyster, common, raw' ~ 'oyster',
  food_item == 'shrimps, in brine, drained' ~ 'shrimp_in brine',
  #food_item == 'shrimps, northern, boiled' ~ 'shrimp',
  food_item == 'shrimps, raw' ~ 'shrimp',
  food_item == 'trout, farmed, raw' ~ 'trout',
  food_item == 'tuna in water, drained, canned' ~ 'tuna_in water canned',
  food_item == 'tuna, in oil, canned' ~ 'tuna_in oil canned',
  food_item == 'halibut, atlantic' ~ 'halibut',
  food_item == 'mackerel fillet, in tomato sauce, canned' ~ 'mackerel_tomato canned',
  food_item == 'mackerel, warm smoked' ~ 'mackerel_smoked',
  food_item == 'cusk, tusk, raw' ~ 'cusk_tusk',
  food_item == "dover sole, raw" ~ 'sole_dover',
  food_item == 'cod roe, raw' ~ 'cod_roe',
  food_item == 'cod liver, raw' ~ 'cod_liver',
  food_item == 'herring, pickled, drained' ~ "herring_pickled",
  food_item == 'lemon sole, raw' ~ 'flounder',
  food_item == 'conger eel, raw' ~ 'eel_conger',
  food_item == 'eel, raw' ~ 'eel',

  #Plant-based
  food_item == 'plant-based minced, soy protein' ~ 'minced meat plant-based_soy',
  food_item == 'plant-based minced, pea protein, naturli' ~ 'minced meat plant-based_pea',
  food_item == 'plant-based product, used as cream cheese, oat based' ~ 'cream cheese_plant-based',
  food_item == 'plant-based product, used as ham' ~ 'ham_plant-based',

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
  food_item == 'poppy seeds' ~ 'seed_poppy',

  #Fruit and vegetables----
  database_ID == '06.114' ~ 'sprout_alfalfa',
  food_item == 'apples, dried' ~ 'apples_dried',
  food_item == 'apples, canned' ~ 'apples_canned',
  food_item == 'apple jam' ~ 'jam_apple',
  food_item == 'apricots, dried' ~ 'apricot_dried',
  food_item == 'apricot jam' ~ 'jam_apricot',
  food_item == 'asparagus, canned, drained' ~ 'apricot_canned',

  food_item == 'banana, dried' ~ 'banana_dried',
  food_item == 'bamboo shoots, canned' ~ 'bamboo shoots',
  food_item == 'yardlong beans, raw' ~ 'bean green_asparagus',
  food_item == 'beans, green, french, raw' ~ 'bean green',
  food_item == 'beetroot, norwegian, raw' ~ 'beetroot',
  food_item == 'beetroot, pickled' ~ 'beet_pickled',
  food_item == 'blackcurrants, raw' ~ 'black currant',
  food_item == 'blackberry jam' ~ 'jam_blackberry',
  food_item == 'blueberry jam' ~ 'jam_blueberry',
  food_item == 'redcurrants, raw' ~ 'currant',
  food_item == 'brussel sprouts, norwegian, raw' ~ 'brussel_sprout',

  food_item %in% c(
    'cabbage, white, raw',
    'cabbage, white, norwegian, raw') ~ 'cabbage', #Standard
  food_item == 'cabbage, pak-choi, bok choy, raw' ~ 'cabbage_bok choi',
  food_item == 'cabbage, red, raw' ~ 'cabbage_red',
  food_item == 'cabbage, spring green, raw' ~ 'cabbage_spring green',
  food_item == 'cabbage, chinese, norwegian, raw' ~ 'cabbage_chinese',
  food_item == 'cape gooseberries, raw' ~ 'physalis',
  food_item == 'carambola, raw' ~ 'starfruit',
  food_item == 'cherry jam' ~ 'jam_cherry',
  food_item == 'cloudberry jam' ~ 'jam_cloudberry',

  food_item == 'cauliflower, norwegian, raw' ~ 'cauliflower',
  food_item %in% c(
  'celariac root, norwegian, raw', 'celeriac root, raw') ~ 'celariac_root',
  food_item == 'celery stalk or stem, norwegian, raw' ~ 'celery_stalk',
  food_item == 'chicory, raddichio, raw' ~ 'chicory_red',
  food_item == 'chicory, endive, raw' ~ 'chicory_white',

  food_item == 'dates, dried' ~ 'dates_dried',

  food_item == 'pepper, chili, green, raw' ~ 'chili pepper_green',
  food_item == 'pepper, chili, red, raw' ~ 'chili pepper_red',
  food_item == 'jalapeño, raw' ~ 'chili pepper_jalapeno',

  food_item == 'aubergine, raw' ~ 'eggplant',

  food_item == 'goji berries, dried' ~ 'goji berry',

  food_item == 'horse-radish, raw' ~ 'horseradish',

  food_item == 'kiwi fruit, raw' ~ 'kiwi',
  food_item == 'kohlrabi, raw' ~ 'swede',

  food_item == 'baby corn, canned' ~ 'corn_baby',
  food_item == 'sweet corn, norwegian, raw' ~ 'corn_cob',
  food_item == 'cucumber, norwegian, raw' ~ 'cucumber',
  food_item == 'cucumber, pickled' ~ 'cucumber_pickled',

  food_item == 'figs, dried' ~ 'fig_dried',
  food_item == "potatoes, french fries, frozen" ~ "french fries",

  food_item == 'tomato purée' ~ 'tomato_puree',
  food_item == 'tomato, small, cherry, imported, raw' ~ 'cherry_tomato',
  food_item == 'beans, white, in tomato sauce, canned' ~ 'bean_tomato', #White beans
  food_item == 'tomatoes, sun-dried' ~ 'tomato_sun dried',
  food_item == 'tomato, canned' ~ 'tomato_canned',

  food_item == 'leek, norwegian, raw' ~ 'leek',
  food_item == 'lemon juice, bottled' ~ 'lemon_juice',
  food_item == 'lemon peel' ~ 'lemon_zest',
  food_item == 'leaf beet, mangold, raw' ~ 'mangold',
  food_item == 'cherries, sweet, raw' ~ 'morel',
  food_item == 'lingonberries, cowberries, raw' ~ 'lingonberry',
  food_item == 'mango, dried' ~ 'mango_dried',
  food_item == 'olives, black, in oil, canned' ~ 'olive_black',
  food_item == 'olives, green, pickled' ~ 'olive_green', #Standard
  food_item == 'onion, norwegian, raw' ~ 'onion',
  food_item == 'orange peel, raw' ~ 'orange_zest',

  food_item == 'parsley root, norwegian, raw' ~ 'parsley_root',
  food_item == 'pears, canned, in natural juice' ~ 'pear_canned',
  food_item == 'peas, sugar-snap, norwegian, raw' ~ 'pea_sugar snap',
  food_item == 'peas, frozen' ~ 'pea',
  food_item == 'persimmon, kaki fruit, raw' ~ 'persimmon',
  food_item == 'pineapple, canned, in natural juice' ~ 'pineapple_canned',
  food_item == 'pineapple, dried' ~ 'pineapple_dried',
  food_item == 'potato flatbread, soft, lompe' ~ 'potato flatbread lompe',
  food_item == "potato crisps" ~ "chips_potato",
  food_item == "lentil crisps" ~ "chips_lentils",
  food_item == 'potatoes, storage, raw' ~ 'potato',
  food_item == 'prunes' ~ 'prune',

  food_item == 'radish, norwegian, raw' ~ 'radish',
  food_item == 'raisins' ~ 'raisin',
  food_item == 'raspberry jam' ~ 'jam_raspberry',
  food_item == 'rowan-berries, raw' ~ 'rowan berry',
  food_item == 'strawberry jam' ~ 'jam_strawberry',
  food_item == 'salad, rocket, raw' ~ 'salad_rocket',
  food_item == 'romaine lettuce' ~ 'salad_romaine',
  food_item == 'lettuce leaves, norwegian, raw' ~ 'salad', #Standard
  food_item == 'squash, zucchini, raw' ~ 'summer squash_zucchini',
  food_item == 'sweet pepper, red, raw' ~ 'sweet pepper_red',
  food_item == 'sweet pepper, yellow/orange, raw' ~ 'sweet pepper_yellow',
  food_item == 'sweet pepper, green, raw' ~ 'sweet pepper_green',
  food_item == 'turnip, norwegian, raw' ~ 'turnip',
  food_item == 'water chestnut, raw' ~ 'chestnut_water',
  food_item == 'melon, water, raw' ~ 'watermelon',
  food_item == 'pumpkin, raw' ~ 'winter squash', #Standard
  food_item == 'pumpkin, butternut, raw' ~ 'winter squash_butternut',
  food_item == 'pumpkin, hokkaido, raw' ~ 'winter squash_hokkaido',
  food_item == 'orange juice, from concentrate' ~ 'orange_juice', #Use as standard
  food_item =='passion fruit juice' ~ 'passion fruit_juice',
  food_item == 'cranberry juice' ~ 'cranberry_juice',
  food_item %in% c('apple juice', 'pineapple juice', 'grape juice',
                   'grapefruit juice', 'orange juice', 'tomato juice', 'tomato ketchup') ~ str_replace(food_item, ' ', '_'),
  food_item == 'sweet corn, canned' ~ 'sweet corn_canned',
  food_item == 'grapes, unspecified, raw' ~ 'grapes',
  food_item == 'peaches, canned, in syrup' ~ 'peach_canned',
  food_item == 'cranberries, dried, sweetened' ~ 'cranberries_dried',
  food_item == 'fruit juice drink, blackcurrant, ready-to-drink' ~ 'black currant_juice',
  food_item == 'onion, roasted' ~ 'onion_fried',

  food_item == 'melon, cantaloupe, raw' ~ 'melon_cantaloupe',
  food_item == 'melon, honeydew, raw' ~ 'melon_honeydew',
  food_item == 'melon, galia, raw' ~ 'melon_galia',
  food_item == 'nashi, asian pear, raw' ~ 'pear_asian',

  food_item == 'wakame salad' ~ 'wakame_salad',

  #Grains----
  food_item == 'adzuki beans, uncooked' ~ 'bean_adzuki',
  food_item == 'almonds' ~ 'almond',
  food_item == 'broad beans, uncooked' ~ 'bean_broad',
  food_item == 'beans, black, canned'  ~ 'bean_black canned',
  food_item == 'beans, brown, uncooked'  ~ 'bean_brown dry',
  food_item == 'beans, brown, cooked'  ~ 'bean_brown cooked',
  food_item == 'beans, red (kidney), canned' ~ 'bean_kidney canned',
  food_item == 'beans, red (kidney), uncooked' ~ 'bean_kidney',
  food_item == 'lima beans, uncooked'  ~ 'bean_lima dry',
  food_item == 'cowpeas, blackeyes beans, uncooked'  ~ 'bean_blackeyed dry',
  food_item == 'millet, grain' ~ 'millet',
  food_item == 'mung beans, sprouted, raw' ~ 'bean_sprout', #Standard
  food_item == 'beans, white, large, canned' ~ 'bean_white canned',
  food_item == 'beans, white, uncooked' ~ 'bean_white',
  food_item == 'beans, soya, uncooked' ~ 'bean_soya',
  food_item == 'beans, mung, uncooked' ~ 'bean_mung',
  food_item == 'breakfast cereal, wheat, barley, rye, oat, no sugar, 4-korn' ~ 'firkorn',
  food_item == 'flatbread, hard' ~ 'bread flat hard',
  food_item == 'bulgur, uncooked' ~ 'bulgur_wheat',
  food_item == 'cashew nuts, salted' ~ 'cashew nut salt',
  food_item == 'peanuts, raw' ~ 'peanut', #These seem to be shelled
  food_item == 'peanuts, roasted, salted' ~ 'peanut_salt',
  food_item == 'peas, chick peas, uncooked' ~ 'chick pea',
  food_item == 'peas, chick peas, canned' ~ 'chick pea_canned',
  food_item == 'corn starch' ~ 'corn_starch',
  food_item == 'cornmeal' ~ 'corn_flour',
  food_item == 'corn flakes, kelloggs' ~ 'cornflakes',
  food_item == 'couscous, uncooked' ~ 'couscous',
  food_item == 'couscous, cooked' ~ 'couscous_cooked',
  food_item == 'cracker, cream cracker' ~ 'cracker_cream',
  food_item == 'crisp bread, wholemeal flour, rye, husman' ~ 'crisp bread_coarse',
  food_item == 'edamame, soy beans, frozen' ~ 'edamame',
  food_item == 'noodles, with egg, uncooked' ~ 'noodle_egg',
  food_item == 'noodles, cooked' ~ 'noodle_cooked',
  food_item == 'noodles, cellophane, uncooked' ~ 'noodle_glass',
  food_item == 'noodles, cellophane, glass, cooked' ~ 'glass noodle_cooked',
  food_item %in% c('pasta, plain, macaroni, spaghetti etc., uncooked', 'pasta, plain, uncooked') ~ 'pasta',
  food_item == 'pasta, plain, cooked' ~ 'pasta_cooked',
  food_item == 'pasta, whole-grain, uncooked' ~ 'pasta_whole grain',
  food_item == 'pasta, whole grain, cooked with salt' ~ 'pasta_whole grain cooked',
  food_item == 'pasta, plain, fresh, uncooked' ~ 'pasta_fresh',
  food_item == 'rice, jasmin, uncooked' ~ 'rice_jasmin',
  food_item %in% c('lentils, green and brown, uncooked', 'lentils, green, uncooked') ~ 'lentils dried_green',
  food_item %in% c('lentils, red/pink, uncooked', 'lentils, red, uncooked') ~ 'lentils dried_red',
  food_item %in% c('lentils, green, puy, uncooked') ~ 'lentil_puy',
  food_item %in% c('lentils, black, beluga, uncooked') ~ 'lentil_beluga',
  food_item == 'lentils, green, canned' ~ 'lentils canned', # default
  food_item == 'lentils, red, canned' ~ 'lentils canned_red',
  food_item == 'lima beans, uncooked' ~ 'bean_lima raw',
  food_item == 'squash seeds, pumpkin seeds' ~ 'pumpkin_seed',
  food_item == 'quinoa, white, uncooked' ~ 'quinoa', # Default
  food_item == 'quinoa, red, uncooked' ~ 'quinoa_red',
  food_item == 'quinoa, black, uncooked' ~ 'quinoa_black',
  food_item == 'noodles, rice, uncooked' ~ 'noodle_rice',
  food_item == 'rice, basmati, uncooked' ~ 'rice_basmati',
  food_item == 'rice, brown, long-grain, uncooked' ~ 'rice brown long grain',
  food_item == 'rice, arborio, risotto rice, uncooked' ~ 'rice_risotto',
  food_item == 'rice, white, long-grain, uncooked' ~ 'rice white long grain',
  food_item == 'rice, white, pre-boiled, uncooked' ~ 'rice parboiled',
  food_item == 'rice, white, short-grain, for porridge, uncooked' ~ 'rice_porridge',
  food_item == 'rolls, white, industrially made' ~ 'rolls white',
  food_item == 'sesame seeds, without shell' ~ 'sesame_seed',
  food_item == 'tortilla chips' ~ 'nacho',
  food_item == 'wheat flour, 80 % extraction' ~ 'wheat flour',
  food_item == 'wheat flour, wholemeal' ~ 'wheat flour_wholemeal',
  food_item == 'rye flour' ~ 'wheat flour_rye',
  food_item == 'rye flour, wholemeal' ~ 'wheat flour rye_wholemeal',
  food_item == 'semolina, wheat meal' ~ 'wheat flour_semolina',
  food_item == 'spelt flour' ~ 'wheat flour_spelt',
  food_item %in% c('hamburger bun', 'peanut butter', 'potato starch') ~ str_replace(food_item, ' ', '_'),
  food_item == 'oatmeal' ~ 'oatmeal',
  food_item == 'rolled oats' ~ 'oat_rolled',
  food_item == 'sunflower seeds' ~ 'sunflower_seed',
  food_item == 'linseeds, flax seeds, crushed' ~ 'flax_seed',
  food_item == 'pecan nuts' ~ 'pecan_nut',
  food_item == 'brazil nuts' ~ 'brazil_nut',
  food_item == 'hazelnuts' ~ 'hazelnut',
  food_item == 'macadamia nuts, raw' ~ 'macademia_nut',
  food_item == 'chestnuts, roasted' ~ 'chestnut_roasted',
  food_item == 'wheat bran, regal' ~ 'wheat bran',
  food_item == "oatbran" ~ 'oat_bran',
  food_item == 'walnuts' ~ "walnut",
  food_item == "cashew nuts" ~ "cashew nut",
  food_item == "pine nuts" ~ "pine nut",
  food_item == "puffed rice" ~ "rice_puffed",
  food_item == "puffed oats" ~ "oat_puffed",
  food_item == "puffed wheat" ~ "wheat_puffed",
  food_item == 'tart shell, no filling' ~ 'tart shell',
  food_item == 'pistachio nuts' ~ 'pistachio nut',
  food_item == 'pistachio nuts, roasted, with salt' ~ 'pistachio nut_salt',
  food_item == 'barley flour' ~ 'wheat flour_barley',
  food_item == 'barley, uncooked' ~ 'barley',
  food_item == 'pearled barley' ~ 'barley_pearl',
  food_item == 'barley, cooked' ~ 'barley_cooked',
  food_item == 'pearled spelt' ~ 'spelt_pearl',
  food_item == 'psyllium husk' ~ 'psyllium husk',
  food_item == 'buckwheat, grain' ~ 'buckwheat',
  food_item == 'buckwheat, flour' ~ 'buckwheat_flour',
  food_item == 'bulgur, cooked' ~ 'bulgur_cooked',
  food_item == 'wheat, grain' ~ 'wheat_whole',
  food_item == 'wheatgerm' ~ 'wheat_germ',
  food_item == 'gram flour' ~ 'flour_gram',
  food_item == 'coconut flour' ~ 'flour_coconut',
  food_item == 'almond flour' ~ 'flour_almond',
  food_item == 'quinoa flour' ~ 'flour_quinoa',
  food_item == 'rice flour' ~ 'flour_rice',
  food_item == 'soy flour' ~ 'flour_soy',
  food_item == 'teff flour' ~ 'flour_teff',
  food_item == 'sorghum' ~ 'sorghum',
  food_item == 'rice, wild rice, uncooked' ~ 'rice_wild',
  food_item == 'chia seeds, dried' ~ 'seed_chia',


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
  food_item == 'oil, wheatgerm' ~ 'wheatgerm_oil',
  food_item == 'oil, coconut' ~ 'coconut_oil',
  food_item == 'oil, linseed' ~ 'flax_oil',
  food_item == 'oil, corn' ~ 'corn_oil',
  food_item == 'oil, palm' ~ 'palm_oil',
  food_item == 'oil, rapeseed, cold pressed, odelia' ~ 'vegetable_oil', #Standard
  food_item == 'oil, walnut' ~ 'walnut_oil',
  food_item == 'cocoa butter' ~ 'cocoa_butter',

  #Dairy and substitutes----
  food_item == 'butter' ~ 'butter',
  food_item == 'butter, unsalted' ~ 'butter_unsalted',
  food_item == 'cheese, blue mold, norzola' ~ 'cheese blue_norzola',
  food_item == 'cheese, blue mold, normanna' ~ 'cheese blue_normanna',
  food_item == 'cheese, blue mold, gorgonzola' ~ 'cheese blue_gorgonzola',
  food_item == 'cheese, blue mold, roquefort' ~ 'cheese blue_roquefort',
  food_item == 'cheese, blue mold, selbu blå' ~ 'cheese blue_selbu',
  food_item == 'cheese, stilton, blue' ~ 'cheese blue_stilton',
  food_item == 'cheese, blue mold, royal blue' ~ 'cheese blue_royal blue',
  food_item == 'cheese, ripened, brie' ~ 'cheese_brie',
  food_item == 'cheese, whey, goat milk' ~ 'cheese brown_goat',
  food_item == 'cheese, whey, cow milk' ~ 'cheese brown',
  food_item == 'cheese, ripened, camembert' ~ 'cheese_camembert',
  food_item == 'cheese, hard, cheddar' ~ 'cheese_cheddar',
  food_item == 'cottage cheese' ~ 'cheese cottage',
  food_item == 'cream cheese, plain' ~ 'cheese cream',
  food_item == 'cheese, goat milk, feta' ~ 'cheese_feta',
  food_item == 'goat cheese, chevre, naturell' ~ 'cheese_chevre',
  food_item == 'goat cheese, hard, white, balsfjord' ~ 'cheese hard goat_balsfjord',
  food_item == 'goat cheese, hard, white, kvitlin' ~ 'cheese hard goat_kvitlin',
  food_item == 'cream cheese, goat milk, snøfrisk' ~ 'cheese cream_goat snøfrisk',
  food_item == 'cheese, halloumi' ~ 'cheese_halloumi',
  food_item == 'cheese, hard, jarlsberg' ~ 'cheese_jarlsberg',
  food_item == 'cheese, mascarpone' ~ 'cheese_mascarpone',
  food_item == 'cheese, mozarella, semi-hard, norwegian' ~ 'cheese_mozzarella',
  food_item == 'cheese, hard, norvegia' ~ 'cheese_norvegia',
  food_item == 'cheese, hard, parmesan' ~ 'cheese_parmesan',
  food_item == 'cheese, semi-hard, port salut' ~ 'cheese_port salut',
  food_item == 'cheese, ricotta' ~ 'cheese_ricotta salata',
  food_item == 'cheese, manchego' ~ 'cheese_manchego',
  food_item == 'cheese, paneer' ~ 'cheese_paneer',
  food_item == 'cheese, hard, sveitser' ~ 'swiss_cheese',
  food_item == 'cheese, white, unspecified' ~ 'semi-hard to hard cheese',
  food_item == 'cream, household, 18 % fat' ~ 'cream household_18',
  food_item == 'cream, whipping, 37 % fat' ~ 'cream whipped_37',
  food_item == 'cream, coffee, 10 % fat' ~ 'cream coffee_10',
  food_item == 'cream, sour, 35 % fat' ~ 'sour cream_35',
  food_item == 'cream, sour, 35 % fat, crème fraîche' ~ 'sour cream_35/crème fraîche_35',
  food_item == 'cream, sour, low-fat, 18 % fat' ~ 'sour cream_18/crème fraîche_18',
  food_item == 'cream, sour, extra low-fat, 10 % fat' ~ 'sour cream_10/crème fraîche_10',
  food_item == 'cheese, whey, unspecified' ~ 'cheese_brown',
  food_item == 'milk, cultured, whole, kefir, tine' ~ 'kefir',
  food_item == 'margarine, hard' ~ 'margarine',
  food_item == 'milk, skimmed, tine' ~ 'milk_0.1',
  food_item == 'milk, semi-skimmed, unspecified' ~ 'milk_1',
  food_item == 'milk, whole, unspecified' ~ 'whole milk_3.5',
  food_item == 'coconut milk, canned' ~ 'milk_coconut',
  food_item == "milk, goat, uht-treated" ~ "milk_goat",
  food_item == 'milk beverage, with chocolate flavour, litago' ~ 'milk beverage_chocolate',
  food_item == 'quark, 1 % fat' ~ 'quark_1',
  food_item == 'quark, 7 % fat' ~ 'quark_7',
  food_item == 'milk, cultured, skimmed, skummet kulturmelk' ~ 'buttermilk', #Similar in nutrient content
  food_item == 'yoghurt, whole milk, plain' ~ 'yoghurt', #Standard
  food_item == 'yoghurt, skimmed, 0,1 % fat, fruit, yoplait' ~ 'yoghurt low fat',
  food_item == 'yoghurt, with muesli and berries' ~ 'yoghurt_with grains',
  food_item == 'cultured, thickened oat product' ~ 'yoghurt_plant-based oat',
  food_item == 'yoghurt, plain, 10 % fat, greek/turkish' ~ 'yoghurt greek_10',
  food_item == 'yoghurt, plain, 2 % fat, greek/turkish' ~ 'yoghurt greek_2',
  food_item == 'milk, condensed, sweetened' ~ 'milk evaporated',
  food_item == 'soy based beverage' ~ 'dairy imitate_soymilk',
  food_item == 'rice based beverage' ~ 'dairy imitate_rice milk',
  food_item == 'oat based beverage' ~ 'dairy imitate oatmilk',
  food_item == 'cultured oat product, for food preparation, 15 % fat' ~ 'dairy imitate oatmilk_full fat',
  food_item == 'ice cream, dairy' ~ 'ice cream',
  food_item == 'cultured milk, with flavour, skyr' ~ 'yoghurt_skyr flavored',
  food_item == 'cultured milk, plain, skyr' ~ 'yoghurt_skyr',
  food_item == "almond based beverage" ~ "dairy imitate_almond milk",
  food_item == 'plant-based product, used as cheese' ~ 'cheese_plant-based',
  food_item == 'coconut based beverage, with calsium og vitamins (vit b12, vit d)' ~ 'dairy imitate_coconut',

  #Mushrooms----
  food_item == 'mushroom, chantherelle, raw' ~ 'mushroom_chanterelle',
  food_item == 'mushroom, trumpet chanterelle, funnel chanterelle, raw' ~ 'mushroom_trumpet chanterelle',
  food_item == 'mushroom, king boletus, yellow boletus, raw' ~ 'mushroom_boletus',
  food_item == 'mushroom, common, raw' ~ 'mushroom',
  food_item == 'mushroom, oyster, raw' ~ 'mushroom_oyster',
  food_item == 'mushroom, portabello' ~ 'mushroom_portebello',
  food_item == 'mushroom, shiitake, raw' ~ 'mushroom_shiitake',
  food_item == 'mushroom, common, canned, drained' ~ 'mushroom_canned',

  #Div----
  food_item %in% c('beer, dark, 4,5 - 4,7 vol-% alcohol, bayer and christmas beers',
                   'beer, dark, 4,5-4,7 vol-% alcohol, bayer and christmas beers') ~ 'beer_dark',
  food_item == 'capers, canned' ~ 'caper',
  food_item == 'spirits, 40 vol-% alcohol' ~ 'spirits 40 vol-% alcohol',
  food_item == 'spirits, 60 vol-% alcohol' ~ 'spirits 60 vol-% alcohol',
  food_item == 'cider, sweet, 4,5 vol-% alcohol' ~ 'apple cider_sweet',
  food_item == 'sugar, brown' ~ 'sugar_brown',
  food_item == 'sugar, white, caster sugar, cube sugar' ~ 'sugar',
  food_item %in% c('fortified wines, sweet vermouth, 15 vol-% alcohol',
                   'wine, fortified, dry, 15 vol-% alcohol') ~ 'fortified wine 15 vol-% alcohol',
  food_item == 'wine, fortified, sweet, 17 vol-% alcohol' ~ 'fortified wine 17 vol-% alcohol',
  food_item == 'fortified wines, sweet, port, 20 vol-% alcohol' ~ 'fortified wine 20 vol-% alcohol',
  food_item == 'sesame paste, tahini' ~ 'tahini',
  food_item == 'vinegar, 7 %' ~ 'vinegar',
  food_item == 'balsamic vinegar' ~ 'vinegar_balsamic',
  food_item == 'vinegar, apple cider' ~ 'vinegar_apple cider',
  food_item == 'wasabi, root, raw' ~ 'wasabi',
  food_item == 'water, tap' ~ 'water',
  food_item == "nutritional yeast" ~ "yeast_nutritional",
  food_item == 'bakers yeast, active, dry' ~ 'yeast_dry',
  food_item == 'bakers yeast, compressed' ~ 'yeast',
  food_item == 'wine, red, unspecified' ~ 'wine_red',
  food_item == 'wine, white, rosé, sparkling, unspecified' ~ 'wine_white',
  food_item == 'wine, dessert, extra sweet, sparkling' ~ 'wine_dessert',
  food_item == 'bouillon powder' ~ 'broth_cube',
  food_item == 'capers, canned' ~ 'caper',
  food_item == 'cocoa powder' ~ 'cocoa_powder',
  food_item == 'honey' ~ 'honey',
  food_item == 'salt, table' ~ 'salt',
  food_item == 'soy sauce' ~ 'soy_sauce',
  food_item == 'soy sauce, sweet' ~ 'soy sauce_sweet',
  food_item == 'tofu, soy bean curd' ~ 'tofu',
  food_item == 'jam, 45 % berries, 25 % sugar' ~ 'jam',
  food_item == 'chocolate bar, milk' ~ 'chocolate_milk',
  food_item == 'chocolate, white' ~ 'chocolate_white',
  food_item == 'chocolate, cooking, plain, minimum 35 % cocoa' ~ 'chocolate_semi-sweet',
  food_item == 'chocolate, snickers' ~ 'chocolate_candy bar',
  food_item == 'chocolate, dark, 70 % cocoa' ~ 'chocolate_dark', #Not really, but darkest they have
  food_item == 'popcorn, air popped, industrially made' ~ 'popcorn',
  food_item == 'gelatin' ~ 'gelatin',
  food_item == 'cloudberries, raw' ~ 'cloudberry',
  food_item == 'remoulade' ~ 'remulade',
  food_item == 'pizza, industrially made' ~ 'pizza',
  food_item == 'nut spread, nugatti' ~ 'nugatti',
  food_item == 'garlic powder' ~ 'garlic_powder',
  food_item == 'cloves ground' ~ 'cloves',
  food_item == 'cumin seeds, ground' ~ 'cumin',
  food_item == 'agave syrup' ~ 'syrup_agave',
  food_item == 'glucose syrup' ~ 'syrup_glucose',
  food_item == 'syrup' ~ 'syrup',
  food_item == 'cocoa beans' ~ 'bean_cocoa',
  food_item == 'vanilla sugar' ~ 'sugar_vanilla',
  food_item == 'baking powder' ~ 'baking powder',
  food_item == 'baking soda' ~ 'baking soda',
  food_item == 'curry paste' ~ 'paste_curry',
  food_item == 'miso' ~ 'miso',
  food_item == 'seitan, wheat gluten' ~ 'seitan',
  food_item == 'tempeh soy bean product' ~ 'tempeh',
  food_item == 'mustard' ~ 'mustard',
  food_item == 'sauerkraut' ~ 'sauerkraut',

  # Beverages
  food_item == 'mineral water, carbonated, bonaqua naturell' ~ 'water_carbonated',
  food_item == 'caffe mocha' ~ 'caffe mocha',
  food_item == 'espresso, single' ~ 'espresso',
  food_item == 'coffee, infusion' ~ 'coffee',
  food_item == 'Ice coffee, cappuchino' ~ 'coffee_ice',
  food_item == 'tea, black, infusion' ~ 'tea_black',
  food_item == 'tea, green, infusion' ~ 'tea_green',
  food_item == 'lemonade, with lemon, from cafe/bakery' ~ 'lemonade', #default
  food_item == 'lemonade, with raspberries, from cafe/bakery' ~ 'lemonade_raspberries',
  food_item == 'soda, with sugar' ~ 'soda', #Default
  food_item == 'soda, artificially sweetened, light' ~ 'soda_artificially sweetened',
  food_item == 'stock, meat, from cube, prepared' ~ 'broth',

  #Keep the unspecified ingredients
  str_detect(food_item, ', unspecified, raw') ~ str_replace(food_item, ', unspecified, raw', ''),

  # Dried, canned, jams fruits etc
  str_detect(food_item, ', dried$') ~ str_replace(food_item, ', dried', '_dried'),
  str_detect(food_item, ', canned$') ~ str_replace(food_item, ', canned', '_canned'),
  str_detect(food_item, ' jam$') ~ paste0("jam_",  str_replace(food_item, ' jam', '')),

  #Remove 'raw' from certain ingredients----
  food_item %in% c('pineapple, raw', 'asparagus, raw', 'avocado, raw', 'banana, raw',
                   'blueberries, raw', 'catfish, raw', 'char, raw', 'mint, raw',
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
    str_detect(food_item, 'nuts') &
      !str_detect(Ingredients, 'pecan|brazil|hazel|pistachio') ~ str_replace(food_item, 'nuts', 'nut'),
    #str_detect(food_item, 'seeds') ~ str_replace(food_item, 'seeds', 'seed'),

    #Some other fruits and vegetable
    Foodgroup %in% c('vegetables, raw and frozen', 'fruit and berries, raw/fresh') &
      str_detect(food_item, ', raw') &
      is.na(Ingredients) ~ str_replace(food_item, ', raw', ''),
    Foodgroup %in%
      c('vegetables, raw and frozen', 'fruit and berries, raw/fresh') &
      str_detect(food_item, ', ') & is.na(Ingredients) ~ str_replace(food_item, ', ', '_'),

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

    #Meat products
    'Beef, variety meats and by-products, tongue, raw', 'Pork, fresh, variety meats and by-products, kidneys, raw',
    'Beef, New Zealand, imported, flank, separable lean and fat, raw', 'Lamb, New Zealand, imported, fore-shank, separable lean and fat, raw',
    #'Lamb, Australian, ground,  85% lean / 15% fat, raw', "Lard",

    #Fruit and veg
    #'Jams and preserves, apricot',
    'Artichokes, (globe or french), raw', 'Plantains, yellow, raw',
    "Tomato products, canned, paste, without salt added (Includes foods for USDA's Food Distribution Program)", 'Peppers, hot chili, red, canned, excluding seeds, solids and liquids',
    'Lime juice, raw', 'Figs, raw', 'Cabbage, chinese (pak-choi), raw', "Blueberries, dried, sweetened", "Bananas, dehydrated, or banana powder",
    'Beets, pickled, canned, solids and liquids', 'Peppers, hot pickled, canned', 'Pomegranate juice, bottled', 'Radish seeds, sprouted, raw',
    #Sorrel
    'Sourdock, young leaves (Alaska Native)',

    #Herbs and spices
    'Tamarind nectar, canned', 'Spices, chervil, dried', 'Spices, allspice, ground', 'Coriander (cilantro) leaves, raw',
    'Spices, cloves, ground',
    #'Spices, cumin seed',
    'Spices, fenugreek seed', 'Lemon grass (citronella), raw', 'Spices, onion powder',
    'Spices, sage, ground', 'Seasoning mix, dry, sazon, coriander & annatto',
    #'Spices, garlic powder',

    #Dairy
    'Cheese, cottage, lowfat, 2% milkfat', 'Cheese spread, pasteurized process, American',
    'Cheese, monterey', 'Cheese, neufchatel', 'Cheese, provolone', 'Cheese, romano',

    #Seafood
    'Mollusks, clam, mixed species, raw', 'Fish, grouper, mixed species, raw', 'Fish, sea bass, mixed species, raw',
    'Seaweed, wakame, raw', 'Cockles, raw (Alaska Native)', 'Crustaceans, crayfish, mixed species, farmed, raw',

    #Div
    'Seaweed, agar, dried', 'Soup, onion, dry, mix', 'Alcoholic beverage, rice (sake)',
    'Shortening, vegetable, household, composite', 'Pickle relish, sweet', 'Syrups, maple',
    'Sauce, ready-to-serve, pepper, TABASCO', 'Tapioca, pearl, dry', 'Molasses', 'Vital wheat gluten',
    'Horseradish, prepared', 'Fat, goose', 'Frostings, glaze, prepared-from-recipe')) %>%

  #Rename to fit ingredient names
  mutate(description = description %>%
           str_replace('Beef, variety meats and by-products, tongue, raw', 'beef_tongue') %>%
           str_replace('Pork, fresh, variety meats and by-products, kidneys, raw', 'pork_kidney') %>%
           str_replace('Beef, New Zealand, imported, flank, separable lean and fat, raw', 'beef_flank') %>%
           #str_replace('Jams and preserves, apricot', 'apricot_jam') %>%
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
           str_replace('Cheese, cottage, lowfat, 2% milkfat', 'cheese cottage_low fat') %>%
           str_replace('Cheese spread, pasteurized process, American', 'cheese_american') %>%
           str_replace('Cheese, monterey', 'cheese_monterey') %>%
           str_replace('Cheese, neufchatel', 'cheese_neufchatel') %>%
           str_replace('Cheese, provolone', 'cheese_provolone') %>%
           str_replace('Cheese, romano', 'cheese_romano') %>%
           str_replace('Yogurt, Greek, plain, whole milk', 'yoghurt_greek') %>%
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
           str_replace("Tempeh", "tempeh") %>%
           str_replace_all(., c(
             'Cockles, raw \\(Alaska Native\\)' = 'cockles',
             "Blueberries, dried, sweetened" = "blueberries_dried",
             "Bananas, dehydrated, or banana powder" = "banana_dried",
             #"Nuts, macadamia nuts, raw" = "macademia_nut",
             "Peppers, hot chili, red, canned, excluding seeds, solids and liquids" = "chili_canned",
             "Lamb, New Zealand, imported, fore-shank, separable lean and fat, raw" = "lamb_shank",
             "Peppers, hot pickled, canned" = "chili pepper_pickled",
             "Radish seeds, sprouted, raw" = "sprouts_radish",
             "Pomegranate juice, bottled" = "pomegranate_juice",
             "Crustaceans, crayfish, mixed species, farmed, raw" = "crayfish",
             "\\bLard\\b" = "pork_lard"
           ))
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
  system.file("extdata", "composite_ingredients_nutrient_content.Rds", package = "sustainableNutRients")) %>%
  #Create an database_ID column
  group_by(Ingredients) %>%
  mutate(database_ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(database_ID = database_ID + 200) %>% mutate(from = 'Composite ingredient not in database')

clean_nutrients <- bind_rows(clean_nutrients, various$component_ingredients_nutrients %>% select(database_ID, Ingredients, from)) %>%
  #Add a shellfish row
  add_row(database_ID = 10000, Ingredients = 'shellfish', from = 'Shellfish in Matvaretabellen')

#Ingredient groups consiting of more than one ingredient
#Create the nutrient content of the shellfish ingredient by taking the mean of the shellfish in the db
various$shellfish <- raw_data %>%

  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', ':0', ' sum', '2n', '3n', '4n', 'Foodgroup'))) %>%
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
              values_from = 'value') %>%
  mutate(Foodgroup = "shellfish, fish offal")

#Dairy grouped ingredients
various$dairy_ingredients <- raw_data %>%
  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', ':0', ' sum', '2n', '3n', '4n', 'Foodgroup'))) %>%
  #Rename to fit
  rename(
    database_ID = FoodID,
    EPA = `C20:5n-3 (EPA)`,
    DPA = `C22:5n-3 (DPA)`,
    DHA = `C22:6n-3 (DHA)`) %>%
  #FFilter out foods to create a composite from
  filter(
    #Yoghurts
    str_detect(`Food Item`, 'Yoghurt') &
      str_detect(`Food Item`, 'anilla|berr|fruit') &
      !str_detect(`Food Item`, 'granola|muesli|grain') |
    #Blue cheese
      str_detect(`Food Item`, 'Cheese, blue mold') |
    # Cheese spread
      str_detect(`Food Item`, 'Cheese spread')) %>%
  mutate(`Food Item` = `Food Item` %>%
           str_replace('Yoghurt, blueberries, 0 % fat, Yoplait', 'yoghurt low fat_berries flavored') %>% #Only low fat berry flavored in dataset
           str_replace('Yoghurt, vanilla, 0 % fat, Yoplait', 'yoghurt low fat_flavored') %>%
           str_replace('Yoghurt, skimmed, 0,1 % fat, fruit, Yoplait', 'yoghurt low fat_fruit flavored') %>%
           str_replace('Yoghurt, vanilla, lactose free|Yoghurt, vanilla', 'yoghurt_flavored') %>%
           str_replace('Yoghurt, strawberry, Q-meieriene|Yoghurt, with strawberry, Tine', 'yoghurt_berries flavored'),
         `Food Item` = case_when(
           str_detect(`Food Item`, 'Cheese, blue mold') ~ 'cheese blue',
           str_detect(`Food Item`, 'Cheese spread') ~ 'cheese_spread',
           TRUE ~ `Food Item`
         )
  ) %>%
  #rename
  rename(Ingredients = `Food Item`) %>%
  #New ID's
  group_by(Ingredients) %>%
  mutate(database_ID = cur_group_id()) %>%
  ungroup() %>%
  mutate(database_ID = database_ID + 11000) %>%
  #Turn everything to numeric
  pivot_longer(.,
               cols = -c(database_ID, Ingredients),
               names_to = 'feature',
               values_to = 'value') %>%
  mutate_at(c('database_ID', 'value'), ~as.numeric(.)) %>%
  #Get the mean values o each nutrient
  group_by(Ingredients, database_ID, feature) %>%
  summarise(value = mean(value, na.rm = TRUE)) %>% ungroup() %>%
  #Turn wide
  pivot_wider(.,
              names_from = 'feature',
              values_from = 'value') %>%
  mutate(Foodgroup = case_when(
    str_detect(Ingredients, "yoghurt") ~ "yoghurt",
    str_detect(Ingredients, "cheese blue") ~ "cheese, extra fat"
  ))

#Add to the clean_nutrients
clean_nutrients <- bind_rows(clean_nutrients,
                             various$dairy_ingredients %>%
                               select(database_ID, Ingredients, Foodgroup) %>%
                               mutate(food_item = "",
                                      from = "Composite of Matvaretabellen ingredients"))

#Create a full nutrient database----
#Nutrients of interest
nutrients_to_use <- raw_data %>%

  #Remove columns and nutrients not needed
  select(!contains(c('ref', 'Edible', ':0', ' sum', '2n', '3n', '4n', 'Foodgroup'))) %>%
  distinct() %>%

  #Rename to fit
  rename(
    database_ID = FoodID,
    EPA = `C20:5n-3 (EPA)`,
    DPA = `C22:5n-3 (DPA)`,
    DHA = `C22:6n-3 (DHA)`) %>%

  #Remove columns and rows
  select(-c(`Food Item`)) %>%
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
  bind_rows(., various$dairy_ingredients) %>%
  full_join(., clean_nutrients, by = c('database_ID')) %>%
  #Give sour cream, crème fraîche and veal liver individual database_IDs, while keeping the nutrition information rows
  mutate(database_ID = case_when(
    str_detect(Ingredients.y, 'crème fraîche|veal_liver') ~ database_ID + 300,
    TRUE ~ database_ID
  )) %>%
  #Remove and rename some columns
  select(-c(Ingredients.x, Ingredients.y, from.x, Foodgroup.x)) %>%
  rename(from = from.y,
         Foodgroup = Foodgroup.y)


#Turn long
matvaretabellen2024 <- nutrients_to_use %>% select(-c(food_item, Foodgroup)) %>%
  pivot_longer(.,
               cols = -c(database_ID, from),
               names_to = 'nutrient',
               values_to = 'nutrient_content_per_hektogram')

#Save matvaretabellen dataframe
saveRDS(matvaretabellen2024, "./data-raw/matvaretabellen2024.Rds")
saveRDS(matvaretabellen2024, "./data-raw/matvaretabellen2022.Rds")

#Save foodgroups info
matvaretabellen2024_foodgroups <- nutrients_to_use %>%
  select(database_ID, Foodgroup) %>%
  rename(foodgroup = Foodgroup)

saveRDS(matvaretabellen2024_foodgroups, "./data-raw/matvaretabellen2024_foodgroups.Rds")
saveRDS(matvaretabellen2024_foodgroups, "./data-raw/matvaretabellen2022_foodgroups.Rds")

#Save database_ID and food_item columns to create a query dataframe
matvaretabellen2024_query_prep <- clean_nutrients %>%
  select(Ingredients, database_ID) %>%
  filter(!str_detect(Ingredients, ', imported|, norwegian|apple,|,'))

#Save the dataframe to use to create the queries
saveRDS(matvaretabellen2024_query_prep, "./data-raw/matvaretabellen2024_query_prep.Rds")
saveRDS(matvaretabellen2024_query_prep, "./data-raw/matvaretabellen2022_query_prep.Rds")
