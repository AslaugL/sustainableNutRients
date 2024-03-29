#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "P".
#' @title standardiseFruitVegP
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "P".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "P" with standardised names.
#'
#' @export
standardiseFruitVegP <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'parsley') & str_detect(Ingredients, 'root') ~ 'parsley root',
      str_detect(Ingredients, 'parsnip') ~ 'parsnip',
      str_detect(Ingredients, 'passion') & str_detect(Ingredients, 'fruit') ~ 'passion fruit',
      str_detect(Ingredients, 'pak') & str_detect(Ingredients, 'choi') ~ 'cabbage pak choi',
      str_detect(Ingredients, 'papaya') & !str_detect(Ingredients, 'muesli|müsli|granola') ~ 'papaya',
      str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'dry|dri') & str_detect(Ingredients, 'yellow') ~ 'peas yellow dried',
      str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'yellow') & !str_detect(Ingredients, 'soup') ~ 'peas yellow',
      str_detect(Ingredients, 'pea') & !str_detect(Ingredients, 'chick|broccoli|nut|sugar|asparagus|onion|pearl|horse|peach|dill|pear|pasta') ~ 'peas green',
      str_detect(Ingredients, 'peach') & str_detect(Ingredients, 'can') ~ 'peach canned',
      str_detect(Ingredients, 'peach') & str_detect(Ingredients, 'dry|dried') ~ 'peach dried',
      str_detect(Ingredients, 'peach') ~ 'peach',
      str_detect(Ingredients, 'pear') & str_detect(Ingredients, 'sirup|syrup') & !str_detect(Ingredients, 'onion|barley') ~ 'syrup pear',
      str_detect(Ingredients, 'pear') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'onion|barley') ~ 'pear juice',
      str_detect(Ingredients, 'pear') & !str_detect(Ingredients, 'onion|barley|sugar') ~ 'pear',
      str_detect(Ingredients, 'persimmon') ~ "persimmon",
      str_detect(Ingredients, 'physalis') ~ 'physalis',
      str_detect(Ingredients, 'pimiento|pimento') ~ 'pimiento chili pepper',
      str_detect(Ingredients, 'pineapple') & str_detect(Ingredients, 'can') ~ 'pineapple canned',
      str_detect(Ingredients, 'pineapple') & str_detect(Ingredients, 'juice') ~ 'pineapple juice',
      str_detect(Ingredients, 'pineapple') ~ 'pineapple',
      str_detect(Ingredients, 'plantain') & str_detect(Ingredients, 'chip') ~ 'plantain chip',
      str_detect(Ingredients, 'plantain') ~ 'plantain',
      str_detect(Ingredients, 'plum') & str_detect(Ingredients, 'red') ~ 'plum red',
      str_detect(Ingredients, 'plum') ~ 'plum',
      str_detect(Ingredients, 'pomegranat') & str_detect(Ingredients, 'kernel') ~ 'pomegranate kernel',
      str_detect(Ingredients, 'pomegranat') & str_detect(Ingredients, 'juice') ~ 'pomegranate juice',
      str_detect(Ingredients, 'pomegranat') ~ 'pomegranate',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'starch') ~ 'potato starch',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'chip') ~ 'potato chip',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'cream') ~ 'potato cream',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'fries') ~ 'french fries',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, '(?<!in )boat') ~ 'potato boat',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'blue') ~ 'potato blue',
      str_detect(Ingredients, 'potato') & !str_detect(Ingredients, 'rice|bread|sweet|mash|flour|mash') ~ 'potato',
      str_detect(Ingredients, 'buttery') & str_detect(Ingredients, 'mash') ~ 'potato mash buttery',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'mash') & !str_detect(Ingredients, 'cook|boil') ~ 'potato mash',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'cook|boil') ~ 'potato boiled',
      str_detect(Ingredients, 'pumpkin') & !str_detect(Ingredients, 'seed|butternut|hokkaido') ~ 'winter squash pumpkin',
      str_detect(Ingredients, 'prune') & str_detect(Ingredients, 'compote') ~ 'prune compote',
      str_detect(Ingredients, 'prune') ~ 'prune',

      TRUE ~ Ingredients_standardised
    ))
}

