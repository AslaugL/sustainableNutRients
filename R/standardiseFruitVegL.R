#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "L".
#' @title standardiseFruitVegL
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "L".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "L" with standardised names.
#'
#' @export
standardiseFruitVegL <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'lettuce') ~ 'salad lamb lettuce',
      str_detect(Ingredients, 'leek') & !str_detect(Ingredients, 'mire|onion|tortilla') ~ 'leek',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'grass') ~ 'lemongrass',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'curd') ~ 'curd lemon',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'peel|zest|shell|rind') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice and zest',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice',
      str_detect(Ingredients, 'lemon') & (str_detect(unit, 'tsp|tbsp|dl') | str_detect(Ingredients, 'drop')) & !str_detect(Ingredients, 'peel|shell|zest|rind') & !str_detect(Ingredients, 'pepper') ~ 'lemon, the juice',
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'shell|peel|zest|rind') & !str_detect(Ingredients, 'pepper') | str_detect(Ingredients, 'sitronskall') ~ 'lemon, the zest',
      str_detect(Ingredients, 'lemon') & !str_detect(Ingredients, 'lime|balm|pepper|beverage') ~ 'lemon',
      str_detect(Ingredients, 'of lime or lemon') ~ 'lemon', #Recipe has lemon in the name
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'peel|zest|rind') ~ 'lime, the juice and zest',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'peel|sheel|zest|rind') & !str_detect(Ingredients, 'juice') ~ 'lime, the zest',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice|pressed') & !str_detect(Ingredients, 'peel') ~ 'lime, the juice',
      str_detect(Ingredients, 'lime') & str_detect(unit, 'tsp|tbsp|dl') ~ 'lime, the juice',
      str_detect(Ingredients, 'lime') & !str_detect(Ingredients, 'sheet|lemon|leaf|beverage') ~ 'lime',
      str_detect(Ingredients, 'lingonberr') & str_detect(Ingredients, 'jam') ~ 'lingonberry jam',
      str_detect(Ingredients, 'lingonberr') ~ 'lingonberry',
      str_detect(Ingredients, 'lychee') ~ 'lychee',

      TRUE ~ Ingredients_standardised
    ))
}

