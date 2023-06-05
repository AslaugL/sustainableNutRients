#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "O".
#' @title standardiseFruitVegO
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "O".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "O" with standardised names.
#'
#' @export
standardiseFruitVegO <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'black|kalamata') ~ 'olive black',
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'fill') ~ 'olive filled',
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'green') ~ 'olive green',
      str_detect(Ingredients, 'olive') & !str_detect(Ingredients, 'oil|tapenade|salad|con queso|garlic') ~ 'olive green',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'pickle') ~ 'onion pickled',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'fried|caramelised|caramelized') ~ 'onion fried',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'ring') ~ 'onion rings',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'red') ~ 'onion red',
      str_detect(Ingredients, 'onion') & !str_detect(Ingredients, 'pickle|spring|green|pearl|leek|mire|garlic|powder|soup|bread|seed') ~ 'onion yellow', #Use as standard
      str_detect(Ingredients, 'pearl onion') & str_detect(Ingredients, 'pickle') ~ 'pearl onion pickled',
      str_detect(Ingredients, 'pearl onion') ~ 'pearl onion',
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice|pressed') & str_detect(Ingredients, 'zest|peel|shell') ~ 'orange, the juice and zest',
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'shell|zest|peel') ~ 'orange, the zest',
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') ~ 'orange juice',
      str_detect(Ingredients, 'orange') & !str_detect(Ingredients, 'jelly') ~ 'orange',

      TRUE ~ Ingredients_standardised
    ))
}

