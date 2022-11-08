#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "E".
#' @title standardiseFruitVegE
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "E".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "E" with standardised names.
#'
#' @export
standardiseFruitVegE <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'eggplant|aubergine') ~ 'eggplant',

      TRUE ~ Ingredients_standardised
    ))
}

