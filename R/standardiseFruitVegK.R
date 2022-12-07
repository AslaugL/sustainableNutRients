#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "K".
#' @title standardiseFruitVegK
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "K".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "K" with standardised names.
#'
#' @export
standardiseFruitVegK <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'kale') ~ 'kale',
      str_detect(Ingredients, 'kimchi') ~ 'kimchi',
      str_detect(Ingredients, 'kiwi') ~ 'kiwi',

      TRUE ~ Ingredients_standardised
    ))
}

