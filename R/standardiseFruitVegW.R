#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "W".
#' @title standardiseFruitVegW
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "W".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "W" with standardised names.
#'
#' @export
standardiseFruitVegW <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'watermelon') ~ 'watermelon',
      str_detect(Ingredients, 'water chestnut') ~ 'water chestnut',

      TRUE ~ Ingredients_standardised
    ))
}

