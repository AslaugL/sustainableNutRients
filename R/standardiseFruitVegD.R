#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "D".
#' @title standardiseFruitVegD
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "D".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "D" with standardised names.
#'
#' @export
standardiseFruitVegD <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      TRUE ~ Ingredients_standardised
    ))
}

