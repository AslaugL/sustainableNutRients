#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "N".
#' @title standardiseFruitVegN
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "N".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "N" with standardised names.
#'
#' @export
standardiseFruitVegN <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(

      str_detect(Ingredients, 'nectarine') ~ 'nectarine',

      TRUE ~ Ingredients_standardised
    ))
}

