#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "J".
#' @title standardiseFruitVegJ
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "J".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "J" with standardised names.
#'
#' @export
standardiseFruitVegJ <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'jerusalem') & str_detect(Ingredients, "artichoke") ~ 'jerusalem artichoke',

      TRUE ~ Ingredients_standardised
    ))
}

