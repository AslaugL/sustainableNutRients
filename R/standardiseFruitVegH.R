#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "H".
#' @title standardiseFruitVegH
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "H".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "H" with standardised names.
#'
#' @export
standardiseFruitVegH <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, "hash") & str_detect(Ingredients, "brown") ~ "hashbrown",
      str_detect(Ingredients, 'horseradish|horse raddish') & !str_detect(Ingredients, 'sauce') ~ 'horseradish',

      TRUE ~ Ingredients_standardised
    ))
}

