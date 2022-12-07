#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "R".
#' @title standardiseFruitVegR
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "R".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "R" with standardised names.
#'
#' @export
standardiseFruitVegR <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'radish') & str_detect(Ingredients, 'daikon') ~ 'radish daikon',
      str_detect(Ingredients, 'radish') & !str_detect(Ingredients, 'horse') ~ 'radish',
      str_detect(Ingredients, 'raisin') & !str_detect(Ingredients, 'brais|flour') ~ 'raisin',
      str_detect(Ingredients, 'raspberry') & str_detect(Ingredients, 'jam') ~ 'raspberries jam',
      str_detect(Ingredients, 'raspbe') ~ 'raspberries',
      str_detect(Ingredients, 'rhubarb') & str_detect(Ingredients, 'juice') ~ 'rhubarb juice',
      str_detect(Ingredients, 'rhubarb') ~ 'rhubarb',
      str_detect(Ingredients, 'romanesque|romanesco') ~ 'broccoli romanesco',

      TRUE ~ Ingredients_standardised
    ))
}

