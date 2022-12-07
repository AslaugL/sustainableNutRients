#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "F".
#' @title standardiseFruitVegF
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "F".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "F" with standardised names.
#'
#' @export
standardiseFruitVegF <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'fennel') & !str_detect(Ingredients, 'seed') ~ 'fennel',
      str_detect(Ingredients, 'fig') & str_detect(Ingredients, 'dried') ~ 'fig dried',
      str_detect(Ingredients, 'fig') & str_detect(Ingredients, 'tart') ~ 'fig tart',
      str_detect(Ingredients, 'fig') ~ 'fig',
      str_detect(Ingredients, 'frozen') & str_detect(Ingredients, 'vegetable|stew mix') ~ 'frozen vegetable mix',
      str_detect(Ingredients, 'french fries') ~ 'french fries',

      TRUE ~ Ingredients_standardised
    ))
}

