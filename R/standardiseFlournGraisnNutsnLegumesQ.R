#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "Q".
#' @title standardiseFlournGraisnNutsnLegumesQ
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "Q".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "Q" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesQ <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'quinoa') ~ 'quinoa',
      
      TRUE ~ Ingredients_standardised))
}

