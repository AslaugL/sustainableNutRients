#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "O".
#' @title standardiseFlournGraisnNutsnLegumesO
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "O".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "O" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesO <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'oatmeal|oat flour') ~ 'oatmeal',
      str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'flake|rolled') ~ 'oat rolled',
      str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'quick|porridge') ~ 'oat quick',
      
      TRUE ~ Ingredients_standardised))
}

