#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "F".
#' @title standardiseFlournGraisnNutsnLegumesF
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "F".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "F" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesF <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'flax') & str_detect(Ingredients, 'meal') ~ 'flaxseed meal',
      str_detect(Ingredients, 'flax|lin') & str_detect(Ingredients, 'seed') ~ 'seed flax',

      TRUE ~ Ingredients_standardised))
}

