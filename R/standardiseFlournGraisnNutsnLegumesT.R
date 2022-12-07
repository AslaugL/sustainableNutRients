#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "T".
#' @title standardiseFlournGraisnNutsnLegumesT
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "T".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "T" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesT <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'tahini') ~ 'tahini',
      str_detect(Ingredients, 'tart') & str_detect(Ingredients, 'shell') ~ 'tart shell',
      str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'whole|coarse') ~ 'tortilla coarse',
      str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'corn') ~ 'tortilla corn',
      str_detect(Ingredients, 'tortilla|wraps') & !str_detect(Ingredients, 'pita|chip') ~ 'tortilla',

      TRUE ~ Ingredients_standardised))
}

