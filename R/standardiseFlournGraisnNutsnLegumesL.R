#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "L".
#' @title standardiseFlournGraisnNutsnLegumesL
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "L".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "L" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesL <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'red') ~ 'lentil red',
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'green') ~ 'lentil green',
      str_detect(Ingredients, 'lentil') & str_detect(Amounts, 'box|can') ~ 'lentil canned',
      str_detect(Ingredients, 'lentil') ~ 'lentil',

      TRUE ~ Ingredients_standardised))
}

