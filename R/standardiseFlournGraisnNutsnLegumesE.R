#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "E".
#' @title standardiseFlournGraisnNutsnLegumesE
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "E".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "E" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesE <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      #Cashews
      str_detect(Ingredients, 'edamame') ~ 'edamame bean',

      TRUE ~ Ingredients_standardised))
}
