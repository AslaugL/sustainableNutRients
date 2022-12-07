#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "F".
#' @title standardiseHerbsnSpicesF
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "F".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "F" with standardised names.
#'
#' @export
standardiseHerbsnSpicesF <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'fajita') & str_detect(Ingredients, 'spice') ~ 'spice mix fajita',
      str_detect(Ingredients, 'fennel') & str_detect(Ingredients, 'seed') ~ 'fennel seed',
      str_detect(Ingredients, 'fenugreek leaf') & str_detect(Ingredients, 'dried') ~ 'fenugreek leaf dried',
      str_detect(Ingredients, 'fenugreek seed') ~ 'fenugreek seed',

      TRUE ~ Ingredients_standardised))
}
