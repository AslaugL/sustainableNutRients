#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "G".
#' @title standardiseHerbsnSpicesG
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "G".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "G" with standardised names.
#'
#' @export
standardiseHerbsnSpicesG <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'garam') ~ 'garam masala',
      str_detect(Ingredients, 'pav bhaji masala') ~ 'pav bhaji masala',
      Ingredients == 'italian seasoning' ~ 'italian seasoning',
      str_detect(Ingredients, 'ginger') & (str_detect(Ingredients, 'fresh|grated|chopped') |
                                             str_detect(unit, 'cm')) ~ 'fresh herbs ginger',
      str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'pickle') ~ 'ginger pickled',
      str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'paste') ~ 'paste ginger',
      str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'syrup') ~ 'syrup ginger',
      str_detect(Ingredients, 'ginger') & !str_detect(Ingredients, 'bread') ~ 'dried ginger',
      str_detect(Ingredients, 'zedoari') ~ 'ginger zedoari', #In the same family
      str_detect(Ingredients, 'guacamole') & str_detect(Ingredients, 'spice mix') ~ 'spice mix guacamole',

      TRUE ~ Ingredients_standardised))
}
