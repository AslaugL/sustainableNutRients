#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "D".
#' @title standardiseHerbsnSpicesD
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "D".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "D" with standardised names.
#'
#' @export
standardiseHerbsnSpicesD <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'dill') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(unit, 'twig|bunch|leaf|neve|dl')) ~ 'dill fresh herbs',
      str_detect(Ingredients, 'dill') & !str_detect(Ingredients, 'in dill|cheese|snow-fresh') ~ 'dill dried', #Standard

      TRUE ~ Ingredients_standardised))
}
