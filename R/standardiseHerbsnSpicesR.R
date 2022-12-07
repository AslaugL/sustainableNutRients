#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "R".
#' @title standardiseHerbsnSpicesR
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "R".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "R" with standardised names.
#'
#' @export
standardiseHerbsnSpicesR <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'raita') & str_detect(Ingredients, 'mix') ~ 'spice mix raita',
      str_detect(Ingredients, 'ratatouille') ~ 'ratatouille',
      str_detect(Ingredients, 'rosemary') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(unit, 'twig|bunch|leaf|neve|dl')) ~ 'rosemary fresh herbs',
      str_detect(Ingredients, 'rosemary') ~ 'rosemary dried', #Standard

      TRUE ~ Ingredients_standardised))
}
