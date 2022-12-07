#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "B".
#' @title standardiseHerbsnSpicesB
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "B".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "B" with standardised names.
#'
#' @export
standardiseHerbsnSpicesB <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'basil') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') |
                                            str_detect(unit, 'twig|bunch|leaf|neve|dl')) ~ 'basil fresh herbs',
      str_detect(Ingredients, 'thaibasil') ~ 'basil fresh herbs',
      str_detect(Ingredients, 'basil') & !str_detect(Ingredients, 'pesto') ~ 'basil dried', #Standard
      str_detect(Ingredients, 'bay leaf') ~ 'bay leaf',
      str_detect(Ingredients, 'burrito spice') ~ 'burrito spice mix',

      TRUE ~ Ingredients_standardised))
}
