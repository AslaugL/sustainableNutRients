#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "M".
#' @title standardiseHerbsnSpicesM
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "M".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "M" with standardised names.
#'
#' @export
standardiseHerbsnSpicesM <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'marjoram') & str_detect(Ingredients, 'twig|chopped') ~ 'marjoram fresh herbs',
      str_detect(Ingredients, 'mint') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(unit, 'twig|bunch|leaf|neve|dl')) ~ 'mint fresh herbs',
      str_detect(Ingredients, 'mint') ~ 'mint dried', #Standard

      TRUE ~ Ingredients_standardised))
}
