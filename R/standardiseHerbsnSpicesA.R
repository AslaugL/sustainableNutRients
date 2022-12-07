#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "A".
#' @title standardiseHerbsnSpicesA
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "A".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "A" with standardised names.
#'
#' @export
standardiseHerbsnSpicesA <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'adobo seasoning') ~ 'adobo seasoning',
      str_detect(Ingredients, 'allspice|all kinds') ~ 'allspice',
      str_detect(Ingredients, 'anise') &str_detect(Ingredients, 'extract') ~ 'anise extract',
      str_detect(Ingredients, 'anis') & !str_detect(Ingredients, 'star|manis') ~ 'anise ground',

      TRUE ~ Ingredients_standardised))
}
