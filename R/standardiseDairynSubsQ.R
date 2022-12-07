#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "Q".
#' @title standardiseDairynSubsQ
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "Q".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "Q" with standardised names.
#'
#' @export
standardiseDairynSubsQ <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'kesam|tine light 2%') & str_detect(Ingredients, 'low fat|1 %') ~ 'quark, 1 %',
      str_detect(Ingredients, 'kesam') ~ 'quark, 7 %',

      TRUE ~ Ingredients_standardised))
}
