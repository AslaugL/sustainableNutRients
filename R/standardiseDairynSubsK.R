#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "K".
#' @title standardiseDairynSubsK
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "K".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "K" with standardised names.
#'
#' @export
standardiseDairynSubsK <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'kefir|cultured milk') ~ 'kefir',

      TRUE ~ Ingredients_standardised))
}
