#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "A".
#' @title standardiseDairynSubsA
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "A".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "A" with standardised names.
#'
#' @export
standardiseDairynSubsA <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      
      TRUE ~ Ingredients_standardised))
}
