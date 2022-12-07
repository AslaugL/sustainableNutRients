#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "J".
#' @title standardiseHerbsnSpicesJ
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "J".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "J" with standardised names.
#'
#' @export
standardiseHerbsnSpicesJ <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'juniper') ~ 'juniper berry',
      
      TRUE ~ Ingredients_standardised))
}
