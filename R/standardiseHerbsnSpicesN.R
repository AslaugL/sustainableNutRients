#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "N".
#' @title standardiseHerbsnSpicesN
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "N".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "N" with standardised names.
#'
#' @export
standardiseHerbsnSpicesN <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'nutmeg') ~ 'nutmeg',
      
      TRUE ~ Ingredients_standardised))
}
