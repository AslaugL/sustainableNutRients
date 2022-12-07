#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "L".
#' @title standardiseHerbsnSpicesL
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "L".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "L" with standardised names.
#'
#' @export
standardiseHerbsnSpicesL <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'lemon balm') ~ 'lemon balm',
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'leaf|leaves|sheet') ~ 'lime leaf',
      
      
      TRUE ~ Ingredients_standardised))
}
