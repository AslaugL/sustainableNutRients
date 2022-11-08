#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "S".
#' @title standardiseHerbsnSpicesS
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "S".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "S" with standardised names.
#'
#' @export
standardiseHerbsnSpicesS <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'saffron') ~ 'saffron',
      str_detect(Ingredients, 'sage') & !str_detect(Ingredients, 'sau') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'sage fresh herbs',
      str_detect(Ingredients, 'sage') & !str_detect(Ingredients, 'sau') ~ 'sage dried', #Standard
      str_detect(Ingredients, 'salvie') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'salvie fresh herbs',
      str_detect(Ingredients, 'salvie') ~ 'salvie dried', #Standard
      str_detect(Ingredients, 'sazon seasoning') ~ 'sazon seasoning',
      str_detect(Ingredients, 'star anis') ~ 'star anise',
      str_detect(Ingredients, 'summer savory') ~ 'summer savory fresh herbs',
      
      TRUE ~ Ingredients_standardised))
}
