#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "T".
#' @title standardiseHerbsnSpicesT
#'
#' @description Support function for "StandardiseHerbsnSpices", standardise names of herbs and spices starting with "T".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "T" with standardised names.
#'
#' @export
standardiseHerbsnSpicesT <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'taco') & str_detect(Ingredients, 'spice|season') ~ 'taco spice mix',
      str_detect(Ingredients, 'tandoori') & !str_detect(Ingredients, 'paste') ~ 'tandoori spice mix',
      str_detect(Ingredients, 'tarragon') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'tarragon fresh herbs',
      str_detect(Ingredients, 'tarragon') & str_detect(Ingredients, 'dried') ~ 'tarragon dried',
      str_detect(Ingredients, 'tarragon') & !str_detect(Ingredients, 'parsley|rosemary|thyme|chervil|mint|basil|chives|dill|coriander') ~ 'tarragon dried', #Standard
      str_detect(Ingredients, 'thyme') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'thyme fresh herbs',
      str_detect(Ingredients, 'thyme') ~ 'thyme dried', #Standard
      str_detect(Ingredients, 'turmeric') ~ 'turmeric',
      
      TRUE ~ Ingredients_standardised))
}
