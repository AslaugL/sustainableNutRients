#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "P".
#' @title standardiseHerbsnSpicesP
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "P".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "P" with standardised names.
#'
#' @export
standardiseHerbsnSpicesP <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'paprika|pepper') & str_detect(Ingredients, 'powder|spice') & !str_detect(Ingredients, 'spice seasoning pepper') ~ 'paprika powder',
      str_detect(Ingredients, 'paprika') & str_detect(Ingredients, 'smoked') ~ 'paprika powder smoked',
      str_detect(Ingredients, 'parsley|mug') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'parsley fresh herbs',
      str_detect(Ingredients, 'parsley') ~ 'parsley dried', #Standard
      
      TRUE ~ Ingredients_standardised))
}


