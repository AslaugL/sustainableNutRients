#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "T".
#' @title standardiseHerbsnSpicesV
#'
#' @description Support function for "StandardiseHerbsnSpices", standardise names of herbs and spices starting with "V".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "V" with standardised names.
#'
#' @export
standardiseHerbsnSpicesV <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'pod|bean') ~ 'vanilla pod',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'bean') ~ 'paste vanilla bean',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'extract') ~ 'vanilla extract',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'essence') ~ 'vanilla essence',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'powder|seed') ~ 'vanilla powder',

      TRUE ~ Ingredients_standardised))
}
