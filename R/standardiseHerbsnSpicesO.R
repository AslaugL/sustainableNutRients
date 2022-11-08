#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "O".
#' @title standardiseHerbsnSpices=
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "O".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "O" with standardised names.
#'
#' @export
standardiseHerbsnSpicesO <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'powder') ~ 'onion powder',
      str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'seed') ~ 'onion seed',
      str_detect(Ingredients, 'oregano') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf') | str_detect(Amounts, 'twig|bunch|leaf|neve|dl')) ~ 'oregano fresh herbs',
      str_detect(Ingredients, 'oregano') & str_detect(Ingredients, 'dried|spice') ~ 'oregano dried',
      str_detect(Ingredients, 'oregano') ~ 'oregano dried', #Standard

      TRUE ~ Ingredients_standardised))
}


