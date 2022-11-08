#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "Y".
#' @title standardiseDairynSubsY
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "Y".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "Y" with standardised names.
#'
#' @export
standardiseDairynSubsY <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'greek') ~ 'yoghurt greek',
      str_detect(Ingredients, 'yogurt|yoghurt') ~ 'yoghurt',

      TRUE ~ Ingredients_standardised))
}







