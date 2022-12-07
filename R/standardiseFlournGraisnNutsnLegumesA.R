#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "A".
#' @title standardiseFlournGraisnNutsnLegumesA
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "A".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "A" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesA <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'flour') ~ 'almond flour',
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'extract') ~ 'almond extract',
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'salt') & !str_detect(Ingredients, 'potato') ~ 'almond salted',
      str_detect(Ingredients, 'almond') & !str_detect(Ingredients, 'potato') ~ 'almond',
      str_detect(Ingredients, 'apple') &
        str_detect(Ingredients, 'cinnamon') & str_detect(Ingredients, 'muesli') ~ 'muesli apple-cinnamon',

      TRUE ~ Ingredients_standardised))
}
