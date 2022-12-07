#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "G".
#' @title standardiseFlournGraisnNutsnLegumesG
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "G".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "G" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesG <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'granola') & str_detect(Ingredients, 'hazelnut') & str_detect(Ingredients, 'coconut') ~ 'granola hazelnut and coconut',
      str_detect(Ingredients, 'granola') ~ 'granola',
      str_detect(Ingredients, 'gluten-free|gluten free') & str_detect(Ingredients, 'flour') ~ 'gluten free flour',

      TRUE ~ Ingredients_standardised))
}

