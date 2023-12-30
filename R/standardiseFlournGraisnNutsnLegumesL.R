#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "L".
#' @title standardiseFlournGraisnNutsnLegumesL
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "L".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "L" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesL <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'dry|dried') & str_detect(Ingredients, 'red') ~ 'lentils dried red',
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'dry|dried') & str_detect(Ingredients, 'green|brown') ~ 'lentils dried green',
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'dry|dried') ~ 'lentils dried',
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'red') ~ 'lentils canned red',
      str_detect(Ingredients, 'lentil') & str_detect(Ingredients, 'green|brown') ~ 'lentils canned green',
      str_detect(Ingredients, 'lentil') & str_detect(unit, 'box|can') |
        str_detect(Ingredients, 'lentil') ~ 'lentils canned', # Use as default

      TRUE ~ Ingredients_standardised))
}

