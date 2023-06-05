#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "M".
#' @title standardiseFlournGraisnNutsnLegumesM
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "M".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "M" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesM <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, '\\bmillet\\b') ~ 'millet',

      #Muesli
      str_detect(Ingredients, 'muesli') & str_detect(Ingredients, 'blueberr') & !str_detect(Ingredients, 'yoghurt|yogurt|bread') ~ 'muesli blueberries',
      str_detect(Ingredients, 'muesli') & str_detect(Ingredients, 'fruit') & !str_detect(Ingredients, 'yoghurt|yogurt|bread') ~ 'muesli fruit',
      str_detect(Ingredients, 'muesli') & str_detect(Ingredients, 'berr') & !str_detect(Ingredients, 'yoghurt|yogurt|bread') ~ 'muesli berries',
      str_detect(Ingredients, 'muesli') & !str_detect(Ingredients, 'yoghurt|yogurt|bread') ~ 'muesli',

      TRUE ~ Ingredients_standardised))
}

