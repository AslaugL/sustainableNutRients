#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "S".
#' @title standardiseFlournGraisnNutsnLegumesS
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "S".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "S" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesS <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'stick') ~ 'salt stick',
      str_detect(Ingredients, 'sesame') & str_detect(Ingredients, 'seed') ~ 'sesame seed',
      str_detect(Ingredients, 'semolina') ~ 'wheat flour semolina',
      str_detect(Ingredients, 'shortcrust pastry') ~ 'shop-bought shortcrust pastry',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'low fat|low-fat') ~ 'soy milk low fat',
      str_detect(Ingredients, 'sunflower') & str_detect(Ingredients, 'seed') & !str_detect(Ingredients, 'oil') ~ 'seed sunflower',

      TRUE ~ Ingredients_standardised))
}

