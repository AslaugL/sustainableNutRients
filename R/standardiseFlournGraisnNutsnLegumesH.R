#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "H".
#' @title standardiseFlournGraisnNutsnLegumesH
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "H".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "H" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesH <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'burger|slider') & str_detect(Ingredients, 'bread|bun') & str_detect(Ingredients, 'coarse|wholemeal|brown') &
        !str_detect(Ingredients, 'margarine') ~ 'hamburger bread coarse',
      str_detect(Ingredients, 'burger|slider') & str_detect(Ingredients, 'bread|bun') &
        !str_detect(Ingredients, 'margarine') ~ 'hamburger bread',
      str_detect(Ingredients, 'burger|slider') & str_detect(Ingredients, 'bread|bun') & !str_detect(Ingredients, 'margarine') ~ 'hamburger bread',
      str_detect(Ingredients, 'hazelnut') & str_detect(Ingredients, 'butter') ~ 'hazelnut butter',
      str_detect(Ingredients, 'hazelnut') & !str_detect(Ingredients, 'oil|granola') ~ 'hazelnut',
      str_detect(Ingredients, 'hemp') & str_detect(Ingredients, 'seed') ~ 'seed hemp',

      TRUE ~ Ingredients_standardised))
}

