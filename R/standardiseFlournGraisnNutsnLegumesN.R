#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "N".
#' @title standardiseFlournGraisnNutsnLegumesN
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "N".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "N" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesN <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      ((str_detect(Ingredients, 'nacho|nachips') | str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'chip')) & !str_detect(Ingredients, 'burger|dip')) & str_detect(Ingredients, 'cheese|chili|garlic') ~ 'nacho flavored',
      (str_detect(Ingredients, 'nacho|nachips') | str_detect(Ingredients, 'tortilla') & str_detect(Ingredients, 'chip')) & !str_detect(Ingredients, 'burger|dip') ~ 'nacho',
      str_detect(Ingredients, 'taco') & str_detect(Ingredients, 'shell|tub|tray|bowl') ~ 'taco shell',

      str_detect(Ingredients, 'nut') & str_detect(Ingredients, 'mix') & !str_detect(Ingredients, 'yoghurt|yogurt') ~ 'nuts mixed',

      TRUE ~ Ingredients_standardised))
}

