#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "R".
#' @title standardiseFlournGraisnNutsnLegumesR
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "R".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "R" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesR <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked|boiled') & !str_detect(Ingredients, 'par|pre|beef|potato|vinegar|wine|barley|broccoli|cauliflower|liquo') ~ 'rice cooked',
      str_detect(Ingredients, 'rice|ris') & str_detect(Ingredients, 'basmati') ~ 'rice basmati',
      str_detect(Ingredients, 'rice|ris') & str_detect(Ingredients, 'risotto|arbori|paella') | str_detect(Ingredients, 'vialone nano') ~ 'rice risotto',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'jasmin') ~ 'rice jasmin',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'parboiled|pre boiled|boil in bag|boil-in-bag') ~ 'rice parboiled',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked') & str_detect(Ingredients, 'wild') ~ 'rice wild cooked',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'wild') ~ 'rice wild',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, '\\bcooked') ~ 'rice cooked',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'whole|brown') ~ 'rice brown long grain',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'sushi') ~ 'rice sushi',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'noodle') ~ 'rice noodle',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'porridge') ~ 'rice porridge',
      str_detect(Ingredients, 'porridge') ~ 'porridge',
      str_detect(Ingredients, 'rice') & !str_detect(Ingredients, 'beef|potato|vinegar|wine|barley|broccoli|cauliflower|liquo') ~ 'rice white long grain',

      TRUE ~ Ingredients_standardised))
}

