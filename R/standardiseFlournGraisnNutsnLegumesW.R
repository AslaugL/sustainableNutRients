#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "W".
#' @title standardiseFlournGraisnNutsnLegumesW
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "W".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "W" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesW <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(

      str_detect(Ingredients, 'waffle') & str_detect(Ingredients, 'mix|powder') ~ 'waffle powder mix',
      str_detect(Ingredients, 'waffle') & str_detect(Ingredients, 'batter') ~ 'waffle batter',
      str_detect(Ingredients, 'waffle') ~ 'waffle',

      str_detect(Ingredients, 'walnut') &
        !str_detect(Ingredients, 'oil') ~ 'walnut',

      #Wheat flour
      str_detect(Ingredients, 'spelt|farro') & str_detect(Ingredients, 'flour') |
        str_detect(Ingredients, 'farro') ~ 'wheat flour spelt',
      str_detect(Ingredients, 'wheat') & str_detect(Ingredients, 'bran') ~ 'wheat bran',
      str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'wholemeal') |
        str_detect(Ingredients, 'wheat') & str_detect(Ingredients, 'whole') & str_detect(Ingredients, 'flour') ~ 'wheat flour wholemeal',
      str_detect(Ingredients, 'rye') & str_detect(Ingredients, 'flour') ~ 'wheat flour rye',
      str_detect(Ingredients, 'rye') & str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'whole') ~ 'wheat flour rye wholemeal',
      str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'pizza|tipo-00|tipo 00') ~ 'wheat flour tipo 00',
      (str_detect(Ingredients, 'wheat flour|all-purpose flour|plain flour|flour|durum wheat|self-raising') &
        !str_detect(Ingredients, 'whole|gram|tortilla|potato|corn|spelt|almond|coconut|gluten~free') & !str_detect(Ingredients, 'bread')) |
        str_detect(Ingredients, 'breading flour') ~ 'wheat flour',

      TRUE ~ Ingredients_standardised))
}

