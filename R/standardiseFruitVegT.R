#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "T".
#' @title standardiseFruitVegT
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "T".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "T" with standardised names.
#'
#' @export
standardiseFruitVegT <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'tamarind juice') ~ 'tamarind juice',
      str_detect(Ingredients, 'tomat') & (str_detect(unit, 'can|box|hp') | str_detect(Ingredients, 'can|box|drain')) & !str_detect(Ingredients, 'water|mackerel|beans|sauce|soup') ~ 'tomato canned',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'beef') ~ 'tomato beef',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'pur') | str_detect(Ingredients, 'passata') ~ 'tomato puree',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'bunch') ~ 'tomato bunch',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'sun') ~ 'tomato sun dried',
      str_detect(Ingredients, 'tomat') & str_detect(Ingredients, 'dried') ~ 'tomato dried',
      str_detect(Ingredients, 'ketchup') ~ 'tomato ketchup',
      str_detect(Ingredients, 'tomat') &
        !str_detect(Ingredients, 'canned|alsa|sauce|canned|cherry|can|box|pur\u00E9e|puree|paste|mackerel|tube|vegetable|sun|beans|cheese|taffel') ~ 'tomato',
      str_detect(Ingredients, 'turnip') ~ 'turnip',

      TRUE ~ Ingredients_standardised
    ))
}

