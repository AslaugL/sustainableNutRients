#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "G".
#' @title standardiseFruitVegG
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "G".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "G" with standardised names.
#'
#' @export
standardiseFruitVegG <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'chinese') ~ 'garlic chinese',
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'wild') ~ 'garlic wild',
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'powder|granule') ~ 'garlic powder',
      str_detect(Ingredients, 'garlic') & str_detect(Ingredients, 'whole') & !str_detect(Ingredients, 'salt|powder') ~ 'whole garlic',
      str_detect(Ingredients, 'garlic') & !str_detect(Ingredients, 'pickle|sauce|paste|oil|baguette|cheese|salt|dressing|scone|pâté|pate|taco mix') ~ 'garlic',
      str_detect(Ingredients, 'grape') & str_detect(Ingredients, 'juice') ~ 'grape juice',
      str_detect(Ingredients, 'grape') & str_detect(Ingredients, 'fruit') ~ 'grape fruit',
      str_detect(Ingredients, 'grape') ~ 'grape',
      str_detect(Ingredients, '\\bgoji\\b') ~ 'goji berry',

      TRUE ~ Ingredients_standardised
    ))
}

