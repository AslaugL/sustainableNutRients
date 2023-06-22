#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "M".
#' @title standardiseFruitVegM
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "M".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "M" with standardised names.
#'
#' @export
standardiseFruitVegM <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'mango') & str_detect(Ingredients, 'dri|dry') ~ 'mango dried',
      str_detect(Ingredients, 'mango') & str_detect(Ingredients, 'puree') ~ 'mango puree',
      str_detect(Ingredients, 'mango') & str_detect(Ingredients, 'juice') ~ 'mango juice',
      str_detect(Ingredients, 'mango') & !str_detect(Ingredients, 'chutney|sauce|yoghurt|yogurt') ~ 'mango',

      str_detect(Ingredients, 'mulberr') & str_detect(Ingredients, 'white') ~ 'mulberries white',

      #Melons
      str_detect(Ingredients, 'melon') & str_detect(Ingredients, 'honeydew') ~ 'melon honeydew',
      str_detect(Ingredients, 'melon') & str_detect(Ingredients, 'piel de sapo') ~ 'melon piel de sapo',
      str_detect(Ingredients, 'melon') & str_detect(Ingredients, 'mix') ~ 'melon mix',

      TRUE ~ Ingredients_standardised
    ))
}

