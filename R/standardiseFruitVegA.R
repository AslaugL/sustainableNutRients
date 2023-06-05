#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter 'A'.
#' @title standardiseFruitVegA
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "A".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "A" with standardised names.
#'
#' @export
standardiseFruitVegA <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'assorted raw vegetables|raw food mix') ~ 'assorted raw vegetables',
      str_detect(Ingredients, 'acorn squash') ~ 'winter squash acorn',
      str_detect(Ingredients, 'acai bowl') ~ 'acai bowl',
      str_detect(Ingredients, 'apple') & !str_detect(Ingredients, 'juice|vinegar|butter|pine|wasabi|sauce|syrup|muesli') ~ 'apple',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'sauce') ~ 'apple sauce',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'juice') & !str_detect(Ingredients, 'pine') ~ 'apple juice',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'syrup') ~ 'syrup apple',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'dried') ~ 'apricot dried',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'jam') ~ 'apricot jam',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'nectar') ~ 'apricot nectar',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'compot') ~ 'apricot compote',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'preserve') ~ 'apricot preserve',
      str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'drained') ~ 'apricot canned',
      str_detect(Ingredients, 'apricot')  ~ 'apricot',
      str_detect(Ingredients, 'artichoke') & str_detect(Ingredients, 'heart') & str_detect(Ingredients, 'drain|can') ~ 'artichoke heart canned',
      str_detect(Ingredients, 'earth shocks|artichoke') ~ 'artichoke',
      str_detect(Ingredients, 'asparagus') & str_detect(Ingredients, 'white') ~ 'asparagus white',
      str_detect(Ingredients, 'asparagus') & str_detect(Ingredients, 'bean') ~ 'bean green asparagus',
      str_detect(Ingredients, 'asparagus') ~ 'asparagus',
      str_detect(Ingredients, 'avocado') & !str_detect(Ingredients, 'wok') ~ 'avocado',

      TRUE ~ Ingredients_standardised

    ))
}

