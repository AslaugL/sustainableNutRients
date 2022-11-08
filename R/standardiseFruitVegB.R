#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "B".
#' @title standardiseFruitVegB
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "B".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "B" with standardised names.
#'
#' @export
standardiseFruitVegB <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'banan') & !str_detect(Ingredients, 'shallot') ~ 'banana',
      str_detect(Ingredients, 'beet|better') & str_detect(Ingredients, 'yellow') ~ 'beetroot yellow',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'cooked|boiled') ~ 'beetroot cooked',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'root') & !str_detect(Ingredients, 'pickle') ~ 'beetroot',
      str_detect(Ingredients, 'curran') & str_detect(Ingredients, 'jam') ~ 'jam currant',
      str_detect(Ingredients, 'black curran') & !str_detect(Ingredients, 'juice') ~ 'black currant',
      str_detect(Ingredients, 'currant') & !str_detect(Ingredients, 'juice') ~ 'black currant', #Use as default
      str_detect(Ingredients, 'blueberr') & str_detect(Ingredients, 'pie fill') ~ 'blueberries pie filling',
      str_detect(Ingredients, 'blueberr') & str_detect(Ingredients, 'jam') ~ 'jam blueberries',
      str_detect(Ingredients, 'blueberr') ~ 'blueberries',
      str_detect(Ingredients, 'broccolini') ~ 'broccolini',
      str_detect(Ingredients, 'broccoli') ~ 'broccoli', #The 'broccoli peas' are broccoli florets according to the recipe site
      str_detect(Ingredients, 'brussel') & str_detect(Ingredients, 'sprout') ~ 'brussel sprout',
      str_detect(Ingredients, 'butternut') ~ 'winter squash butternut',

      TRUE ~ Ingredients_standardised

    ))
}

