#' Standardising ingredients names in a recipe, here oils.
#' @title standardiseOils
#'
#' @description Standardise names of oils
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with oils with standardised names.
#'
#' @export
standardiseOils <- function(df) {

  #Oil with flavors
  flavors <- 'lemon|garlic|basil|truffle|chili'

  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'canola|rapeseed') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'rapeseed oil for deep frying',
      str_detect(Ingredients, 'canola|rapeseed') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'rapeseed oil for cooking',
      str_detect(Ingredients, 'canola|rapeseed') & str_detect(Ingredients, 'oil')  ~ 'rapeseed oil',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'sichuan') ~ 'oil chili sichuan',
      str_detect(Ingredients, 'coconut oil') & str_detect(Ingredients, 'deep fry') ~ 'coconut oil for deep frying',
      str_detect(Ingredients, 'coconut oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'coconut oil for cooking',
      str_detect(Ingredients, 'coconut oil|coconut fat') ~ 'coconut oil',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'oil corn for deep frying',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'oil') ~ 'oil corn',

      str_detect(Ingredients, 'flax|linseed') & str_detect(Ingredients, 'oil') ~ 'oil flaxseed',

      str_detect(Ingredients, 'garlic oil') ~ 'garlic oil',

      str_detect(Ingredients, 'hazelnut oil') ~ 'hazelnut oil',

      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'olive oil for deep frying',
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'olive oil for cooking',
      str_detect(Ingredients, 'olive') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, flavors) ~ 'olive oil flavored',
      str_detect(Ingredients, 'olive oil|olivenolje|extra-virgin olive|olive oil') ~ 'olive oil',

      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'peanut oil for deep frying',
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'peanut oil for cooking',
      str_detect(Ingredients, 'peanut|groundnut') & str_detect(Ingredients, 'oil') ~ 'peanut oil',

      str_detect(Ingredients, 'sesame oil') ~ 'sesame oil',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'deep fry') ~ 'soybean oil for deep frying',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'frying|waking|browning|roasting|greasing|brushing') ~ 'soybean oil for deep frying',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'oil') ~ 'soybean oil',
      str_detect(Ingredients, 'sunflower oil') & str_detect(Ingredients, 'deep fry') ~ 'sunflower oil for deep frying',
      str_detect(Ingredients, 'sunflower oil') & str_detect(Ingredients, 'frying|waking|browning|roasting|greasing|brushing') ~ 'sunflower oil for cooking',
      str_detect(Ingredients, 'sunflower oil') ~ 'sunflower oil',

      str_detect(Ingredients, 'walnut') & str_detect(Ingredients, 'oil') ~ 'walnut oil',

      str_detect(Ingredients, 'oil') & str_detect(Ingredients, 'truffle') ~ 'oil truffle',

      str_detect(Ingredients, 'oil for deep frying') | Ingredients == 'frying oil' ~ 'vegetable oil for deep frying',
      Ingredients %in% c('oil for frying', 'oil for brushing', 'lubricating and brushing oil') ~ 'vegetable oil for cooking',
      str_detect(Ingredients, 'vegetable oil|salad oil|oil, neutral|vegetabie oil') & !str_detect(Ingredients, 'tomato') | Ingredients %in% c('oil', 'of oil', 'oil neutral')  ~ 'vegetable oil',

      str_detect(Ingredients, 'mayo') & str_detect(Ingredients, 'vegan') ~ 'mayonnaise vegan',
      str_detect(Ingredients, 'mayo') ~ 'mayonnaise',

      str_detect(Ingredients, "oil") & !str_detect(Ingredients, "boil|tomato") ~ "vegetable oil", #Standard

      TRUE ~ Ingredients_standardised))
}

