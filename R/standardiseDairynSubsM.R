#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "M".
#' @title standardiseDairynSubsM
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "M".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "M" with standardised names.
#'
#' @export
standardiseDairynSubsM <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      #Margarine
      str_detect(Ingredients, 'margarin') & str_detect(Ingredients, 'frying') ~ 'margarine for cooking',
      str_detect(Ingredients, 'margarin') & str_detect(Ingredients, 'plant-based|plant based|vegan') ~ 'margarine plant based',
      str_detect(Ingredients, 'margarin') ~ 'margarine',
      #Milk and substitutes
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'milk') ~ 'almond milk',
      str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'milk') ~ 'dairy imitate oatmilk',
      str_detect(Ingredients, 'milk|tinemelk') & !str_detect(Ingredients, 'whole|full-fat|coconut|butter|extra|almond|soy|evaporated|powder|condensed|chocolate') ~ 'milk 1 %', #Standard
      str_detect(Ingredients, 'milk|melk') & str_detect(Ingredients, 'whole|full-fat') ~ 'whole milk 3.5 %',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'extra light|skim milk') ~ 'milk 0.1 %',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'evaporated|condensed') ~ 'milk evaporated',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'powder') & str_detect(Ingredients, 'nonfat') ~ 'milk powder nonfat',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'coconut') ~ 'milk coconut',
      str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'coconut') ~ 'milk coconut cream full fat',


      TRUE ~ Ingredients_standardised))
}
