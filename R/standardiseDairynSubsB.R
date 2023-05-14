#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "B".
#' @title standardiseDairynSubsB
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "B".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "B" with standardised names.
#'
#' @export
standardiseDairynSubsB <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      #Butter
      str_detect(Ingredients, 'ghee|clarified butter') ~ 'butter clarified ghee',
      str_detect(Ingredients, 'buttercream') ~ 'buttercream',
      str_detect(Ingredients, 'spice') & str_detect(Ingredients, 'butter') ~ 'spice butter',
      str_detect(Ingredients, 'butter|sm\u00F8r') & str_detect(Ingredients, 'unsalted|usalted') ~ 'unsalted butter',
      str_detect(Ingredients, 'butter|sm\u00F8r') & !str_detect(Ingredients, 'frying|dough|unsalted|browning|brushing|pepper|sour cream|roasting|butternut|pastry|greasing|milk|beans|peanut|vanilla aroma|nut|almond') ~ 'butter',
      str_detect(Ingredients, 'butter') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'butter for cooking',
      #Buttermilk
      str_detect(Ingredients, 'buttermilk') & !str_detect(Ingredients, 'dough') ~ 'buttermilk',

      TRUE ~ Ingredients_standardised))
}
