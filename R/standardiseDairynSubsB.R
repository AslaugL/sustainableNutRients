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

      #Biola
      str_detect(Ingredients, 'biola') & str_detect(Ingredients, 'without added sugar|no added sugar') ~ 'biola yoghurt sugar free',
      str_detect(Ingredients, 'biola') & str_detect(Ingredients, 'berry|berries') ~ 'biola yoghurt berry flavored',
      str_detect(Ingredients, 'biola') & str_detect(Ingredients, 'melon|passion fruit|mango|apple|rhubarb') ~ 'biola yoghurt fruit flavored',
      str_detect(Ingredients, 'biola') ~ 'biola yoghurt',

      #Butter
      str_detect(Ingredients, 'ghee|clarified butter') ~ 'butter clarified ghee',
      str_detect(Ingredients, 'buttercream') ~ 'buttercream',
      str_detect(Ingredients, 'spice') & str_detect(Ingredients, 'butter') ~ 'spice butter',
      str_detect(Ingredients, 'butter|sm\u00F8r') & str_detect(Ingredients, 'unsalted|usalted') ~ 'unsalted butter',
      str_detect(Ingredients, 'butter') & str_detect(Ingredients, 'frying|browning|roasting|greasing|brushing') ~ 'butter for cooking',
      str_detect(Ingredients, 'butter|sm\u00F8r') &
        !str_detect(Ingredients, 'dough|pepper|sour cream|butternut|pastry|milk|beans|peanut|vanilla aroma|nut|almond|salad|chicken') ~ 'butter',
      #Buttermilk
      str_detect(Ingredients, 'buttermilk') & !str_detect(Ingredients, 'dough') ~ 'buttermilk',

      TRUE ~ Ingredients_standardised))
}
