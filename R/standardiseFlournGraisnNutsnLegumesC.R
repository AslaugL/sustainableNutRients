#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "C".
#' @title standardiseFlournGraisnNutsnLegumesC
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "C".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "C" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesC <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      #Cashews
      str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'nut') ~ 'cashew nut salt',
      str_detect(Ingredients, 'cashew') & str_detect(Ingredients, 'roast|toast') ~ 'cashew nut roasted',
      str_detect(Ingredients, 'cashew') ~ 'cashew nut',

      #Chia
      str_detect(Ingredients, 'chia seed') ~ 'chia seed',

      #Chickpeas
      str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'chick') &
        (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'chick pea canned',
      str_detect(Ingredients, 'flour') & str_detect(Ingredients, 'chick|gram') ~ 'chick pea flour',
      str_detect(Ingredients, 'chickpea|chick pea') & !str_detect(Ingredients, 'lentil') ~ 'chick pea',

      #Ciabatta
      str_detect(Ingredients, 'ciabatta|italian bread') ~ 'ciabatta',

      #Corn starch/meal
      (str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'starch')) | str_detect(Ingredients, 'maizena') | str_detect(Ingredients, 'corn') & !str_detect(Ingredients, 'oil|pepper|crispy|cob|minim|coat|water to the|flour') & str_detect(Amounts, 'tbsp|tsp') ~ 'corn starch',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'flour') ~ 'corn flour',
      str_detect(Ingredients, 'corn') & str_detect(Ingredients, 'meal') & str_detect(Ingredients, 'mix') ~ 'corn meal mix',
      str_detect(Ingredients, 'polenta') ~ 'corn flour polenta',

      #Cookies, crackers and biscuits
      str_detect(Ingredients, 'cookie') & str_detect(Ingredients, 'amarettini') ~ 'cookies amarettini',
      str_detect(Ingredients, 'cream cracker') ~ 'cracker cream',
      str_detect(Ingredients, 'saltine cracker') ~ 'cracker saltine',
      str_detect(Ingredients, 'graham cracker') & str_detect(Ingredients, 'crust') ~ 'graham cracker crust',
      str_detect(Ingredients, 'graham cracker') ~ 'graham cracker',

      #Couscous
      str_detect(Ingredients, 'couscous|cous cous') ~ 'couscous',

      TRUE ~ Ingredients_standardised))
}
