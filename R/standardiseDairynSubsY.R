#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "Y".
#' @title standardiseDairynSubsY
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "Y".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "Y" with standardised names.
#'
#' @export
standardiseDairynSubsY <- function(df) {

  #Yoghurt flavors
  #Fruit flavors
  fruits <- 'apple|banana|coconut|lemon|lime|melon|mango|peach|prune|passion|rhubarb|tropical'
  #Berries
  berries <- 'berr'
  #Grains
  with_grains_nuts <- 'korn|muesli|\\boat\\b|with nut|pecan'
  #Other flavors
  other_flavors <- 'vanilla|caramel|pannacotta|toffee|honey'

  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(

      #Skyr
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
        str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, berries) ~ 'yoghurt skyr berries flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
         str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, other_flavors) ~ 'yoghurt skyr flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
         str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, fruits) ~ 'yoghurt skyr fruit flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
         str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, with_grains_nuts) ~ 'yoghurt skyr with grains',
      str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
        str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt|\\bskyr') ~ 'yoghurt plain skyr',

      #Low fat
      str_detect(Ingredients, 'yoplait|double 0 %') & str_detect(Ingredients, berries) ~ 'yoghurt low fat berries flavored',
      str_detect(Ingredients, 'yoplait|double 0 %') & str_detect(Ingredients, fruits) ~ 'yoghurt low fat fruit flavored',
      str_detect(Ingredients, 'yoplait|double 0 %') & str_detect(Ingredients, other_flavors) ~ 'yoghurt low fat flavored',
      str_detect(Ingredients, 'yoplait|double 0 %') ~ 'yoghurt plain low fat',

      #Greek / turkish yoghurt
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, 'greek') & str_detect(Ingredients, paste0(fruits, berries, other_flavors, collapse = "|")) ~ 'yoghurt greek flavored',
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, 'greek') & str_detect(Ingredients, '2 %') ~ 'yoghurt plain greek low fat',
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, 'greek') ~ 'yoghurt plain greek',
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, 'turkish') ~ 'yoghurt plain turkish',

      #Standard yoghurt
      ((str_detect(Ingredients, 'yogurt|yoghurt') &
         str_detect(Ingredients, 'plant-based|plant based|vegan|soy')) | str_detect(Ingredients, "kogurt|yokos|yoko's")) &
        str_detect(Ingredients, paste0(fruits, berries, other_flavors, collapse = "|")) ~ 'yoghurt plant-based flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, 'plant-based|plant based|vegan|soy')) | str_detect(Ingredients, "kogurt|yokos|yoko's") ~ 'yoghurt plant-based',

      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, with_grains_nuts) ~ 'yoghurt with grains',
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, berries) ~ 'yoghurt berries flavored',
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, fruits) ~ 'yoghurt fruit flavored',
      str_detect(Ingredients, 'yogurt|yoghurt') &
        str_detect(Ingredients, other_flavors) ~ 'yoghurt flavored',

      #Standard
      str_detect(Ingredients, 'yogurt|yoghurt') &
        !str_detect(Ingredients, '(?<!coco)nut') ~ 'yoghurt plain',

      TRUE ~ Ingredients_standardised))
}







