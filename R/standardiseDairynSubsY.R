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
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'greek') ~ 'yoghurt greek',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
        str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, 'berries') ~ 'yoghurt skyr berries flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
         str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, 'caramel') ~ 'yoghurt skyr caramel flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
         str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, 'citrus') ~ 'yoghurt skyr citrus flavored',
      (str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
         str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt')) & str_detect(Ingredients, 'tropical') ~ 'yoghurt skyr tropical flavored',
      str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'skyr') |
        str_detect(Ingredients, 'icelandic yoghurt|icelandic yogurt') ~ 'yoghurt skyr',
      str_detect(Ingredients, 'yogurt|yoghurt') & str_detect(Ingredients, 'plant-based|plant based|vegan') ~ 'yoghurt plant based',
      str_detect(Ingredients, 'yogurt|yoghurt') ~ 'yoghurt plain',

      TRUE ~ Ingredients_standardised))
}







