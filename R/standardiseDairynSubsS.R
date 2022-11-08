#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "S".
#' @title standardiseDairynSubsS
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "S".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "S" with standardised names.
#'
#' @export
standardiseDairynSubsS <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'lightroom 10 %|light flow 10 %') | str_detect(Ingredients, 'sour') & str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'light') ~ 'sour cream 10 %',
      str_detect(Ingredients, 'lightroom 18 %|light stream|light flow 18%|lightroom 18%|light flow 18 %|light rømme') |
        (str_detect(Ingredients, 'sour cream|soured cream') & !str_detect(Ingredients, '%')) |
        (str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'light') & !str_detect(Ingredients, 'alternativ')) ~ 'sour cream 18 %', #Standard
      str_detect(Ingredients, 'seatroom|seat cream') | str_detect(Ingredients, 'sour') & str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'full fat') ~ 'sour cream 35 %',

      TRUE ~ Ingredients_standardised))
}
