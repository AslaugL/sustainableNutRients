#' Standardising ingredients names in a recipe, here dairy and substitutes.
#' @title standardiseDairynSubs
#'
#' @description Standardise names of dairy and dairy substitute.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes standardised.
#'
#' @export
standardiseDairynSubs <- function(df) {
  df  %>%

    #Standardise
    standardiseDairynSubsB() %>%
    standardiseDairynSubsC() %>%
    standardiseDairynSubsK() %>%
    standardiseDairynSubsM() %>%
    standardiseDairynSubsQ() %>%
    standardiseDairynSubsS() %>%
    standardiseDairynSubsY()
}







