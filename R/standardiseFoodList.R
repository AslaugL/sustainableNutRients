#' Standardise a list of foods into standardised food names and units.
#' @title standardiseFoodList
#'
#' @description Standardises the names of foods and their units.
#'
#' @param df A dataframe with an Ingredients column.
#'
#' @return A dataframe with the amounts and standardised units of foods in their own column, a new Ingredients column with standardised
#'  names and an Original_ingredients column with the original ingredient names.
#'
#' @export
standardiseFoodList <- function(df) {

  df %>%
    #Standardise how the amounts are written
    standardiseAmounts() %>%
    #Extract amounts and units to their own columns
    extractAmounts() %>%
    #Standardise food name
    standardiseFoodNames() %>%
    #Standardise unit types
    standardiseUnitsType()

}
