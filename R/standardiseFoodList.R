#' Standardise a list of foods into standardised food names and units.
#' @title standardiseFoodList
#'
#' @description Standardises the names of foods and their units.
#'
#' @param df A dataframe with an Ingredients column.
#' @param convertCitrusPeelAndJuice Should citrus juice and peel be converted to their equivalent whole fruits? TRUE or FALSE. Default TRUE.
#' @param convertFreshHerbsToDried Should fresh herbs (those not found in Matvaretabellen) be converted to dried? TRUE or FALSE. Default TRUE.
#' @param cookedToRaw Should cooked ingredients be converted to their raw equivalents? TRUE or FALSE. Default TRUE.
#'
#' @return A dataframe with the amounts and standardised units of foods in their own column, a new Ingredients column with standardised
#'  names and an Original_ingredients column with the original ingredient names.
#'
#' @export
standardiseFoodList <- function(df, convertCitrusPeelAndJuice = TRUE, convertFreshHerbsToDried = TRUE, cookedToRaw = TRUE) {

  standardised <- df %>%
    #Standardise how the amounts are written
    standardiseAmounts() %>%
    #Extract amounts and units to their own columns
    extractAmounts() %>%
    #Standardise food name
    standardiseFoodNames() %>%
    #Standardise unit types
    standardiseUnitsType()

   #Convert citrus zest/juice to the equivalent of whole fruit
    if(isTRUE(convertCitrusPeelAndJuice)) {

      standardised <- standardised %>%
        calculateCitrusJuiceZest()

    } else if(isFALSE(convertCitrusPeelAndJuice)) {
      standardised <- standardised
    }

    #Convert fresh herbs to dried/dried her to fresh
    if(isTRUE(convertFreshHerbsToDried)) {

      standardised <- standardised %>%
        convertFreshHerbsToDried()

    } else if(isFALSE(convertFreshHerbsToDried)) {
      standardised <- standardised
    }

    #Convert cooked products into their raw equivalents, using the convertion factors from Helsedirekttortatet Maal Vekt og Porsjonsstorrelser
    if(isTRUE(cookedToRaw)) {

      standardised <- standardised %>%
        cookedToRaw()

    } else if(isFALSE(cookedToRaw)) {
      standardised <- standardised
    }

}
