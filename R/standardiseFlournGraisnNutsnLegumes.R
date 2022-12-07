#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes.
#' @title standardiseFlournGraisnNutsnLegumes
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumes <- function(df) {
  df  %>%

    #Standardise
    standardiseFlournGraisnNutsnLegumesA() %>%
    standardiseFlournGraisnNutsnLegumesB() %>%
    standardiseFlournGraisnNutsnLegumesC() %>%
    standardiseFlournGraisnNutsnLegumesE() %>%
    standardiseFlournGraisnNutsnLegumesF() %>%
    standardiseFlournGraisnNutsnLegumesG() %>%
    standardiseFlournGraisnNutsnLegumesH() %>%
    standardiseFlournGraisnNutsnLegumesL() %>%
    standardiseFlournGraisnNutsnLegumesN() %>%
    standardiseFlournGraisnNutsnLegumesO() %>%
    standardiseFlournGraisnNutsnLegumesP() %>%
    standardiseFlournGraisnNutsnLegumesQ() %>%
    standardiseFlournGraisnNutsnLegumesR() %>%
    standardiseFlournGraisnNutsnLegumesS() %>%
    standardiseFlournGraisnNutsnLegumesT() %>%
    standardiseFlournGraisnNutsnLegumesW()

}

