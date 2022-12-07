#' Standardising ingredients names in a recipe, here all fruit and vegetables.
#' @title standardiseFruitVeg
#'
#' @description Standardise names of fruit and vegetables.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables with standardised names in the "Ingredients_standardised" column.
#'
#' @export
standardiseFruitVeg <- function(df){
  df  %>%

    #Standardise
    standardiseFruitVegA() %>%
    standardiseFruitVegB() %>%
    standardiseFruitVegC() %>%
    standardiseFruitVegD() %>%
    standardiseFruitVegE() %>%
    standardiseFruitVegF() %>%
    standardiseFruitVegG() %>%
    standardiseFruitVegH() %>%
    standardiseFruitVegI() %>%
    standardiseFruitVegJ() %>%
    standardiseFruitVegK() %>%
    standardiseFruitVegL() %>%
    standardiseFruitVegM() %>%
    standardiseFruitVegN() %>%
    standardiseFruitVegO() %>%
    standardiseFruitVegP() %>%
    standardiseFruitVegQ() %>%
    standardiseFruitVegR() %>%
    standardiseFruitVegS() %>%
    standardiseFruitVegT() %>%
    standardiseFruitVegW()


}

