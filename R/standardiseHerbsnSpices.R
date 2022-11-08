#' Standardising ingredients names in a recipe, here herbs and spices.
#' @title standardiseHerbsnSpices
#'
#' @description Standardise names of herbs and spices.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices with standardised names.
#'
#' @export
standardiseHerbsnSpices <- function(df) {
  df  %>%

    #Standardise
    standardiseHerbsnSpicesA() %>%
    standardiseHerbsnSpicesB() %>%
    standardiseHerbsnSpicesC() %>%
    standardiseHerbsnSpicesD() %>%
    standardiseHerbsnSpicesF() %>%
    standardiseHerbsnSpicesG() %>%
    standardiseHerbsnSpicesJ() %>%
    standardiseHerbsnSpicesL() %>%
    standardiseHerbsnSpicesM() %>%
    standardiseHerbsnSpicesN() %>%
    standardiseHerbsnSpicesO() %>%
    standardiseHerbsnSpicesP() %>%
    standardiseHerbsnSpicesR() %>%
    standardiseHerbsnSpicesS() %>%
    standardiseHerbsnSpicesT() %>%
    standardiseHerbsnSpicesV()

}
