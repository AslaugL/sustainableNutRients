#' Standardising ingredients names in a recipe.
#' @title standardiseFoods
#'
#' @description Standardise names of all types of ingredients, running all the functions for standardising names of individual categories of food.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with ingredients with standardised names.
#'
#' @export
standardiseFoods <- function(df) {
  df  %>%

    #Turn Ingredients lowercase

    mutate(
      Ingredients = stri_trans_tolower(Ingredients),
      #Create an ingredients_standardised column
      Ingredients_standardised = "") %>%

    #Standardise
    standardiseFruitVeg() %>%
    standardiseDairynSubs() %>%
    standardiseHerbsnSpices() %>%
    standardiseFlournGraisnNutsnLegumes() %>%
    standardiseMushrooms() %>%
    standardiseOils() %>%
    standardisePoultry() %>%
    standardiseRedMeat() %>%
    standardiseSeafood() %>%
    standardiseOthers() %>%

    #Rename original ingrediet column
    rename(Original_ingredients = Ingredients)

}

