#' Standardising food names in a recipe.
#' @title standardiseFoodNames
#'
#' @description Standardise names of all types of foods, running all the functions for standardising names of individual categories of food.
#'
#' @param df A dataframe with an Ingredients column, listing each food in an individual rows, an unit column noting which unit the food has, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with foods with standardised names.
#'
#' @export
standardiseFoodNames <- function(df) {
  df %>%

    mutate(
      #Turn Ingredients lowercase, remove commas etc and trim Ingredients column of whitespace
      Ingredients = Ingredients %>%
        stri_trans_tolower() %>%
        str_trim() %>%
        str_remove_all(",") %>%
        #Remove & sign
        str_replace("&", "and") %>%
        #Replace different ways to write salt
        str_replace('sea salt|maldon salt|maldonsalt|seasalt|flaked salt|flake salt', 'salt'),
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
    standardiseRedMeatnSubs() %>%
    standardiseSeafood() %>%
    standardiseCondiments() %>%
    standardiseBeverages() %>%
    standardiseOthers() %>%

    #Rename
    rename(
      #original ingredient column
      Original_ingredients = Ingredients,
      #new ingredient column
      Ingredients = Ingredients_standardised)

}

