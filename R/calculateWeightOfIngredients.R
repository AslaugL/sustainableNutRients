#' Calculate weight of ingredients after using findFoodInDatabase "volume_weight".
#' @title calculateWeightOfIngredients
#'
#' @description Calculate weight of ingredients after using findFoodInDatabase "volume_weight".
#'
#' @param df Output from findFoodInDatabase "volume_weight".
#' @param weight_in Unit the weight should be calculated to. Three options "grams", "hektograms" or "kilograms".
#'
#' @return A dataframe with the weight of ingredients in the chosen weight unit.
#'
#' @export
calculateWeightOfIngredients <- function(df, weight_in = "hektograms") {

  #Rename amounts column
  new_amounts <- df %>%
    rename(tmp = Amounts)

  if(weight_in == 'grams') {

    new_amounts <- new_amounts %>%
      mutate(
        Amounts = .data$tmp*grams_per_unit,
        unit = 'grams')

  }else if(weight_in == 'hektograms') {

    new_amounts <- new_amounts %>%
      mutate(
        Amounts = .data$tmp*grams_per_unit/100,
        unit = 'hektograms')

  }else if(weight_in == 'kilograms'){

    new_amounts <- new_amounts %>%
      mutate(
        Amounts = .data$tmp*grams_per_unit/1000,
        unit = 'kilograms')

  }

  new_amounts %>%
    select(-c(database_reference, database_ID, grams_per_unit, .data$tmp))%>%
    select(Original_ingredients, Ingredients, Amounts, unit, everything())
}
