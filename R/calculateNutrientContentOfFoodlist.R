#' Calculate nutrient content of a list of foods with their amounts in hektograms.
#' @title calculateNutrientContentOfFoodlist
#'
#' @description Calculate nutrient content of a list of foods with their amounts in hektograms.
#'
#' @param df The output from findFoodInDatabase "nutrients". To calculate nutrient content per 100 g amounts must be in hektograms.
#' @param calculate_nutrients Calculate nutrient content for the "total", "per 100 g" or "per portion". The default is for the "total"
#' of the list of foods. To calculate per 100 g, amounts of ingredients must be in hektograms. If "per portion" df must have a
#' "number_of_portions" column.
#' @param identifier  A the column name for a column with an identifier for where the ingredients comes from. Default is "recipe_name".
#'
#' @return The nutrient content of each individual variable in the "identifier" column, either by total weight of all ingredients,
#' per 100 grams or per portion.
#'
#' @export
calculateNutrientContentOfFoodlist <- function(df, calculate_nutrients = 'total', identifier = 'recipe_name') {

  if(calculate_nutrients == 'total') {

    df %>%
      mutate(nutrient_content = Amounts*nutrient_content_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, nutrient_content) %>%
      group_by(across(all_of(identifier)), nutrient) %>%
      summarise(nutrient_content = sum(nutrient_content, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(Ingredients = all_of(identifier))

  }else if(calculate_nutrients == 'per 100 g'){

    df %>%
      mutate(nutrient_content = Amounts*nutrient_content_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, nutrient_content, Amounts) %>%
      group_by(across(all_of(identifier)), nutrient) %>%
      summarise(nutrient_content = sum(nutrient_content, na.rm = TRUE),
                total_recipe_weight = sum(Amounts, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(nutrient_content_per_hektogram = nutrient_content/total_recipe_weight) %>%
      select(-c(total_recipe_weight, nutrient_content)) %>%
      rename(Ingredients = all_of(identifier))

  }else if(calculate_nutrients == 'per portion') {

    df %>%
      mutate(nutrient_content = Amounts*nutrient_content_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, nutrient_content, Amounts, number_of_portions) %>%
      group_by(across(all_of(identifier)), nutrient, number_of_portions) %>%
      summarise(nutrient_content = sum(nutrient_content, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(nutrient_content_per_portion = nutrient_content/number_of_portions) %>%
      rename(Ingredients = all_of(identifier))

  }else{
    stop("calculate_nutrients must be either 'total', 'per 100 g' or 'per portion'")
  }


}
