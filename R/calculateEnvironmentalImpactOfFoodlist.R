#' Calculate environmental impact of a list of foods with their amounts in hektograms.
#' @title calculateEnvironmentalImpactOfFoodlist
#'
#' @description Calculate environmental impact of a list of foods with their amounts in hektograms.
#'
#' @param df The output from findFoodInDatabase "sustainability". To calculate environmental impact per 100 g amounts must be in hektograms.
#' @param calculate_sustainability Calculate environmental impact for the "total", "per 100 g" or "per portion". The default is for the "total"
#' of the list of foods. To calculate per 100 g, amounts of ingredients must be in hektograms. If "per portion" df must have a
#' "number_of_portions" column.
#' @param identifier  A the column name for a column with an identifier for where the ingredients comes from. Default is "recipe_name".
#'
#' @return The environmental impact of each individual variable in the "identifier" column, either by total weight of all ingredients,
#' per 100 grams or per portion.
#'
#' @export
calculateEnvironmentalImpactOfFoodlist <- function(df, calculate_sustainability = 'total', identifier = 'recipe_name') {

  if(calculate_sustainability == 'total') {

    df %>%
      mutate(environmental_impact = Amounts*environmental_impact_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, environmental_impact) %>%
      group_by(across(all_of(identifier)), nutrient) %>%
      summarise(environmental_impact = sum(environmental_impact, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(Ingredients = all_of(identifier))

  }else if(calculate_sustainability == 'per 100 g'){

    df %>%
      mutate(environmental_impact = Amounts*environmental_impact_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, environmental_impact, Amounts) %>%
      group_by(across(all_of(identifier)), nutrient) %>%
      summarise(environmental_impact = sum(environmental_impact, na.rm = TRUE),
                total_recipe_weight = sum(Amounts, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(environmental_impact_per_hektogram = environmental_impact/total_recipe_weight) %>%
      select(-c(total_recipe_weight, environmental_impact)) %>%
      rename(Ingredients = all_of(identifier))

  }else if(calculate_sustainability == 'per portion') {

    df %>%
      mutate(environmental_impact = Amounts*environmental_impact_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, environmental_impact, Amounts, number_of_portions) %>%
      group_by(across(all_of(identifier)), nutrient, number_of_portions) %>%
      summarise(environmental_impact = sum(environmental_impact, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(environmental_impact_per_portion = environmental_impact/number_of_portions) %>%
      rename(Ingredients = all_of(identifier))

  }else{
    stop("calculate_sustainability must be either 'total', 'per 100 g' or 'per portion'")
  }


}
