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

  #Drop NA values help function
  drop_nas <- function(df, identifier) {

    if(sum(is.na(df$environmental_impact_indicator)) > 0) {

      #Print recipe names and warning
      has_missing <- df %>%
        filter(is.na(environmental_impact_indicator)) %>%
        select(all_of(identifier)) %>% unique() %>%
        as.character()

      print(paste0(
        "These ", noquote(identifier), " ", has_missing,
        " have values not mapped to Matvaretabellen, be sure mapping has been correct. NA values are removed before calculating environmental impact"))

      #Filter out the NA foods
      without_NAs <- df %>%
        filter(!is.na(environmental_impact_indicator))

    } else {

      without_NAs <- df

    }

    without_NAs

  }


  if(calculate_nutrients == 'total') {

    without_NAs <- drop_nas(df = df, identifier = identifier)

    without_NAs %>%
      mutate(nutrient_content = Amounts*nutrient_content_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, nutrient_content) %>%
      group_by(across(all_of(identifier)), nutrient) %>%
      summarise(nutrient_content = sum(nutrient_content, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(recipe_name = all_of(identifier))

  }else if(calculate_nutrients == 'per 100 g'){

    without_NAs <- drop_nas(df = df, identifier = identifier)

    without_NAs %>%
      mutate(nutrient_content = Amounts*nutrient_content_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, nutrient_content, Amounts) %>%
      group_by(across(all_of(identifier)), nutrient) %>%
      summarise(nutrient_content = sum(nutrient_content, na.rm = TRUE),
                total_recipe_weight = sum(Amounts, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(nutrient_content_per_hektogram = nutrient_content/total_recipe_weight) %>%
      select(-c(total_recipe_weight, nutrient_content)) %>%
      rename(recipe_name = all_of(identifier))

  }else if(calculate_nutrients == 'per portion') {

    without_NAs <- drop_nas(df = df, identifier = identifier)

    without_NAs %>%
      mutate(nutrient_content = Amounts*nutrient_content_per_hektogram) %>%
      select(all_of(identifier), Ingredients, nutrient, nutrient_content, Amounts, number_of_portions) %>%
      group_by(across(all_of(identifier)), nutrient, number_of_portions) %>%
      summarise(nutrient_content = sum(nutrient_content, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(nutrient_content_per_portion = nutrient_content/number_of_portions) %>%
      rename(recipe_name = all_of(identifier))

  }else{
    stop("calculate_nutrients must be either 'total', 'per 100 g' or 'per portion'")
  }


}
