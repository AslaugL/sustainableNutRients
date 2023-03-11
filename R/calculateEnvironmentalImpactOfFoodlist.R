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
        " have values not mapped to SHARP DB, be sure mapping has been correct. NA values are removed before calculating environmental impact"))

      #Filter out the NA foods
      without_NAs <- df %>%
        filter(!is.na(environmental_impact_indicator))

    } else {

      without_NAs <- df

    }

    without_NAs

  }


  if(calculate_sustainability == 'total') {

    without_NAs <- drop_nas(df = df, identifier = identifier)

    without_NAs %>%
      mutate(environmental_impact = Amounts*environmental_impact_per_hektogram) %>%
      select(all_of(identifier), Ingredients, environmental_impact_indicator, environmental_impact) %>%
      group_by(across(all_of(identifier)), environmental_impact_indicator) %>%
      summarise(environmental_impact = sum(environmental_impact, na.rm = TRUE)) %>%
      ungroup() %>%
      rename(recipe_name = all_of(identifier))

  }else if(calculate_sustainability == 'per 100 g'){

    without_NAs <- drop_nas(df = df, identifier = identifier)

    without_NAs %>%
      mutate(environmental_impact = Amounts*environmental_impact_per_hektogram) %>%
      select(all_of(identifier), Ingredients, environmental_impact_indicator, environmental_impact, Amounts) %>%
      group_by(across(all_of(identifier)), environmental_impact_indicator) %>%
      summarise(environmental_impact = sum(environmental_impact, na.rm = TRUE),
                total_recipe_weight = sum(Amounts, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(environmental_impact_per_hektogram = environmental_impact/total_recipe_weight) %>%
      select(-c(total_recipe_weight, environmental_impact)) %>%
      rename(recipe_name = all_of(identifier))

  }else if(calculate_sustainability == 'per portion') {

    without_NAs <- drop_nas(df = df, identifier = identifier)

    without_NAs %>%
      mutate(environmental_impact = Amounts*environmental_impact_per_hektogram) %>%
      select(all_of(identifier), Ingredients, environmental_impact_indicator, environmental_impact, Amounts, number_of_portions) %>%
      group_by(across(all_of(identifier)), environmental_impact_indicator, number_of_portions) %>%
      summarise(environmental_impact = sum(environmental_impact, na.rm = TRUE)) %>%
      ungroup() %>%
      mutate(environmental_impact_per_portion = environmental_impact/number_of_portions) %>%
      rename(recipe_name = all_of(identifier))

  }else{
    stop("calculate_sustainability must be either 'total', 'per 100 g' or 'per portion'")
  }


}
