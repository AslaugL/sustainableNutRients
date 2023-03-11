#' Create new database entries to map foods to when using findFoodInDatabase.
#' @title createNewDatabaseEntry
#'
#' @description Create new database entries to map foods to when using findFoodInDatabase.
#'
#' @param df
#' \itemize{
#'  \item{For volume_weight, a dataframe three columns: either an identifier (default recipe_name) and Ingredients or "database_ID" depending on if Ingredient is new or already in database, "unit" and "grams_per_unit"}
#'  \item{For "nutrients", the output from calculateNutrientContentOfFoodlist with option "per 100 g" selected or a three column dataframe with
#'  an "Ingredients" column with food name, same as what can be found in the recipes or food list, a "nutrient" column with nutrient
#'  names similar to what is found in Matvaretabellen and "nutrient_content_per_100g" of the food.}
#'  \item{For "sustainability" the output from calculateEnvironmentalImpactOfFoodlist with option "per 100 g"
#'  selected, or a three column dataframe with an "Ingredients" column with food name, same as what can be found
#'   in the recipes or food list, a "environmental_impact_indicator" column with the name of the environmental impact
#'   indicator, same as in SHARP indicators database, and "environmental_impact_per_100g" of the food.}
#' }
#' @param database Database to create new entries for. Either "volume_weight", "nutrients" or "sustainability.
#' @param identifier  A the column name for a column with an identifier for with the name of the new ingredient. Default is recipe_name.
#'
#' @return The nutrient content of each individual variable in the "identifier" column, either by total weight of all ingredients,
#' per 100 grams or per portion.
#'
#' @export
createNewDatabaseEntry <- function(df, database, identifier = 'recipe_name') {

  #Function to format new database entries
  supportFunction <- function(df2) {

    #Suppress warning from separate function call (will come if there are no words separated by '_')
    suppressWarnings(
      list(

      "db" = df2 %>%
        group_by(across(all_of(identifier))) %>%
        mutate(tmp = 0.999,
               database_ID = tmp + cur_group_id()) %>%
        select(-tmp) %>%
        ungroup() %>%
        select(-contains(c(identifier, "Ingredients"))),

      "query_words" = df2 %>%
        select(all_of(identifier)) %>%
        unique() %>%
        group_by(across(all_of(identifier))) %>%
        mutate(tmp = 0.999,
               database_ID = tmp + cur_group_id()) %>%
        ungroup() %>%
        separate(col = contains(identifier), into = c("first_word", "second_word"), sep = "_") %>%
        replace_na(list(second_word = '\\')) %>%
        select(first_word, second_word, database_ID)
    ))


  }

  if(database == 'volume_weight') {

    if(isTRUE(all.equal(names(df), c(identifier, 'unit', 'grams_per_unit')))) {

      new_entries <- supportFunction(df2 = df)

    }else if(isTRUE(all.equal(names(df), c('database_ID', 'unit', 'grams_per_unit')))){

      new_entries <- list(

        'db' = df %>%
          select(unit, grams_per_unit, database_ID),

        'query_words' = unit_weights_query %>%
          filter(.data$language == "english") %>%
          filter(database_ID %in% df$database_ID) %>%
          select(-.data$language)

        )

    }else{
      stop("To create database entries for the volume_weight database, df must include the columns identifier or database_ID, unit and grams_per_unit.")
    }


  }else if(database == 'nutrients') {

    if(isTRUE(all.equal(names(df), c(identifier, "nutrient", "nutrient_content_per_hektogram")))) {

      new_entries <- supportFunction(df2 = df)

    } else {
      stop("To create database entries for the nutrients database, df must only include the columns identifier, nutrient and nutrients_per_hektogram")
    }


  }else if(database == 'sustainability') {

    if(isTRUE(all.equal(names(df), c(identifier, 'environmental_impact_indicator', 'environmental_impact_per_hektogram')))) {

      new_entries <- supportFunction(df2 = df)

    } else {
      stop("To create database entries for the sustainability database, df must include only the columns identifier, environmental_impact_indicator and environmental_impact_per_hektogram.")
    }

  }else{
    stop("Database must be either 'volume_weight', 'nutrients' or 'sustainability'.")
  }

  new_entries
}
