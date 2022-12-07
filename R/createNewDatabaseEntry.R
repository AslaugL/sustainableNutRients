#' Create new database entries to map foods to when using findFoodInDatabase.
#' @title createNewDatabaseEntry
#'
#' @description Create new database entries to map foods to when using findFoodInDatabase.
#'
#' @param database Database to create new entries for. Either "volume_weight", "nutrients" or "sustainability.
#' @param df
#' \itemize{
#'  \item{For volume_weight, a dataframe three columns: either "Ingredients" or "database_ID" depending on if Ingredient is new or already in database, "unit" and "grams_per_unit"}
#'  \item{For "nutrients", the output from calculateNutrientContentOfFoodlist with option "per 100 g" selected or a three column dataframe with
#'  an "Ingredients" column with food name, same as what can be found in the recipes or food list, a "nutrient" column with nutrient
#'  names similar to what is found in Matvaretabellen and "nutrient_content_per_100g" of the food.}
#'  \item{For "sustainability" the output from calculateEnvironmentalImpactOfFoodlist with option "per 100 g"
#'  selected, or a three column dataframe with an "Ingredients" column with food name, same as what can be found
#'   in the recipes or food list, a "environmental_impact_indicator" column with the name of the environmental impact
#'   indicator, same as in SHARP indicators database, and "environmental_impact_per_100g" of the food.}
#' }
#'
#' @return The nutrient content of each individual variable in the "identifier" column, either by total weight of all ingredients,
#' per 100 grams or per portion.
#'
#' @export
createNewDatabaseEntry <- function(df, database) {

  #Function to format new database entries
  supportFunction <- function(df2) {

    #Suppress warning from separate function call (will come if there are no words separated by '_')
    suppressWarnings(
      list(

      "db" = df2 %>%
        rownames_to_column() %>%
        mutate(database_ID = paste0("9999.", .data$rowname),
               database_ID = as.numeric(database_ID)) %>%
        select(-c(.data$rowname, Ingredients)),

      "query_words" = df2 %>%
        select(Ingredients) %>%
        unique() %>%
        separate(col = Ingredients, into = c("first_word", "second_word"), sep = "_") %>%
        rownames_to_column() %>%
        mutate(database_ID = paste0("9999.", .data$rowname),
               database_ID = as.numeric(database_ID)) %>%
        replace_na(list(second_word = '\\\\')) %>%
        select(-.data$rowname)
    ))


  }

  if(database == 'volume_weight') {

    if(isTRUE(all.equal(names(df), c('Ingredients', 'unit', 'grams_per_unit')))) {

      new_entries <- supportFunction(df2 = df)

    }else if(isTRUE(all.equal(names(df), c('database_ID', 'unit', 'grams_per_unit')))){

      new_entries <- list(

        'db' = df %>%
          select(unit, grams_per_unit, database_ID),

        'query_words' = nutRients:::unit_weights_query %>%
          filter(language == "english") %>%
          filter(database_ID %in% df$database_ID) %>%
          select(-language)

        )

    }else{
      stop("To create database entries for the volume_weight database, df must include the columns Ingredients or database_ID, unit and grams_per_unit.")
    }


  }else if(database == 'nutrients') {

    if(isTRUE(all.equal(names(df), c("Ingredients", "nutrient", "nutrient_content_per_hektogram")))) {

      new_entries <- supportFunction(df2 = df)

    } else {
      stop("To create database entries for the nutrients database, df must only include the columns Ingredients, nutrient and nutrients_per_hektogram")
    }


  }else if(database == 'sustainability') {

    if(isTRUE(all.equal(names(df), c('Ingredients', 'environmental_impact_indicator', 'environmental_impact_per_hektogram')))) {

      new_entries <- supportFunction(df2 = df)

    } else {
      stop("To create database entries for the sustainability database, df must include only the columns Ingredients, environmental_impact_indicator and environmental_impact_per_hektogram.")
    }

  }else{
    stop("Database must be either 'volume_weight', 'nutrients' or 'sustainability'.")
  }

  new_entries
}
