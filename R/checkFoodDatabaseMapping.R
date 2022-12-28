#' Check what reference foods from the database the foods have been mapped to.
#' @title checkFoodDatabaseMapping
#'
#' @param df An output dataframe from findFoodInDatabase.
#'
#' @return A three column dataframe with the foods and their database reference with name and individual ID.
#'
#' @export
checkFoodDatabaseMapping <- function(df) {

  df %>%
    select(Ingredients, database_reference, database_ID) %>%
    filter(database_reference != "Not looked up in database as weight already given") %>% #These ingredients have not been mapped to the database
    unique()

}
