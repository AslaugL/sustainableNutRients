#' Fix the ID number of an ingredient if findFoodInDatabase made a mistake.
#' @title fixFoodMappingError
#'
#' @description Set a new ID number for an ingredient if findFoodInDatabase made a mistake.
#'
#' @param database Reference database to look up.
#' @param first_w The correct first word in reference database.
#' @param second_w The correct second word in reference database.
#'
#' @return The updated ID number.
#'
#' @export

fixFoodMappingError <- function(database, first_w, second_w = '\\') {

  ID <- database %>% filter(first_word == first_w & second_word == second_w) %>% select(database_ID) %>% as.numeric()

}
