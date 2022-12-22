#' Look over the standardised names and amounts in a food list.
#' @title checkFoodStandardisation
#'
#' @description Look over the standardised names and amounts in a food list.
#'
#' @param df  Output from standardiseFoodList function.
#'
#' @return A tibble with the unique values from the Original_ingredients and Ingredients column.
#'
#' @export
checkFoodStandardisation <- function(df) {

  df %>% select(Original_ingredients, Ingredients) %>% unique()

}
