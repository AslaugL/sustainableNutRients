#' Split broth into water and broth cubes, as these ingredients are found in the databases not broth.
#' @title splitBrothtoWaterBrotchcubes
#'
#' @description Split broth into water and broth cubes, as these ingredients are found in the databases not broth.
#'
#' @param df A dataframe with a standardised Ingredients column.
#'
#' @return A dataframe with the broth ingredients split into the equivalent amounts of water and broth cubes needed to make the broth.
#'
#' @export

splitBrothtoWaterBrotchcubes <- function(df){

  df %>%
    filter(str_detect(Ingredients, 'water broth')) %>%
    #Add / to split the rows into two separate ingredients, and add "cube" to broth
    mutate(Ingredients = str_replace(Ingredients, 'water broth', 'water/broth cube')) %>%
    separate_rows(Ingredients, sep = '/') %>%
    #Turn the broth cube amounts into number of broth cubes, 1 cube per 5 dl/0.5 kg water
    mutate(
      Amounts = case_when(
        str_detect(Ingredients, 'broth cube') ~ Amounts/0.5,
        TRUE ~ Amounts),
      unit = case_when(
        str_detect(Ingredients, 'broth cube') ~ 'pcs',
        TRUE ~ unit)
   )

}
