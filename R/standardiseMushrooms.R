#' Standardising ingredients names in a recipe, here mushrooms.
#' @title standardiseMushrooms
#'
#' @description Standardise names of mushrooms.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with mushrooms with standardised names.
#'
#' @export
standardiseMushrooms <- function(df) {
  df  %>%
    
    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'aroma') & str_detect(Ingredients, 'champignon|mushroom|soup') ~ 'mushroom aroma champignon',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'porcini') & str_detect(Ingredients, 'dried') ~ 'mushroom porcini dried',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'dry|dried') ~ 'mushroom dried',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'shiitake|shitake') ~ 'mushroom shiitake',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'shimeji') ~ 'mushroom shimeji',
      str_detect(Ingredients, 'portebello') ~ 'mushroom portebello',
      str_detect(Ingredients, 'chanterelle') ~ 'mushroom chanterelle',
      str_detect(Ingredients, 'champig') ~ 'mushroom champignon',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'chestnut') ~ 'mushroom chestnut',
      str_detect(Ingredients, 'mushroom') & str_detect(Ingredients, 'drained') ~ 'mushroom canned',
      str_detect(Ingredients, 'mushroom') & !str_detect(Ingredients, 'rice|condensed') ~ 'mushroom',
      
      TRUE ~ Ingredients_standardised))
}
