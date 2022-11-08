#' Extract amounts and units from the Ingredents column.
#' @title extractAmounts
#'
#' @description Extract Amounts and units from the Ingredients column to their own columns.
#'
#' @param df A dataframe with an Ingredients column.
#'
#' @return The dataframe with amounts and units extracted into their own columns.
#'
#' @export
extractAmounts <- function(df){

  #Units to look for in the Ingredients column
  units = c('tsp', 'tbsp', 'l', 'dl', 'g', 'kg', 'hp', 'krm', 'tins', 'cm', 'leaf', 'can',
              'stk', 'glass', 'cup', 'box', 'pinch', 'flaske', 'portion', 'slice', '\\bclove\\b',
              'neve', 'ml', 'cl', 'bunch', 'pack', 'plate', 'drop', 'twig', 'pound', 'ounce', 'stalk',
              'quart') %>%
      #Add whitespace on both sides to only match a unit in a string
      sapply(., function(x) {paste0('\\s', x, '\\s')})

  #Extract units and numbers in front of them to their own column
  df %>% mutate(Amounts = case_when(

    #Extract amounts with units
    str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) ~
      str_extract(Ingredients, '((\\d+\\.\\d+|\\d+-\\d+|\\d+)\\s\\w+)'),
    #Extract pure numbers
    !str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) &
      str_detect(Ingredients, '^\\d+') ~ str_extract(Ingredients, '^[^\\s]+')),

    #Remove this information from Ingredients columns
    Ingredients = case_when(
      str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) ~
        str_remove(Ingredients, '((\\d+\\.\\d+|\\d+-\\d+|\\d+)\\s\\w+)'),
      !str_detect(Ingredients, paste0('(\\d+\\.\\d+|\\d+-\\d+|\\d+)', units, collapse = '|')) & str_detect(Ingredients, '^\\d+') ~
        str_remove_all(Ingredients, '^[^\\s]+'),
      TRUE ~ Ingredients))

}
