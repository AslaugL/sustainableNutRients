#' Extract amounts and units from the Ingredients column.
#' @title extractAmounts
#'
#' @description Extract Amounts and units from the Ingredients column to their own columns,
#'  by recognizing the pattern "number" "unit" at the beginning of the string.
#'
#' @param df A dataframe with an Ingredients column.
#'
#' @return The dataframe with amounts and units extracted into their own columns.
#'
#' @export
extractAmounts <- function(df){

  #Extract units and numbers in front of them to their own column
  df %>% mutate(tmp = case_when(

    #Extract amounts with units
    str_detect(Ingredients, paste0('^(\\d+\\.\\d+|\\d+-\\d+|\\d+)', type_of_units, collapse = '|')) ~
      str_extract(Ingredients, paste0('^(\\d+\\.\\d+|\\d+-\\d+|\\d+)', type_of_units, collapse = '|')),
    #Extract pure numbers and add 'pcs' as default unit value
    !str_detect(Ingredients, paste0('^(\\d+\\.\\d+|\\d+-\\d+|\\d+)', type_of_units, collapse = '|')) &
      str_detect(Ingredients, '^\\d+') ~ paste0(str_extract(Ingredients, '^[^\\s]+'), ' pcs')),

    #Remove this information from Ingredients columns
    Ingredients = case_when(
      str_detect(Ingredients, paste0('^(\\d+\\.\\d+|\\d+-\\d+|\\d+)', type_of_units, collapse = '|')) ~
        str_remove(Ingredients, paste0('^(\\d+\\.\\d+|\\d+-\\d+|\\d+)', type_of_units, collapse = '|')),
      !str_detect(Ingredients, paste0('^(\\d+\\.\\d+|\\d+-\\d+|\\d+)', type_of_units, collapse = '|')) & str_detect(Ingredients, '^\\d+') ~
        str_remove_all(Ingredients, '^[^\\s]+'),
      TRUE ~ Ingredients)) %>%

    #Split into a column for amounts and one for units
    mutate(tmp = str_trim(.data$tmp)) %>% #Remove whitespace
    separate(.data$tmp, into = c("Amounts", "unit"), sep = " ") %>%
    mutate(Amounts = as.numeric(Amounts)) %>%
    #Trim
    mutate(Ingredients = str_trim(Ingredients))

}
