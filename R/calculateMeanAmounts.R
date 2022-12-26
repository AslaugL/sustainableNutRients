#' Calculate the mean amount of ingredients when recipe says 3-4 and similar.
#' @title calculateCitrusJuiceZest
#'
#' @description Calculate the mean amount of ingredients when recipe says 3-4 and similar.
#'
#' @param df A dataframe with an Ingredients column.
#'
#' @return The dataframe with the mean of these type of amounts.
#'
#' @export
calculateMeanAmounts <- function(df) {
  
  #Find the pattern
  numerics <- df %>%
    mutate(tmp = str_extract(Ingredients, "\\d+-\\d+")) %>%
    #Separate it
    select(tmp) %>%
    unique() %>%
    #calculate mean of the two numbers
    separate(., tmp, c("no1", "no2"), sep = "-", remove = FALSE) %>%
    mutate(across(c(no1, no2), ~as.numeric(.))) %>%
    mutate(mean = as.character((no1 + no2)/2)) %>% #Character as Ingredients string is character
    drop_na()
  
  #Replace in original
  df %>%
    mutate(Ingredients = str_replace_all(Ingredients,
                                         #named vector for replacements
                                         setNames(numerics$mean, numerics$tmp)))
  
}