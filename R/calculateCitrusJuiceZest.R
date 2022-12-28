#' Calculate the amounts of citrus juice and zest when recipe says to use t.ex "juice of one lemon".
#' @title calculateCitrusJuiceZest
#'
#' @description Calculate the amounts of citrus juice and zest when recipe says to use t.ex "juice of one lemon".
#'
#' @param df A dataframe with standardised ingredient and unit names.
#'
#' @return The dataframe with the amounts of citrus juice/zest from whole foods in actual amounts of juice/zest
#'
#' @export
calculateCitrusJuiceZest <- function(df){

#Change the amount of lemon/orange/lime juice/zest from whole pieces of fruit to dl when applicable
temp <- df %>%
  filter(str_detect(Ingredients, 'the juice|the zest')) %>%

  #If it says 'juice and zest' treat as a whole fruit
  mutate(Ingredients = str_replace(Ingredients, ', the juice and zest', '')) %>%

  #Turn juice/zest of whole fruits into the equivalent amount of juice in kg and zest in dl
  mutate(
    Amounts = case_when(
      #Juice (info from https://www.webstaurantstore.com/blog/2760/juice-in-citrus-fruits.html)
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(unit, 'pcs') ~ (Amounts*3)*0.015,
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(unit, 'pcs') ~ (Amounts*2)*0.015,
      #Zest (info from https://bakingbites.com/2017/01/how-much-zest-does-citrus-lemon-yield/)
      str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'zest') & str_detect(unit, 'pcs') ~ (Amounts*(1/3))*0.15,
      str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'zest') & str_detect(unit, 'pcs') ~ (Amounts*1)*0.15,
      str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'pcs') ~ (Amounts*1.5)*0.15,

      TRUE ~ Amounts),
    unit = case_when(
      str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'pcs') ~ 'kg',
      str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'pcs') ~ 'dl',

      TRUE ~ unit)
  )

#Add back
df %>%
  #Remove old rows
  filter(!str_detect(Ingredients, 'the juice|the zest')) %>%
  #Add updated ones
  bind_rows(temp)

}
