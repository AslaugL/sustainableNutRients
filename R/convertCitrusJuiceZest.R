#' Calculate the amounts of citrus juice and zest when recipe says to use t.ex "juice of one lemon".
#' @title convertCitrusJuiceZest
#'
#' @description Convert volume amounts of citrus juice and zest when recipe says to use t.ex "juice of one lemon" from whole fruits to volume. Or from kilograms to while fruits.
#'
#' @param df A dataframe with standardised ingredient and unit names.
#' @param whole_fruit_to_volume_units Convert whole fruits to volume units, f.ex. if a food list includes "juice of half a lemon". Default TRUE.
#' @param weight_to_whole_fruits Convert weight of juice and zest to whole fruits.
#'
#' @return The dataframe with the amounts of citrus juice/zest from whole foods in actual amounts of juice/zest
#'
#' @export
convertCitrusJuiceZest <- function(df, whole_fruit_to_volume_units = TRUE, weight_to_whole_fruits){

if(isTRUE(whole_fruit_to_volume_units)) {

  #Change the amount of lemon/orange/lime juice/zest from whole pieces of fruit to dl when applicable
  temp <- df %>%
    filter(str_detect(Ingredients, 'the juice|the zest|orange juice') & unit == 'pcs') %>%

    #If it says 'juice and zest' treat as a whole fruit
    mutate(Ingredients = str_replace(Ingredients, ', the juice and zest', '')) %>%

    #Turn juice/zest of whole fruits into the equivalent amount of juice in kg and zest in dl
    mutate(
      Amounts = case_when(
        #Juice (info from https://www.webstaurantstore.com/blog/2760/juice-in-citrus-fruits.html)
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(unit, 'pcs') ~ (Amounts*3)*0.015,
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(unit, 'pcs') ~ (Amounts*2)*0.015,
        str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'pcs') ~ (Amounts*4)*0.015,
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
    filter(!(str_detect(Ingredients, 'the juice|the zest|orange juice') & unit == 'pcs')) %>%
    #Add updated ones
    bind_rows(temp)

} else if(isTRUE(weight_to_whole_fruits)) {

  #Change the amount of lemon/orange/lime juice/zest from weight units to whole fruits
  temp <- df %>%
    filter(str_detect(Ingredients, 'the juice|the zest|orange juice') & unit == 'kg') %>%

    #If it says 'juice and zest' treat as a whole fruit
    mutate(Ingredients = str_replace(Ingredients, ', the juice and zest', '')) %>%

    #Turn juice/zest of citrus fruit to the equivalent whole fruits
    mutate(
      Amounts = case_when(
        #Juice (info from https://www.webstaurantstore.com/blog/2760/juice-in-citrus-fruits.html), opposite of the above to turn whole fruit to dl
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ (Amounts/3)/0.015,
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ (Amounts/2)/0.015,
        str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ (Amounts/4)/0.015,
        #Zest (info from https://bakingbites.com/2017/01/how-much-zest-does-citrus-lemon-yield/),
        # first divide kg by how many dl there is in 1 kg (from volume weight database), then find how many fruits are needed for that amount of dls
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ (Amounts/0.04058)/(Amounts/(1/3))/0.15,
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ (Amounts/0.04058)(Amounts/1)/0.15,
        str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ (Amounts/0.04058)(Amounts/1.5)/0.15,

        TRUE ~ Amounts),
      unit = case_when(
        str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ 'pcs',
        str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ 'pcs',

        TRUE ~ unit)
    )

  #Add back
  df %>%
    #Remove old rows
    filter(!(str_detect(Ingredients, 'the juice|the zest|orange juice') & unit == 'kg')) %>%
    #Add updated ones
    bind_rows(temp)

}


}
