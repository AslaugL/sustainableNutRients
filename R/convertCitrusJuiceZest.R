#' Calculate the amounts of citrus juice and zest when recipe says to use t.ex "juice of one lemon".
#' @title convertCitrusJuiceZest
#'
#' @description Convert volume amounts of citrus juice and zest when recipe says to use t.ex "juice of one lemon" from whole fruits to volume. Or from kilograms to kilogram of whole fruits.
#'
#' @param df A dataframe with standardised ingredient and unit names.
#' @param whole_fruit_to_volume_units Convert whole fruits to volume units, f.ex. if a food list includes "juice of half a lemon". Default TRUE.
#' @param weight_to_whole_fruits Convert weight of juice and zest to whole fruits.
#'
#' @return The dataframe with the amounts of citrus juice/zest from whole foods in actual amounts of juice/zest
#'
#' @export
convertCitrusJuiceZest <- function(df, whole_fruit_to_volume_units = TRUE, weight_to_whole_fruits = FALSE){

if(isTRUE(whole_fruit_to_volume_units) & isFALSE(weight_to_whole_fruits)) {

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

} else if(isTRUE(weight_to_whole_fruits) & isFALSE(whole_fruit_to_volume_units)) {

  #Change the amount of lemon/orange/lime juice/zest from weight units to whole fruits in kg

  #List of weight of zest in dl to make code below easier to read
  #Weight from unit_weights database
  weight_of_zest_per_dl <- list(

    lemon = sustainableNutRients::unit_weights %>%
      filter(Ingredients == "lemon zest" & unit == "dl") %>% pull(grams_per_unit),
    lime = sustainableNutRients::unit_weights %>%
      filter(Ingredients == "lime zest" & unit == "dl") %>% pull(grams_per_unit),
    orange = sustainableNutRients::unit_weights %>%
      filter(Ingredients == "orange zest" & unit == "dl") %>% pull(grams_per_unit)

  )
  #Weight of whole fruits
  weight_of_whole_fruits <- list(

    lemon = sustainableNutRients::unit_weights %>%
      filter(Ingredients == "lemon" & unit == "pcs") %>% pull(grams_per_unit) %>% unique(),
    lime = sustainableNutRients::unit_weights %>%
      filter(Ingredients == "lime" & unit == "pcs") %>% pull(grams_per_unit) %>% unique(),
    orange = sustainableNutRients::unit_weights %>%
      filter(Ingredients == "orange" & unit == "pcs") %>% pull(grams_per_unit) %>% unique()

  )

  #Weight of juice assumed similar to water

  #Convert
  temp <- df %>%
    filter(str_detect(Ingredients, 'the juice|the zest') & unit %in% c('kg','dl')) %>%

    #If it says 'juice and zest' treat as a whole fruit
    mutate(Ingredients = str_replace(Ingredients, ', the juice and zest', '')) %>%

    #Turn juice/zest of citrus fruit to the equivalent whole fruits
    mutate(
      Amounts = case_when(
        #Juice (info from https://www.webstaurantstore.com/blog/2760/juice-in-citrus-fruits.html), opposite of the above to turn whole fruit to dl
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ (Amounts/3)/0.015*(weight_of_whole_fruits$lemon/1000),
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ (Amounts/2)/0.015*(weight_of_whole_fruits$lime/1000),
        #str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ (Amounts/4)/0.015,

        #Zest (info from https://bakingbites.com/2017/01/how-much-zest-does-citrus-lemon-yield/),
        # first divide kg by how many dl there is in 1 kg (from volume weight database), then find how many fruits are needed for that amount of dls
        # Turn the pcs of whole fruits to the equivalent in kg
        # Formula used in words: Amounts in kg / dl per kg zest / number of whole fruit needed for 1 dl * weight of 1 whole fruit in kg
        #When weight is already in kg
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ (Amounts/weight_of_zest_per_dl$lemon)/(Amounts/(1/3))/0.15*(weight_of_whole_fruits$lemon/1000),
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ (Amounts/weight_of_zest_per_dl$lime)/(Amounts/1)/0.15*(weight_of_whole_fruits$lime/1000),
        str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg') ~ (Amounts/weight_of_zest_per_dl$orange)/(Amounts/1.5)/0.15*(weight_of_whole_fruits$orange/1000),
        #If weight in dl
        str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'zest') & str_detect(unit, 'dl') ~ (Amounts/(1/3))/0.15*(weight_of_whole_fruits$lemon/1000),
        str_detect(Ingredients, 'lime') & str_detect(Ingredients, 'zest') & str_detect(unit, 'dl') ~ (Amounts/1)/0.15*(weight_of_whole_fruits$lime/1000),
        str_detect(Ingredients, 'orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'dl') ~ (Amounts/1.5)/0.15*(weight_of_whole_fruits$orange/1000),

        TRUE ~ Amounts),
      unit = case_when(
        str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'juice') & str_detect(unit, 'kg') ~ 'kg',
        str_detect(Ingredients, 'lemon|lime|orange') & str_detect(Ingredients, 'zest') & str_detect(unit, 'kg|dl') ~ 'kg',

        TRUE ~ unit),
      #Turn Ingredient names into the whole fruit
      Ingredients = case_when(
        str_detect(Ingredients, 'lemon') ~ 'lemon',
        str_detect(Ingredients, 'lime') ~ 'lime',
        str_detect(Ingredients, 'orange') ~ 'orange',

        TRUE ~ Ingredients)
    )

  #Add back
  df %>%
    #Remove old rows
    filter(!(str_detect(Ingredients, 'the juice|the zest') & unit %in% c('kg', 'dl'))) %>%
    #Add updated ones
    bind_rows(temp)

  #Some error messages
} else if(isTRUE(whole_fruit_to_volume_units) & isTRUE(weight_to_whole_fruits)) {

  stop("Only one of whole_fruit_to_volume_units and weight_to_whole_fruits can be TRUE at the same time.")

} else if(isFALSE(whole_fruit_to_volume_units) & isTRUE(weight_to_whole_fruits)) {

  stop("Only one of whole_fruit_to_volume_units and weight_to_whole_fruits can be FALSE at the same time.")

} else if(!is.logical(whole_fruit_to_volume_units) | !is.logical(weight_to_whole_fruits)) {

  stop("whole_fruit_to_volume_units and weight_to_whole_fruits must be either TRUE or FALSE.")

}


}
