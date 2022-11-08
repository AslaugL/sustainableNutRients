#' Standardising ingredients names in a recipe, here various sauces of foods.
#' @title standardiseSauces
#'
#' @description Standardise names of various sauces
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with various other foods with standardised names.
#'
#' @export
standardiseSauces <- function(df) {

  df %>%
  #Standardise
  mutate(Ingredients_standardised = case_when(

    #Bases/powders
    str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'base|bag') ~ 'bolognese base',
    str_detect(Ingredients, 'bearnaise|b\u00E9arnaise') & str_detect(Ingredients, 'base') ~ 'bearnaise base',
    str_detect(Ingredients, 'bearnaise|b\u00E9arnaise') & str_detect(Ingredients, 'mix') ~ 'bearnaise powder mix',
    str_detect(Ingredients, 'brown sauce') & str_detect(Ingredients, 'mix') ~ 'brown sauce powder mix',
    str_detect(Ingredients, 'browning') & str_detect(Ingredients, 'sauce') ~ 'sauce browning',

    str_detect(Ingredients, 'cream sauce') & str_detect(Ingredients, 'base') ~ 'cream sauce base',

    str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'base') ~ 'hollandaise base',
    str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'mix') ~ 'hollandaise powder mix',

    #Sauces/gravy
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'barbeque|barbecue|bbq') ~ 'sauce barbeque',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'bearnaise|b\u00E9arnaise') |
      str_detect(Ingredients, 'bernaise|b\u00E9arnaise') ~ 'sauce bearnaise',
    str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'beef') ~ 'beef gravy',
    str_detect(Ingredients, 'gravy') ~ 'beef gravy', #Use as default

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'cheese') ~ 'sauce cheese',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'brittany') ~ 'sauce chicken brittany',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chicken') ~ 'sauce chicken',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sweet') ~ 'sauce sweet chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'hot') |
      str_detect(Ingredients, 'sriracha sauce') ~ 'sauce hot chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sour') ~ 'sauce sour chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'salt') ~ 'sauce salt chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'mild') ~ 'sauce mild chili',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'garlic') ~ 'sauce chili garlic',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'chili') ~ 'sauce chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'cranberr') ~ 'sauce cranberry',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'cream') ~ 'sauce cream',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'fish|fisk') ~ 'sauce fish',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'hoisin') ~ 'sauce hoisin',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'horseradish') ~ 'sauce horseradish',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot pepper') ~ 'sauce hot pepper',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot') ~ 'sauce hot',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, '\\bhp') ~ 'sauce hp',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'mint') ~ 'sauce mint',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'pad thai') ~ 'sauce pad thai',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pasta|spagetti|spaghetti') ~ 'sauce pasta',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'piri-piri') ~ 'sauce piri-piri',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'white') ~ 'sauce pizza white',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'red') |
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') ~ 'sauce pizza red', #Standard
    str_detect(Ingredients, 'ponzu') & str_detect(Ingredients, 'sauce') ~ 'sauce ponzu',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'sweet') |
      str_detect(Ingredients, 'ketjap medja') ~ 'sauce sweet soy',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'soy') ~ 'sauce soy',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'taco') ~ 'sauce taco',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'teriyaki') ~ 'sauce teriyaki',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'tikka masala') ~ 'sauce tikka masala',
    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'tomat') ~ 'sauce tomato',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'oyster') ~ 'sauce oyster',

    str_detect(Ingredients, 'sauce|saus') & str_detect(Ingredients, 'white') ~ 'sauce white',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'worcestershire') ~ 'sauce worcestershire',


    TRUE ~ Ingredients_standardised))

}
