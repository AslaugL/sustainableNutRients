#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "P".
#' @title standardiseFlournGraisnNutsnLegumesP
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "P".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "P" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesP <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'pancake') & str_detect(Ingredients, 'mix') ~ 'pancake dry mix',
      str_detect(Ingredients, 'pancake') & !str_detect(Ingredients, 'syrup') ~ 'pancake',

      #Pasta
      str_detect(Ingredients, 'pasta|paste|spagetti|spaghetti') & str_detect(Ingredients, 'whole') ~ 'pasta whole grain',
      str_detect(Ingredients, 'lasagna|lasagne') & str_detect(Ingredients, 'plate|sheet') ~ 'lasagna plate pasta',
      str_detect(Ingredients, 'tortellini') ~ 'pasta filled',
      str_detect(Ingredients, 'pasta|spagetti|spaghetti|tagli|pens|macaroni') & !str_detect(Ingredients, 'lasagna') & str_detect(Ingredients, '\\bcooked') ~ 'pasta cooked',
      str_detect(Ingredients, 'pasta|spagetti|spaghetti|tagli|pens|macaroni|tortellini') & !str_detect(Ingredients, 'lasagna') & !str_detect(Ingredients, 'sauce') ~ 'pasta',
      str_detect(Ingredients, 'lasagna noodles') ~ 'pasta',

      #Pearl barley
      str_detect(Ingredients, 'barley') & !str_detect(Ingredients, 'salad') ~ 'pearl barley',

      #Nuts and seeds
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'butter') ~ 'peanut butter',
      str_detect(Ingredients, 'peanut') & str_detect(Ingredients, 'salt') & !str_detect(Ingredients, 'unsalted') ~ 'peanut salt',
      str_detect(Ingredients, 'peanut') & !str_detect(Ingredients, 'oil') ~ 'peanut',
      str_detect(Ingredients, 'pecans') ~ 'pecan',
      str_detect(Ingredients, 'pine') & str_detect(Ingredients, 'nut|seed|kernel') & !str_detect(Ingredients, 'apple') ~ 'pine nut',
      str_detect(Ingredients, 'pistachio') ~ 'pistachio nut',
      str_detect(Ingredients, 'pumpkin seed') ~ 'pumpkin seed',

      #Others
      str_detect(Ingredients, 'phyllo dough') ~ 'phyllo dough',
      str_detect(Ingredients, 'pie dough') ~ 'pie dough',
      str_detect(Ingredients, 'pizza base') & str_detect (Ingredients, 'mini') ~ 'pizza base mini',
      str_detect(Ingredients, 'pizza base') & str_detect (Ingredients, 'mix') ~ 'pizza base mix',
      str_detect(Ingredients, 'pizza base') ~ 'pizza base',
      str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'crust') & str_detect(Ingredients, 'italian') ~ 'pizza crust italian',
      str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'crust') ~ 'pizza crust',
      str_detect(Ingredients, 'pizza dough') ~ 'pizza dough',
      str_detect(Ingredients, 'potato') & str_detect(Ingredients, 'flour') ~ 'potato starch',
      str_detect(Ingredients, 'lompe') |
        str_detect(Ingredients, 'potet') & str_detect(Ingredients, 'lefse') ~ 'potato flatbread lompe',
      str_detect(Ingredients, 'puff pastry|butter dough') ~ 'puff pastry',
      str_detect(Ingredients, 'lefse') ~ 'lefse',

      TRUE ~ Ingredients_standardised))
}

