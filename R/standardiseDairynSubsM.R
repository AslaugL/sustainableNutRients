#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "M".
#' @title standardiseDairynSubsM
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "M".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "M" with standardised names.
#'
#' @export
standardiseDairynSubsM <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      #Margarine
      str_detect(Ingredients, 'margarin') & str_detect(Ingredients, 'frying') ~ 'margarine for cooking',
      str_detect(Ingredients, 'margarin') & str_detect(Ingredients, 'plant-based|plant based|vegan') ~ 'margarine plant-based',
      str_detect(Ingredients, 'margarin') ~ 'margarine',

      #Milk and substitutes
      #Plant milks
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'milk|drink') &
        str_detect(Ingredients, 'unsweetened') ~ 'almond milk unsweetened',
      str_detect(Ingredients, 'sproud|pea') & str_detect(Ingredients, 'milk|drink') &
        str_detect(Ingredients, 'unsweetened') ~ 'pea protein milk unsweetened',
      str_detect(Ingredients, 'sproud|pea') & str_detect(Ingredients, 'milk|drink') &
        str_detect(Ingredients, 'chocolate') ~ 'pea protein milk chocolate',
      str_detect(Ingredients, 'sproud|pea') & str_detect(Ingredients, 'milk|drink') | str_detect(Ingredients, '\\bsproud\\b') ~ 'pea protein milk',
      str_detect(Ingredients, 'almond cream') ~ 'almond milk cream',
      str_detect(Ingredients, 'almond') & str_detect(Ingredients, 'milk|drink') ~ 'almond milk',
      str_detect(Ingredients, 'rice') & str_detect(Ingredients, 'milk|drink') ~ 'rice milk',
      str_detect(Ingredients, 'oat|mylk') & str_detect(Ingredients, 'barista|rich|cream') & !str_detect(Ingredients, 'goat') ~ 'dairy imitate oatmilk full fat',
      ((str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'milk|drink')) | str_detect(Ingredients, 'mylk')) &
        str_detect(Ingredients, 'chocolate|dumle') ~ 'dairy imitate oatmilk chocolate',
      ((str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'milk|drink')) | str_detect(Ingredients, 'mylk')) &
        str_detect(Ingredients, 'vanilla') ~ 'dairy imitate oatmilk vanilla',
      ((str_detect(Ingredients, 'oat') & str_detect(Ingredients, 'milk|drink')) | str_detect(Ingredients, 'mylk')) &
        !str_detect(Ingredients, 'goat') ~ 'dairy imitate oatmilk',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'milk|drink') &
        str_detect(Ingredients, 'chocolate') ~ 'dairy imitate soymilk chocolate',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'milk|drink') &
        str_detect(Ingredients, 'vanilla') ~ 'dairy imitate soymilk vanilla',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'milk|drink') &
        str_detect(Ingredients, 'unsweetened') ~ 'dairy imitate soymilk unsweetened',
      str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'milk|drink') ~ 'dairy imitate soymilk',
      str_detect(Ingredients, 'milk|drink') & str_detect(Ingredients, 'coconut') | str_detect(Ingredients, 'koko dairy free') ~ 'milk coconut',
      str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'coconut') ~ 'milk coconut cream full fat',

      #Dairy
      str_detect(Ingredients, 'milk|melk') & str_detect(Ingredients, 'whole|full-fat') ~ 'whole milk 3.5 %',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'extra light|skim milk|skimmed|0.1') ~ 'milk 0.1 %',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'evaporated|condensed') ~ 'milk evaporated',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'powder') & str_detect(Ingredients, 'nonfat') ~ 'milk powder nonfat',
      str_detect(Ingredients, 'milk') & str_detect(Ingredients, 'powder') ~ 'milk powder',
      str_detect(Ingredients, 'litago') & !str_detect(Ingredients, 'choco') ~ 'milk 1 % flavored',
      str_detect(Ingredients, 'milk|tinemelk') &
        !str_detect(Ingredients,
                    'coconut|butter|extra|condensed|chocolate|cultured|cheese') &
        (!str_detect(Ingredients, 'without') & str_detect(Ingredients, 'milk')) ~ 'milk 1 %', #Standard

      TRUE ~ Ingredients_standardised)
      )

}
