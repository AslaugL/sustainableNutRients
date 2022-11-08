#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "C".
#' @title standardiseFruitVegC
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "C".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "C" with standardised names.
#'
#' @export
standardiseFruitVegC <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'napa') ~ 'cabbage napa',
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'red') & !str_detect(Ingredients, 'shred') ~ 'cabbage red',
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'china|chinese') ~ 'cabbage china',
      str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'savoy') ~ 'cabbage savoy',
      str_detect(Ingredients, 'cabbage') & !str_detect(Ingredients, 'meat|root') ~ 'cabbage',
      str_detect(Ingredients, 'bok choi|bok choy') ~ 'cabbage bok choi',
      str_detect(Ingredients, 'carrot|raw yellow') & !str_detect(Ingredients, 'paste|wok|mire') ~ 'carrot',
      str_detect(Ingredients, 'cauliflower') & !str_detect(Ingredients, 'butter') ~ 'cauliflower',
      str_detect(Ingredients, 'celery|cellery') & !str_detect(Ingredients, 'salt|soup|seed') ~ 'celery', #Use celery for stangselleri
      str_detect(Ingredients, 'celeriac') & !str_detect(Ingredients, 'mire') ~ 'celariac root',
      str_detect(Ingredients, 'chard') & !str_detect(Ingredients, 'wine') ~ 'mangold',
      str_detect(Ingredients, 'cherry tomato') & str_detect(Ingredients, 'can') ~ 'cherry tomato canned',
      str_detect(Ingredients, 'cherry tomato') ~ 'cherry tomato',
      str_detect(Ingredients, 'cherry|cherries') & str_detect(Ingredients, 'can|in syrup') & !str_detect(Ingredients, 'tomato') ~ 'cherries canned', #Name used in SHARP and Matvaretabellen
      str_detect(Ingredients, 'cherry|cherries') & !str_detect(Ingredients, 'tomato') ~ 'cherries', #Name used in SHARP and Matvaretabellen
      str_detect(Ingredients, 'chicory') & str_detect(Ingredients, 'white') ~ 'chicory white',
      str_detect(Ingredients, 'chicory') & str_detect(Ingredients, 'red') ~ 'chicory red',
      str_detect(Ingredients, 'chicory') ~ 'chicory',
      str_detect(Ingredients, 'jalap') ~ 'chili pepper jalapeno',
      str_detect(Ingredients, 'chili|chile') & str_detect(Ingredients, 'green') ~ 'chili pepper green',
      ((str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'chili')) | (str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'red|rød'))) & !str_detect(Ingredients, 'powder') |
        str_detect(Ingredients, 'mild chili') & !str_detect(Ingredients, 'sauce') | str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'chop') |
        str_detect(Ingredients, 'chili') & str_detect(Amounts, 'stk') |
        str_detect(Ingredients, 'red') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'hot') & str_detect(Ingredients, 'slice') |
        str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'seed') ~ 'chili pepper red',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'dried') & !str_detect(Ingredients, 'flake') ~ 'chili pepper dried',
      str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'pickle') ~ 'chili pepper pickled',
      str_detect(Ingredients, 'clemen') ~ 'clementine',
      str_detect(Ingredients, 'coconut') & !str_detect(Ingredients, 'milk|cream|oil') ~ 'coconut',
      str_detect(Ingredients, 'minima|baby corn') ~ 'corn baby',
      str_detect(Ingredients, 'corn') & str_detect(Amounts, 'g') &
        str_detect(Ingredients, 'can') |
        str_detect(Ingredients, 'kernel') & str_detect(Ingredients, 'drained') |
        str_detect(Ingredients, 'sweetcorn') & str_detect(Ingredients, 'canned') ~ 'sweet corn kernels canned',
      str_detect(Ingredients, 'corn') & str_detect(Amounts, 'g') &
        !str_detect(Ingredients, 'starch|tortilla|oil') | str_detect(Ingredients, 'corn kernel') ~ 'sweet corn kernels',
      str_detect(Ingredients, 'corn') & str_detect(Amounts, 'stk') &
        !str_detect(Ingredients, 'pepper') ~ 'corn cob',
      str_detect(Ingredients, 'cranberr') & str_detect(Ingredients, 'jam') ~ 'cranberries jam',
      str_detect(Ingredients, 'cranberr') & !str_detect(Ingredients, 'sauce') ~ 'cranberries',
      str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'snake') ~ 'cucumber snake',
      str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'jam|pickle') ~ 'cucumber pickled',
      str_detect(Ingredients, 'cucumber') & str_detect(Ingredients, 'sandwichspread') ~ 'sandwichspread cucumber',
      str_detect(Ingredients, 'cucumber') ~ 'cucumber',

      TRUE ~ Ingredients_standardised

    ))
}

