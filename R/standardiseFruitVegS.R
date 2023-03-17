#' Standardising ingredients names in a recipe, here fruit and vegetables starting with the letter "S".
#' @title standardiseFruitVegS
#'
#' @description Support function for "standardiseFruitVeg", standardise names of fruit and veg starting with "S".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a new with fruit and vegetables starting with the letter "S" with standardised names.
#'
#' @export
standardiseFruitVegS <- function(df){
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'heart') ~ 'salad heart',
      str_detect(Ingredients, 'ruccula|rocket salad|rocket|arugula|ruccola|peppery salad') ~ 'salad rocket',
      str_detect(Ingredients, 'iceberg') ~ 'salad iceberg lettuce',
      str_detect(Ingredients, 'lettuce') & !str_detect(Ingredients, 'lamb')  ~ 'salad lettuce',
      str_detect(Ingredients, 'salad') &str_detect(Ingredients, 'crispi|crispy') ~ 'salad crispi',
      str_detect(Ingredients, 'lollo rosso') ~ 'salad lollo rosso',
      str_detect(Ingredients, 'salad') & !str_detect(Ingredients, 'shrimp|oil|lentil|bean|beetroot|chicken|ready-made|ready made|potato') ~ 'salad',
      str_detect(Ingredients, 'sauerkraut') ~ 'sauerkraut',
      str_detect(Ingredients, 'scallion|green onion|spring onion') ~ 'scallion',
      str_detect(Ingredients, 'seaweed') & !str_detect(Ingredients, 'nori') ~ 'seaweed',
      str_detect(Ingredients, 'shallot') ~ 'shallot',
      str_detect(Ingredients, 'smoothie mix') & str_detect(Ingredients, 'tropical') ~ 'smoothie mix tropical',
      str_detect(Ingredients, 'smoothie mix') ~ 'smoothie mix',
      str_detect(Ingredients, 'sorrel') ~ 'sorrel',
      str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'baby') ~ 'spinach baby',
      str_detect(Ingredients, 'spinach') ~ 'spinach',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'alfalfa') ~ 'sprouts alfalfa',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'bean') ~ 'sprouts bean',
      str_detect(Ingredients, 'sprout') ~ 'sprouts alfalfa', #Default

      str_detect(Ingredients, 'stew') & str_detect(Ingredients, 'mix') ~ 'stew mix',
      str_detect(Ingredients, 'stirfry|stir-fry|stir fry') & str_detect(Ingredients, 'mix') ~ 'stir fry assorted vegetables',
      str_detect(Ingredients, 'strawberr') & str_detect(Ingredients, 'jam') ~ 'jam strawberry',
      str_detect(Ingredients, 'strawberr') ~ 'strawberries',

      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'pea') ~ 'sugar snap pea',
      str_detect(Ingredients, 'swede') | (str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'root')) | str_detect(Ingredients, 'rutabaga') ~ 'swede',
      str_detect(Ingredients, 'pickle') & str_detect(Ingredients, 'red pepper|paprika|sweet pepper') ~ 'sweet pepper pickled',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') & str_detect(Ingredients, 'green') ~ 'sweet pepper green grilled',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') & str_detect(Ingredients, 'can') ~ 'sweet pepper grilled canned',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') ~ 'sweet pepper grilled',
      str_detect(Ingredients, 'sweet pepper') & str_detect(Ingredients, 'can|drain') ~ 'sweet pepper canned',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'green') |
        str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'green') & str_detect(Ingredients, 'slice|deseed|strips') ~ 'sweet pepper green',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'yellow') |
        str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'yellow') & str_detect(Ingredients, 'slice|deseed') ~ 'sweet pepper yellow',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & !str_detect(Ingredients, 'grilled|pickled|powder|spice|smoked|potato') |
        str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'red') & str_detect(Ingredients, 'slice|deseed|chopped') & !str_detect(Ingredients, 'hot|chili|chilli|potato') ~ 'sweet pepper red',
      str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'potato') ~ 'sweet potato',
      str_detect(Ingredients, 'squash|zucchini|courgette') & !str_detect(Ingredients, 'butter|acorn') ~ 'summer squash zucchini', #Standard

      TRUE ~ Ingredients_standardised
    ))
}

