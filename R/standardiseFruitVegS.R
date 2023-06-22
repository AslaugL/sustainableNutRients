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

      #Salads
      str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'heart') ~ 'salad heart',
      str_detect(Ingredients, 'ruccula|rocket salad|rocket|arugula|ruccola|peppery salad') ~ 'salad rocket',
      str_detect(Ingredients, 'iceberg') ~ 'salad iceberg lettuce',
      str_detect(Ingredients, 'lettuce') & !str_detect(Ingredients, 'lamb')  ~ 'salad lettuce',
      str_detect(Ingredients, 'salad') &str_detect(Ingredients, 'crispi|crispy') ~ 'salad crispi',
      str_detect(Ingredients, 'lollo rosso|lollo salad|oak leaf|big oak') & str_detect(Ingredients, 'green') ~ 'salad lollo rosso green',
      str_detect(Ingredients, 'lollo rosso|lollo salad|oak leaf|big oak') ~ 'salad lollo rosso',
      str_detect(Ingredients, 'radicchio rosso') ~ 'salad radicchio rosso',
      str_detect(Ingredients, 'romano') & str_detect(Ingredients, 'green') ~ 'salad romano green',
      str_detect(Ingredients, 'romano') & str_detect(Ingredients, 'min') ~ 'salad romano mix',
      str_detect(Ingredients, 'romano') & !str_detect(Ingredients, 'pecorino') ~ 'salad romano',
      str_detect(Ingredients, 'batavia') & str_detect(Ingredients, 'green') ~ 'salad batavia green',
      str_detect(Ingredients, 'batavia') ~ 'salad batavia red', #Most common type
      str_detect(Ingredients, 'baby leaf') & str_detect(Ingredients, 'mix') ~ 'salad baby leaf mix',
      str_detect(Ingredients, 'feld') & str_detect(Ingredients, 'salad') ~ 'salad feld',
      str_detect(Ingredients, 'butterhead') & str_detect(Ingredients, 'salad') ~ 'salad butterhead',
      str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'mix') ~ 'salad mix',
      str_detect(Ingredients, 'salad') & !str_detect(Ingredients, 'shrimp|oil|lentil|bean|beetroot|chicken|ready-made|ready made|potato|crab|skagen|prawn|breakfast|italian|taco|cheese|ham') ~ 'salad',

      str_detect(Ingredients, 'sauerkraut') ~ 'sauerkraut',
      str_detect(Ingredients, 'scallion|green onion|spring onion') ~ 'scallion',
      str_detect(Ingredients, 'seaweed') & !str_detect(Ingredients, 'nori') ~ 'seaweed',
      str_detect(Ingredients, 'shallot') ~ 'shallot',
      str_detect(Ingredients, 'sorrel') ~ 'sorrel',
      str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'baby') ~ 'spinach baby',
      str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'juice') ~ 'spinach juice',
      str_detect(Ingredients, 'spinach') ~ 'spinach',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'alfalfa') ~ 'sprouts alfalfa',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'bean') ~ 'sprouts bean',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'clove') ~ 'sprouts cloves',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'broccoli') ~ 'sprouts broccoli',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'radish') ~ 'sprouts radish',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'pea') ~ 'sprouts pea',
      str_detect(Ingredients, 'sprout') & str_detect(Ingredients, 'leek') ~ 'sprouts leeks',
      str_detect(Ingredients, 'sprout') & !str_detect(Ingredients, 'brussel') ~ 'sprouts alfalfa', #Default
      str_detect(Ingredients, 'stew') & str_detect(Ingredients, 'mix') ~ 'stew mix',
      str_detect(Ingredients, 'stirfry|stir-fry|stir fry') & str_detect(Ingredients, 'mix') ~ 'stir fry assorted vegetables',
      str_detect(Ingredients, 'strawberr') & str_detect(Ingredients, 'jam') ~ 'jam strawberry',
      str_detect(Ingredients, 'strawberr') & str_detect(Ingredients, 'puree') ~ 'strawberry puree',
      str_detect(Ingredients, 'strawberr') ~ 'strawberries',

      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'pea') & !str_detect(Ingredients, 'peach|pearl') ~ 'sugar snap pea',
      str_detect(Ingredients, 'swede') | (str_detect(Ingredients, 'cabbage') & str_detect(Ingredients, 'root')) | str_detect(Ingredients, 'rutabaga') ~ 'swede',
      str_detect(Ingredients, 'pickle') & str_detect(Ingredients, 'red pepper|paprika|sweet pepper') ~ 'sweet pepper pickled',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') & str_detect(Ingredients, 'green') ~ 'sweet pepper green grilled',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') & str_detect(Ingredients, 'can') ~ 'sweet pepper grilled canned',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'grilled') ~ 'sweet pepper grilled',
      str_detect(Ingredients, 'sweet pepper') & str_detect(Ingredients, 'can|drain') ~ 'sweet pepper canned',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'green') |
        str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'green|padron') & str_detect(Ingredients, 'slice|deseed|strips') ~ 'sweet pepper green',
      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') & str_detect(Ingredients, 'yellow') |
        str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'yellow') & str_detect(Ingredients, 'slice|deseed') ~ 'sweet pepper yellow',

      str_detect(Ingredients, 'sweet pepper|bell pepper|paprika') &
        !str_detect(Ingredients, 'grilled|pickled|powder|spice|smoked|potato|cheese|sandwich|ground|fill|nut') |
        str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'red') & str_detect(Ingredients, 'slice|deseed|chopped') &
        !str_detect(Ingredients, 'hot|chili|chilli|potato|cheese|sandwich|ground|fill|nut') ~ 'sweet pepper red',

      str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'potato') &
        str_detect(Ingredients, 'chip') ~ 'sweet potato chips',
      str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'potato') &
        str_detect(Ingredients, 'fries') ~ 'sweet potato fries',
      str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'potato') ~ 'sweet potato',
      str_detect(Ingredients, 'squash|zucchini|courgette') & !str_detect(Ingredients, 'butter|acorn') ~ 'summer squash zucchini', #Standard

      TRUE ~ Ingredients_standardised
    ))
}

