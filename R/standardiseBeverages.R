#' Standardising ingredients names in a recipe, here different beverages
#' @title standardiseBeverages
#'
#' @description Support function for "standardiseFoodNames", standardises names of different beverages
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with beverages with standardised names.
#'
#' @export
standardiseBeverages <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(

      # Alcohol
      str_detect(Ingredients, "aquavit") ~ "aquavit",
      str_detect(Ingredients, 'beer') & str_detect(Ingredients, 'dark|amber|christmas') ~ 'beer dark',
      str_detect(Ingredients, 'beer|\\bale\\b') & !str_detect(Ingredients, 'sausage') ~ 'beer',
      str_detect(Ingredients, 'brandy') ~ 'spirits 40 vol-% alcohol brandy',
      str_detect(Ingredients, 'cider') & !str_detect(Ingredients, 'vinegar') ~ 'cider',
      str_detect(Ingredients, 'cognac') ~ 'spirits 40 vol-% alcohol cognac',
      str_detect(Ingredients, 'dark rum') | Ingredients == 'rum' ~ 'spirits 40 vol-% alcohol dark rum',
      str_detect(Ingredients, 'kirsch') ~ 'spirits 40 vol-% alcohol kirsch',
      str_detect(Ingredients, 'madeira') ~ 'madeira fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'marsala') ~ 'marsala fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'liquor') ~ 'fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'sake') ~ 'sake',
      str_detect(Ingredients, 'sherry') & !str_detect(Ingredients, 'vinegar') ~ 'sherry fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'vermouth') ~ 'vermouth fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'vodka') ~ 'spirits 40 vol-% alcohol vodka',
      str_detect(Ingredients, 'whisky|whiskey') ~ 'whisky spirits 40 vol-% alcohol',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'rice') ~ 'wine rice',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'white') & !str_detect(Ingredients, 'vinegar') ~ 'wine white',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'red|merlot') & !str_detect(Ingredients, 'vinegar|sausage|salami|sauce') ~ 'wine red',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'port') ~ 'wine port fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'mirin japanese sweet wine') ~ 'wine mirin',
      str_detect(Ingredients, 'liqueur') & str_detect(Ingredients, 'orange') ~ 'orange liqueur',
      str_detect(Ingredients, 'liqueur') & str_detect(Ingredients, 'raspberry') ~ 'raspberry liqueur',
      str_detect(Ingredients, 'liqueur') & str_detect(Ingredients, 'coffee') ~ 'coffee liqueur',
      str_detect(Ingredients, 'seltzer') ~ 'seltzer',

      # Coffee, tea and cocoa
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, 'filter') & str_detect(Ingredients, 'brew') ~ 'coffee filter brew',
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, 'filter') ~ 'coffee filter powder',
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, 'espresso') & str_detect(Ingredients, 'powder') ~ 'coffee espresso powder',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'drink') & !str_detect(Ingredients, 'soy|oat|pea|coconut|rice|sproud') ~ 'chocolate drink',
      str_detect(Ingredients, 'espresso') & str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'ground') ~ 'espresso bean coffee ground',
      str_detect(Ingredients, 'iced coffee') & str_detect(Ingredients, 'without added sugar|no sugar') ~ 'iced coffee without sugar',
      str_detect(Ingredients, 'iced coffee') & str_detect(Ingredients, 'sproud|vegan|plant-based|plant based') ~ 'iced coffee plant-based',
      str_detect(Ingredients, 'iced coffee') ~ 'iced coffee',
      str_detect(Ingredients, 'iced tea') ~ 'iced tea',
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, "espresso") & !str_detect(Ingredients, "ice|bean|chocolate") ~ "coffee espresso",
      str_detect(Ingredients, 'coffee') & !str_detect(Ingredients, "ice|bean|chocolate") ~ "coffee",

      (str_detect(Ingredients, 'tea') & str_detect(Ingredients, 'black')) | str_detect(Ingredients, 'earl grey|lady gray|english breakfast') ~ 'tea black',
      str_detect(Ingredients, 'tea') & str_detect(Ingredients, 'green') ~ 'tea green',
      str_detect(Ingredients, '\\btea\\b|twinings') ~ 'tea',

      # Sodas, fruit and energy drinks
      str_detect(Ingredients, 'coca cola') ~ 'coca cola',
      (str_detect(Ingredients, 'soda') & str_detect(Ingredients, 'flavor')) |
        str_detect(Ingredients, 'soft drink') ~ 'soft drink',

      str_detect(Ingredients, 'battery') & str_detect(Ingredients, 'blueberr') ~ 'energy drink battery blueberries flavor',
      str_detect(Ingredients, 'battery') & str_detect(Ingredients, 'peach') ~ 'energy drink battery peach flavor',
      str_detect(Ingredients, 'battery') & str_detect(Ingredients, 'strawberr') ~ 'energy drink battery strawberries flavor',
      str_detect(Ingredients, 'battery') & str_detect(Ingredients, 'energy') ~ 'energy drink battery',
      str_detect(Ingredients, 'burn') & str_detect(Ingredients, 'peach') ~ 'energy drink burn peach flavor',
      str_detect(Ingredients, 'red bull') & str_detect(Ingredients, 'tropical') ~ 'energy drink red bull tropical flavor',
      str_detect(Ingredients, 'red bull') & str_detect(Ingredients, 'peach') ~ 'energy drink red bull peach flavor',
      str_detect(Ingredients, 'red bull') & str_detect(Ingredients, 'watermelon') ~ 'energy drink red bull watermelon flavor',
      str_detect(Ingredients, 'red bull') & str_detect(Ingredients, 'blueberr') ~ 'energy drink red bull blueberries flavor',
      str_detect(Ingredients, 'red bull') & str_detect(Ingredients, 'sugar free') ~ 'energy drink red bull sugar free',
      str_detect(Ingredients, 'red bull') ~ 'energy drink red bull',
      str_detect(Ingredients, 'monster energy') ~ 'energy drink monster',
      str_detect(Ingredients, 'nocco energy') ~ 'energy drink nocco',
      str_detect(Ingredients, 'powerade') ~ 'energy drink powerade',

      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'raspberr') ~ 'fruit drink fun light raspberries',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'ice tea') ~ 'fruit drink fun light ice tea',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'orange') ~ 'fruit drink fun light orange',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'peach') ~ 'fruit drink fun light peach',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'mandarin') ~ 'fruit drink fun light mandarin',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'wil berr') ~ 'fruit drink fun light wild berries',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'watermelon') ~ 'fruit drink fun light watermelon',
      str_detect(Ingredients, 'fun light') & str_detect(Ingredients, 'mango') ~ 'fruit drink fun light mango',
      str_detect(Ingredients, 'fun light') ~ 'fruit drink fun light',

      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'blueberr')  &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink blueberries no sugar',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'blackcurrant') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink blackcurrants no sugar',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'currant') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink currants no sugar',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'raspberries') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink raspberries no sugar',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'rhubarb') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink rhubarb no sugar',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'plum') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink plum no sugar',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'cherr') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugar') ~ 'fruit drink cherries no sugar',
      str_detect(Ingredients, 'canned fruit drink') ~ 'canned fruit drink',

      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'blueberr') ~ 'fruit drink blueberries',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'blackcurrant') ~ 'fruit drink blackcurrants',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'currant') ~ 'fruit drink currants',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'rhubarb') ~ 'fruit drink rhubarb',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'plum') ~ 'fruit drink plum',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'cherr') ~ 'fruit drink cherries',
      str_detect(Ingredients, 'drink') & str_detect(Ingredients, 'raspberries') ~ 'fruit drink raspberries',
      str_detect(Ingredients, 'drink') &
        str_detect(Ingredients, 'without added sugar|no added sugar|no sugar|without sugart') ~ 'fruit drink no sugar',
      str_detect(Ingredients, 'fruit drink') ~ 'fruit drink',

      # Smoothie packs and smoothies
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'mix|pack') &
        str_detect(Ingredients, 'tropical') ~ 'smoothie mix tropical',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'mix|pack') &
        str_detect(Ingredients, 'dragon fruit') ~ 'smoothie mix dragon fruit',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'mix|pack') &
        str_detect(Ingredients, 'passion fruit') ~ 'smoothie mix passion fruit',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'mix|pack') &
        str_detect(Ingredients, 'mango') ~ 'smoothie mix mango',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'mix|pack') ~ 'smoothie mix',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'carrot') & str_detect(Ingredients, 'ginger') ~ 'smoothie carrot and ginger',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'apple|pear|pine apple|orange|banana|berry|berries|currant') ~ 'smoothie fruit or berries',
      str_detect(Ingredients, 'smoothie') & str_detect(Ingredients, 'green|spinach|kale') ~ 'smoothie green leaves',

      # Others
      str_detect(Ingredients, 'household juice') ~ 'household juice',
      str_detect(Ingredients, 'kombucha') & str_detect(Ingredients, 'start') ~ 'kombucha starter',
      str_detect(Ingredients, 'kombucha') ~ 'kombucha',
      str_detect(Ingredients, 'lemonade') ~ 'lemonade',
      str_detect(Ingredients, 'beverage') & str_detect(Ingredients, 'carbonated') & str_detect(Ingredients, 'lemon') &
        str_detect(Ingredients, 'lime') ~ 'carbonated beverage lemon-lime',
      str_detect(Ingredients, 'farris|carbonated water|bonaqua|\\bolden\\b') ~ 'carbonated water',
      str_detect(Ingredients, 'water') & !str_detect(Ingredients, 'corn|beef|tuna|coffee|chili|cream|cress|chestnut|melon|and water') ~ 'water',
      str_detect(Ingredients, 'water to the corn') ~ 'water',
      str_detect(Ingredients, 'vinaigrette') ~ 'vinaigrette',
      str_detect(Ingredients, 'juice') & str_detect(Ingredients, 'strawberr') ~ "strawberry juice",
      str_detect(Ingredients, 'soda') & !str_detect(Ingredients, 'baking') ~ "soda",


      TRUE ~ Ingredients_standardised))
}
