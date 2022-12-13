#' Standardising ingredients names in a recipe, here flours, grains nuts and legumes starting with the letter "B".
#' @title standardiseFlournGraisnNutsnLegumesB
#'
#' @description Support function for "standardiseFlournGraisnNutsnLegumes", standardise names of flours, grains nuts and legumes starting with "B".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with flours, grains nuts and legumes starting with the letter "B" with standardised names.
#'
#' @export
standardiseFlournGraisnNutsnLegumesB <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      #Bagels and rolls
      str_detect(Ingredients, 'bagel') ~ 'rolls white bagel',
      str_detect(Ingredients, 'rolls') & str_detect(Ingredients, 'coarse') ~ 'rolls coarse',
      str_detect(Ingredients, 'rolls|poppyseed hot dog bun') ~ 'rolls white',
      str_detect(Ingredients, 'baguette') & str_detect(Ingredients, 'garlic') ~ 'rolls white baguette garlic',
      str_detect(Ingredients, 'baguette') ~ 'rolls white baguette',

      #Beans
      str_detect(Ingredients, 'canned chickpeas or mixed beans') | str_detect(Ingredients, 'chickpea|chick pea|garbanzo bean') & str_detect(Ingredients, 'drain') ~ 'chick pea canned', #Chickpeas are in the name of the recipe
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'sprout') ~ 'bean sprout',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'ferment') & str_detect(Ingredients, 'black') & str_detect(Ingredients, 'rinse|drain') ~ 'bean black canned fermented',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'black') &
        (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean black canned',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|navy|cannellini|butter') &
        (str_detect(Ingredients, 'can|box|carton|drained|boiled') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean white canned',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'kidney|red') &
        (str_detect(Ingredients, 'can|box|carton') | str_detect(Ingredients, 'drained') | str_detect(Amounts, 'can|box|stk|carton') ) ~ 'bean kidney canned',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'black') ~ 'bean black',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'white|navy|cannellini|butter') ~ 'bean white',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'kidney|red') ~ 'bean kidney',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'green|french|break|snap') ~ 'bean green',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'horse|broad|fava|brew') ~ 'bean broad',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'tomat') & !str_detect(Ingredients, 'chili sin carne') ~ 'bean white tomato',
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'can') ~ "beans'n'pork canned",
      str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'can') ~ 'bean canned',

      #Biscuits
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'digestive') | Ingredients %in% c('wheat biscuits', 'tea biscuit') ~ 'biscuit digestive', #Digestive biscuits can be used as "tea biscuits" as a the bottom layer
      str_detect(Ingredients, 'speculaas|speculoos') & str_detect(Ingredients, 'spread') ~ 'spread speculaas',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'speculaas|speculoos') ~ 'biscuit speculaas',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'plain') ~ 'biscuit plain',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'dough') ~ 'gingerbread dough',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'ginger') & str_detect(Ingredients, 'house') ~ 'gingerbread house',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'ginger') ~ 'biscuit gingerbread',
      str_detect(Ingredients, 'kruidnoten') ~ 'biscuit kruidnoten',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'children') ~ 'biscuit children',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'marie') ~ 'biscuit marie',
      str_detect(Ingredients, 'biscuit|cookie') & str_detect(Ingredients, 'oreo') ~ 'biscuit oreo',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'sourdough') ~ 'biscuit sourdough',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'spelt') ~ 'biscuit spelt',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'salt') ~ 'biscuit salt',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'chocolate chip|chocolate cover') ~ 'biscuit chocolate chip',
      str_detect(Ingredients, 'biscuit') & str_detect(Ingredients, 'ice cream') & str_detect(Ingredients, 'chocolate') ~ 'biscuit ice cream chocolate',

      #Breads
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'rye') & str_detect(Ingredients, 'crumb') ~ 'bread crumb rye',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'rye') ~ 'bread rye',
      str_detect(Ingredients, 'ontbijtkoek') ~ 'bread ontbijtkoek',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'pumpernickel') ~ 'bread pumpernickel',
      (str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crumb|grate') & str_detect(Ingredients, 'white')) | str_detect(Ingredients, 'grilling flour') & str_detect(Ingredients, 'white') ~ 'bread crumb white',
      (str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crumb|grate')) | str_detect(Ingredients, 'grilling flour') ~ 'bread crumb',
      str_detect(Ingredients, 'crouton') ~ 'bread crouton',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'stick') ~ 'breadstick',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crisp') & str_detect(Ingredients, 'coarse') ~ 'crisp bread coarse',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'crisp') ~ 'crisp bread',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'coarse|whole wheat|whole-wheat|whole grain|whole-grain|wholegrain|brown') ~ 'bread coarse',
      str_detect(Ingredients, 'chapati') ~ 'bread brown chapati',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'white') & str_detect(Ingredients, 'mix') ~ 'white bread mix',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'white|fine') | str_detect(Ingredients, 'loff') ~ 'bread white',
      str_detect(Ingredients, 'flatbread') ~ 'bread flat hard',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'nan|naan') ~ 'bread naan',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'nan|naan') ~ 'bread crisp',
      str_detect(Ingredients, 'pita') & str_detect(Ingredients, 'whole-wheat|whole wheat') ~ 'bread coarse pita',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'pocket|pita') ~ 'bread white pita',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'polar') ~ 'bread polar',
      str_detect(Ingredients, 'bread') & !str_detect(Ingredients, 'flat|burger|rolls|pita|italian|olive oil|flour|margarine|sausage') ~ 'bread',
      str_detect(Ingredients, 'bread') & str_detect(Ingredients, 'sausage') ~ 'bread sausage',
      str_detect(Ingredients, 'bulgur|bulgar') ~ 'bulgur wheat',
      str_detect(Ingredients, 'bao|steam') & str_detect(Ingredients, 'bun') ~ 'bao bun',
      str_detect(Ingredients, 'focaccia') ~ 'bread white foccacia',
      str_detect(Ingredients, 'paratha') & str_detect(Ingredients, 'bread') ~ 'bread paratha',

      #Other
      str_detect(Ingredients, 'brazil') & str_detect(Ingredients, 'nut') ~ 'nut brazil',

      TRUE ~ Ingredients_standardised))
}
