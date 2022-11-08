#' Standardising the names of units in a recipe
#' @title standardiseUnitNames
#'
#' @description Standardise different ways of spelling unit names.
#'
#' @param df A dataframe with an Ingredients column.
#'
#' @return The dataframe with all unit names standardised.
#'
#' @export
standardiseUnitNames <- function(df){

  #Turn everything into the same unit
  #Numbers
  df %>% mutate(Ingredients = Ingredients %>%
                 str_replace_all('["()]|possibly', '') %>%
                 str_replace(' s ', '') %>%
                 str_replace('1,000|1 000', '1000') %>%
                 str_replace('(?<=\\d)(?=[^\\d\\s\\.-])', ' ') %>%
                 str_replace('(?<=\\d) ½', '.5') %>%
                 str_replace('(?<=\\d) ⅘', '.8') %>%
                 str_replace('(?<=\\d) ⅕', '.2') %>%
                 str_replace('(?<=\\d) ⅓', '.33') %>%
                 str_replace('(?<=\\d) ¼', '.25') %>%
                 str_replace('⅓', '0.33') %>%
                 str_replace('½', '0.5') %>%
                 str_replace('¼', '0.25') %>%
                 str_replace('¾', '0.75') %>%
                 str_replace('⅔', '0.67') %>%
                 str_replace('2 -3|2-3', '3') %>% #Use three of each of the two foods with 2-3 written in the recipe
                 str_replace('half(?= pac)', '0.5')) %>%

    mutate(Ingredients = Ingredients %>%

             #Missing spaces
             str_replace('\\bg(?=[^ |^arlic|^uacamole|^hee])', 'g ') %>%
             str_replace('\\bkg(?=\\w)', 'kg ') %>%
             str_replace('\\bpacks(?=\\w)', 'pack ') %>%
             str_replace('\\bpack(?=[a-rt-z])', 'pack ') %>%
             str_replace('\\bpcs(?=\\w)', 'pcs ') %>%
             str_replace('\\bpieces(?=\\w)', 'piece ') %>%
             str_replace('\\bpiece(?=[a-rt-z])', 'piece ') %>%
             str_replace('\\btbsp(?=[a-rt-z])', 'tbsp ') %>%
             str_replace('\\bkg(?=[a-rt-z])', 'kg ') %>%
             str_replace('\\btablespoons(?=\\w)', 'tablespoon ') %>%
             str_replace('\\btablespoon(?=[a-rt-z])', 'tablespoon ') %>%
             str_replace('\\bdl|\\bDL(?=\\w)', 'dl ') %>%
             str_replace('slice(?=[^s|^\\s|^d])', 'slice ') %>%
             str_replace('(?<=\\d)slice', ' slice ') %>%
             str_replace('(?<=\\w|,)slice', ' slice') %>%
             str_replace('paprikai', 'paprika i') %>%

             #Spelling
             str_replace('bacon slice|pieces of bacon|pieces of good sliced bacon|thin slices of bacon|slices of bacon thin|rashers of smoked streaky bacon', 'slice bacon') %>%
             str_replace('large garlic cloves|cloves of garlic|pieces minced garlic|ts garlic minced|fat garlic|clove of garlic|cloves garlic|garlic cloves|garlic clove|garlic boats|garlic boat|piece garlic|pieces of pressed garlic cloves|pieces of garlic|teaspoon garlic clove|tsp finely chopped garlic|tsp finely grated garlic|boats finely chopped garlic|cloves finely chopped garlic|cloves with garlic', 'clove garlic') %>% #A tsp garlic is about a clove
             str_replace('10 pcs loff, whole, slices without crust', '10 slice white bread') %>%
             str_replace('4 pieces of chapati', '160 g chapati') %>% #Assume the weight of a regular tortilla
             str_replace('0.5 stem celery', '0.5 stk celery') %>%
             str_replace('celery root', 'celeriac root') %>% #Must be fixed before changing the units of celery stalks)
             str_replace('stk of celery|stalks of celery|celery rod|celery stalks|celery stalk|chopped bar celery|celery bars|stems celery|stem celery|pieces celery|piece celery|pieces of celery|piece of celery|stk celery stalks|stk celery stalk|rod celery|twig celery', 'stalk celery') %>%
             str_replace('4 slices entrecote', '4 portion entrecote') %>%
             str_replace('1 piece of pinched', '1 pinch') %>%
             str_replace('1 stem of spring onion', '1 stk spring onion') %>%
             str_replace('coat corn, or bread grater from 2 slices of bread', '2 slice bread') %>%
             str_replace('tins of canned', 'can') %>%
             str_replace('parts|DL', 'dl') %>%
             str_replace('packs|packet|pakke', 'pack') %>%
             str_replace('\\sbags\\s', ' pack ') %>%
             str_replace('\\sbag\\s', ' pack ') %>%
             str_replace('\\stb\\s', ' tbsp ') %>%
             str_replace('tablespoons|tbsps|table spoons', 'tbsp') %>%
             str_replace('tablespoon|tbsp|table spoon|\\bss\\b', 'tbsp') %>%
             str_replace('\\st\\s', ' tsp ') %>%
             str_replace('teaspoons|tsps', 'tsp') %>%
             str_replace('teaspoon|tsp|\\bts\\b', 'tsp') %>%
             str_replace('kilo', 'kg') %>%
             str_replace('\\sgr\\s', ' g ') %>%
             str_replace('grams', 'g') %>%
             str_replace('stk ltr|liter|litre', 'l') %>%
             str_replace('portions|servings', 'portion') %>%
             str_replace('portion|serving', 'portion') %>%
             str_replace('pinches|pinched|knife-wiped', 'pinch') %>%
             str_replace('\\bbts\\b|tassels|bundles', 'bunch') %>%
             str_replace('\\bbt\\b|tassel|bundle', 'bunch') %>%
             str_replace('boxes', 'box') %>%
             str_replace('cups', 'cup') %>%
             str_replace('glasses', 'glass') %>%
             str_replace('drops', 'drop') %>%
             str_replace('pieces|pcs', 'stk') %>%
             str_replace('piece', 'stk') %>%
             str_replace('\\spcs\\s|\\spc\\s', ' stk ') %>%
             str_replace('slices', 'slice') %>%
             str_replace('thick slice', 'slice') %>%
             str_replace('fists|handfuls', 'neve') %>%
             str_replace('fist|handful', 'neve') %>%
             str_replace('leaves', 'leaf') %>%
             str_replace('sage leaves|sage leaf', 'leaf sage') %>%
             str_replace('twigs fresh thyme|thyme twigs|stk s of thyme twigs|twigs thyme|thyme sprig|fresh thyme sprigs dried can also be used|sprigs of fresh thyme|sprig of fresh thyme|sprig thyme|sprig of thyme|large sprig of thyme', 'twig thyme') %>%
             str_replace('stk of neve fresh thyme', 'neve fresh thyme') %>%
             str_replace('small bunch of fresh thyme, leaf only', 'bunch fresh thyme') %>%
             str_replace('stk of parsley sprig|pieces of parsley stalks|stk of parsley stalks', 'twig parsley') %>%
             str_replace('stk of rosemary sprig|stems fresh rosemary|piece of rosemary sprig|pieces of rosemary|pieces of fresh rosemary', 'twig rosemary') %>%
             str_replace('1 mug of parsley', '1 bunch parsley') %>%
             str_replace('1 mug of parsley', '1 bunch parsley') %>%
             str_replace('piece basil, fresh', 'twig basil fresh') %>%
             str_replace('stk of neve coriander', 'neve coriander') %>%
             str_replace('stk leaf sage', 'leaf sage') %>%
             str_replace('stk small dill bunch', 'bunch dill') %>%
             str_replace('sprigs|sprig|\\bstems\\b|\\bstem\\b|twigs', 'twig') %>%
             str_replace('pieces rosemary, fresh|stk rosemary fresh', 'bunch rosemary') %>%
             str_replace('basil leaf|fresh basil leaf, cut into thin strips', 'leaf basil') %>%
             str_replace('pounds', 'pound') %>%
             str_replace('ounces', 'ounce') %>%
             str_replace('chili stk', 'stk chili'))
}
