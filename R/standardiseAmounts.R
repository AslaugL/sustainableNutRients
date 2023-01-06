#' Standardise amounts of an ingredient.
#' @title standardiseAmounts
#'
#' @description Standardise different ways of spelling amounts and units used of an ingredient.
#'
#' @param df A dataframe with an Ingredients column.
#'
#' @return The dataframe with amounts standardised.
#'
#' @export
standardiseAmounts <- function(df){

  df %>%
    #Turn everything into the same unit
    #Remove spaces
    mutate(Ingredients = Ingredients %>%
             #Remove invisible spaces
             str_replace_all('\u2028', '') %>%
             #Find fractions or numbers with a - or + between them and remove spaces
             str_replace_all('(?<=([\u00BC-\u00BE\u2150-\u215E]|\\d{1,3}))( - )|( \\+ )|(?<=([\u00BC-\u00BE\u2150-\u215E]|\\d{1,3}))( -)|( \\+)', '-') %>%
             #Remove spaces between / and numbers
             str_replace_all('(?<=([\u00BC-\u00BE\u2150-\u215E]|\\d{1,3})) \\/(?=([\u00BC-\u00BE\u2150-\u215E]|\\d))', '\\/')) %>%
    #Numbers
    mutate(Ingredients = Ingredients %>%
                 str_trim() %>%
                 str_replace_all('["()]|possibly', '') %>%
                 str_replace(' s ', '') %>%
                 str_replace('1,000|1 000', '1000') %>%
                 str_replace('(?<=\\d),(?=\\d)', ".") %>%
                 str_replace('(?<=\\d)(?=[^\\d\\s\\.-])', ' ') %>%
                 str_replace('(?<=\\d) \u00BD', '.5') %>%
                 str_replace('(?<=\\d) \u2158', '.8') %>%
                 str_replace('(?<=\\d) \u2155', '.2') %>%
                 str_replace('(?<=\\d) \u2153', '.33') %>%
                 str_replace('(?<=\\d) \u00BC', '.25') %>%
                 str_replace('(?<=\\d) \u215B', '.125') %>%
                 str_replace('\u2153', '0.33') %>%
                 str_replace('\u00BD|1\\/2', '0.5') %>%
                 str_replace('\u00BC|1\\/4', '0.25') %>%
                 str_replace('\u00BE|3\\/4', '0.75') %>%
                 str_replace('\u2154|2\\/3', '0.67') %>%
                 str_replace('\u215B', '0.125') %>%
                 str_replace('half(?= pac)', '0.5')) %>%

    #Use the mean of units like 3-4 etc.
    calculateMeanAmounts() %>%

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
             str_replace('(?<=\\d)slice| skive ', ' slice ') %>%
             str_replace('(?<=\\d)oz\\b', ' ounce') %>%
             str_replace('(?<=\\w|,)slice', ' slice') %>%
             str_replace('paprikai', 'paprika i') %>%

             #Spelling
             str_replace('bacon slice|pieces of bacon|pieces of good sliced bacon|thin slices of bacon|slices of bacon thin|rashers of smoked streaky bacon', 'slice bacon') %>%
             str_replace('fedd garlic|large garlic cloves|cloves of garlic|pieces minced garlic|ts garlic minced|fat garlic|clove of garlic|cloves garlic|garlic cloves|garlic clove|garlic boats|garlic boat|piece garlic|pieces of pressed garlic cloves|pieces of garlic|teaspoon garlic clove|tsp finely chopped garlic|tsp finely grated garlic|boats finely chopped garlic|cloves finely chopped garlic|cloves with garlic', 'clove garlic') %>% #A tsp garlic is about a clove
             str_replace('10 pcs loff, whole, slices without crust', '10 slice white bread') %>%
             str_replace('4 pieces of chapati', '160 g chapati') %>% #Assume the weight of a regular tortilla
             str_replace('0.5 stem celery', '0.5 pcs celery') %>%
             str_replace('celery root', 'celeriac root') %>% #Must be fixed before changing the units of celery stalks)
             str_replace('stk of celery|stalks of celery|celery rod|celery stalks|celery stalk|chopped bar celery|celery bars|stems celery|stem celery|pieces celery|piece celery|pieces of celery|piece of celery|stk celery stalks|stk celery stalk|rod celery|twig celery', 'stalk celery') %>%
             str_replace('4 slices entrecote', '4 portion entrecote') %>%
             str_replace('1 piece of pinched', '1 pinch') %>%
             str_replace('1 stem of spring onion', '1 pcs spring onion') %>%
             str_replace('coat corn, or bread grater from 2 slices of bread', '2 slice bread') %>%
             str_replace('tins of canned', 'can') %>%

             #Units
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
             str_replace_all('((?<=\\d).in\\b)|((?<=\\d).inch\\b)|((?<=\\d).inches\\b)', ' inch') %>%
             str_replace_all('((?<=\\d).oz\\b)|((?<=\\d).oz\\b)|((?<=\\d).oz\\b)', ' ounce') %>%
             str_replace('(?<=\\d{1,3})"\\b', ' inch') %>%
             str_replace('stk ltr|liter|litre', 'l') %>%
             str_replace('portions|servings', 'portion') %>%
             str_replace('portion|serving', 'portion') %>%
             str_replace('pinches|pinched|knife-wiped|klype', 'pinch') %>%
             str_replace('\\bbts\\b|tassels|bundles', 'bunch') %>%
             str_replace('\\bbt\\b|tassel|bundle', 'bunch') %>%
             str_replace('boxes', 'box') %>%
             str_replace('cups', 'cup') %>%
             str_replace('glasses', 'glass') %>%
             str_replace('drops', 'drop') %>%
             str_replace('pieces|pcs', 'pcs') %>%
             str_replace('piece', 'pcs') %>%
             str_replace('\\spcs\\s|\\spc\\s', ' pcs ') %>%
             str_replace('slices', 'slice') %>%
             str_replace('thick slice', 'slice') %>%
             str_replace('fists|handfuls', 'handful') %>%
             str_replace('fist|handful', 'handful') %>%
             str_replace('leaves', 'leaf') %>%
             str_replace('sage leaves|sage leaf', 'leaf sage') %>%
             str_replace('twigs fresh thyme|thyme twigs|stk s of thyme twigs|twigs thyme|thyme sprig|fresh thyme sprigs dried can also be used|sprigs of fresh thyme|sprig of fresh thyme|sprig thyme|sprig of thyme|large sprig of thyme', 'twig thyme') %>%
             str_replace('stk of neve fresh thyme', 'handful fresh thyme') %>%
             str_replace('small bunch of fresh thyme, leaf only', 'bunch fresh thyme') %>%
             str_replace('stk of parsley sprig|pieces of parsley stalks|stk of parsley stalks', 'twig parsley') %>%
             str_replace('stk of rosemary sprig|stems fresh rosemary|piece of rosemary sprig|pieces of rosemary|pieces of fresh rosemary', 'twig rosemary') %>%
             str_replace('1 mug of parsley', '1 bunch parsley') %>%
             str_replace('piece basil, fresh', 'twig basil fresh') %>%
             str_replace('stk of neve coriander', 'handful coriander') %>%
             str_replace('stk leaf sage', 'leaf sage') %>%
             str_replace('stk small dill bunch', 'bunch dill') %>%
             str_replace('sprigs|sprig|\\bstems\\b|\\bstem\\b|twigs', 'twig') %>%
             str_replace('pieces rosemary, fresh|stk rosemary fresh', 'bunch rosemary') %>%
             str_replace('basil leaf|fresh basil leaf, cut into thin strips', 'leaf basil') %>%
             str_replace('pounds|\\blbs\\b', 'pound') %>%
             str_replace('\\blb\\b', 'pound') %>%
             str_replace('ounces', 'ounce') %>%
             str_replace('chili stk|chili pcs', 'pcs chili') %>%
             str_replace('\\bpk\\b', 'pack') %>%
             str_replace('\\bstilk\\b', 'stalk') %>%
             str_replace('celery ribs', "stalk celery") %>%
             str_replace('celery rib', "stalk celery")) %>%

    mutate(Ingredients = Ingredients %>%
             str_replace('neve|h\u00E5ndfull', 'handful') %>%
             str_replace('\\bstk\\b', 'pcs') %>%
             str_replace('\\bfedd\\b', 'clove') %>%
             str_replace('inch piece|inch pcs', 'inch')) #Only keep inch when present
}
