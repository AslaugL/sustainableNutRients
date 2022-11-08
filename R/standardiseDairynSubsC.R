#' Standardising ingredients names in a recipe, here dairy and substitutes starting with the letter "C".
#' @title standardiseDairynSubsC
#'
#' @description Support function for "standardiseDairynSubs", standardise names of dairy and dairy substitute starting with "C".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with dairy and substitutes starting with the letter "C" with standardised names.
#'
#' @export
standardiseDairynSubsC <- function(df) {
  df  %>%

  #Standardise
  mutate(Ingredients_standardised = case_when(
    #Cheeses
    str_detect(Ingredients, 'american cheese') ~ 'cheese american',
    str_detect(Ingredients, 'asiago') & str_detect(Ingredients, 'cheese') ~ 'cheese asiago',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'norman') ~ 'cheese blue normanna', #Tine cheese
    str_detect(Ingredients, 'norzola') ~ 'cheese blue norzola',
    str_detect(Ingredients, 'gorgonzola') ~ 'cheese gorgonzola',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cotija') ~ 'cheese cotjia',
    str_detect(Ingredients, 'stilton') & str_detect(Ingredients, 'cheese') ~ 'cheese blue stilton',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'blue') ~ 'cheese blue',
    str_detect(Ingredients, 'brie') ~ 'cheese brie',
    str_detect(Ingredients, 'camembert') ~ 'cheese camembert',
    str_detect(Ingredients, 'real goat cheese') ~ 'goat brown cheese',
    str_detect(Ingredients, 'cheddar') ~ 'cheese cheddar',
    str_detect(Ingredients, 'garlic cheese') ~ 'cheese garlic',
    str_detect(Ingredients, 'gruyère|gruyere') ~ 'cheese gruyere',
    str_detect(Ingredients, 'chevre') ~ 'cheese goat chevre white',
    str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'cheese') & !str_detect(Ingredients, 'hard') ~ 'cheese goat',
    str_detect(Ingredients, 'feta|fat cheese in cubes|semi-solid cheese in cubes') & str_detect(Ingredients, 'cheese') | str_detect(Ingredients, 'feta') & str_detect(Ingredients, 'crumbled') ~ 'cheese feta', #Fat cheese is a translation error from Norwegian
    str_detect(Ingredients, 'halloumi') ~ 'cheese halloumi',
    str_detect(Ingredients, 'jarlsberg') ~ 'cheese jarlsberg',
    str_detect(Ingredients, 'manchego') ~ 'cheese manchego',
    str_detect(Ingredients, 'mascarpone') ~ 'cheese mascarpone',
    str_detect(Ingredients, 'mozzarella') ~ 'cheese mozzarella',
    str_detect(Ingredients, 'norvegia') ~ 'cheese norvegia',
    str_detect(Ingredients, 'port salut') ~ 'cheese port salut',
    str_detect(Ingredients, 'ricotta') ~ 'cheese ricotta salata',
    str_detect(Ingredients, 'romano') & str_detect(Ingredients, 'cheese') ~ 'cheese romano',
    str_detect(Ingredients, 'swiss') & str_detect(Ingredients, 'cheese') ~ 'cheese swiss',
    str_detect(Ingredients, 'provolone') & str_detect(Ingredients, 'cheese') ~ 'cheese provolone',
    str_detect(Ingredients, 'monterey jack|pepperjack') & str_detect(Ingredients, 'cheese') ~ 'cheese monterey jack',
    str_detect(Ingredients, 'neufchatel') & str_detect(Ingredients, 'cheese') ~ 'cheese neufchatel',
    str_detect(Ingredients, 'pecorino') & str_detect(Ingredients, 'cheese') ~ 'cheese pecorino',
    str_detect(Ingredients, 'emmentaler') & str_detect(Ingredients, 'cheese') ~ 'cheese emmentaler',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'hard') ~ 'cheese hard goat',
    str_detect(Ingredients, 'snøfrisk|snow fresh') ~ 'cheese cream goat snøfrisk',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cream') | str_detect(Ingredients, 'kremgo') ~ 'cheese cream',
    (str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'skinny|low fat|lean|mager')) | str_detect(Ingredients, 'paneer cheese') ~ 'cheese cottage low fat', #Paneer is a cheese like low fat cc
    str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'cheese') ~ 'cheese cottage',
    str_detect(Ingredients, 'parmesan') ~ 'parmesan cheese',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'soft') ~ 'cheese soft',
    str_detect(Ingredients, 'cheese') & !str_detect(Ingredients, 'yogurt|yoghurt') ~ 'cheese semi-hard',

    str_detect(Ingredients, 'whip') & str_detect(Ingredients, 'it|stabilizer') ~ 'whip it stabilizer',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'double') ~ 'cream double 48 %',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'whip|heavy') | str_detect(Ingredients, 'whipped topping') ~ 'cream whipped 37 %',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'ice') & str_detect(Ingredients, 'vanilla') ~ 'ice cream vanilla',
    str_detect(Ingredients, 'cream') & (str_detect(Ingredients, 'food') | !str_detect(Ingredients, 'cheese|sour|cracker|sauce|coconut|light|condensed|ice')) ~ 'cream household 18 %', #Standard
    str_detect(Ingredients, 'crème fraîche 18 %') ~ 'crème fraîche 18 %',
    str_detect(Ingredients, 'crème fraîche 10 %') ~ 'crème fraîche 10 %',
    str_detect(Ingredients, 'crème fraîche|creme fraiche') ~ 'crème fraîche 35 %', #The original

    TRUE ~ Ingredients_standardised))
}
