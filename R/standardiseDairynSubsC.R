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
    str_detect(Ingredients, 'burrata') ~ "cheese burrata mozzarella",
    str_detect(Ingredients, 'norzola') ~ 'cheese blue norzola',
    str_detect(Ingredients, 'gorgonzola') ~ 'cheese blue gorgonzola',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cotija') ~ 'cheese cotjia',
    str_detect(Ingredients, 'castello') & str_detect(Ingredients, 'blue') ~ 'cheese blue castello',
    str_detect(Ingredients, 'selbu') & str_detect(Ingredients, 'cheese') ~ 'cheese blue selbu',
    str_detect(Ingredients, 'stilton') & str_detect(Ingredients, 'cheese') ~ 'cheese blue stilton',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'blue') ~ 'cheese blue',
    str_detect(Ingredients, 'brie') & !str_detect(Ingredients, 'marmelade|marmalade') ~ 'cheese brie',
    str_detect(Ingredients, 'camembert') & str_detect(Ingredients, 'vegan|plant based|plant-based') ~ 'cheese plant-based camembert',
    str_detect(Ingredients, 'camembert') & !str_detect(Ingredients, 'marmelade|marmalade') ~ 'cheese camembert',
    str_detect(Ingredients, 'real goat cheese') ~ 'cheese brown goat',
    (str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'brown') |
       (str_detect(Ingredients, 'gudbrandsdal|grandmother') & str_detect(Ingredients, 'cheese') )     ) &
      str_detect(Ingredients, "low fat|light") ~ 'cheese brown low fat',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'brown') | (str_detect(Ingredients, 'gudbrandsdal|grandmother') & str_detect(Ingredients, 'cheese') ) ~ 'cheese brown',
    str_detect(Ingredients, 'cheddar') & str_detect(Ingredients, 'vegan|plant based|plant-based')  ~ 'cheese plant-based cheddar',
    str_detect(Ingredients, 'cheddar') ~ 'cheese cheddar',
    str_detect(Ingredients, 'garlic cheese') ~ 'cheese garlic',
    str_detect(Ingredients, 'gruy+u00E8re|gruyere') ~ 'cheese gruyere',
    str_detect(Ingredients, 'chevre|chevrè|chèvre') & !str_detect(Ingredients, 'marmelade|marmalade') ~ 'cheese goat chevre white',
    str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'cheese') & !str_detect(Ingredients, 'hard') ~ 'cheese goat',
    str_detect(Ingredients, 'feta|fat cheese in cubes|semi-solid cheese in cubes') & str_detect(Ingredients, 'cheese') | str_detect(Ingredients, 'feta') & str_detect(Ingredients, 'crumbled') ~ 'cheese feta', #Fat cheese is a translation error from Norwegian
    str_detect(Ingredients, 'halloumi') ~ 'cheese halloumi',
    str_detect(Ingredients, 'jarlsberg') ~ 'cheese jarlsberg',
    str_detect(Ingredients, 'manchego') ~ 'cheese manchego',
    str_detect(Ingredients, 'gouda') ~ 'cheese gouda',
    str_detect(Ingredients, 'mascarpone') ~ 'cheese mascarpone',
    str_detect(Ingredients, 'mozzarella') & str_detect(Ingredients, 'vegan|plant') ~ 'cheese plant-based mozzarella',
    str_detect(Ingredients, 'mozzarella') & !str_detect(Ingredients, 'tube|spread|pizza') ~ 'cheese mozzarella',
    str_detect(Ingredients, 'norvegia') ~ 'cheese norvegia',
    str_detect(Ingredients, 'port salut') ~ 'cheese port salut',
    str_detect(Ingredients, 'ricotta') ~ 'cheese ricotta salata',
    str_detect(Ingredients, 'le cr\u00E9mier de chaumes') ~ 'cheese le cr\u00E9mier de chaumes',
    str_detect(Ingredients, 'romano') & str_detect(Ingredients, 'cheese') ~ 'cheese romano',
    str_detect(Ingredients, 'roquefort') & !str_detect(Ingredients, 'marmelade|marmalade') ~ 'cheese blue roquefort',
    str_detect(Ingredients, 'swiss') & str_detect(Ingredients, 'cheese') ~ 'cheese swiss',
    str_detect(Ingredients, 'provolone') & str_detect(Ingredients, 'cheese') ~ 'cheese provolone',
    str_detect(Ingredients, 'monterey jack|pepperjack') & str_detect(Ingredients, 'cheese') ~ 'cheese monterey jack',
    str_detect(Ingredients, 'neufchatel') & str_detect(Ingredients, 'cheese') ~ 'cheese neufchatel',
    str_detect(Ingredients, 'pecorino') & str_detect(Ingredients, 'cheese|romano') ~ 'cheese pecorino',
    str_detect(Ingredients, 'saint agur') ~ 'cheese blue saint agur',
    str_detect(Ingredients, 'emmentaler') & str_detect(Ingredients, 'cheese') ~ 'cheese emmentaler',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'goat') & str_detect(Ingredients, 'hard') ~ 'cheese goat hard',
    str_detect(Ingredients, 'sn\u00f8frisk|snow fresh') ~ 'cheese cream goat sn\u00f8frisk',
    (str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cream|fresh') | str_detect(Ingredients, 'kremgo') | str_detect(Ingredients, 'philadelphia')) & str_detect(Ingredients, 'herb|chocolate|season|truffle|garlic|chive|spice|apricot|pepper') ~ 'cheese cream flavored',
    (str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'cream|fresh') | str_detect(Ingredients, 'kremgo') | str_detect(Ingredients, 'philadelphia')) & !str_detect(Ingredients, 'glaze') ~ 'cheese cream',
    (str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'skinny|low fat|lean|mager|low-fat')) | str_detect(Ingredients, 'paneer cheese') ~ 'cheese cottage low fat', #Paneer is a cheese like low fat cc
    str_detect(Ingredients, 'cottage') & str_detect(Ingredients, 'cheese') ~ 'cheese cottage',
    str_detect(Ingredients, 'parmesan') ~ 'parmesan cheese',
    str_detect(Ingredients, 'parmigiano reggiano|parmigiano reggiano') ~ 'cheese parmigiano reggiano',
    str_detect(Ingredients, 'castello') & str_detect(Ingredients, 'white')  ~ 'cheese soft castello',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'soft') ~ 'cheese soft',
    str_detect(Ingredients, 'cheese le crémier de chaumes') ~ 'cheese soft cheese le cremier de chaumes',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'spread|tube') ~ 'cheese spread',
    str_detect(Ingredients, 'cheese') & str_detect(Ingredients, 'plant-based|vegan') ~ 'cheese plant-based',
    str_detect(Ingredients, 'bacon cheese') ~ 'cheese bacon',
    str_detect(Ingredients, 'cheese') &
      !str_detect(Ingredients, 'yogurt|yoghurt|pancake|cracker|mac|bacon|for cheese|salad|sandwich|cake|tube|schnitzel|sausage|knacker|marmalade|marmelade|potato|filled|pizza|grandiosa|hold-it|glaze|\\bdip\\b') ~ 'cheese semi-hard',

    str_detect(Ingredients, 'whip') & str_detect(Ingredients, 'it|stabilizer') ~ 'whip it stabilizer',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'double') ~ 'cream double 48 \u0025',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'whip|heavy|33|38|39|37') | str_detect(Ingredients, 'whipped topping') ~ 'cream whipped 37 \u0025',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'ice') & str_detect(Ingredients, 'vanilla') ~ 'ice cream vanilla',
    str_detect(Ingredients, 'ice cream') & str_detect(Ingredients, 'pin up') ~ 'ice cream pinup',
    str_detect(Ingredients, 'ice cream') & str_detect(Ingredients, 'boat') ~ 'ice cream boat',
    str_detect(Ingredients, 'ice cream') & str_detect(Ingredients, 'sandwich') ~ 'ice cream sandwich',
    str_detect(Ingredients, 'ice cream') & str_detect(Ingredients, 'lollipop') ~ 'ice cream lollipop',
    str_detect(Ingredients, 'ice cream') & str_detect(Ingredients, 'cake') ~ 'ice cream cake',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, '\\bice') & str_detect(Ingredients, 'vegan|plant-based|plant based') ~ 'ice cream plant-based',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, '\\bice') ~ 'ice cream',
    str_detect(Ingredients, 'cream') & str_detect(Ingredients, 'vegan|plant-based|plant based|rice') ~ 'cream plant-based',
    str_detect(Ingredients, 'cream') & (str_detect(Ingredients, 'coffee|cooking') | str_detect(Ingredients, '10 \u0025')) ~ 'cream coffee 10 \u0025',
    str_detect(Ingredients, 'cream') &
      ((str_detect(Ingredients, 'food|cooking') |
          !str_detect(Ingredients,
                      'cheese|sour|cracker|sauce|coconut|light|condensed|ice|balsamic|potato|vegan|plant|\\boat\\b|castello|cake|vanilla|and cream|with cream'))
       ) ~ 'cream household 18 \u0025', #Standard
    str_detect(Ingredients, 'cr\u00E8me fra\u00EEche 18 \u0025') | str_detect(Ingredients, 'cr\u00E8me fra\u00EEche') & str_detect(Ingredients, 'light|18') ~ 'cr\u00E8me fra\u00EEche 18 \u0025',
    str_detect(Ingredients, 'cr\u00E8me fra\u00EEche 10 \u0025') ~ 'cr\u00E8me fra\u00EEche 10 \u0025',
    str_detect(Ingredients, 'cr\u00E8me fra\u00EEche|creme fraiche') & str_detect(Ingredients, 'plant-based|plant based|vegan') ~ 'cr\u00E8me fra\u00EEche plant-based',
    str_detect(Ingredients, 'cr\u00E8me fra\u00EEche|creme fraiche') ~ 'cr\u00E8me fra\u00EEche 35 \u0025', #The original

    TRUE ~ Ingredients_standardised))
}
