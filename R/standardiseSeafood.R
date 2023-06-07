#' Standardising ingredients names in a recipe, here seafood.
#' @title standardiseSeafood
#'
#' @description Standardise names of seafood.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with seafood with standardised names.
#'
#' @export
standardiseSeafood <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(

      #Balls, burgers and cake
      str_detect(Ingredients, 'fish ball') & str_detect(Ingredients, 'can') ~ 'fish balls canned',
      str_detect(Ingredients, 'fish ball') & str_detect(Ingredients, 'curry sauce') ~ 'fish balls curry sauce',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'ball') ~ 'fish balls',
      str_detect(Ingredients, 'burger') & str_detect(Ingredients, 'white fish|cod|haddock|pollock') ~ 'fish burger white fish',
      str_detect(Ingredients, 'burger') & str_detect(Ingredients, 'red fish|fatty fish') ~ 'fish burger fatty fish',
      str_detect(Ingredients, 'salma') & str_detect(Ingredients, 'burger') ~ 'pure salmon burger',
      str_detect(Ingredients, 'fish burger') ~ 'fish burger',
      str_detect(Ingredients, 'fish nugget') | str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'cripsy') ~ 'fish nugget',

      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'coarse') ~ 'fish cakes coarse',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'cake') ~ 'fish cakes',
      str_detect(Ingredients, 'fish, head, back bone') | Ingredients == 'fish cut' ~ 'fish scraps for broth',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'stick') ~ 'fish sticks',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'gratin') ~ 'fish gratin',

      #Seafood
      str_detect(Ingredients, 'anchov|sardine') & str_detect(Ingredients, 'fillet') ~ 'anchovy fillet',
      str_detect(Ingredients, '\\banchov') ~ 'anchovy canned', #Standard
      str_detect(Ingredients, 'angler fish|anglerfish') ~ 'anglerfish',

      str_detect(Ingredients, 'bacalao') ~ 'bacalao',

      str_detect(Ingredients, 'catfish|wolf fish|wolffish') ~ 'catfish',
      str_detect(Ingredients, 'caviar') ~ 'caviar',
      str_detect(Ingredients, 'ishavsr\u00f8ye|arctic char') ~ 'arctic char fatty fish',
      str_detect(Ingredients, 'clam') ~ 'clam',
      str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'lutefisk') | str_detect(Ingredients, 'lutefisk|lutefish') ~ 'cod lutefisk',
      str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'clip') | str_detect(Ingredients, 'clipfish') ~ 'cod clipfish',
      str_detect(Ingredients, 'cod') & str_detect(Ingredients, 'cooked') ~ 'cod cooked',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'dried|dry') ~ 'cod dried',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'bread|crisp') ~ 'cod breaded',
      str_detect(Ingredients, 'fish fillet|firm white fish|different fillets of white fish|optional fish without skin and bone')
      & !str_detect(Ingredients, 'angler|cat|cod|pollock|burger|cake') ~ 'cod',
      str_detect(Ingredients, 'cod') ~ 'cod fillet',
      str_detect(Ingredients, 'crab') & str_detect(Ingredients, '(?<!de-|de)shell') ~ 'crab shell',
      str_detect(Ingredients, 'crab') & str_detect(Ingredients, 'claw') ~ 'crab claw',
      str_detect(Ingredients, 'crab') ~ 'crab',
      str_detect(Ingredients, '\\btusk\\b|\\bcusk\\b|brosme') ~ 'cusk tusk white fish',

      str_detect(Ingredients, 'flounder') ~ 'flounder',

      str_detect(Ingredients, 'grouper') ~ 'grouper',

      str_detect(Ingredients, 'haddock') & str_detect(Ingredients, 'bread|crisp') ~ 'haddock breaded',
      str_detect(Ingredients, 'haddock') & !str_detect(Ingredients, 'burger|cake') ~ 'haddock',
      str_detect(Ingredients, 'halibut') ~ 'halibut',
      str_detect(Ingredients, 'herring') & str_detect(Ingredients, 'smoked') ~ 'herring smoked',
      str_detect(Ingredients, 'herring') & str_detect(Ingredients, 'pickle') ~ 'herring pickled',
      str_detect(Ingredients, 'herring') ~ 'herring',

      str_detect(Ingredients, 'lobster') & str_detect(Ingredients, '\\bcooked') ~ 'lobster cooked',
      str_detect(Ingredients, 'lobster') & !str_detect(Ingredients, 'shell') ~ 'lobster',

      str_detect(Ingredients, 'mackerel') & str_detect(Ingredients, 'tomato') ~ 'mackerel tomato canned',
      str_detect(Ingredients, 'mackerel') & str_detect(Ingredients, 'smoked') ~ 'mackerel smoked',
      str_detect(Ingredients, 'mackerel') ~ 'mackerel',
      str_detect(Ingredients, 'mussels') & !str_detect(Ingredients, 'power') ~ 'mussels',
      str_detect(Ingredients, 'sand shell') ~ 'scallop', #Similar in nutrition value

      str_detect(Ingredients, 'oyster') & !str_detect(Ingredients, 'sauce|mushroom') ~ 'oyster',

      str_detect(Ingredients, 'pollock') & str_detect(Ingredients, 'smoked') ~ 'pollock smoked',
      str_detect(Ingredients, 'pollock|pollack|chop fillet, without skins and bones|saithe') & !str_detect(Ingredients, 'burger|cake') ~ 'pollock',
      str_detect(Ingredients, 'prawn') ~ 'prawn',

      str_detect(Ingredients, 'redfish') ~ 'redfish',
      str_detect(Ingredients, 'rakfisk|fermented trout') ~ 'rakfisk trout fermented',
      str_detect(Ingredients, 'roe') & str_detect(Ingredients, 'salmon') ~ 'roe salmon',
      str_detect(Ingredients, '\\broe\\b') ~ 'roe',

      str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'spread') ~ 'salmon spread',
      str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'smoked') & !str_detect(Ingredients, 'spread') ~ 'salmon smoked',
      str_detect(Ingredients, 'salmon') & str_detect(Ingredients, 'roe') ~ 'salmon roe',
      str_detect(Ingredients, 'salmon') ~ 'salmon',
      str_detect(Ingredients, 'sandshell') ~ 'sandshell',
      str_detect(Ingredients, 'sardine') ~ 'sardine',
      str_detect(Ingredients, 'scallop') ~ 'scallop',
      str_detect(Ingredients, 'scampi') ~ 'scampi',
      str_detect(Ingredients, 'sea bass') ~ 'sea bass',
      str_detect(Ingredients, 'sea urchin') ~ 'sea urchin',
      str_detect(Ingredients, 'shellfish') & !str_detect(Ingredients, 'broth|stock|salad') ~ 'shellfish',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, '\\bcooked') ~ 'shrimp cooked',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, 'lake') ~ 'shrimp in brine',
      str_detect(Ingredients, 'shrimp') & !str_detect(Ingredients, 'paste|salad|shellfish|tube|dim sum') ~ 'shrimp',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, 'salad') ~ 'shrimp salad',
      str_detect(Ingredients, '\\bsole\\b') ~ 'sole',
      str_detect(Ingredients, 'squid') & !str_detect(Ingredients, 'honey') ~ 'squid',
      str_detect(Ingredients, 'sea') & str_detect(Ingredients, 'urchin') ~ 'sea urchin',

      str_detect(Ingredients, 'trout') & str_detect(Ingredients, 'cured') ~ 'cured trout',
      str_detect(Ingredients, 'trout') & str_detect(Ingredients, 'caviar') ~ 'trout caviar',
      str_detect(Ingredients, 'trout') & str_detect(Ingredients, 'smoke') ~ 'smoked trout',
      str_detect(Ingredients, 'trout') ~ 'trout',
      str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'oil') ~ 'tuna in oil canned',
      str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'water|drained') ~ 'tuna in water canned',
      str_detect(Ingredients, 'tuna') & str_detect(Ingredients, 'can') ~ 'tuna in oil canned', #Default
      str_detect(Ingredients, 'tuna') ~ 'tuna',

      TRUE ~ Ingredients_standardised))
}

