#' Standardising ingredients names in a recipe, here poultry.
#' @title standardisePoultry
#'
#' @description Standardise names of poultry.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with poultry with standardised names.
#'
#' @export
standardisePoultry <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'breast|fillet|filet') &
        str_detect(Ingredients, 'without|skinless|no skin') &
        str_detect(Ingredients, 'cooked|fried') & !str_detect(unit, 'pcs') ~ 'chicken breast without skin cooked',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'breast|fillet|filet') &
        str_detect(Ingredients, 'without|skinless|no skin') ~ 'chicken breast without skin',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'breast|fillet|filet') &
        str_detect(Ingredients, 'cooked|fried') & !str_detect(unit, 'pcs') ~ 'chicken breast cooked',
        str_detect(Ingredients, 'chicken') &
          str_detect(Ingredients, 'breast|fillet|filet') &
          str_detect(Ingredients, 'herb|garlic|pepper|paprika') & !str_detect(unit, 'pcs') ~ 'chicken breast seasoned',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'breast|fillet|filet') & !str_detect(Ingredients, 'thigh|skewer|sandwich|casserole') ~ 'chicken breast',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'thigh|leg') &
        str_detect(Ingredients, 'without|skinless|no skin') ~ 'chicken thigh without skin',
      str_detect(Ingredients, 'chicken') &
        str_detect(Ingredients, 'thigh|leg') &
        str_detect(Ingredients, 'grilled|cooked|crispy') & !str_detect(unit, 'pcs') ~ 'chicken thigh cooked',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'thigh|leg') ~ 'chicken thigh',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'wing') & str_detect(Ingredients, 'grilled') ~ 'chicken wing cooked',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'wing') ~ 'chicken wing',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'grilled|cooked') & !str_detect(unit, 'pcs') ~ 'chicken whole cooked',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'drum|club') ~ 'chicken drumstick',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'skewer') & str_detect(Ingredients, 'satay') ~ 'chicken skewer satay',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'skewer') ~ 'chicken skewer',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'soup') ~ 'chicken soup',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'casserole') ~ 'chicken casserole',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'pate|spread|pâté') ~ 'chicken pate',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'liver') ~ 'chicken liver',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'nugget') ~ 'chicken nugget',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'meatball|meat ball') ~ 'chicken meatball',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'burger') & str_detect(Ingredients, 'cheese') ~ 'chicken burger cheese',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'burger') ~ 'chicken burger',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'sour') ~ 'chicken sweet and sour',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'mince|ground') ~ 'chicken minced meat',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'wiener') ~ 'chicken sausage wiener',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'ham') ~ 'chicken ham',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'sausage') ~ 'chicken sausage grill', #Standard
      str_detect(Ingredients, 'chicken') &
        !str_detect(Ingredients, 'power|condensed|broth|stock|sauce|salad|spice mix|soup|egg|tube|taco|servelat|sausage|wiener|tandoori|pizza|season|with chicken|sandwich|toast|tapast|ball|pastrami|bbq|barbecue|butter') ~ 'chicken whole',

      str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'breast|filet|fillet') ~ 'duck breast',
      str_detect(Ingredients, 'duck') & str_detect(Ingredients, 'leg') ~ 'duck leg',
      str_detect(Ingredients, 'duck') & !str_detect(Ingredients, 'pancake') ~ 'duck whole',

      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'boil|hard cook|hard-cook|hard boiled') & !str_detect(unit, 'pcs') ~ 'egg boiled',
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'noodle') ~ 'egg noodle',
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'white') ~ 'egg white',
      str_detect(Ingredients, 'egg') & str_detect(Ingredients, 'yolk') ~ 'egg yolk',
      str_detect(Ingredients, '\\begg\\b|\\beggs\\b') & !str_detect(Ingredients, 'plant|noodle|vegg') ~ 'egg',

      str_detect(Ingredients, 'grouse') ~ 'hen breast fillet grouse',

      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'ground|dough') ~ 'turkey minced meat',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'breast|fillet') & str_detect(Ingredients, 'smoke') ~ 'turkey breast smoked',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'breast|fillet') ~ 'turkey breast',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'club') ~ 'turkey drumstick chicken', #Add chicken to use to calculate nutrition values
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'ham') ~ 'turkey ham canned',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'sausage') ~ 'turkey sausage',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'grill') ~ 'sausage grill turkey chicken', #Prior turkey chicken grill sausage
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'cooked') ~ 'turkey meat cooked',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'bacon') ~ 'bacon turkey',
      str_detect(Ingredients, 'turkey') & str_detect(Ingredients, 'offal') ~ 'turkey offal',
      str_detect(Ingredients, 'turkey') & !str_detect(Ingredients, 'broth|stock|fund|escalope') ~ 'whole turkey',

      TRUE ~ Ingredients_standardised))
}

