#' Standardising ingredients names in a recipe, here various condiments of foods.
#' @title standardiseCondiments
#'
#' @description Standardise names of various condiments
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with various other foods with standardised names.
#'
#' @export
standardiseCondiments <- function(df) {

  df %>%
  #Standardise
  mutate(Ingredients_standardised = case_when(

    # Chutney, salsa etc
    str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mango') ~ 'chutney mango',
    str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mint') ~ 'chutney mint',
    str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'chunky') ~ 'salsa chunky',
    str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'tomato') ~ 'salsa tomato',
    str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'mango|pineapple|papaya|lime') ~ 'salsa fruit',
    str_detect(Ingredients, 'salsa') & !str_detect(Ingredients, 'mackerel') ~ 'salsa tomato', #Standard
    str_detect(Ingredients, 'harissa') & str_detect(Ingredients, 'mild') ~ 'harissa mild',
    str_detect(Ingredients, 'harissa') ~ 'harissa',

    # Dressings and dips
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'avocado') & str_detect(Ingredients, 'lime') ~ 'dressing avocado and lime',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'burger') & str_detect(Ingredients, 'plant-based|vegan') ~ 'dressing hamburger plant-based',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'burger') ~ 'dressing hamburger',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'french') ~ 'dressing french',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'caesar') ~ 'dressing caesar',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'kebab') ~ 'dressing kebab',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'blue cheese') ~ 'dressing blue cheese',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'sour cream') ~ 'dressing sour cream',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'garlic') ~ 'dressing garlic',
    str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'pizza') ~ 'dressing pizza',
    str_detect(Ingredients, 'dressing') & !str_detect(Ingredients, 'with dressing') ~ 'dressing',
    str_detect(Ingredients, 'dip mix') ~ 'dip mix',
    str_detect(Ingredients, 'guacamole') & !str_detect(Ingredients, 'spice') ~ 'guacamole',

    # Jams, jelly and marmalade
    str_detect(Ingredients, '\\bapple') & str_detect(Ingredients, 'jam') ~ 'jam apple',
    str_detect(Ingredients, '\\bapple') & str_detect(Ingredients, 'jelly') ~ 'jelly apple',
    str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'jam') ~ 'jam apricot',
    str_detect(Ingredients, 'jam') & str_detect(Ingredients, 'powder') ~ 'jam powder',
    str_detect(Ingredients, 'jam') ~ 'jam',
    str_detect(Ingredients, 'jelly') & str_detect(Ingredients, 'mix|powder|instant') ~ 'jelly instant powder',
    str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'blueberr') ~ 'marmelade blueberry',
    str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'fig') ~ 'marmelade fig',
    str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'orange') ~ 'marmelade orange',
    str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'pear') ~ 'marmelade pear',

    # Mustard
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'herring') ~ 'herring mustard',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'seed') ~ 'mustard seed',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'powder') ~ 'mustard powder',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'whole|grain|coarse') ~ 'mustard whole grain',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'dijon') ~ 'mustard dijon',
    str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'honey') ~ 'mustard honey',
    str_detect(Ingredients, 'mustard') & !str_detect(Ingredients, 'chees|saucee') ~ 'mustard',

    # Pastes
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'sichuan') & str_detect(Ingredients, 'bean') ~ 'chili bean paste sichuan',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'chili') | str_detect(Ingredients, 'sambal|rose harissa') ~ 'paste chili',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'carrot') ~ 'paste carrot',
    str_detect(Ingredients, 'paste|pasta') & str_detect(Ingredients, 'curry') ~ 'paste curry',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'garlic') ~ 'paste garlic',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'shrimp') ~ 'paste shrimp',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'sun') ~ 'paste tomato sun-dried',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tikka masala') ~ 'paste tikka masala',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'korma') ~ 'paste korma',
    str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tandoori') ~ 'paste tandoori',

    # Pesto
    str_detect(Ingredients, 'pesto') & str_detect(Ingredients, 'vegan') ~ 'pesto vegan',
    str_detect(Ingredients, 'pesto') ~ 'pesto',

    #Bases/powders
    str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'base|bag') ~ 'bolognese base',
    str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'mix|powder|pack') ~ 'bolognese powder mix',
    str_detect(Ingredients, 'bearnaise|b\u00E9arnaise') & str_detect(Ingredients, 'base') ~ 'bearnaise base',
    str_detect(Ingredients, 'bearnaise|b\u00E9arnaise') & str_detect(Ingredients, 'mix|powder|pack') ~ 'bearnaise powder mix',
    str_detect(Ingredients, 'brown sauce') & str_detect(Ingredients, 'mix|powder|pack') ~ 'brown sauce powder mix',
    str_detect(Ingredients, 'browning') & str_detect(Ingredients, 'sauce') ~ 'sauce browning',
    str_detect(Ingredients, 'brown') & str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'mix') |
      str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'powder') ~ 'gravy brown mix',

    str_detect(Ingredients, 'carbonara') & str_detect(Ingredients, 'sauce') &
      str_detect(Ingredients, 'mix|powder|pack') ~ 'carbonara powder mix',
    str_detect(Ingredients, 'carbonara') & str_detect(Ingredients, 'sauce') ~ 'carbonara sauce',
    str_detect(Ingredients, 'cream sauce') & str_detect(Ingredients, 'base') ~ 'cream sauce base',

    str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'base') ~ 'hollandaise base',
    str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'mix|powder|pack') ~ 'hollandaise powder mix',
    str_detect(Ingredients, 'hollandaise') & str_detect(Ingredients, 'sauce') & !str_detect(Ingredients, 'with hollandaise') ~ 'sauce hollandaise',

    str_detect(Ingredients, 'pepper sauce') & str_detect(Ingredients, 'mix|powder|pack') ~ 'pepper sauce powder mix',

    #Sauces/gravy
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'apricot') & str_detect(Ingredients, 'coriander') ~ 'sauce apricot and coriander',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'barbeque|barbecue|bbq') ~ 'sauce barbeque',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'bearnaise|b\u00E9arnaise') |
      str_detect(Ingredients, 'bernaise|b\u00E9arnaise') ~ 'sauce bearnaise',
    str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'beef') ~ 'beef gravy',
    str_detect(Ingredients, 'gravy') ~ 'beef gravy', #Use as default
    str_detect(Ingredients, 'brown sauce') ~ 'brown sauce',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'cheese') ~ 'sauce cheese',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'brittany') ~ 'sauce chicken brittany',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'butter') & str_detect(Ingredients, 'chicken') ~ 'sauce butterchicken',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chicken') ~ 'sauce chicken',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sweet') ~ 'sauce sweet chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'hot') |
      str_detect(Ingredients, 'sriracha sauce') ~ 'sauce hot chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'sour') ~ 'sauce sour chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'salt') ~ 'sauce salt chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'mild') ~ 'sauce mild chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & str_detect(Ingredients, 'garlic') ~ 'sauce chili garlic',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'chili') & !str_detect(Ingredients, "sausage|pasta") ~ 'sauce chili',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'cranberr') ~ 'sauce cranberry',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'cream') ~ 'sauce cream',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'curry') & str_detect(Ingredients, 'mango') ~ 'sauce curry mango',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'curry') ~ 'sauce curry',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'fish|fisk') ~ 'sauce fish',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'ginger') ~ 'sauce ginger',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hoisin') ~ 'sauce hoisin',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'horseradish') ~ 'sauce horseradish',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot pepper') ~ 'sauce hot pepper',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'hot') ~ 'sauce hot',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, '\\bhp') ~ 'sauce hp',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'korma') ~ 'sauce korma',

    str_detect(Ingredients, 'meatball') & str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'moroccan') &
      !str_detect(Ingredients, '\\bin\\b') ~ 'sauce meatball moroccan',
    str_detect(Ingredients, 'meatball') & str_detect(Ingredients, 'sauce') &
      !str_detect(Ingredients, '\\bin\\b') ~ 'sauce meatball',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'mint') ~ 'sauce mint',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'mushroom') ~ 'sauce mushroom',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'mustard') ~ 'sauce mustard',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pad thai') ~ 'sauce pad thai',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pasta|spagetti|spaghetti') ~ 'sauce pasta',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'piri-piri') ~ 'sauce piri-piri',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'white') ~ 'sauce pizza white',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'red') |
      str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'pizza') ~ 'sauce pizza red', #Standard
    str_detect(Ingredients, 'ponzu') & str_detect(Ingredients, 'sauce') ~ 'sauce ponzu',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'ravigotte') ~ 'sauce ravigotte',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'red wine') ~ 'sauce red wine',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'sandefjord') ~ 'sauce sandefjord',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'satay') ~ 'sauce satay',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'sherry') ~ 'sauce sherry',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'soy') & str_detect(Ingredients, 'sweet') |
      str_detect(Ingredients, 'ketjap medja|ketjap manis') ~ 'sauce sweet soy',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'soy') ~ 'sauce soy',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'sweet') & str_detect(Ingredients, 'sour') ~ 'sauce sweet and sour',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'taco') ~ 'sauce taco',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'tandoori') ~ 'sauce tandoori',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'teriyaki') ~ 'sauce teriyaki',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'tikka masala') ~ 'sauce tikka masala',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'tomat') & !str_detect(Ingredients, 'stew|\\bin\\b') ~ 'sauce tomato',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'oyster') ~ 'sauce oyster',

    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'white') &
      str_detect(Ingredients, 'powder|mix') ~ 'sauce white powder mix',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'white') ~ 'sauce white',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'wok') ~ 'sauce wok',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'worcestershire') ~ 'sauce worcestershire',

    # Sweet sauces
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'caramel') ~ 'sauce caramel',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'raspberr') ~ 'sauce raspberries',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'strawberr') ~ 'sauce strawberries',
    str_detect(Ingredients, 'sauce') &
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'unsweet|no added sugar') ~ 'sauce vanilla no added sugar',
    str_detect(Ingredients, 'sauce') & str_detect(Ingredients, 'vanilla') ~ 'sauce vanilla',

    # Others
    str_detect(Ingredients, 'aioli') & str_detect(Ingredients, 'vegan|plant-based') ~ 'aioli plant-based',
    str_detect(Ingredients, 'aioli') & !str_detect(Ingredients, 'in aioli') ~ 'aioli',
    str_detect(Ingredients, 'mayo') & str_detect(Ingredients, 'vegan') ~ 'mayonnaise vegan',
    str_detect(Ingredients, 'mayo') & !str_detect(Ingredients, 'prawn') ~ 'mayonnaise',
    str_detect(Ingredients, 'tabasco') ~ 'tabasco',
    str_detect(Ingredients, 'tzatziki') ~ 'tzatziki',
    str_detect(Ingredients, 'wasabi') & !str_detect(Ingredients, 'nut') ~ 'wasabi',
    str_detect(Ingredients, 'hummus') ~ 'hummus',
    str_detect(Ingredients, 'remoulade|remulade') ~ 'remulade',
    str_detect(Ingredients, 'sweet green pickle relish') ~ 'sweet green pickle relish',
    str_detect(Ingredients, 'olive paste tapenade') ~ 'olive paste tapenade',
    str_detect(Ingredients, 'miso') & str_detect(Ingredients, 'white') & str_detect(Ingredients, 'paste') ~ 'miso paste white',

    TRUE ~ Ingredients_standardised))

}
