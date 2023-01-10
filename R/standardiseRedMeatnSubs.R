#' Standardising ingredients names in a recipe, here red meat.
#' @title standardiseRedMeatnSubs
#'
#' @description Standardise names of red meat and meat substitutions
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with red meat with standardised names.
#'
#' @export
standardiseRedMeatnSubs <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') & str_detect(Ingredients, 'cooked') ~ 'bacon cooked',
      str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') & str_detect(Ingredients, 'fat') ~ 'bacon fat',
      str_detect(Ingredients, 'bacon|lettsaltet sideflesk|lardons') & !str_detect(Ingredients, 'cheese|spread') ~ 'bacon',
      str_detect(Ingredients, '\\blard') ~ 'lard pork fat',
      str_detect(Ingredients, 'bankekj\u00F8tt|beef round roast|bottom round roast|knocked meat') | str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'round steak') ~ 'beef bottom round',
      str_detect(Ingredients, 'roast beef') ~ 'beef bottom round roast beef',
      str_detect(Ingredients, 'beef|angus') & str_detect(Ingredients, 'sirloin|tip') ~ 'beef sirloin', #Tips often come from the sirloin
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'comb|entre') |
        str_detect(Ingredients, 'entrecote|cote de boef|scotch or black welsh beef|standing rib roast, bone in|rib-eye steak|rib eye steak|ribeye steak') ~ 'beef rib-eye steak', #Also used for the steak in Steak with potato salad
      str_detect(Ingredients, 'beef|cattle') & str_detect(Ingredients, 'tenderloin|fillet') & !str_detect(Ingredients, 'outer') ~ 'beef tenderloin',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'rump') | str_detect(Ingredients, 'top round steak') ~ 'beef roast of knuckle',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shank') & !str_detect(Ingredients, 'calf') ~ 'beef shank',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'high back|high-roast|stew meat|pureed') | str_detect(Ingredients, 'chuck steak|cubed beef') | Ingredients == 'of beef' ~ 'beef chuck roll',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shoulder') | str_detect(Ingredients, 'shin of beef|beef leg') ~ 'beef shoulder', #Shoulder and shin are similar
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'brisket|short rib') ~ 'beef brisket',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'tongue') & str_detect(Ingredients, 'cooked|boiled') ~ 'beef tongue cooked',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'tongue') ~ 'beef tongue',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'bone') & !str_detect(Ingredients, 'steak') ~ 'beef bones',
      str_detect(Ingredients, 'ox') & str_detect(Ingredients, 'tail') ~ 'beef oxtail',
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'tail') ~ 'beef calf oxtail',
      str_detect(Ingredients, 'hanger steak|flank steak') ~ 'beef flank steak', #Cut from the same are of the animal
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'flat') & !str_detect(Ingredients, 'elk|deer') ~ 'beef topside', #Also some veal meat
      str_detect(Ingredients, 'beef|angus|cattle') & str_detect(Ingredients, 'outer') ~ 'beef striploin',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince') & str_detect(Ingredients, 'lean') ~ 'beef minced meat 6 %',str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'ground|mince|all-beef hot dog|all-beef patty') |
      str_detect(Ingredients, 'ground|mince') & str_detect(Ingredients, 'meat') & !str_detect(Ingredients, 'pork|turkey|deer|tofu|chicken|lamb|plant-based|plant based|vegan|vegetarian') |
        str_detect(Ingredients, 'meat dough|chop dough') ~ 'beef minced meat', #Standard
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'shredded|steak strip|sirloin butt') ~ 'beef sirloin butt', #Also used in Beef quesedillas and Steak Strips On Spaghetti With Parsley Pesto
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'liver') ~ 'beef calf liver',
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'leg') ~ 'beef calf shoulder', #Actually hind leg but not in database,
      str_detect(Ingredients, 'calf') & str_detect(Ingredients, 'steak') ~ 'beef veal for roast',
      str_detect(Ingredients, 'beef|cattle') & !str_detect(Ingredients, 'fund|broth|stock|bouilljon|bouillion|bouillon|consomme|gravy|tomato') ~ 'beef chuck roll',
      str_detect(Ingredients, 'bone marrow') ~ 'marrow bone',

      str_detect(Ingredients, 'deer') & str_detect(Ingredients, 'ground') & !str_detect(Ingredients, 'rein|rain') ~ 'roe deer minced meat',

      str_detect(Ingredients, 'elk') & str_detect(Ingredients, 'flat') ~ 'elk moose inside round',
      str_detect(Ingredients, 'elk') & str_detect(Ingredients, 'thigh') ~ 'elk shoulder',

      str_detect(Ingredients, 'game meat with bone') ~ 'game beef elk shoulder',
      str_detect(Ingredients, 'wild meat with bones') ~ 'game beef venison shoulder',

      str_detect(Ingredients, 'ham') & str_detect(Ingredients, 'cured') | str_detect(Ingredients, 'pancetta|prosciutto') ~ 'ham cured',
      str_detect(Ingredients, 'ham') & str_detect(Ingredients, 'smoked') ~ 'ham smoked',
      str_detect(Ingredients, 'ham') & !str_detect(Ingredients, 'bacon|tenderloin|hamburger|champignon|pork from|turkey|steak|bone|cheese|graham') ~ 'ham',
      str_detect(Ingredients, 'hamburger') & str_detect(Ingredients, 'vegetarian|vegan') ~ 'hamburger plant-based',
      str_detect(Ingredients, 'hamburger') & !str_detect(Ingredients, 'bun') ~ 'hamburger beef patty',

      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'shoulder|in slice|neck|with bone') ~ 'lamb shoulder', #Shoulder and neck meat can be interchanged
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'breast and skirt') ~ 'lamb breast skirt',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'ground|mince') ~ 'lamb minced meat',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'stick meat|cured rib') ~ 'lamb cured rib',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') & str_detect(Ingredients, 'smoke') ~ 'lamb leg smoked',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') & str_detect(Ingredients, 'cured') ~ 'lamb leg cured',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'leg|thigh') ~ 'lamb leg roast',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'stew|pot') ~ 'lamb stew meat',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'shank') ~ 'lamb shank',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'carree') ~ 'lamb hind saddle',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'chop|rib') & !str_detect(Ingredients, 'cure') ~ 'lamb chop',
      str_detect(Ingredients, 'lamb') & str_detect(Ingredients, 'cooked') ~ 'lamb shoulder cooked', #Default
      str_detect(Ingredients, 'lamb') & !str_detect(Ingredients, 'power|broth|bouillon|stock') ~ 'lamb shoulder', #Default
      str_detect(Ingredients, 'sheep cabbage meat|mutton cabbage meat') ~ 'lamb sheep cabbage stew meat',
      str_detect(Ingredients, 'sheep head') ~ 'lamb sheep head',
      str_detect(Ingredients, 'liver') & str_detect(Ingredients, 'pate|p\u00E2t\u00E9|paste|spread') ~ 'liver pate',

      str_detect(Ingredients, 'meat') & str_detect(Ingredients, 'ball') &str_detect(Ingredients, 'plant-based|plant based|vegetarian') ~ 'meatball plant-based',
      str_detect(Ingredients, 'meat') & str_detect(Ingredients, 'ball') ~ 'meatball',

      str_detect(Ingredients, 'nugget') & str_detect(Ingredients, 'plant|vegan') ~ 'nugget plant-based',

      str_detect(Ingredients, 'pepperoni') ~ 'sausage pepperoni',
      str_detect(Ingredients, 'plant-based|plant based|vegan|vegetarian') &str_detect(Ingredients, 'mince|ground') ~ 'plant based minced meat',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'butt') ~ 'pork shoulder',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ground|mince') & str_detect(Ingredients, 'medister|high fat|25') ~ 'pork minced meat 25%',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ground|mince') ~ 'pork minced meat',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'patty|patties') & str_detect(Ingredients, 'medister') ~ 'pork patty medister',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'medister') ~ 'pork sausage medister',
      str_detect(Ingredients, 'neck') & str_detect(Ingredients, 'chop') ~ 'pork neck chop',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'chop') ~ 'pork chop',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'neck') ~ 'pork neck',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'tenderloin|fillet|escalope') | Ingredients == 'thinly sliced pork loin' ~ 'pork tenderloin',
      Ingredients == 'cold roast pork loin' ~ 'pork tenderloin cooked',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'shred') | Ingredients == 'pork steak, cut into strips' ~ 'pork inside round', #What is used to create shredded pork in Norway, also why it says inside round in Satay of pigs with honey and ginger
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'hock') ~ 'pork hock',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'stew|shoulder') ~ 'pork shoulder',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'pulled') ~ 'pork shoulder cooked',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'salt') ~ 'pork shoulder salt',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'kidney') ~ 'pork kidney',
      str_detect(Ingredients, 'pig') & str_detect(Ingredients, 'liver') ~ 'pork liver',
      str_detect(Ingredients, 'pork belly') ~ 'pork belly',
      str_detect(Ingredients, 'pork rib roast') ~ 'pork rib roast',
      str_detect(Ingredients, 'pork') & str_detect(Ingredients, 'ham') | str_detect(Ingredients, 'ham steak') ~ 'pork ham roast',
      str_detect(Ingredients, 'bone-in ham') ~ 'pork ham roast bone in',
      str_detect(Ingredients, 'spare rib|baby back rib|sparerib') & !str_detect(Ingredients, 'beef') ~ 'pork spare rib',
      str_detect(Ingredients, 'pork') & !str_detect(Ingredients, 'sausage|bratwurst') ~ 'pork shoulder', #Default

      str_detect(Ingredients, 'rabbit') ~ 'rabbit',
      str_detect(Ingredients, 'rib roll') & str_detect(Ingredients, 'beef') ~ 'rib roll beef',
      str_detect(Ingredients, 'rib roll') & str_detect(Ingredients, 'lamb') ~ 'rib roll lamb',
      str_detect(Ingredients, 'rib roll') & str_detect(Ingredients, 'pork') |
        str_detect(Ingredients, 'rib roll') ~ 'rib roll pork', #default
      str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'fillet') ~ 'reindeer tenderloin',
      str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'pot meat') ~ 'reindeer chuck roll',
      str_detect(Ingredients, 'reindeer') & str_detect(Ingredients, 'flat') ~ 'reindeer inside round',
      str_detect(Ingredients, 'reindeer') ~ 'reindeer',

      str_detect(Ingredients, 'salami') & !str_detect(Ingredients, 'fennel') ~ 'salami',
      str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'chorizo') | str_detect(Ingredients, 'chorizo') ~ 'sausage chorizo',
      str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'vossa') ~ 'sausage vossa',
      str_detect(Ingredients, 'sausage') & str_detect(Ingredients, 'chipolata') ~ 'sausage chipolata',
      str_detect(Ingredients, 'sausage|bratwurst') & str_detect(Ingredients, 'pork') ~ 'sausage pork',
      str_detect(Ingredients, 'sausage') & !str_detect(Ingredients, 'mustard|sauce|bread') ~ 'sausage',

      str_detect(Ingredients, 'vegetable') & str_detect(Ingredients, 'pate|paste|spread') ~ 'vegetable spread',

      TRUE ~ Ingredients_standardised))
}

