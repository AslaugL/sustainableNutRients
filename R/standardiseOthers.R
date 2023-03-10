#' Standardising ingredients names in a recipe, here various other type of foods.
#' @title standardiseOthers
#'
#' @description Standardise names of various other foods.
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with various other foods with standardised names.
#'
#' @export
standardiseOthers <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'agar') ~ 'agar',
      str_detect(Ingredients, 'agave') & str_detect(Ingredients, 'nectar') ~ 'agave nectar',
      str_detect(Ingredients, 'agave') & str_detect(Ingredients, 'syrup') ~ 'agave syrup',
      str_detect(Ingredients, 'aioli') ~ 'aioli',
      str_detect(Ingredients, 'asafoetida powder') ~ 'asafoetida powder',

      str_detect(Ingredients, 'baking powder') ~ 'baking powder',
      str_detect(Ingredients, 'baking soda|bicarbonate of soda') ~ 'baking soda',
      str_detect(Ingredients, 'barbecue|bbq|barbeque') & str_detect(Ingredients, 'marinade') ~ 'barbecue marinade',
      str_detect(Ingredients, 'barbecue|bbq|barbeque') & str_detect(Ingredients, 'rub') ~ 'barbecue rub',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'fund') ~ 'beef fund',
      str_detect(Ingredients, 'beer') & str_detect(Ingredients, 'dark|amber|christmas') ~ 'beer dark',
      str_detect(Ingredients, 'beer|\\bale\\b') ~ 'beer',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'pickle') ~ 'beetroot pickled',
      Ingredients == 'black truffle or 2 tbsp s truffle oil' ~ 'black truffle',
      str_detect(Ingredients, "blackstrap") & str_detect(Ingredients, "molass") ~ "molasses blackstrap",
      str_detect(Ingredients, 'molass') ~ "molasses blackstrap",
      str_detect(Ingredients, 'brandy') ~ 'spirits 40 vol-% alcohol brandy',
      str_detect(Ingredients, 'brine') & !str_detect(Ingredients, 'shrimp|prawn') ~ 'water brine',
      str_detect(Ingredients, 'beverage') & str_detect(Ingredients, 'carbonated') & str_detect(Ingredients, 'lemon') & str_detect(Ingredients, 'lime') ~ 'carbonated beverage lemon-lime',
      str_detect(Ingredients, 'brown') & str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'mix') | str_detect(Ingredients, 'gravy') & str_detect(Ingredients, 'powder') ~ 'gravy brown mix',


      str_detect(Ingredients, 'candy mix') ~ 'candy mixed',
      str_detect(Ingredients, 'candy') & str_detect(Ingredients, 'jell') ~ 'candy jelly',
      str_detect(Ingredients, 'canned fruit drink') ~ 'canned fruit drink',
      str_detect(Ingredients, 'caper|hijack') ~ 'caper',
      str_detect(Ingredients, 'caramel') & !str_detect(Ingredients, 'sauce|yogurt|yoghurt|skyr') ~ 'caramel',
      str_detect(Ingredients, 'german') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') & str_detect(Ingredients, 'chocolate') ~ 'german chocolate cake mix',
      str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') & str_detect(Ingredients, 'chocolate') ~ 'chocolate cake mix',
      str_detect(Ingredients, 'carrot') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') ~ 'carrot cake mix',
      str_detect(Ingredients, 'instant chocolate pudding mix') ~ 'chocolate pudding mix',
      str_detect(Ingredients, 'brownie') & str_detect(Ingredients, 'mix') ~ 'brownie mix',
      str_detect(Ingredients, 'chili sin carne') ~ 'chili sin carne',
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, 'filter') & str_detect(Ingredients, 'brew') ~ 'coffee filter brew',
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, 'filter') ~ 'coffee filter powder',
      str_detect(Ingredients, 'coffee') & str_detect(Ingredients, 'espresso') & str_detect(Ingredients, 'powder') ~ 'coffee espresso powder',
      (str_detect(Ingredients, 'chocolate milk') & !str_detect(Ingredients, 'candy')) ~ 'milk beverage chocolate',
      str_detect(Ingredients, 'milk chocolate') | str_detect(Ingredients, 'milk heart') ~ 'milk chocolate',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'semi|dark') ~ 'chocolate semi-sweet',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'unsweetened') ~ 'chocolate unsweetened',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'white') ~ 'chocolate white',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'sauce') ~ 'chocolate sauce',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'glaze') & str_detect(Ingredients, 'mix') ~ 'chocolate glaze mix',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'mousse') & str_detect(Ingredients, 'mix|instant|powder') ~ 'chocolate mousse powder mix',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'spread') & str_detect(Ingredients, 'plant-based|plant based|vegan') ~ 'chocolate spread plant-based',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'spread') ~ 'chocolate spread',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'vermicelli') ~ 'chocolate semi-sweet vermicelli',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'candy') & str_detect(Ingredients, 'bar') | str_detect(Ingredients, 'chocolatebar') ~ 'chocolate candy bar',
      str_detect(Ingredients, 'chocolate') & !str_detect(Ingredients, 'syrup|icing|kruidnoten|mousse|sauce|spread|biscuit|marzipan|nut mix') ~ 'chocolate semi-sweet', #Default
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'icing|glaze') ~ 'icing chocolate',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'sprinkles') ~ 'sprinkles chocolate',
      str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mango') ~ 'chutney mango',
      str_detect(Ingredients, 'chutney') & str_detect(Ingredients, 'mint') ~ 'chutney mint',
      str_detect(Ingredients, 'cider') & !str_detect(Ingredients, 'vinegar') ~ 'cider',
      str_detect(Ingredients, 'coca cola') ~ 'coca cola',
      str_detect(Ingredients, 'cocoa') | str_detect(Ingredients, 'cacao') & str_detect(Ingredients, 'powder') ~ 'cocoa powder',
      str_detect(Ingredients, 'cognac') ~ 'spirits 40 vol-% alcohol cognac',
      str_detect(Ingredients, 'color') & str_detect(Ingredients, 'sprinkles') ~ 'sprinkles colored',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'celery') ~ 'condensed cream of celery soup',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'chicken') ~ 'condensed cream of chicken soup',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'mushroom') ~ 'condensed cream of mushroom soup',
      str_detect(Ingredients, 'condensed') & str_detect(Ingredients, 'tomato soup') ~ 'condensed tomato soup',
      str_detect(Ingredients, 'cooking') & str_detect(Ingredients, 'spray') ~ 'cooking spray',
      str_detect(Ingredients, 'crisps') ~ 'crisps',
      str_detect(Ingredients, 'custard') & str_detect(Ingredients, 'hot') ~ 'custard hot',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'custard') ~ 'custard vanilla',

      str_detect(Ingredients, 'dark rum') | Ingredients == 'rum' ~ 'spirits 40 vol-% alcohol dark rum',
      str_detect(Ingredients, 'decorative glaze') ~ 'decorative glaze',
      str_detect(Ingredients, 'dip mix') ~ 'dip mix',
      str_detect(Ingredients, 'dressing') & str_detect(Ingredients, 'garlic') ~ 'dressing garlic',

      str_detect(Ingredients, 'erythriol') ~ 'erythriol',
      str_detect(Ingredients, 'espresso') & str_detect(Ingredients, 'bean') & str_detect(Ingredients, 'ground') ~ 'espresso bean coffee ground',

      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'base') ~ 'fish soup base',
      str_detect(Ingredients, 'fondant') & str_detect(Ingredients, 'white') ~ 'fondant white',
      str_detect(Ingredients, 'food') & str_detect(Ingredients, 'coloring|colouring') ~ 'food coloring',

      str_detect(Ingredients, 'gelatin') & str_detect(Ingredients, 'sheet') ~ 'gelatin sheet',
      str_detect(Ingredients, '\\bgelatin\\b') ~ 'gelatin',
      str_detect(Ingredients, '\\bgel\\b') & str_detect(Ingredients, 'color') ~ 'gel coloring',
      str_detect(Ingredients, '\\bgel\\b') & str_detect(Ingredients, 'top') ~ 'gel tops',
      str_detect(Ingredients, 'guacamole') & !str_detect(Ingredients, 'spice') ~ 'guacamole',

      str_detect(Ingredients, 'harissa') & str_detect(Ingredients, 'mild') ~ 'harissa mild',
      str_detect(Ingredients, 'harissa') ~ 'harissa',
      str_detect(Ingredients, 'herb') & !str_detect(Ingredients, 'basil|thyme|parsley|rosemary|dill') ~ 'herbs',
      str_detect(Ingredients, 'honey') & !str_detect(Ingredients, 'mustard|melon|dew|apple') ~ 'honey',
      str_detect(Ingredients, 'hummus') ~ 'hummus',

      str_detect(Ingredients, 'ice cube') ~ 'ice cube',

      str_detect(Ingredients, 'jam') ~ 'jam',
      str_detect(Ingredients, 'jelly') & str_detect(Ingredients, 'mix|powder|instant') ~ 'jelly instant powder',

      str_detect(Ingredients, 'kirsch') ~ 'spirits 40 vol-% alcohol kirsch',
      str_detect(Ingredients, 'kombu') & str_detect(Ingredients, 'dashi') ~ 'dashi kombu dried kelp',
      str_detect(Ingredients, 'kombucha') & str_detect(Ingredients, 'start') ~ 'kombucha starter',
      str_detect(Ingredients, 'kombucha') ~ 'kombucha',

      str_detect(Ingredients, 'liquorice') & str_detect(Ingredients, 'powder') ~ 'liquorice powder',
      str_detect(Ingredients, 'liquorice') ~ 'liquorice',

      str_detect(Ingredients, "mac") & str_detect(Ingredients, "cheese") & str_detect(Ingredients, "bacon") ~ "mac and cheese bacon",
      str_detect(Ingredients, "mac") & str_detect(Ingredients, "cheese") ~ "mac and cheese",
      str_detect(Ingredients, 'madeira') ~ 'madeira fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'blueberr') ~ 'marmelade blueberry',
      str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'fig') ~ 'marmelade fig',
      str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'orange') ~ 'marmelade orange',
      str_detect(Ingredients, 'marmelade|marmalade') & str_detect(Ingredients, 'pear') ~ 'marmelade pear',
      str_detect(Ingredients, 'marsala') ~ 'marsala fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'marshmallow') & str_detect(Ingredients, 'cream') ~ 'marshmallow cream',
      str_detect(Ingredients, 'marshmallow') ~ 'marshmallow',
      str_detect(Ingredients, 'mire poix') ~ 'mire poix',
      str_detect(Ingredients, 'miso') & str_detect(Ingredients, 'white') & str_detect(Ingredients, 'paste') ~ 'miso paste white',
      str_detect(Ingredients, 'muffin') & str_detect(Ingredients, 'mix|powder|instant') & str_detect(Ingredients, 'chocolate') ~ 'muffin chocolate powder mix',
      str_detect(Ingredients, 'muffin') & str_detect(Ingredients, 'mix|powder|instant') ~ 'muffin powder mix',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'seed') ~ 'mustard seed',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'powder') ~ 'mustard powder',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'whole|grain|coarse') ~ 'mustard whole grain',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'dijon') ~ 'mustard dijon',
      str_detect(Ingredients, 'mustard') & str_detect(Ingredients, 'honey') ~ 'mustard honey',
      str_detect(Ingredients, 'mustard') & !str_detect(Ingredients, 'cheese') ~ 'mustard',

      str_detect(Ingredients, 'marzipan') & str_detect(Ingredients, 'chocolate') ~ 'marzipan chocolate',
      str_detect(Ingredients, 'marzipan') ~ 'marzipan',
      str_detect(Ingredients, 'mayo') & str_detect(Ingredients, 'vegan') ~ 'mayonnaise vegan',
      str_detect(Ingredients, 'mayo') ~ 'mayonnaise',

      str_detect(Ingredients, 'non-stop') ~ 'milk chocolate non-stop',
      str_detect(Ingredients, 'noodle') & str_detect(Ingredients, 'egg') ~ 'egg noodle',
      str_detect(Ingredients, 'noodle') & str_detect(Ingredients, 'glass') ~ 'glass noodle',
      str_detect(Ingredients, 'noodle') ~ 'rice noodle', #Default
      str_detect(Ingredients, 'nori') & str_detect(Ingredients, 'flak|seaweed|sheet|sushi') ~ 'nori seaweed',

      str_detect(Ingredients, 'olive paste tapenade') ~ 'olive paste tapenade',

      #Paste
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'sichuan') & str_detect(Ingredients, 'bean') ~ 'chili bean paste sichuan',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'chili') | str_detect(Ingredients, 'sambal|rose harissa') ~ 'paste chili',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'carrot') ~ 'paste carrot',
      str_detect(Ingredients, 'paste|pasta') & str_detect(Ingredients, 'curry') ~ 'paste curry',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'garlic') ~ 'paste garlic',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'shrimp') ~ 'paste shrimp',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'sun') ~ 'paste tomato sun-dried',
      str_detect(Ingredients, 'paste') & str_detect(Ingredients, 'tomato') ~ 'paste tomato',

      str_detect(Ingredients, 'pavlova') & str_detect(Ingredients, 'mix|instant|powder') ~ 'pavlova powder mix',

      #Pesto
      str_detect(Ingredients, 'pesto') & str_detect(Ingredients, 'vegan') ~ 'pesto vegan',
      str_detect(Ingredients, 'pesto') ~ 'pesto',

      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'white') ~ 'white pepper',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'cayenne') ~ 'cayenne pepper',
      str_detect(Ingredients, 'pepper|Pepper') &
        !str_detect(Ingredients, 'chili|white|sweet|cayenne|spice|bell|salad|sauce|mackerel|pepperoni|ham') & !str_detect(Ingredients, 'salt') ~ 'black pepper',
      str_detect(Ingredients, 'sweet green pickle relish') ~ 'sweet green pickle relish',
      str_detect(Ingredients, 'pickle') & !(str_detect(Ingredients, 'cucumber|ginger|sweet pepper|onion|beet|paprika|jalap')) | str_detect(Ingredients, 'gherkin') ~ 'cucumber pickled', #Standard
      str_detect(Ingredients, 'pizza dressing') ~ 'pizza dressing',
      str_detect(Ingredients, 'pizza filling') ~ 'pizza filling',
      str_detect(Ingredients, 'popcorn') ~ 'popcorn',

      str_detect(Ingredients, 'remoulade') ~ 'remoulade',
      #Ready made meals
      str_detect(Ingredients, 'bean taco') & str_detect(Ingredients, 'ready-made|ready made') ~ 'bean taco',
      str_detect(Ingredients, 'chana masala') & str_detect(Ingredients, 'ready-made|ready made') ~ 'chana masala',
      str_detect(Ingredients, 'sausage stew') & str_detect(Ingredients, 'ready-made|ready made') ~ 'sausage stew',
      str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'ready-made|ready made')  & str_detect(Ingredients, 'vegetarian') ~ 'bolognese vegetarian',
      str_detect(Ingredients, 'beans in chili|chili con carne') & str_detect(Ingredients, 'ready-made|ready made') ~ 'chili con carne',
      str_detect(Ingredients, 'lasagna') & str_detect(Ingredients, 'ready-made|ready made') ~ 'lasagna',
      str_detect(Ingredients, 'lapskaus') & str_detect(Ingredients, 'ready-made|ready made') ~ 'lapsskaus',
      str_detect(Ingredients, 'meat stew') & str_detect(Ingredients, 'ready-made|ready made') ~ 'meat stew',
      str_detect(Ingredients, 'meatball') & str_detect(Ingredients, 'tomato sauce') & str_detect(Ingredients, 'ready-made|ready made') ~ 'meatballs in tomato sauce',
      str_detect(Ingredients, 'mexican') & str_detect(Ingredients, 'bean|lentil') &
        str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'ready-made|ready made') ~ 'mexican lentil bean salad',
      str_detect(Ingredients, 'mediterranean') & str_detect(Ingredients, 'vegan') &
        str_detect(Ingredients, 'salad') & str_detect(Ingredients, 'ready-made|ready made') ~ 'mediterranean vegan salad',
      str_detect(Ingredients, 'italian') & str_detect(Ingredients, 'salad') &
        str_detect(Ingredients, 'ready-made|ready made') ~ 'italian salad',
      str_detect(Ingredients, 'barley salad') ~ 'barley salad',
      str_detect(Ingredients, 'bean salad') ~ 'bean salad',
      str_detect(Ingredients, 'beetroot salad') ~ 'beetroot salad',
      str_detect(Ingredients, 'chicken salad') ~ 'chicken salad',
      str_detect(Ingredients, 'potato salad') ~ 'potato salad',
      str_detect(Ingredients, 'shellfish salad') ~ 'shellfish salad',
      #Toro powders
      str_detect(Ingredients, 'toro') & str_detect(Ingredients, 'greek moussaka') ~ 'greek moussaka powder mix',
      str_detect(Ingredients, 'toro') & str_detect(Ingredients, 'bali stew') ~ 'bali stew powder mix',
      str_detect(Ingredients, 'toro') & str_detect(Ingredients, 'mexican stew') ~ 'mexican stew powder mix',

      str_detect(Ingredients, 'sake') ~ 'sake',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'oil') ~ 'salt and pepper and oil',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'butter') ~ 'salt and pepper and butter',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') ~ 'salt and pepper',
      str_detect(Ingredients, 'salt') & !str_detect(Ingredients, 'peanut|lamb|pork|anchovy|soy|flesk|cashew|butter|almond|bacon|biscuit|cod|pecan|potato|chip|stick|bar') ~ 'salt',
      str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'chunky') ~ 'salsa chunky',
      str_detect(Ingredients, 'salsa') & str_detect(Ingredients, 'tomato') ~ 'salsa tomato',
      str_detect(Ingredients, 'salsa') ~ 'salsa tomato', #Standard
      str_detect(Ingredients, 'sherry') & !str_detect(Ingredients, 'vinegar') ~ 'sherry fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'shortening') & str_detect(Ingredients, 'vegetable') ~ 'shortening vegetable',
      str_detect(Ingredients, 'shortening') ~ 'shortening',
      str_detect(Ingredients, '\\bsmash\\b') ~ 'candy smash',
      str_detect(Ingredients, 'snow bead') ~ 'candy filled chocolate balls',
      (str_detect(Ingredients, 'soda') & str_detect(Ingredients, 'flavor')) |
        str_detect(Ingredients, 'soft drink') ~ 'soft drink',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'brown|castor') ~ 'sugar brown',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'confect|icing|powder') ~ 'sugar confectioners',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'vanilla') ~ 'sugar vanilla',
      str_detect(Ingredients, 'sugar') & !str_detect(Ingredients, 'asparagus|pea|cola') ~ 'sugar',
      str_detect(Ingredients, 'syrup|sirup') & str_detect(Ingredients, 'maple') ~ 'syrup maple',
      str_detect(Ingredients, 'syrup') & str_detect(Ingredients, 'chocolate') ~ 'syrup chocolate',
      str_detect(Ingredients, 'syrup') ~ 'syrup',
      str_detect(Ingredients, 'spring roll paper') ~ 'spring roll paper',
      str_detect(Ingredients, 'sprinkles') & str_detect(Ingredients, 'christmas') ~ 'sprinkles christmas',
      str_detect(Ingredients, 'sprinkles') ~ 'sprinkles',

      #Soup
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'mix|powder|instant') ~ 'soup onion instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'onion') ~ 'onion soup',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'mix|instant') ~ 'soup spinach instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'spinach') ~ 'soup spinach',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'mix|instant') & str_detect(Ingredients, 'mexican') ~ 'soup tomato mexican instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'mexican') ~ 'soup tomato mexican',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'mix|instant') ~ 'soup tomato instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') ~ 'soup tomato',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'cauliflower') & str_detect(Ingredients, 'mix|instant') ~ 'soup cauliflower instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'cauliflower') ~ 'soup cauliflower',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'thai') & str_detect(Ingredients, 'mix|instant') ~ 'soup thai instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'thai') ~ 'soup thai',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'beta') & str_detect(Ingredients, 'mix|instant') ~ 'soup beta instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'beta') ~ 'soup beta',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'mix|instant') ~ 'soup fish instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'fish') ~ 'soup fish',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'mix|instant') ~ 'soup pea instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'pea') ~ 'pea cauliflower',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'mix|instant') ~ 'soup chicken instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'chicken') ~ 'soup chicken',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'carrot') & str_detect(Ingredients, 'mix|instant') ~ 'soup carrot instant',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'carrot') ~ 'soup carrot',

      str_detect(Ingredients, 'tabasco') ~ 'tabasco',
      str_detect(Ingredients, 'tea') & str_detect(Ingredients, 'green') ~ 'tea green',
      str_detect(Ingredients, 'toenjang') ~ 'toenjang soybean paste',
      str_detect(Ingredients, 'tofu') ~ 'tofu',
      str_detect(Ingredients, 'toro jegergryte') ~ 'toro jegegryte',
      str_detect(Ingredients, 'toro moussaka') ~ 'toro moussaka',
      str_detect(Ingredients, 'tzatziki') ~ 'tzatziki',

      str_detect(Ingredients, 'vermouth') ~ 'vermouth fortified wine 15 vol-% alcohol',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'apple|cider') ~ 'vinegar apple cider',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'balsamic') ~ 'vinegar balsamic',
      str_detect(Ingredients, 'balsamic') & str_detect(Ingredients, 'cream') ~ 'balsamic cream',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'red wine') ~ 'vinegar red wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'rice') ~ 'vinegar rice',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'sherry') ~ 'vinegar sherry',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'white wine') ~ 'vinegar white wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ 'vinegar wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'brown') ~ 'vinegar brown',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'clear|light|5%') ~ 'vinegar',
      str_detect(Ingredients, 'vinegar') ~ 'vinegar',
      str_detect(Ingredients, 'vodka') ~ 'spirits 40 vol-% alcohol vodka',

      str_detect(Ingredients, 'water') & !str_detect(Ingredients, 'corn|beef|tuna|coffee|chili|cream|cress|chestnut|melon') ~ 'water',
      str_detect(Ingredients, 'water to the corn') ~ 'water',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'vegetable') ~ 'broth cube vegetable',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'fish') ~ 'broth cube fish',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'beef|meat') ~ 'broth cube beef',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice') & str_detect(Ingredients, 'chicken') ~ 'broth cube chicken',
      str_detect(Ingredients, 'stock|broth|bouillon|borth') & str_detect(Ingredients, 'cube|dice') ~ 'broth cube',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'chicken') ~ 'water broth chicken',
      str_detect(Ingredients, 'stock|broth|bouillon|bouilljon|consomme') & str_detect(Ingredients, 'beef|meat') ~ 'water broth beef',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'shellfish') ~ 'water broth shellfish',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'game|wild') ~ 'water broth game',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'turkey') ~ 'water broth turkey',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'vegetable') ~ 'water broth vegetable',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'lamb') ~ 'water broth lamb',
      str_detect(Ingredients, 'stock|broth|bouillon|power') & str_detect(Ingredients, 'fish') ~ 'water broth fish',
      str_detect(Ingredients, 'stock|broth|bouillon|frying pan|power') ~ 'water broth',
      str_detect(Ingredients, 'wasabi') ~ 'wasabi',
      str_detect(Ingredients, 'whisky|whiskey') ~ 'whisky spirits 40 vol-% alcohol',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'rice') ~ 'wine rice',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'white') & !str_detect(Ingredients, 'vinegar') ~ 'wine white',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'red|merlot') & !str_detect(Ingredients, 'vinegar') ~ 'wine red',
      str_detect(Ingredients, 'wine') & str_detect(Ingredients, 'port') ~ 'wine port fortified wine 20 vol-% alcohol',
      str_detect(Ingredients, 'mirin japanese sweet wine') ~ 'wine mirin',

      str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'dry|dried') ~ 'yeast dry',
      str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'nutritional') ~ 'yeast nutritional',
      str_detect(Ingredients, 'yeast') ~ 'yeast',
      str_detect(Ingredients, 'yellow') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') ~ 'yellow cake mix',

      #Baking ingredients
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'bean') ~ 'vanilla bean',

      TRUE ~ Ingredients_standardised))
}

