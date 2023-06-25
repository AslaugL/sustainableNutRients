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

      #Baking mixes
      str_detect(Ingredients, 'german') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') & str_detect(Ingredients, 'chocolate') ~ 'german chocolate cake mix',
      str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') & str_detect(Ingredients, 'chocolate') ~ 'chocolate cake mix',
      str_detect(Ingredients, 'carrot') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') ~ 'carrot cake mix',
      str_detect(Ingredients, 'instant chocolate pudding mix') ~ 'chocolate pudding mix',
      str_detect(Ingredients, 'brownie') & str_detect(Ingredients, 'mix') ~ 'brownie mix',
      str_detect(Ingredients, 'pavlova') & str_detect(Ingredients, 'mix|instant|powder') ~ 'pavlova powder mix',
      str_detect(Ingredients, 'muffin') & str_detect(Ingredients, 'mix|powder|instant') & str_detect(Ingredients, 'chocolate') ~ 'muffin powder mix chocolate',
      str_detect(Ingredients, 'muffin') & str_detect(Ingredients, 'mix|powder|instant') ~ 'muffin powder mix',
      str_detect(Ingredients, 'yellow') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') ~ 'yellow cake mix',
      str_detect(Ingredients, 'apple') & str_detect(Ingredients, 'cake') & str_detect(Ingredients, 'mix') ~ 'apple cake mix',

      str_detect(Ingredients, 'cheesecake') & !str_detect(Ingredients, 'yoghurt|yogurt') ~ 'cheesecake',
      str_detect(Ingredients, 'chili sin carne') ~ 'chili sin carne',

      #Candy and chocolate products
      str_detect(Ingredients, 'candy mix') ~ 'candy mixed',
      str_detect(Ingredients, 'candy') & str_detect(Ingredients, 'jell') ~ 'candy jelly',
      str_detect(Ingredients, 'caramel') & !str_detect(Ingredients, 'sauce|yogurt|yoghurt|skyr|onion|coffee|\\btea\\b|\\blatte\\b|milk|litago') ~ 'caramel',
      str_detect(Ingredients, '\\bsmash\\b') ~ 'candy smash',
      str_detect(Ingredients, 'snow bead') ~ 'candy filled chocolate balls',

      (str_detect(Ingredients, 'chocolate milk|litago chocolate|milk low fat chocolate|litago milk chocolate') & !str_detect(Ingredients, 'candy')) ~ 'milk beverage chocolate',
      str_detect(Ingredients, 'milk chocolate') & !str_detect(Ingredients, 'cheese|philadelphia') |
        str_detect(Ingredients, 'milk heart') | (str_detect(Ingredients, 'cooking chocolate') & str_detect(Ingredients, 'light')) ~ 'milk chocolate',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'fruit') | Ingredients == 'fruit nut' ~ 'milk chocolate fruit',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'semi|dark') & !str_detect(Ingredients, 'with dark chocolate|with semi-dark chocolate|chocolate flavor') ~ 'chocolate semi-sweet',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'unsweetened') ~ 'chocolate unsweetened',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'white') ~ 'chocolate white',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'sauce') ~ 'chocolate sauce',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'muffin') ~ 'muffin chocolate',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'glaze') & str_detect(Ingredients, 'mix') ~ 'chocolate glaze mix',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'mousse') & str_detect(Ingredients, 'mix|instant|powder') ~ 'chocolate mousse powder mix',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'spread') & str_detect(Ingredients, 'plant-based|plant based|vegan') ~ 'chocolate spread plant-based',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'spread') ~ 'chocolate spread',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'vermicelli') ~ 'chocolate semi-sweet vermicelli',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'candy') & str_detect(Ingredients, 'bar') | str_detect(Ingredients, 'chocolatebar') ~ 'chocolate candy bar',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'icing|glaze') ~ 'icing chocolate',
      str_detect(Ingredients, 'chocolate') & str_detect(Ingredients, 'sprinkle') ~ 'sprinkles chocolate',
      str_detect(Ingredients, 'cocoa') & str_detect(Ingredients, 'butter') ~ 'cocoa butter',
      (str_detect(Ingredients, 'cocoa|chocolate powder') | str_detect(Ingredients, 'cacao') & str_detect(Ingredients, 'powder'))
      & !str_detect(Ingredients, 'muesli|müsli|granola') ~ 'cocoa powder',
      str_detect(Ingredients, 'chocolate') &
        !str_detect(Ingredients,
                    'syrup|icing|kruidnoten|mousse|sauce|spread|biscuit|marzipan|nut mix|drink|bun|with chocolate|and chocolate|& chocolate|brownie|yoghurt|yogurt|milkshake|philadelphia|cheese') ~ 'chocolate semi-sweet', #Default
      str_detect(Ingredients, 'coconut') & str_detect(Ingredients, 'bun') ~ 'bun coconut',
      str_detect(Ingredients, 'non-stop') ~ 'milk chocolate non-stop',
      str_detect(Ingredients, 'tenacious men') ~ 'jelly candy seigmenn',
      str_detect(Ingredients, '\\bgel\\b') & str_detect(Ingredients, 'top') ~ 'jelly candy gel tops',

      str_detect(Ingredients, 'liquorice|licorice') & str_detect(Ingredients, 'powder') ~ 'liquorice powder',
      str_detect(Ingredients, 'liquorice|licorice') ~ 'liquorice',
      str_detect(Ingredients, 'marzipan') & str_detect(Ingredients, 'chocolate') ~ 'marzipan chocolate',
      str_detect(Ingredients, 'marzipan') ~ 'marzipan',
      str_detect(Ingredients, 'marshmallow') & str_detect(Ingredients, 'cream') ~ 'marshmallow cream',
      str_detect(Ingredients, 'marshmallow') ~ 'marshmallow',
      str_detect(Ingredients, 'popcorn') ~ 'popcorn',

      #Ready made meals
      str_detect(Ingredients, 'bean taco') & str_detect(Ingredients, 'ready-made|ready made|done') ~ 'bean taco',
      str_detect(Ingredients, 'chana masala') & str_detect(Ingredients, 'ready-made|ready made|done') ~ 'chana masala',
      str_detect(Ingredients, 'sausage stew') & str_detect(Ingredients, 'ready-made|ready made|done') ~ 'sausage stew',
      str_detect(Ingredients, 'bolognese') & str_detect(Ingredients, 'ready-made|ready made|done')  & str_detect(Ingredients, 'vegetarian') ~ 'bolognese vegetarian',
      str_detect(Ingredients, 'beans in chili|chili con carne') & str_detect(Ingredients, 'ready-made|ready made|done') ~ 'chili con carne',
      str_detect(Ingredients, 'lasagna') & str_detect(Ingredients, 'ready-made|ready made') ~ 'lasagna',
      str_detect(Ingredients, 'lapskaus') & str_detect(Ingredients, 'ready-made|ready made') ~ 'lapsskaus',
      str_detect(Ingredients, 'meat stew') & str_detect(Ingredients, 'ready-made|ready made') ~ 'meat stew',
      str_detect(Ingredients, 'meatball') & str_detect(Ingredients, 'tomato sauce') ~ 'meatballs in tomato sauce',
      str_detect(Ingredients, 'mexican') & str_detect(Ingredients, 'bean|lentil') &
        str_detect(Ingredients, 'salad') ~ 'mexican lentil bean salad',
      str_detect(Ingredients, 'mediterranean') & str_detect(Ingredients, 'vegan') &
        str_detect(Ingredients, 'salad') ~ 'mediterranean vegan salad',
      str_detect(Ingredients, 'italian') & str_detect(Ingredients, 'salad') &
        str_detect(Ingredients, 'ready-made|ready made') ~ 'italian salad',
      str_detect(Ingredients, 'barley salad') ~ 'barley salad',
      str_detect(Ingredients, 'bean salad') ~ 'bean salad',
      str_detect(Ingredients, 'beetroot salad') ~ 'beetroot salad',
      str_detect(Ingredients, 'chicken salad') & str_detect(Ingredients, 'curry') ~ 'chicken salad curry',
      str_detect(Ingredients, 'chicken salad') & str_detect(Ingredients, 'creole') ~ 'chicken salad creole',
      str_detect(Ingredients, 'chicken salad') ~ 'chicken salad',
      str_detect(Ingredients, 'potato salad') ~ 'potato salad',
      str_detect(Ingredients, 'shellfish salad') ~ 'shellfish salad',
      str_detect(Ingredients, 'crab\\b salad') ~ 'crab salad',
      str_detect(Ingredients, 'prawn salad') ~ 'prawn salad',
      str_detect(Ingredients, 'shrimp') & str_detect(Ingredients, 'lobster') & str_detect(Ingredients, 'salad') ~ 'shrimp lobster salad',
      str_detect(Ingredients, 'egg salad') ~ 'egg salad',
      str_detect(Ingredients, 'waldorf salad') ~ 'waldorf salad',
      str_detect(Ingredients, 'crabstick salad') ~ 'crabstick salad',
      str_detect(Ingredients, 'eggplant salad') ~ 'eggplant salad',
      str_detect(Ingredients, "mac") & str_detect(Ingredients, "cheese") & str_detect(Ingredients, "bacon") ~ "mac and cheese bacon",
      str_detect(Ingredients, "mac") & str_detect(Ingredients, "cheese") ~ "mac and cheese",
      #Toro powders
      str_detect(Ingredients, 'toro') & str_detect(Ingredients, 'greek moussaka') ~ 'greek moussaka powder mix',
      str_detect(Ingredients, 'toro') & str_detect(Ingredients, 'bali stew') ~ 'bali stew powder mix',
      str_detect(Ingredients, 'toro') & str_detect(Ingredients, 'mexican stew') ~ 'mexican stew powder mix',

      #Soups and stews
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'celery') ~ 'condensed cream of celery soup',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'chicken') ~ 'condensed cream of chicken soup',
      str_detect(Ingredients, 'condensed cream') & str_detect(Ingredients, 'mushroom') ~ 'condensed cream of mushroom soup',
      str_detect(Ingredients, 'condensed') & str_detect(Ingredients, 'tomato soup') ~ 'condensed tomato soup',

      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'onion') & str_detect(Ingredients, 'mix|powder|instant') ~ 'soup instant onion',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'onion') ~ 'onion soup',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'spinach') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant spinach',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'spinach') ~ 'soup spinach',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'mix|instant') & str_detect(Ingredients, 'mexican') ~ 'soup instant tomato mexican',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'mexican') ~ 'soup tomato mexican',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant tomato',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'tomato') ~ 'soup tomato',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'cauliflower') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant cauliflower',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'cauliflower') ~ 'soup cauliflower',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'thai') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant thai',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'thai') ~ 'soup thai',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'beta') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant beta',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'beta') ~ 'soup beta',
      str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'base') ~ 'fish soup base',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'fish') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant fish',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'fish') ~ 'soup fish',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'pea') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant pea',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'pea') & !str_detect(Ingredients, 'peanut') ~ 'pea soup',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant chicken',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'chicken') ~ 'soup chicken',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'carrot') & str_detect(Ingredients, 'mix|instant') ~ 'soup instant carrot',
      str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'carrot') ~ 'soup carrot',
      str_detect(Ingredients, 'stew') & str_detect(Ingredients, 'cabbage') ~ 'stew cabbage',
      str_detect(Ingredients, 'stew') & str_detect(Ingredients, 'pea') ~ 'stew peas',

      str_detect(Ingredients, 'toenjang') ~ 'toenjang soybean paste',
      str_detect(Ingredients, 'tofu') & !str_detect(Ingredients, 'bao bun|spring roll') ~ 'tofu',
      str_detect(Ingredients, 'toro jegergryte') ~ 'toro jegegryte',
      str_detect(Ingredients, 'toro moussaka') ~ 'toro moussaka',

      # Vinegars
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'apple|cider') ~ 'vinegar apple cider',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'balsamic') ~ 'vinegar balsamic',
      str_detect(Ingredients, 'balsamic') & str_detect(Ingredients, 'cream') ~ 'balsamic cream',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'raspberr') ~ 'vinegar raspberries',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'red wine') ~ 'vinegar red wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'rice') ~ 'vinegar rice',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'sherry') ~ 'vinegar sherry',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'white wine') ~ 'vinegar white wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ 'vinegar wine',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'brown') ~ 'vinegar brown',
      str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'clear|light|5%') ~ 'vinegar',
      str_detect(Ingredients, 'vinegar') ~ 'vinegar',

      #Broth, stock, fund
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'fund|fond') ~ 'chicken fund',
      str_detect(Ingredients, 'beef') & str_detect(Ingredients, 'fund|fond') ~ 'beef fund',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice|powder') & str_detect(Ingredients, 'vegetable') ~ 'broth cube vegetable',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice|powder') & str_detect(Ingredients, 'fish') ~ 'broth cube fish',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice|powder') & str_detect(Ingredients, 'beef|meat') ~ 'broth cube beef',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'cube|dice|powder') & str_detect(Ingredients, 'chicken') ~ 'broth cube chicken',
      str_detect(Ingredients, 'stock|broth|bouillon|borth') & str_detect(Ingredients, 'cube|dice|powder') ~ 'broth cube',
      str_detect(Ingredients, 'stock|broth|bouillon|power\\b') & str_detect(Ingredients, 'chicken') ~ 'water broth chicken',
      str_detect(Ingredients, 'stock|broth|bouillon|bouilljon|consomme') & str_detect(Ingredients, 'beef|meat') ~ 'water broth beef',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'shellfish') ~ 'water broth shellfish',
      str_detect(Ingredients, 'stock|broth|bouillon|power\\b') & str_detect(Ingredients, 'game|wild') ~ 'water broth game',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'turkey') ~ 'water broth turkey',
      str_detect(Ingredients, 'stock|broth|bouillon') & str_detect(Ingredients, 'vegetable') ~ 'water broth vegetable',
      str_detect(Ingredients, 'stock|broth|bouillon|power\\b') & str_detect(Ingredients, 'lamb') ~ 'water broth lamb',
      str_detect(Ingredients, 'stock|broth|bouillon|power\\b') & str_detect(Ingredients, 'fish') ~ 'water broth fish',
      str_detect(Ingredients, 'stock|broth|bouillon|frying pan|power\\b') ~ 'water broth',

      #Baking ingredients
      str_detect(Ingredients, 'agave') & str_detect(Ingredients, 'nectar') ~ 'agave nectar',
      str_detect(Ingredients, 'agave') & str_detect(Ingredients, 'syrup') ~ 'agave syrup',

      str_detect(Ingredients, 'baking powder') ~ 'baking powder',
      str_detect(Ingredients, 'baking soda|bicarbonate of soda') ~ 'baking soda',
      str_detect(Ingredients, "blackstrap") & str_detect(Ingredients, "molass") ~ "molasses blackstrap",
      str_detect(Ingredients, 'molass') ~ "molasses blackstrap",

      str_detect(Ingredients, 'citric acid') & str_detect(Ingredients, 'powder') ~ 'citric acid powder',

      str_detect(Ingredients, 'decorative glaze') ~ 'decorative glaze',

      str_detect(Ingredients, 'erythriol') ~ 'erythriol',

      str_detect(Ingredients, 'fondant') & str_detect(Ingredients, 'white') ~ 'fondant white',
      str_detect(Ingredients, 'food') & str_detect(Ingredients, 'coloring|colouring') ~ 'food coloring',

      str_detect(Ingredients, 'pectin') ~ 'pectin',

      str_detect(Ingredients, 'shortening') & str_detect(Ingredients, 'vegetable') ~ 'shortening vegetable',
      str_detect(Ingredients, 'shortening') ~ 'shortening',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'brown\\b|castor') ~ 'sugar brown',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'confect|icing|powder') ~ 'sugar confectioners',
      str_detect(Ingredients, 'sugar') & str_detect(Ingredients, 'vanilla') & !str_detect(Ingredients, 'milk|yogurt|yoghurt|kesam|without added sugar|without sugar|no added sugar|0% sugar|less sugar|low sugar|sugar free') ~ 'sugar vanilla',
      str_detect(Ingredients, 'sugar') & !str_detect(Ingredients, 'asparagus|pea|cola|without added sugar|without sugar|no added sugar|0% sugar|0 % sugar|color liquid|yogurt|yoghurt|muesli|granola|biola|less sugar|low sugar|sugar free') ~ 'sugar',
      str_detect(Ingredients, 'syrup|sirup') & str_detect(Ingredients, 'date') ~ 'syrup date',
      str_detect(Ingredients, 'syrup|sirup') & str_detect(Ingredients, 'currant') ~ 'syrup currant',
      str_detect(Ingredients, 'syrup|sirup') & str_detect(Ingredients, 'maple') ~ 'syrup maple',
      str_detect(Ingredients, 'syrup|sirup') & str_detect(Ingredients, 'pomegr') ~ 'syrup pomegranate',
      str_detect(Ingredients, 'syrup') & str_detect(Ingredients, 'chocolate') ~ 'syrup chocolate',
      str_detect(Ingredients, 'syrup') ~ 'syrup',

      str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'dry|dried') ~ 'yeast dry',
      str_detect(Ingredients, 'yeast') & str_detect(Ingredients, 'nutritional') ~ 'yeast nutritional',
      str_detect(Ingredients, 'yeast') ~ 'yeast',

      str_detect(Ingredients, 'color') & str_detect(Ingredients, 'sprinkles') ~ 'sprinkles colored',
      str_detect(Ingredients, 'sprinkles') & str_detect(Ingredients, 'christmas') ~ 'sprinkles christmas',
      str_detect(Ingredients, 'sprinkles') ~ 'sprinkles',
      str_detect(Ingredients, 'gelatin') & str_detect(Ingredients, 'sheet|plate') ~ 'gelatin sheet',
      str_detect(Ingredients, '\\bgelatin\\b') ~ 'gelatin',
      str_detect(Ingredients, '\\bgel\\b') & str_detect(Ingredients, 'color') ~ 'gel coloring',

      #Pizza, and similar
      str_detect(Ingredients, 'pizza filling') ~ 'pizza filling',
      str_detect(Ingredients, 'pizza') & str_detect(Ingredients, 'dough| \\bcm\\b|fresh|fried|square') & !str_detect(Ingredients, 'meat dough') ~ 'pizza dough',
      str_detect(Ingredients, 'pizza') & !str_detect(Ingredients, 'sauce|base|topping|cheese|dressing|dough|flour|for pizza|seasoning|spice') ~ 'pizza',
      str_detect(Ingredients, 'savory filled biscuits') ~ 'savory filled biscuits',

      # Others
      str_detect(Ingredients, 'agar') ~ 'agar',
      str_detect(Ingredients, 'asafoetida powder') ~ 'asafoetida powder',
      str_detect(Ingredients, '\\bapple') & str_detect(Ingredients, 'gold\\b') ~ 'apple chips',
      str_detect(Ingredients, '\\bapple') & str_detect(Ingredients, 'crumble') ~ 'apple crumble',

      str_detect(Ingredients, 'barbecue|bbq|barbeque') & str_detect(Ingredients, 'marinade') ~ 'barbecue marinade',
      str_detect(Ingredients, 'barbecue|bbq|barbeque') & str_detect(Ingredients, 'rub') ~ 'barbecue rub',
      str_detect(Ingredients, 'beet') & str_detect(Ingredients, 'pickle') ~ 'beetroot pickled',
      Ingredients == 'black truffle or 2 tbsp s truffle oil' ~ 'black truffle',
      str_detect(Ingredients, 'brine') & !str_detect(Ingredients, 'shrimp|prawn') ~ 'water brine',

      str_detect(Ingredients, 'caper|hijack') ~ 'caper',

      str_detect(Ingredients, 'cooking') & str_detect(Ingredients, 'spray') ~ 'cooking spray',
      str_detect(Ingredients, 'crisps') ~ 'crisps',
      str_detect(Ingredients, 'cucumber mix') ~ 'cucumber mix pickled',
      str_detect(Ingredients, 'custard') & str_detect(Ingredients, 'hot') ~ 'custard hot',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'custard') ~ 'custard vanilla',
      str_detect(Ingredients, 'vanilla') & str_detect(Ingredients, 'cream') & !str_detect(Ingredients, "ice|flavor|drink") ~ 'vanilla cream',

      str_detect(Ingredients, 'fig tart') ~ 'fig tart',

      str_detect(Ingredients, 'herb') & !str_detect(Ingredients, 'basil|thyme|parsley|rosemary|dill|sauce|cheese|philadelphia|tomato|in herb|with herb|and herb|& herb|herb marinate|\\btea\\b') ~ 'herbs',
      str_detect(Ingredients, 'honey') &
        !str_detect(Ingredients, 'mustard|melon|dew|apple|pecan|nut|with hiney|in honey|puff|yoghurt|yogurt|honey-roast|honey roast|honey tomato|honey glaze|cereal') ~ 'honey',

      str_detect(Ingredients, 'ice cube') ~ 'ice cube',

      str_detect(Ingredients, 'kombu') & str_detect(Ingredients, 'dashi') ~ 'dashi kombu dried kelp',
      str_detect(Ingredients, 'mire poix') ~ 'mire poix',

      str_detect(Ingredients, 'noodle') & str_detect(Ingredients, 'egg') ~ 'egg noodle',
      str_detect(Ingredients, 'noodle') & str_detect(Ingredients, 'glass') ~ 'glass noodle',
      str_detect(Ingredients, 'noodle') ~ 'rice noodle', #Default
      str_detect(Ingredients, 'nori') & str_detect(Ingredients, 'flak|seaweed|sheet|sushi') ~ 'nori seaweed',

      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'white') ~ 'white pepper',
      str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'cayenne') ~ 'cayenne pepper',
      str_detect(Ingredients, 'pepper') &
        !str_detect(Ingredients,
                    'chili|sweet|spice|bell|salad|sauce|mackerel|pepperoni|ham|red|hot|olive|cheddar|brie|cheese|crème|creme|with pepper|and pepper|steak|loin|salmon|salma|burger|sausage|pate|chicken|pâté|snack|spread|pasta|pesto')
      & !str_detect(Ingredients, 'salt') ~ 'black pepper',
      str_detect(Ingredients, 'pickle') & !(str_detect(Ingredients, 'cucumber|ginger|sweet pepper|onion|beet|paprika|jalap|herring')) | str_detect(Ingredients, 'gherkin') ~ 'cucumber pickled', #Standard

      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'oil') ~ 'salt and pepper and oil',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & str_detect(Ingredients, 'butter') ~ 'salt and pepper and butter',
      str_detect(Ingredients, 'salt') & str_detect(Ingredients, 'pepper') & !str_detect(Ingredients, 'cashew|crouton|crisp') ~ 'salt and pepper',
      str_detect(Ingredients, 'salt\\b') &
        #Foods that can have salt information
        !(str_detect(Ingredients,
                     'peanut|lamb|pork|anchovy|soy|flesk|cashew|butter|almond|nut|bacon|biscuit|cod|pecan|potato|chip|stick|bar|urchin|crouton|horn salt') |
            #Ways to add salt infomation
            str_detect(Ingredients, 'with salt|without salt|and salt')) ~ 'salt',
      str_detect(Ingredients, 'spring roll') & str_detect(Ingredients, 'fill') ~ 'spring roll filled',
      str_detect(Ingredients, 'spring roll paper') ~ 'spring roll paper',
      str_detect(Ingredients, 'spring roll') ~ 'spring roll',
      str_detect(Ingredients, '(yogurt|yoghurt)(?=( nut|nut))') ~ 'yoghurt nuts',

      TRUE ~ Ingredients_standardised))
}

