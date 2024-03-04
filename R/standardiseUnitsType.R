#' Standardising the unit types in a recipe. Volume units to dl, weight to g, cooked to raw.
#' @title standardiseUnitsType
#'
#' @description Standardise type of units: volume units to dl and weight units to grams.
#'
#' @param df A dataframe with a unit column indicating what type of unit is used, and a numerical Amount column listing the amount.
#'
#' @return The dataframe with all ingredient units and volume standardised.
#'
#' @export
 standardiseUnitsType <- function(df){

   #Liquid foods with the same weight as water
   similar_to_water <- c(
     "almond essence", "apple juice", "aquavit",

     "beer", "broth",

     "cider", "cognac", "cream", "cr\u00E8me fra\u00CEche", "coffee",
     "coffee espresso", "coffee liqueur", "iced coffee",

     "eddik",

     "fund",

     "household juice",

     "jelly apple",  "juice",

     "kefir", "kraft",

     "lemonade",

     "madeira", "marsala", "milk",

     "orange juice", "orange liqueur",

     "r\u00F8mme", "raspberry liqueur", "reduction",

     "sherry", "soda", "spirits", "stock", "strawberry juice", 'soda', 'seltzer',

     "tea black",

     "vanilla essence", "vanilla extract", "vin", "vinegar", "vinaigrette",

     "water", "wine",

     "yogurt", "yoghurt")

   #Turn everything into the same unit----
   standardised <- df %>% mutate(

     #Amounts column
     Amounts = case_when(
       #Volume to dl
      str_detect(Ingredients, 'cider') & unit == 'glass' ~ 3.41,
      unit == 'cup' ~ Amounts * 2.45,
      unit == 'l' ~ Amounts * 10,
      unit == 'ml' ~ Amounts / 100,
      unit == 'cl' ~ Amounts / 10,
      unit == 'quart' ~ Amounts * 9.46,
      unit == 'tbsp' ~ Amounts / 6.67,
      unit == 'tsp' ~ Amounts / 20,
      unit == 'krm' ~ Amounts / 100,
      unit == 'drop' ~ Amounts / 2000, #One drop is 0.05ml
      unit == 'pinch' ~ Amounts / (20*16), #A pinch is usually defined as 1/16 of a tsp
      unit == 'inch' ~ Amounts*2.54, #To cm

      #Weight to grams
      unit %in% c('ounce', 'oz') ~ Amounts * 28.35,
      unit == 'pound' ~ Amounts * 453.59,

      TRUE ~ Amounts),

     #Units column
     unit = case_when(

       #Volume to dl
       unit %in% c('cup', 'l', 'ml', 'cl', 'tsp', 'tbsp', 'krm', 'drop', 'pinch', 'quart') | str_detect(Ingredients, 'cider') & unit == 'glass' ~ 'dl',
       #Weight to gram
       unit %in% c('ounce', 'pound') ~ 'g',

       unit == 'inch' ~ "cm",

       #Some special cases
       Ingredients %in% c('shrimp', 'salad rocket') ~ str_replace(unit, 'handful', 'dl'),
       str_detect(Ingredients, 'beef') & !str_detect(Ingredients, 'tongue|cube|tomato') ~ str_replace(unit, 'pcs|slice', 'portion'),
       str_detect(Ingredients, 'cod fillet') ~ str_replace(unit, 'slice', 'portion'),
       str_detect(Ingredients, 'salad|caper|parsley') ~ str_replace(unit, 'handful', 'dl'),

       TRUE ~ unit)
     ) %>%


      #Turn juice, water, vinegar and other liquids with similar density to water from dl/l
      #to grams as they are all about 100g/dl
      mutate(
         Amounts = case_when(
            (str_detect(Ingredients, paste0(similar_to_water, collapse = '|')) &
                unit == 'dl') &
               !str_detect(Ingredients, 'sugar|cheese|flour|\\bice\\b') ~ Amounts * 100,
            TRUE ~ Amounts),
         unit = case_when(
            (str_detect(Ingredients, paste0(similar_to_water, collapse = '|')) &
                unit == 'dl') &
               !str_detect(Ingredients, 'sugar|cheese|flour|\\bice\\b') ~ 'g',
            TRUE ~ unit)) %>%

      #From grams into kilos
      mutate(
         Amounts = case_when(
            unit == 'g' ~ Amounts/1000,
            TRUE ~ Amounts),
         unit = unit %>%
            str_replace('\\bg\\b', 'kg'))


   #Single types of ingredients that needs standardisation----
   #Split rows with 'salt and pepper and oil' and similar
   standardised <- standardised %>%
      separate_rows(Ingredients, sep = '(?<=salt|pepper|oil) and ') %>%
      #Remove "slice" of pepper/salt, mistaken translation from Norwegian
      mutate(unit = case_when(
         !(Ingredients %in% c('salt', 'pepper') & unit == 'slice') ~ unit
      ))

   #Split broth into water and broth cubes
   #Pull out broth ingredients
   temp <- standardised %>% splitBrothtoWaterBrotchcubes()

   #Add back to recipes
   standardised <- standardised %>%
      #Remove broth wthout broth cubes
      filter(!(str_detect(Ingredients, 'water broth') & !is.na(Amounts))) %>%
      #Add back with broth cues
      bind_rows(temp)

   standardised
 }
