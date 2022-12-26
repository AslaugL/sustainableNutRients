#' Standardising ingredients names in a recipe, here herbs and spices starting with the letter "C".
#' @title standardiseHerbsnSpicesC
#'
#' @description Support function for "standardiseHerbsnSpices", standardise names of herbs and spices starting with "C".
#'
#' @param df A dataframe with an Ingredients column, listing each ingredient of the recipe in individual rows, and an Ingredients_standardised column.
#'
#' @return The dataframe with a column with herbs and spices starting with the letter "C" with standardised names.
#'
#' @export
standardiseHerbsnSpicesC <- function(df) {
  df  %>%

    #Standardise
    mutate(Ingredients_standardised = case_when(
      str_detect(Ingredients, 'cajun') & str_detect(Ingredients, 'spice') ~ 'cajun spice',
      str_detect(Ingredients, 'caraway') ~ 'caraway seed',
      str_detect(Ingredients, 'cardamom') & str_detect(Ingredients, 'fruit|pod|capsule') ~ 'cardamom pod',
      str_detect(Ingredients, 'cardamom') ~ 'cardamom',
      str_detect(Ingredients, 'celery') & str_detect(Ingredients, 'seed') ~ 'celery seed',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'spice mix') & str_detect(Ingredients, 'roast') ~ 'spice mix chicken roast',
      str_detect(Ingredients, 'chicken') & str_detect(Ingredients, 'spice mix') ~ 'spice mix chicken',
      str_detect(Ingredients, 'chervil') &
        !str_detect(Ingredients, 'parsley|rosemary|thyme|mint|basil|oregano|dill|coriander|tarragon') ~ 'chervil fresh herb', #All chervils are fresh
      str_detect(Ingredients, 'chives') ~ 'chives fresh herb', #Standard
      (str_detect(Ingredients, 'chili|chilli') & str_detect(Ingredients, 'dried') & str_detect(Ingredients, 'flake')) |
        str_detect(Ingredients, 'red pepper flate|red pepper flake') |
        str_detect(Ingredients, 'chili flake|chilli flake') ~ 'chili flake dried',
      (str_detect(Ingredients, 'chili|chilli') & str_detect(Ingredients, 'powder|spice')) |
        str_detect(Ingredients, 'ground red pepper')  |
        (str_detect(Ingredients, 'chili|chilli') & !str_detect(Ingredients, 'sauce|paste') & str_detect(unit, 'tsp|tbsp')) ~ 'chili powder',
      str_detect(Ingredients, 'chinese') & str_detect(Ingredients, 'spice') ~ 'chinese five spice',
      str_detect(Ingredients, 'cinnamon') & str_detect(Ingredients, 'bar|rod|stick') ~ 'cinnamon bar',
      str_detect(Ingredients, 'cinnamon') & !str_detect(Ingredients, 'muesli') ~ 'cinnamon',
      str_detect(Ingredients, 'clove|carnation') & !str_detect(Ingredients, 'garlic') ~ 'cloves',
      str_detect(Ingredients, 'coriander') & str_detect(Ingredients, 'seed') ~ 'coriander seed',
      str_detect(Ingredients, 'coriander|cilantro') & !str_detect(Ingredients, 'seed') & (str_detect(Ingredients, 'fresh|chop|crush|neve|twig|leaf|malt') | str_detect(unit, 'twig|bunch|leaf|neve|dl')) ~ 'coriander fresh herbs',
      str_detect(Ingredients, 'coriander|cilantro') ~ 'coriander dried', #Standard
      str_detect(Ingredients, 'cress') ~ 'cress fresh herbs',
      str_detect(Ingredients, 'cumin') ~ 'cumin',
      str_detect(Ingredients, 'curry') & !str_detect(Ingredients, 'paste') ~ 'curry powder',

      TRUE ~ Ingredients_standardised))
}
