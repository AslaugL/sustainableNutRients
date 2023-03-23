#' Calculate the mean amount of ingredients when recipe says 3-4 and similar.
#' @title calculateNutriscore
#'
#' @description Calculate the nutriscore of a recipe.
#'
#' @param df Dataframe with nutrient content per 100g and percentage of weight of fruit/vegetables and healthy oils formatted
#'  similarly to the output of calculateNutrientContentOfFoodlist calculated "per 100 g". Fruit/vegetables and healthy oils
#'  should be present in the "nutrient" column with the variable name "nutriscore_amounts_pct" and the value in percent should
#'  be in the "nutrient_content_per_hektogram" column.
#'
#' @return The dataframe with the recipes raw nutriscore and equivalent nutriscore letter.
#'
#' @export
calculateNutriscore <- function(df, raw_scores = FALSE){

  #Join them together and calculate nutriscore
  nutriscore_raw <- df %>%

    #Calculate nutriscore
    mutate(nutriscore_raw = case_when(
      #Energy
      nutrient == 'Kilojoules' & nutrient_content_per_hektogram <= 335 ~ 0,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >335 & nutrient_content_per_hektogram <= 670) ~ 1,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >670 & nutrient_content_per_hektogram <= 1005) ~ 2,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >1005 & nutrient_content_per_hektogram <= 1340) ~ 3,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >1340 & nutrient_content_per_hektogram <= 1675) ~ 4,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >1675 & nutrient_content_per_hektogram <= 2010) ~ 5,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >2010 & nutrient_content_per_hektogram <= 2345) ~ 6,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >2345 & nutrient_content_per_hektogram <= 2680) ~ 7,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >2680 & nutrient_content_per_hektogram <= 3015) ~ 8,
      nutrient == 'Kilojoules' & (nutrient_content_per_hektogram >3015 & nutrient_content_per_hektogram <= 3350) ~ 9,
      nutrient == 'Kilojoules' & nutrient_content_per_hektogram >3350 ~ 10,

      #(Added) Sugars
      nutrient == 'Sugar' & nutrient_content_per_hektogram <= 4.5 ~ 0,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >4.5 & nutrient_content_per_hektogram <= 9) ~ 1,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >9 & nutrient_content_per_hektogram <= 13.5) ~ 2,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >13.5 & nutrient_content_per_hektogram <= 18) ~ 3,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >18 & nutrient_content_per_hektogram <= 22.5) ~ 4,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >22.5 & nutrient_content_per_hektogram <= 27) ~ 5,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >27 & nutrient_content_per_hektogram <= 31) ~ 6,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >31 & nutrient_content_per_hektogram <= 36) ~ 7,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >36 & nutrient_content_per_hektogram <= 40) ~ 8,
      nutrient == 'Sugar' & (nutrient_content_per_hektogram >40 & nutrient_content_per_hektogram <= 45) ~ 9,
      nutrient == 'Sugar' & nutrient_content_per_hektogram >45 ~ 10,

      #Saturated Fat
      nutrient == 'SatFa' & nutrient_content_per_hektogram <= 1 ~ 0,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >1 & nutrient_content_per_hektogram <= 2) ~ 1,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >2 & nutrient_content_per_hektogram <= 3) ~ 2,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >3 & nutrient_content_per_hektogram <= 4) ~ 3,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >4 & nutrient_content_per_hektogram <= 5) ~ 4,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >5 & nutrient_content_per_hektogram <= 6) ~ 5,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >6 & nutrient_content_per_hektogram <= 7) ~ 6,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >7 & nutrient_content_per_hektogram <= 8) ~ 7,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >8 & nutrient_content_per_hektogram <= 9) ~ 8,
      nutrient == 'SatFa' & (nutrient_content_per_hektogram >9 & nutrient_content_per_hektogram <= 10) ~ 9,
      nutrient == 'SatFa' & nutrient_content_per_hektogram >10 ~ 10,

      #Sodium
      nutrient == 'Sodium' & nutrient_content_per_hektogram <= 90 ~ 0,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >90 & nutrient_content_per_hektogram <= 180) ~ 1,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >180 & nutrient_content_per_hektogram <= 270) ~ 2,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >270 & nutrient_content_per_hektogram <= 360) ~ 3,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >360 & nutrient_content_per_hektogram <= 450) ~ 4,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >450 & nutrient_content_per_hektogram <= 540) ~ 5,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >540 & nutrient_content_per_hektogram <= 630) ~ 6,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >630 & nutrient_content_per_hektogram <= 720) ~ 7,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >720 & nutrient_content_per_hektogram <= 810) ~ 8,
      nutrient == 'Sodium' & (nutrient_content_per_hektogram >810 & nutrient_content_per_hektogram <= 900) ~ 9,
      nutrient == 'Sodium' & nutrient_content_per_hektogram >900 ~ 10,

      #Fruits and veggies an legumes and nuts and (certain) oils
      nutrient == 'nutriscore_amounts_pct' & nutrient_content_per_hektogram <= 40 ~ 0,
      nutrient == 'nutriscore_amounts_pct' & (nutrient_content_per_hektogram > 40 & nutrient_content_per_hektogram <= 60) ~ 1,
      nutrient == 'nutriscore_amounts_pct' & (nutrient_content_per_hektogram > 60 & nutrient_content_per_hektogram <= 80) ~ 2,
      nutrient == 'nutriscore_amounts_pct' & nutrient_content_per_hektogram > 80 ~ 5,

      #Fibre
      nutrient == 'Dietary fibre' & nutrient_content_per_hektogram <= 0.9 ~ 0,
      nutrient == 'Dietary fibre' & (nutrient_content_per_hektogram >0.9 & nutrient_content_per_hektogram <=1.9) ~ 1,
      nutrient == 'Dietary fibre' & (nutrient_content_per_hektogram >1.9 & nutrient_content_per_hektogram <=2.8) ~ 2,
      nutrient == 'Dietary fibre' & (nutrient_content_per_hektogram >2.8 & nutrient_content_per_hektogram <=3.7) ~ 3,
      nutrient == 'Dietary fibre' & (nutrient_content_per_hektogram >3.7 & nutrient_content_per_hektogram <=4.7) ~ 4,
      nutrient == 'Dietary fibre' & nutrient_content_per_hektogram >4.7 ~ 5,

      #Protein
      nutrient == 'Protein' & nutrient_content_per_hektogram <= 1.6 ~ 0,
      nutrient == 'Protein' & (nutrient_content_per_hektogram >1.6 & nutrient_content_per_hektogram <=3.2) ~ 1,
      nutrient == 'Protein' & (nutrient_content_per_hektogram >3.2 & nutrient_content_per_hektogram <=4.8) ~ 2,
      nutrient == 'Protein' & (nutrient_content_per_hektogram >4.8 & nutrient_content_per_hektogram <=6.4) ~ 3,
      nutrient == 'Protein' & (nutrient_content_per_hektogram >6.4 & nutrient_content_per_hektogram <=8.0) ~ 4,
      nutrient == 'Protein' & nutrient_content_per_hektogram >8.0 ~ 5,
    )) %>% drop_na(nutriscore_raw) #Drop nutrients not needed for calculations

  #temporary categories used to calculate final score
  categories <- nutriscore_raw %>%
    mutate(category = case_when(
      nutrient %in% c('Kilojoules', 'Sugar', 'SatFa', 'Sodium') ~ 'N',
      nutrient %in% c('nutriscore_amounts_pct', 'Dietary fibre', 'Protein') ~ 'P'
    )) %>% drop_na(category) %>%
    #get scores for the categories
    group_by(recipe_name, category) %>%
    summarise(nutriscore_raw = sum(nutriscore_raw)) %>%
    ungroup() %>%
    #Turn into wide format
    pivot_wider(
      names_from = 'category',
      values_from = 'nutriscore_raw'
    )

  categories_2 <- nutriscore_raw %>%
    mutate(category = case_when(
      nutrient == 'nutriscore_amounts_pct' ~ 'FruitVegLegumesNutsOils',
      nutrient == 'Dietary fibre' ~ 'Fibre',
      nutrient == 'Protein' ~ 'Protein'
    )) %>% drop_na(category) %>%
    #get scores for the categories
    group_by(recipe_name, category) %>%
    summarise(nutriscore_raw = sum(nutriscore_raw)) %>%
    ungroup() %>%
    #Turn into wide format
    pivot_wider(
      names_from = 'category',
      values_from = 'nutriscore_raw'
    ) %>%
    # 0 for recipes without fruit legumes nuts
    mutate(FruitVegLegumesNutsOils = case_when(
      is.na(FruitVegLegumesNutsOils) ~ 0,
      TRUE ~ FruitVegLegumesNutsOils
    ))

  #Calculate nutriscore
  nutriscore <- full_join(categories, categories_2) %>%
    mutate(nutriscore = case_when(
      N < 11 ~ N-P,
      N >= 11 & FruitVegLegumesNutsOils >= 5 ~ N-P,
      N >= 11 & FruitVegLegumesNutsOils < 5 ~ N-(FruitVegLegumesNutsOils+Fibre)
    ),
    nutriscore_letter = case_when(
      nutriscore <= -1 ~ 'A',
      nutriscore >= 0 & nutriscore <=2 ~ 'B',
      nutriscore >= 3 & nutriscore <=10 ~ 'C',
      nutriscore >= 11 & nutriscore <=18 ~ 'D',
      nutriscore >= 19 ~ 'E',
    )) %>%
    #Remove columns not needed
    select(recipe_name, nutriscore, nutriscore_letter) %>%
    #Turn letters into factor
    mutate(nutriscore_letter = as.factor(nutriscore_letter))
}
