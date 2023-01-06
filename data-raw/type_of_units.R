library(dplyr)

#Units to look for in the Ingredients column
type_of_units = c(
  'tsp', 'tbsp', 'krm', 'pinch',

  'cl', 'cup', 'dl', 'drop', 'flaske', 'glass', 'handful', 'l', 'ml', 'tins', 'quart',

  'g', 'kg', 'ounce', 'oz', 'pound', "lb",

  'box', 'bunch', 'can', 'cm', 'hp',  'leaf', 'pot', 'tube', 'inch',

  'portion', 'pcs', 'slice', '\\bclove\\b',
  'pack', 'plate', 'twig', 'stalk'
  ) %>%
  #Add whitespace on both sides to only match a unit in a string
  sapply(., function(x) {paste0('\\s', x, '\\s')})

#Save
saveRDS(type_of_units, "./data-raw/type-of-units.Rds")

