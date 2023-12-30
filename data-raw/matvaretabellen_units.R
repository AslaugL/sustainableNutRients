#Units from Matvaretabellen
matvaretabellen_units <- tibble(
  'nutrient' = c('Vitamin A', 'Retinol', 'Beta-carotene',
                'Vitamin D','Vitamin E', 'Thiamin',
                'Riboflavin','Niacin', 'Vitamin B6',
                'Folate', 'Vitamin B12', 'Vitamin C',
                'Calcium', 'Iron', 'Sodium',
                'Potassium', 'Magnesium', 'Zinc',
                'Selenium', 'Copper', 'Phosphorus',
                'Iodine', 'Water'),

  'unit' = c("mcg-RE", "mcg", "mcg",
             "mcg", "mg-ATE", "mg",
             "mg", "mg", "mg",
             "mcg", "mcg", "mg",
             "mg", "mg", "mg",
             "mg", "mg", "mg",
             "mcg", "mg", "mg",
             "mcg", 'g')
)

saveRDS(matvaretabellen_units,"./data-raw/matvaretabellen_units.Rds")
