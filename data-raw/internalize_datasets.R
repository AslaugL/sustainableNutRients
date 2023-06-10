#Read the Rds files and save them to sysdata
#Databases
unit_weights <- readRDS("./data-raw/unit_weights2.Rds")
unit_weights_query <- readRDS("./data-raw/unit_weights_query.Rds")

matvaretabellen2020 <- readRDS("./data-raw/matvaretabellen2020.Rds")
matvaretabellen2020_query <- readRDS("./data-raw/matvaretabellen2020_query.Rds")

matvaretabellen2022 <- readRDS("./data-raw/matvaretabellen2022.Rds")
matvaretabellen2022_query <- readRDS("./data-raw/matvaretabellen2022_query.Rds")

SHARP2018 <- readRDS("./data-raw/SHARP2018.Rds")
SHARP2018_query <- readRDS("./data-raw/SHARP2018_query.Rds")

#Vectors used in functions
pcs <- readRDS("./data-raw/pcs.Rds")
type_of_units <- readRDS("./data-raw/type-of-units.Rds")

#Foodgroups from matvaretabellen and SHARP
matvaretabellen2020_foodgroups <- readRDS("./data-raw/matvaretabellen2020_foodgroups.Rds")
SHARP2018_foodgroups <- readRDS("./data-raw/SHARP2018_foodgroups.Rds")

#units for matvaretabellen
matvaretabellen_units <- readRDS("./data-raw/matvaretabellen_units.Rds")


#Composite ingredients in Oda
composite_ingredients_oda <- readRDS("./data-raw/composite_ingredients_oda.Rds")

#Save for export
usethis::use_data(unit_weights, unit_weights_query,
                  matvaretabellen2020, matvaretabellen2020_query,
                  SHARP2018, SHARP2018_query,
                  pcs, type_of_units,
                  matvaretabellen2020_foodgroups, SHARP2018_foodgroups,
                  matvaretabellen_units,
                  composite_ingredients_oda,
                  overwrite = TRUE,
                  internal = FALSE)

#Save in sysdata
usethis::use_data(unit_weights, unit_weights_query,
                  matvaretabellen2020, matvaretabellen2020_query,
                  SHARP2018, SHARP2018_query,
                  pcs, type_of_units,
                  matvaretabellen2020_foodgroups, SHARP2018_foodgroups,
                  composite_ingredients_oda,
                  overwrite = TRUE,
                  internal = TRUE)


