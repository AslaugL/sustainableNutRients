#Read the Rds files and save them to sysdata
#Databases
unit_weights <- readRDS("./data-raw/unit_weights.Rds")
unit_weights_query <- readRDS("./data-raw/unit_weights_query.Rds")

matvaretabellen2020 <- readRDS("./data-raw/matvaretabellen2020.Rds")
matvaretabellen2020_query <- readRDS("./data-raw/matvaretabellen2020_query.Rds")

SHARP2018 <- readRDS("./data-raw/SHARP2018.Rds")
SHARP2018_query <- readRDS("./data-raw/SHARP2018_query.Rds")

#Vectors used in functions
pcs <- readRDS("./data-raw/pcs.Rds")
type_of_units <- readRDS("./data-raw/type-of-units.Rds")

#Foodgroups from matvaretabellen and SHARP
foodgroups <- list(
  "matvaretabellen" = readRDS("./data-raw/matvaretabellen2020_foodgroups.Rds"),
  "SHARP" = readRDS("./data-raw/SHARP2018_foodgroups.Rds")
)

#Save in sysdata
usethis::use_data(unit_weights, unit_weights_query,
                  matvaretabellen2020, matvaretabellen2020_query,
                  SHARP2018, SHARP2018_query,
                  pcs, type_of_units,
                  overwrite = TRUE,
                  internal = TRUE)


