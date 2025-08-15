#' standardiserFoodNames
#' Standardise names of food items in Norwegian.
#'
#' @param df Dataframe with an FoodItem column with food item names to standardise.
#'
#' @return The dataframe with an additional standardisedFoodItem column with standardised food item names.
#' @export

# Function
standardiserFoodNames <- function(df) {

  meat_products <- c("tenderloin", "striploin", "sirloin",
                     "minced meat", "meat patties", "chuck|shoulder",
                     "karbonadedeig", "karbonader", "rib-eye|rib eye|ribeye|entrecote|entrecôte",
                     "stew meat", "burger", "bottom round", "inside round", "t-bone",
                     "chop",
                     "smokey ribs")
  other_product_categories <- c("pampas", "okse", "storfe", "angus", "cheese", "ost", "plant-based",
                                "brød", "bread", "lam", "svin", "gris", "hjort", "elg", "rein",
                                "salma", "laks", "fisk", "torsk", "meatish", "selleri",  "bønne",
                                "vegetar", "plantebasert", "vegan", "kylling|chicken", "kalkun",
                                "bacon", "dressing", "krydder", "kveite", "tomat", "naturli",
                                "beyond", "relish", "buns", "steinbit", "vegan", "kjøtterstatning|kjøttdeigerstatning",
                                "sensational burger", "soya", "sei")
  multiple_spelling <- list(
    creme_fraiche = c("creme fraiche", "crème fraîche", "crème fraiche",
                      "créme fraîche", "créme fraïche", "crème fraîce",
                      "créme fraiche", "crême fraice", "crème fraîche",
                      "crême fraiche", "cremé fraîche"),
    chili = c("chili", "chilli"),
    bbq = c("bbq", "barbecue", "barbeque"),
    kjøttdeig = c("minced meat", "ground meat")
  )


  # Standardise----
  df %>%

    mutate(
      # Add classifier to meat and flour products
      Ingredients = case_when(
        (str_detect(stri_trans_tolower(Ingredients),
                    paste0(meat_products, collapse = "|")) &
           !str_detect(stri_trans_tolower(Ingredients),
                       paste0(other_product_categories, collapse = "|"))) ~ paste0(Ingredients, " storfe"),
        str_detect(stri_trans_tolower(Ingredients), 'innmat') &
          !str_detect(stri_trans_tolower(Ingredients), 'kalkun') &
          str_detect(stri_trans_tolower(recipe_name), "kalkun") ~ paste0(Ingredients, " kalkun"),
        str_detect(stri_trans_tolower(Ingredients), "flintstek") &
          !str_detect(stri_trans_tolower(Ingredients), "svin") ~ paste0(Ingredients, " svin"),
        str_detect(stri_trans_tolower(Ingredients), "toro glutenfri") & !str_detect(stri_trans_tolower(Ingredients), "mel") ~ "toro glutenfri melblanding",
        str_detect(Ingredients, "kjøttrester") & !str_detect(stri_trans_tolower(Ingredients), paste0(c(other_product_categories, "pinnekjøtt"), collapse = "|")) ~ str_replace(Ingredients, "kjøttrester", "bog, svin"), # Usually pork that is used
        TRUE ~ Ingredients),
      # Turn into small letters
      Ingredients = stri_trans_tolower(Ingredients),
      # Fix some common different spellings
      Ingredients = str_replace_all(Ingredients, c(
        "chili|chilli" = "chili",
        "creme fraiche|crème fraîche|crème fraiche|créme fraîche|créme fraïche|crème fraîce|créme fraiche|crême fraice|crème fraîche|crême fraiche|cremé fraîche" = "creme fraiche",
        "bbq|barbecue|barbeque" = "bbq",
        "kjøttdeig|kvernet deig|kjøttkakedeig|minced" = "kjøttdeig",
        "(?<=nøtt)kjerner" = "er",
        "pistasjkjerner" = "pistasjnøtter",
        "granateple kjerner|granateple, kjerner" = "granateplekjerner",
        "((?<!granateple)(?<!pinje))kjerner" = "frø",
        "kvernet deig" = "kjøttdeig",
        "sauce" = "saus",
        "/u" = "uten",
        "spiskummen|spisskumin" = "spisskummen",
        "\\bkumin|cummin" = "kummin",
        "\\bsmøre\\b" = "",
        "avocado" = "avokado",

        # Symbols
        "\\&" = "og"
      ))) %>%
    mutate(
      # Standardise using the expressions in rulesFoodNamesStandardisation,
      standardisedIngredients = case_when(
        !!!rlang::parse_exprs(paste(norwegianIngredients::rulesFoodNameStandardisation$expression))
      ))

}


