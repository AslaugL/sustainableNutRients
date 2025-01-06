#' Find food in reference database.
#' @title findFoodInDatabase
#'
#' @description Find foods within a reference database, to later calculate weight from volume units, nutrient content or environmental sustainability markers from weight units.
#'
#' @param df A dataframe with an Ingredients column to be checked against the reference database, and a unit column. For best accuracy units should be standardised.
#' @param database The reference database, either "volume_weight", "nutrients" or "sustainability".
#' @param additional_entries Additional entries to the databases that foods can found in. Must be formatted same as the output from "createNewDatabaseEntry".
#' @param fix_errors Fix known errors automatically? Default TRUE.
#' @param include_water Some front of pack labels or other nutrient calculations use the dry weight of ingredients. Should the % of water in an ingredient be included in the nutrient calculations? TRUE or FALSE. Default FALSE.
#'
#' @return A dataframe with the ingredient name and the reference database_ID of the first hit in the database.
#'
#' @export

#Look through a list of ingredients
findFoodInDatabase <- function(df, database, additional_entries = NULL, fix_errors = TRUE, include_water = FALSE){

  #Helper function to look for one ingredient only
  temp <- function(ingredient, reference){
    #Reference is a list of names from a food database

    #Fill tibble with info
    results <- tibble(
      Ingredients = character(),
      database_reference = character(),
      database_ID = numeric(),
      loop = character()
    )


    #Look for both search terms in Ingredient
    for(i in 1:nrow(reference)){


      #Only look for the whole word found in the reference
      if(str_detect(ingredient, regex(paste0('\\b', reference$second_word[i], '\\b|\\b', reference$second_word[i], '\\w+'), ignore_case = TRUE)) &
         str_detect(ingredient, regex(paste0('\\b', reference$first_word[i], '\\b|\\b', reference$first_word[i], '\\w+'), ignore_case = TRUE)) ){

        results <- results %>%
          add_row(Ingredients = ingredient,
                  database_reference = paste0(reference$first_word[i], ', ', reference$second_word[i]),
                  database_ID = as.numeric(reference$database_ID[i]),
                  loop = 'first loop')

        #Break after first hit
        break
      }
    }

    #Look for foods not identified by both first and second word i ref
    if(!ingredient %in% results$Ingredients){

      for(i in 1:nrow(reference)){

        if (str_detect(ingredient, regex(paste0('\\b', reference$first_word[i], '\\b'), ignore_case = TRUE))  &
            isFALSE(str_detect(ingredient, regex(paste0('\\b', reference$second_word[i], '\\b|\\b', reference$second_word[i], '\\w+'), ignore_case = TRUE)) )){

          results <- results %>%
            add_row(Ingredients = ingredient,
                    database_reference = reference$first_word[i],
                    database_ID = as.numeric(reference$database_ID[i]),
                    loop = 'second loop')

          #Break after first hit
          break

        } else if (str_detect(ingredient, regex(paste0('\\b', reference$first_word[i], '\\w+'), ignore_case = TRUE)) &
                   reference$second_word[i] == '\\') {

          results <- results %>%
            add_row(Ingredients = ingredient,
                    database_reference = reference$first_word[i],
                    database_ID = as.numeric(reference$database_ID[i]),
                    loop = 'third loop')

          break
        }

      }


    }

    results

  }

  #Pull the reference database from sustainableNutRient internal data
  if(database == "volume_weight"){

    #Separate out ingredients with weight in kilo already, unnecessary to look through them
    if(length(unique(df$unit) > 1)) {

      ingredients_to_check <- df %>% filter(unit != 'kg')

    }else if(unique(df$unit) == "kg") {

      stop("All the ingredients are in kilograms, no need to check against volume weight database.")

    }

    #No additional entries for the database
    if(is.null(additional_entries)){

      #Database query words
      reference <- unit_weights_query %>%
        filter(.data$language == 'english') %>%
        select(-.data$language)

      #The database
      db <- unit_weights %>%
        filter(.data$language == 'english') %>%
        rename(unit = unit_enhet) %>%
        mutate(unit = unit %>%
                 str_replace('neve', 'handful')) %>%
        #Use brutto weight when possible
        group_by(Ingredients) %>%
        mutate(unit = case_when(
          any(unit == "brutto") ~ str_replace(unit, "brutto", "pcs"),
          TRUE ~ str_replace(unit, "netto", "pcs")
        )) %>% ungroup() %>%
        select(-c(Ingredients, .data$language, reference))

    }else if(!is.null(additional_entries)){

      #Check if input has required format
      if(isTRUE(is.list(additional_entries) &
                all.equal(names(additional_entries), c('db', 'query_words')) &
                all.equal(names(additional_entries$db), c('unit', 'grams_per_unit', 'database_ID')) &
                all.equal(names(additional_entries$query_words), c('first_word', 'second_word', 'database_ID')))
      ) {

        #Database query words
        reference <- unit_weights_query %>%
          filter(.data$language == 'english') %>%
          select(-.data$language) %>%
          bind_rows(additional_entries$query_words) %>% unique() %>%
          #Add user entries
          bind_rows(additional_entries$query_words) %>%
          #Arrange by lexogographical order and number of words in the first search term
          mutate(
            number_of_words1 = str_count(first_word, "\\b\\w+\\b"),
            number_of_words2 = str_count(second_word, "\\b\\w+\\b"),
            number_of_words2 = case_when(

              second_word == "\\" ~ 10,
              TRUE ~ number_of_words2

            )) %>%
          arrange(desc(number_of_words1), desc(number_of_words2), first_word, second_word) %>%
          select(-starts_with("number_of_words"))

        #The database
        db <- unit_weights %>%
          filter(.data$language == 'english') %>%
          rename(unit = unit_enhet) %>%
          mutate(unit = unit %>%
                   str_replace('neve', 'handful')) %>%
          #Use brutto weight when possible
          group_by(Ingredients) %>%
          mutate(unit = case_when(
            any(unit == "brutto") ~ str_replace(unit, "brutto", "pcs"),
            TRUE ~ str_replace(unit, "netto", "pcs")
          )) %>% ungroup() %>%
          select(-c(Ingredients, .data$language, reference)) %>%
          bind_rows(additional_entries$db) %>% unique()

      } else if(isFALSE(is.list(additional_entries) &
                       all.equal(names(additional_entries), c('db', 'query_words')) &
                       all.equal(names(additional_entries$db), c('unit', 'grams_per_unit', 'database_ID')) &
                       all.equal(names(additional_entries$query_words), c('first_word', 'second_word', 'database_ID')))
      ){
        stop("Have additional_entries been formatted correctly? Should be a list with two dataframes,
             'db' with names 'unit', 'grams_per_unit' and 'database_ID',
             and 'query_words' with names 'first_word', 'second_word' and 'database_ID",)
      }



    }


  } else if(database == "nutrients"){

    #No additional entries for the db, use nutRients defaults
    if(is.null(additional_entries)) {

      #Database query words
      reference <- matvaretabellen2024_query

      #The database, remove amount of water per food item unless user asks for it
      if(isTRUE(include_water)){
        db <- matvaretabellen2024
      } else if(isFALSE(include_water)){
        db <- matvaretabellen2024 %>%
          filter(nutrient != "Water")
      }


    } else if(!is.null(additional_entries)){

      #Check if formatting is correct
      if(isTRUE(is.list(additional_entries) & #If user supplies aditional endries, add to db and reference
                all.equal(names(additional_entries), c('db', 'query_words')) &
                all.equal(names(additional_entries$db), c('nutrient', 'nutrient_content_per_hektogram', 'database_ID')) &
                all.equal(names(additional_entries$query_words), c('first_word', 'second_word', 'database_ID')))
      ) {

        #Database query words
        reference <- matvaretabellen2024_query %>%
          #Add user entries
          bind_rows(additional_entries$query_words) %>%
          #Arrange by lexogographical order and number of words in the first search term
          mutate(
            number_of_words1 = str_count(first_word, "\\b\\w+\\b"),
            number_of_words2 = str_count(second_word, "\\b\\w+\\b"),
            number_of_words2 = case_when(

              second_word == "\\" ~ 10,
              TRUE ~ number_of_words2

            )) %>%
          arrange(desc(number_of_words1), desc(number_of_words2), first_word, second_word) %>%
          select(-starts_with("number_of_words"))

        #The database
        if(isTRUE(include_water)){
          db <- matvaretabellen2024 %>% bind_rows(additional_entries$db) #Add user entries
        } else if(isFALSE(include_water)){
          db <- matvaretabellen2024 %>% bind_rows(additional_entries$db) %>% #Add user entries
            #Remove water
            filter(nutrient != "Water")
          }

      } else {
        #Ask user to check additional entries formatting
        stop("Has additional_entries been formatted correctly?")
      }

    }

    #Ingredients
    ingredients_to_check <- df

  } else if(database == "sustainability"){

    #No additional entries for the db, use nutRients defaults
    if(is.null(additional_entries)) {

      #Database query words
      reference <- SHARP2018_query

      #The database
      db <- SHARP2018


    } else if(!is.null(additional_entries)){

      #Check if formatting is correct
      if(isTRUE(is.list(additional_entries) & #If user supplies aditional endries, add to db and reference
                all.equal(names(additional_entries), c('db', 'query_words')) &
                all.equal(names(additional_entries$db), c('environmental_impact_indicator', 'environmental_impact_per_hektogram', 'database_ID')) &
                all.equal(names(additional_entries$query_words), c('first_word', 'second_word', 'database_ID')))
      ) {

        #Database query words
        reference <- SHARP2018_query %>%
          #Add user entries
          bind_rows(additional_entries$query_words) %>%
          #Arrange by lexogographical order and number of words in the first search term
          mutate(
            number_of_words1 = str_count(first_word, "\\b\\w+\\b"),
            number_of_words2 = str_count(second_word, "\\b\\w+\\b"),
            number_of_words2 = case_when(

              second_word == "\\" ~ 10,
              TRUE ~ number_of_words2

            )) %>%
          arrange(desc(number_of_words1), desc(number_of_words2), first_word, second_word) %>%
          select(-starts_with("number_of_words"))

        #The database
        db <- SHARP2018 %>% bind_rows(additional_entries$db) #Add user entries

      } else {
        #Ask user to check additional entries formatting
        stop("Has additional_entries been formatted correctly?")
      }

    }

    #Ingredients
    ingredients_to_check <- df

  }

  #Run on all ingredients
  results <- lapply(ingredients_to_check$Ingredients %>% unique(), temp, reference = reference) %>% bind_rows()

  #Fix some known errors
  if(isTRUE(fix_errors)){

    #Fix volume weight hits
    if(database == 'volume_weight'){

      #Add ingredient metadata
      temp <- results %>%
        full_join(df) %>%

        #Fix errors
        mutate(database_ID = case_when(
          Ingredients == 'butter clarified ghee' ~ fixFoodMappingError(database = reference, 'ghee'),
          Ingredients == 'eggplant' ~ fixFoodMappingError(database = reference, 'eggplant'),
          Ingredients == 'sugar' ~ fixFoodMappingError(database = reference, 'sugar', 'white'),
          Ingredients == 'bread flat hard' ~ fixFoodMappingError(database = reference, 'flatbread', 'hard'),
          Ingredients == 'caper' ~ fixFoodMappingError(database = reference, 'capers'),
          Ingredients == 'cheese mozzarella' ~ fixFoodMappingError(database = reference, 'mozzarella'),
          Ingredients %in% c('chili pepper green', 'chili pepper jalapeno') ~ fixFoodMappingError(database = reference, 'chili', 'red'), #Same in volume
          Ingredients == 'mackerel tomato canned' ~ fixFoodMappingError(database = reference, 'mackerel', 'fillet'),
          Ingredients == 'peas green' ~ fixFoodMappingError(database = reference, 'pea', 'frozen'),
          Ingredients == 'pork neck chop' ~ fixFoodMappingError(database = reference, 'pork', 'neck'),
          Ingredients == 'sweet pepper grilled' & unit == 'pcs' ~ fixFoodMappingError(database = reference, 'sweet', 'pepper'),
          Ingredients == 'sweet pepper grilled' ~ fixFoodMappingError(database = reference, 'sweet pepper', 'grilled'),
          Ingredients == 'turkey chicken drumstick' ~ fixFoodMappingError(database = reference, 'turkey', 'drumstick'),
          Ingredients == 'lemongrass' ~ fixFoodMappingError(database = reference, 'lemongrass'),
          Ingredients == 'fig' ~ fixFoodMappingError(database = reference, 'fig'),
          Ingredients == 'shrimp' & unit == 'dl' ~ fixFoodMappingError(database = reference, 'shrimps', 'in'),
          Ingredients == 'bean white canned' ~ fixFoodMappingError(database = reference, 'bean white', 'canned'),
          Ingredients == 'bean kidney canned' ~ fixFoodMappingError(database = reference, 'bean kidney', 'canned'),
          Ingredients == 'bean black canned' ~ fixFoodMappingError(database = reference, 'bean black', 'canned'),
          Ingredients == 'chick pea canned' ~ fixFoodMappingError(database = reference, 'chick pea', 'canned'),
          Ingredients == 'mustard powder' ~ fixFoodMappingError(database = reference, 'mustard', 'powder'),
          str_detect(Ingredients, 'salad') & unit == 'pcs' &
            !str_detect(Ingredients,
                        'potato|shellfish|beetroot|mexican|mediterranean|indian|bulgur|buckwheat|couscous') ~ fixFoodMappingError(database = reference, 'heart', 'salad'),
          Ingredients %in% c('lettuce', 'salad lettuce') & unit == 'dl' ~ fixFoodMappingError(database = reference, 'iceberg', 'lettuce'),
          Ingredients == 'chopped parsley or generous sprinkling dill fronds, or mixture optional' ~ fixFoodMappingError(database = reference, 'parsley', 'fresh'),
          Ingredients == 'basil' & unit == 'twig' ~ fixFoodMappingError(database = reference, 'basil', 'fresh'),
          Ingredients == 'coriander' & unit == 'twig' ~ fixFoodMappingError(database = reference, 'coriander', 'fresh'),
          Ingredients %in% c('bean canned', 'bean black') ~ fixFoodMappingError(database = reference, 'bean black', 'canned'),
          Ingredients == 'rice brown long grain' ~ fixFoodMappingError(database = reference, 'rice'),
          str_detect(Ingredients, 'polenta') ~ fixFoodMappingError(database = reference, 'cornmeal', 'polenta'),
          Ingredients == 'corn' & unit == 'dl' ~ fixFoodMappingError(database = reference, 'corn', 'kernel'),
          Ingredients %in% c('cranberries jam', 'jam apricot') ~ fixFoodMappingError(database = reference, 'jam', 'marmalade'),
          Ingredients == 'hamburger bun' ~ fixFoodMappingError(database = reference, 'hamburger', 'bread'),
          Ingredients == 'mustard honey' ~ fixFoodMappingError(database = reference, 'mustard'),
          Ingredients %in% c('salad lollo rosso', 'salad heart') & unit == 'leaf' ~ fixFoodMappingError(database = reference, 'lettuce'),
          Ingredients == 'bread crumb' & unit == 'slice' ~ fixFoodMappingError(database = reference, 'bread'),
          Ingredients == 'cheese goat chevre white' & unit == 'pcs' ~ fixFoodMappingError(database = reference, 'soft ripened cheese'),
          Ingredients == 'watermelon' ~ fixFoodMappingError(database = reference, 'melon', 'water'),
          Ingredients == 'broccolini' ~ fixFoodMappingError(database = reference, 'broccolini'),
          Ingredients == 'scampi' & unit == 'pcs' ~ fixFoodMappingError(database = reference, 'prawn'),
          (str_detect(Ingredients, 'salsa') & !str_detect(Ingredients, "tomato|chunky")) & unit == 'dl' ~ fixFoodMappingError(database = reference, 'salsa'),
          Ingredients == 'sausage turkey chicken' ~ fixFoodMappingError(database = reference, 'sausage turkey chicken'),
          Ingredients %in% c('chili peppers', 'chili', 'strong chili') ~ fixFoodMappingError(database = reference, 'chili', 'red'),
          Ingredients == 'corn flour' ~ fixFoodMappingError(database = reference, 'cornmeal', 'polenta'),
          Ingredients == 'chicken breast' & unit == 'dl' ~ fixFoodMappingError(database = reference, 'chicken', 'diced'),
          Ingredients == 'salad rocket' ~ fixFoodMappingError(database = reference, 'ruccola'),
          Ingredients == 'lime leaf' ~ fixFoodMappingError(database = reference, 'bay', 'leaf'), #Assume similar
          Ingredients == 'oat quick' ~ fixFoodMappingError(database = reference, 'rolled', 'oat'),
          Ingredients == 'peach canned' ~ fixFoodMappingError(database = reference, 'peach', 'canned'),
          Ingredients == 'bread brown chapati' & unit == 'pcs' ~ fixFoodMappingError(database = reference, 'chapati'), #Assume similar
          str_detect(Ingredients, 'ice cream') & !str_detect(Ingredients, 'cake') ~ fixFoodMappingError(database = reference, 'ice cream'),
          Ingredients == 'pimiento chili pepper' ~ fixFoodMappingError(database = reference, 'chili', 'red'), #Small pepper
          Ingredients == "chili pepper jalapeno pickled" & unit == "pcs" ~ fixFoodMappingError(database = reference, 'jalapeño'), #Regular jalapeno
          str_detect(Ingredients, "granola") ~ fixFoodMappingError(database = reference, 'granola'),
          Ingredients %in% c("rolls white baguette garlic") ~ fixFoodMappingError(database = reference, 'baguette', 'half'),

          #Cheeses
          str_detect(Ingredients, 'cheddar|jarlsberg|norvegia|semi-hard|cheese romano') ~ fixFoodMappingError(database = reference, 'hard to semi-hard cheese'),
          Ingredients %in% c("cheese cheddar", "cheese semi-hard") &
            unit == "slice" ~ fixFoodMappingError(database = reference, 'hard to semi-hard cheese'), #Standard Norwegian white cheese slices
          Ingredients %in% c("cheese pizza", 'cheese manchego') ~ fixFoodMappingError(database = reference, 'hard to semi-hard cheese'),
          Ingredients %in%
            c('cheese brie', 'cheese mascarpone', 'cheese camembert','cheese le crémier de chaumes',
              'cheese goat chevre white', 'cheese goat') ~ fixFoodMappingError(database = reference, 'soft ripened cheese'),

          #Similar ingredients to exchange for each other
          Ingredients == 'garlic wild' ~ fixFoodMappingError(database = reference, 'scallion'),
          Ingredients == 'agave nectar' ~ fixFoodMappingError(database = reference, 'honey'),
          Ingredients %in% c('shortening', 'shortening vegetable') ~ fixFoodMappingError(database = reference, 'margarine'),
          Ingredients %in% c('onion seed', 'chia seeds') ~ fixFoodMappingError(database = reference, 'poppy', 'seed'), #Both are small seeds
          Ingredients == 'graham cracker' ~ fixFoodMappingError(database = reference, 'cracker', 'cream'),
          Ingredients == 'marjoram fresh herbs' ~ fixFoodMappingError(database = reference, 'oregano', 'fresh'),
          Ingredients %in% c('harissa mild', "harissa") ~ fixFoodMappingError(database = reference, 'chili', 'paste'),
          Ingredients == 'spread speculaas' ~ fixFoodMappingError(database = reference, 'peanut', 'butter'),
          Ingredients == 'onion pickled' ~ fixFoodMappingError(database = reference, 'beetroot', 'pickled'),
          Ingredients == 'pizza sauce red' ~ fixFoodMappingError(database = reference, 'tomato', 'canned'),
          Ingredients %in% c("margarine", "lard pork fat", "bacon fat") ~ fixFoodMappingError(database = reference, 'butter'), #Similar
          str_detect(Ingredients, 'spice mix') & unit == 'pack' ~ fixFoodMappingError(database = reference, 'taco', 'spice'),
          str_detect(Ingredients, 'plantain') & unit == 'dl' ~ fixFoodMappingError(database = reference, 'banana'), #Similar
          Ingredients == 'mangold' ~ fixFoodMappingError(database = reference, 'celery', 'stalk'), #Similar
          str_detect(Ingredients, "quark") ~ fixFoodMappingError(database = reference, 'cottage', 'cheese'),
          Ingredients == "garlic powder" ~ fixFoodMappingError(database = reference, 'onion', 'powder'),
          Ingredients == "remulade" ~ fixFoodMappingError(database = reference, "mayonnaise"),
          Ingredients %in% c("cheese plant-based", 'cheese, plant-based') ~ fixFoodMappingError(database = reference, "dairy imitate"),
          Ingredients == "almond butter" ~ fixFoodMappingError(database = reference, 'peanut', 'butter'),
          Ingredients == "barbecue seasoning" ~ fixFoodMappingError(database = reference, 'taco', 'spice'),
          Ingredients %in% c("syrup chocolate", "syrup currant", "syrup blackcurrant", 'glucose') ~ fixFoodMappingError(database = reference, 'syrup'),
          Ingredients %in% c("erythriol", 'pearl sugar') ~ fixFoodMappingError(database = reference, 'sugar', 'white'),
          Ingredients == "egg yolk boiled" ~ fixFoodMappingError(database = reference, 'egg', 'yolk'),
          Ingredients == "o'boy pulver" ~ fixFoodMappingError(database = reference, 'cocoa', 'powder'),

          #Ingredients with no references
          ((Ingredients %in% c('mustard powder', 'chinese five spice', 'dip mix', 'asafoetida powder', 'lemon gel',
                              'sauce browning', 'trout caviar', 'whip it stabilizer', 'vanillin', 'turkey offal',
                              'sugar color') |
             str_detect(Ingredients, 'powder mix')) &
            #If user have added these ingredients, keep
            !str_detect(database_ID, ".999")) ~ 0,

          TRUE ~ database_ID
        ))

      #Add the new reference words and join with the database
      results <- temp %>%
        left_join(reference %>% filter(database_ID %in% temp$database_ID)) %>%
        mutate(database_reference = paste0(first_word, ' ', second_word)) %>%
        #Remove unnecessary columns
        select(-c(first_word, second_word, .data$loop)) %>% unique() %>%
        left_join(db,
                  by = c("database_ID", "unit")) %>%
        mutate(
          grams_per_unit = case_when(unit == 'kg' ~ 1000, TRUE ~ grams_per_unit),
          database_reference = case_when(
            unit == 'kg' ~ "Not looked up in database as weight already given",
            is.na(Amounts) ~ "Not looked up in database as no amounts given",
            is.na(database_ID) ~ "Not found in database",
            TRUE ~ database_reference
          ))

    } else if(database == 'nutrients'){

      #Add ingredient metadata
      temp <- results %>%
        full_join(df) %>%

        #Fix errors
        mutate(database_ID = case_when(

          #Fruit and veg
          Ingredients == 'eggplant' ~ fixFoodMappingError(database = reference, 'eggplant'),
          Ingredients == 'peach' ~ fixFoodMappingError(database = reference, 'peach'),
          Ingredients %in% c('red chili', 'strong chili', 'chili peppers') ~ fixFoodMappingError(database = reference, 'chili pepper', 'red'),
          Ingredients == 'sweet corn kernels' ~ fixFoodMappingError(database = reference, 'sweet corn', 'canned'),
          Ingredients == 'sweet potato' ~ fixFoodMappingError(database = reference, 'sweet potato'),
          Ingredients %in% c('pimiento chili pepper', 'sweet pepper grilled', 'sweet pepper canned') ~ fixFoodMappingError(database = reference, 'sweet pepper', 'red'),
          Ingredients %in% c('sweet pepper pickled', 'chili pepper jalapeno pickled') ~ fixFoodMappingError(database = reference, 'chili pepper', 'pickled'),
          Ingredients == c('chili pepper dried') ~ fixFoodMappingError(database = reference, 'chili pepper', 'red'),
          Ingredients %in% c('potato', 'potato boiled') ~ fixFoodMappingError(database = reference, 'potato'),
          Ingredients == 'jerusalem artichoke' ~ fixFoodMappingError(database = reference, 'jerusalem artichoke'),
          Ingredients == 'mangold' ~ fixFoodMappingError(database = reference, 'mangold'),
          str_detect(Ingredients, 'butternut') ~ fixFoodMappingError(database = reference, 'winter squash', 'butternut'),
          Ingredients == 'watermelon' ~ fixFoodMappingError(database = reference, 'watermelon'),
          Ingredients == 'salsa' ~ fixFoodMappingError(database = reference, 'chunky', 'salsa'),
          Ingredients == 'pear' ~ fixFoodMappingError(database = reference, 'pear'),
          Ingredients == 'jam blueberries' ~ fixFoodMappingError(database = reference, 'jam'),
          Ingredients == 'tomato beef' ~ fixFoodMappingError(database = reference, 'tomato'),
          Ingredients == "grape fruit" ~  fixFoodMappingError(database = reference, 'grapefruit'),

          #Dairy
          Ingredients == 'parmesan cheese' |
            #Can be substituted for eachother in recipes
            Ingredients %in% c('cheese asiago', 'cheese cotjia', 'cheese pecorino',
                               'cheese romano', 'cheese gruyere', 'cheese parmigiano reggiano') ~ fixFoodMappingError(database = reference, 'cheese', 'parmesan'),
          Ingredients == 'butter clarified ghee' ~ fixFoodMappingError(database = reference, 'ghee'),
          Ingredients == 'cheese cottage low fat' ~ fixFoodMappingError(database = reference, 'cheese cottage', 'low fat'),
          Ingredients == 'cheese cottage' ~ fixFoodMappingError(database = reference, 'cheese cottage'),
          Ingredients %in% c('cheese goat chevre white', 'cheese goat') ~ fixFoodMappingError(database = reference, 'cheese', 'chevre'), #Goat cheese
          Ingredients %in% c('cheese cream', 'cheese soft', 'cheese le crémier de chaumes') ~ fixFoodMappingError(database = reference, 'cheese cream'),
          Ingredients == 'cheese hard goat' ~ fixFoodMappingError(database = reference, 'cheese hard goat', 'kvitlin'), #Use as standard for time being
          Ingredients == 'cheese jarlsberg' ~ fixFoodMappingError(database = reference, 'cheese', 'jarlsberg'),
          Ingredients == 'cheese manchego' ~ fixFoodMappingError(database = reference, 'cheese', 'cheddar'), #Can be substituted in recipes
          Ingredients == 'cheese mozzarella' ~ fixFoodMappingError(database = reference, 'cheese', 'mozzarella'),
          Ingredients == 'cheese norvegia' |
            Ingredients %in% c('cheese semi-hard', 'cheese emmentaler', 'cheese garlic', 'cheese pizza')
            ~ fixFoodMappingError(database = reference, 'cheese', 'norvegia'),
          Ingredients == 'cheese ricotta salata' ~ fixFoodMappingError(database = reference, 'cheese', 'ricotta salata'),
          Ingredients == 'cheese port salut' ~ fixFoodMappingError(database = reference, 'cheese', 'port salut'),
          Ingredients == 'cheese burrata mozzarella' ~ fixFoodMappingError(database = reference, 'cheese', 'mozzarella'),
          Ingredients == 'goat brown cheese' ~ fixFoodMappingError(database = reference, 'cheese brown_goat'),
          Ingredients %in% c('cheese cream goat sn\u00f8frisk', 'cream cheese goat') ~ fixFoodMappingError(database = reference, 'cheese cream', 'goat sn\u00f8frisk'),
          Ingredients == 'cheese mascarpone' ~ fixFoodMappingError(database = reference, 'cheese', 'mascarpone'),
          Ingredients == 'tine light 2 \u0025 a good alternative to sour cream' ~ fixFoodMappingError(database = reference, 'quark', '1'), #Closest in nutritional value
          Ingredients == 'milk evaporated' ~ fixFoodMappingError(database = reference, 'milk evaporated'),
          Ingredients == 'buttermilk' ~ fixFoodMappingError(database = reference, 'buttermilk'),
          #Milk with cocoa powder
          str_detect(Ingredients, 'milk beverage chocolate') ~ fixFoodMappingError(database = reference, 'milk beverage', 'chocolate'),
          Ingredients %in% c('cheese blue', 'cheese blue castello') ~ fixFoodMappingError(database = reference, 'cheese blue'),

          #Div
          Ingredients %in% c('mushroom', 'mushroom chestnut') ~ fixFoodMappingError(database = reference, 'mushroom'),
          Ingredients == 'sesame seed oil' ~ fixFoodMappingError(database = reference, 'sesame', 'oil'),
          Ingredients %in% c('sugar', 'pearl sugar') ~  fixFoodMappingError(database = reference, 'sugar'),
          str_detect(Ingredients, 'water broth') ~ fixFoodMappingError(database = reference, 'water'),
          database_reference == 'mushroom' & !str_detect(Ingredients, 'condensed cream of mushroom soup|sauce') ~ fixFoodMappingError(database = reference, 'mushroom'),
          Ingredients == 'condensed cream of celery soup' ~ fixFoodMappingError(database = reference, 'condensed cream of celery soup'),
          Ingredients == 'condensed cream of chicken soup' ~ fixFoodMappingError(database = reference, 'condensed cream of chicken soup'),
          Ingredients == 'condensed cream of mushroom soup' ~ fixFoodMappingError(database = reference, 'condensed cream of mushroom soup'),
          Ingredients == 'oil corn' ~ fixFoodMappingError(database = reference, 'vegetable', 'oil'),
          Ingredients == 'soup onion instant' ~ fixFoodMappingError(database = reference, 'onion soup mix'),
          Ingredients == 'sauce hot pepper' ~ fixFoodMappingError(database = reference, 'hot pepper sauce'),
          Ingredients == 'sauce pasta' ~ fixFoodMappingError(database = reference, 'tomato', 'sauce'), #Use as substitute for time being
          Ingredients == 'sauce hot' ~ fixFoodMappingError(database = reference, 'hot pepper sauce'),
          Ingredients == 'olive paste tapenade' ~ fixFoodMappingError(database = reference, 'olive paste tapenade'),
          Ingredients == 'homemade beef gravy' ~ fixFoodMappingError(database = reference, 'beef gravy'),
          Ingredients == 'sweet chili sauce' ~ fixFoodMappingError(database = reference, 'chili sauce', 'sweet'),
          Ingredients == 'refrigerated buttermilk biscuit dough' ~ fixFoodMappingError(database = reference, 'refrigerated buttermilk biscuit dough'),
          Ingredients == 'beef gravy' ~ fixFoodMappingError(database = reference, 'beef', 'gravy'),
          Ingredients == 'sauce piri-piri' ~ fixFoodMappingError(database = reference, 'sauce piri-piri'),
          Ingredients == 'sauce tikka masala' ~ fixFoodMappingError(database = reference, 'sauce tikka masala'),
          Ingredients == 'sauce pad thai' ~ fixFoodMappingError(database = reference, 'sauce pad thai'),
          Ingredients == 'ice cube' ~  fixFoodMappingError(database = reference,'water'),

          #Grains, seeds nuts
          Ingredients == 'chick pea' ~ fixFoodMappingError(database = reference, 'chick pea'),
          Ingredients == 'rice white long grain' ~ fixFoodMappingError(database = reference, 'rice white long grain'),
          Ingredients == 'dried soybeans' ~ fixFoodMappingError(database = reference, 'bean', 'soya'),
          Ingredients %in% c('cashew nut salt', 'cashew nut roasted') ~ fixFoodMappingError(database = reference, 'cashew', 'nut'),
          Ingredients %in% c('bread crumb', 'bread', 'bread naan', 'breadstick') ~ fixFoodMappingError(database = reference, 'bread'),
          Ingredients %in% c('crisp bread', 'crisp bread coarse') ~ fixFoodMappingError(database = reference, 'crisp bread', 'coarse'),
          Ingredients %in% c('rolls white baguette garlic', 'bread sausage', 'hamburger bread') ~ fixFoodMappingError(database = reference, 'bread', 'white'),
          Ingredients %in% c('bread brown chapati', 'tortilla coarse', 'rolls coarse', 'rolls coarse baguette') ~ fixFoodMappingError(database = reference, 'bread', 'coarse'),
          Ingredients == 'bean canned' ~ fixFoodMappingError(database = reference, 'bean', 'kidney canned'), #Standard
          Ingredients == 'peanut' ~ fixFoodMappingError(database = reference, 'peanut'),
          Ingredients == 'peanut salt' ~ fixFoodMappingError(database = reference, 'peanut', 'salt'),
          Ingredients == 'rice parboiled' ~ fixFoodMappingError(database = reference, 'rice parboiled'),
          Ingredients == 'rice brown long grain' ~ fixFoodMappingError(database = reference, 'rice brown long grain'),
          Ingredients == 'white bread mix' ~ fixFoodMappingError(database = reference, 'white bread', 'mix'),
          Ingredients %in% c('flaxseed meal', 'seed flax') ~ fixFoodMappingError(database = reference, 'flax', 'seed'),
          Ingredients == 'cookies amarettini' ~ fixFoodMappingError(database = reference, 'amaretti cookie'),
          Ingredients == 'bean salad' ~ fixFoodMappingError(database = reference, 'bean salad'),
          Ingredients == 'taco shell' ~ fixFoodMappingError(database = reference, 'nacho'),
          Ingredients == 'lasagna plate pasta' ~ fixFoodMappingError(database = reference, 'pasta'),
          str_detect(Ingredients, 'barley cooked') ~ fixFoodMappingError(database = reference, 'barley', 'cooked'),

          #Seafood
          Ingredients == 'cod lutefisk' ~ fixFoodMappingError(database = reference, 'lutefisk'),
          Ingredients == 'mackerel tomato canned' ~ fixFoodMappingError(database = reference, 'mackerel', 'tomato canned'),
          Ingredients == 'fish cakes coarse' ~ fixFoodMappingError(database = reference, 'fish cakes', 'coarse'),

          #Herbs spices and condiments
          Ingredients == 'parsley' ~ fixFoodMappingError(database = reference, 'parsley', 'fresh'),
          Ingredients == 'dry mustard' ~ fixFoodMappingError(database = reference, 'mustard'),
          Ingredients == 'mayonnaise sauce' ~ fixFoodMappingError(database = reference, 'mayonnaise'),
          Ingredients == 'chili flake dried' ~ fixFoodMappingError(database = reference, 'chili', 'powder'),
          str_detect(Ingredients, 'vinegar') & !str_detect(database_reference, 'vinegar') ~ fixFoodMappingError(database = reference, 'vinegar'),
          Ingredients == 'mustard honey' ~ fixFoodMappingError(database = reference, 'mustard'),
          Ingredients == 'lemongrass' ~ fixFoodMappingError(database = reference, 'lemongrass'),
          Ingredients == 'spice mix taco' ~ fixFoodMappingError(database = reference,'taco spice mix'),
          Ingredients == 'lemon balm' ~ fixFoodMappingError(database = reference,'mint', 'fresh'),

          #Meat
          Ingredients == 'pork neck chop' ~ fixFoodMappingError(database = reference, 'pork', 'neck chop'),
          Ingredients == 'sausage' ~  fixFoodMappingError(database = reference, 'sausage'),
          Ingredients == 'chicken' ~ fixFoodMappingError(database = reference, 'chicken', 'whole'),
          str_detect(Ingredients, 'grouse') ~ fixFoodMappingError(database = reference, 'grouse'),
          Ingredients == 'whole turkey' ~ fixFoodMappingError(database = reference, 'turkey', 'meat'),
          Ingredients == 'pork neck' ~ fixFoodMappingError(database = reference, 'pork', 'neck chop'),
          Ingredients == "meatballs in tomato sauce" ~ fixFoodMappingError(database = reference, "meatball", "tomato sauce"),
          Ingredients == "bacon fat" ~ fixFoodMappingError(database = reference, "pork", "lard"), #Closest match
          Ingredients == 'chicken wing' ~ fixFoodMappingError(database = reference, "chicken", "drumstick"),
          Ingredients == 'hamburger beef patty' ~ fixFoodMappingError(database = reference, "beef", "minced meat"),
          Ingredients == 'lamb ribs' ~ fixFoodMappingError(database = reference, "lamb", "chop"),

          #Substitutions or ingredients not found in Matvaretabellen
          Ingredients %in% c('garlic oil', 'oil truffle') ~ fixFoodMappingError(database = reference, 'olive', 'oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
          Ingredients %in% c('frying oil', 'oil') ~ fixFoodMappingError(database = reference, 'vegetable', 'oil'),
          Ingredients == 'hazelnut oil' ~ fixFoodMappingError(database = reference, 'walnut', 'oil'), #Another nut oil
          Ingredients == 'bean canned' ~ fixFoodMappingError(database = reference, 'bean black', 'canned'),
          Ingredients == 'scampi' ~ fixFoodMappingError(database = reference, 'shrimp'),
          Ingredients == 'ciabatta' ~ fixFoodMappingError(database = reference, 'bread', 'white'),
          Ingredients == 'elk shoulder' ~ fixFoodMappingError(database = reference, 'elk moose'),
          Ingredients == 'elk tenderloin' ~ fixFoodMappingError(database = reference, 'beef', 'tenderloin'),
          Ingredients == 'lime, the zest' ~ fixFoodMappingError(database = reference, 'lemon', 'zest'),
          Ingredients %in% c('salsa', 'salsa tomato') ~ fixFoodMappingError(database = reference, 'chunky', 'salsa'),
          Ingredients %in% c('syrup apple', 'syrup pear') ~ fixFoodMappingError(database = reference, 'syrup', 'maple'),
          Ingredients == 'sugar vanilla' ~ fixFoodMappingError(database = reference, 'sugar'),
          Ingredients %in% c('beef shank', 'beef oxtail') ~ fixFoodMappingError(database = reference, 'beef', 'veal chops'), #Cut with the highest percentage of bone in Matvaretabellen
          Ingredients %in% c('syrup apple', 'syrup pear', 'syrup currant', 'syrup', 'agave syrup',
                             'syrup ginger', 'syrup chocolate', 'syrup caramel', 'glucose') ~ fixFoodMappingError(database = reference, 'syrup', 'maple'),
          Ingredients == 'cream double 48 \u0025' ~ fixFoodMappingError(database = reference, 'cream whipped', '37'), #Highest in the database
          Ingredients == 'chocolate unsweetened' ~ fixFoodMappingError(database = reference, 'chocolate', 'dark'), #Highes cocoa percentage in database
          Ingredients == 'aioli' ~ fixFoodMappingError(database = reference, 'mayonnaise'), #Similar
          Ingredients == 'trout smoked' ~ fixFoodMappingError(database = reference, 'salmon', 'smoked'),
          Ingredients == 'fresh herbs ginger' ~ fixFoodMappingError(database = reference, 'ginger'),
          Ingredients == "butter plant-based" ~ fixFoodMappingError(database = reference, 'margarine'),
          Ingredients == 'apple cider' ~ fixFoodMappingError(database = reference, 'cider'),
          Ingredients == 'currant juice' ~ fixFoodMappingError(database = reference, 'black currant', 'juice'),

          #Not in reference
          ((Ingredients %in% c('duck or goose fat for confit', 'lime leaf', "beans'n'pork canned", 'onion seed',
                             'cooking spray', 'red food coloring', 'beef fund', 'fish scraps for broth', 'chili bean paste sichuan',
                             'pack high quality charcoal briquettes', 'pomegranate kernel', 'condensed tomato soup',
                             'salmon roe', 'spice seasoning pepper', 'toro greek moussaka', 'paste chili', 'carbonated beverage lemon-lime',
                             'fish soup base', 'spice mix guacamole', 'lamb sheep head', 'can tomato soup', 'sauce white',
                             'marrow bone', 'rhubarb juice', 'beef bones', 'whip it stabilizer', 'toenjang soybean paste',
                             '20 pound pack high quality charcoal briquettes', 'wine rice', 'trout caviar', 'vanillin', 'cream sauce base',
                             'vanilla pod', 'butter-vanilla aroma', 'paste vanilla bean', 'blueberries pie filling', 'almond essence', 'vanilla essence',
                             'milk powder nonfat', 'apricot nectar', 'apricot preserve', 'apple sauce', 'oil chili sichuan',
                             'frozen vegetable mix', 'sauce curry', 'gingerbread house', 'lemonade',
                             'sweet potato fries', 'coriander paste', 'vanilla essence', 'vanilla pod',
                             'cheese schnitzel', 'cheese pancakes', 'honey crunch', 'lemon gel', 'mint jelly',
                             'prune marinade', 'roe salmon', 'vanilla pod', 'vanilla powder', 'vanilla essence',
                             'quick lunch', 'prune marinade', 'orange liqueur', 'turkey offal') |
              str_detect(Ingredients, 'spice mix(?! taco)|powder mix|soup instant')
            )  &
            #If user have added these ingredients, keep
            !str_detect(database_ID, ".999")) ~ 0,

          # Extra check for instant soups not in ref
           (str_detect(Ingredients, 'soup') & str_detect(Ingredients, 'instant') & !str_detect(database_reference, 'instant')) &
            !(str_detect(database_ID, ".999") & str_detect(database_reference, 'instant')) ~ 0,

          TRUE ~ database_ID
        ))

      #Add the new reference words
      results <- temp %>%
        left_join(reference, by = 'database_ID') %>%
        mutate(database_reference = paste0(first_word, ' ', second_word)) %>%
        #Remove unnecessary columns
        select(-c(first_word, second_word, .data$loop)) %>% unique() %>%
        left_join(db,
                  by = "database_ID") %>%
        select(-.data$from) %>%
        mutate(database_reference = str_replace(database_reference, "NA NA", "Not in database"))


    } else if(database == 'sustainability'){

      #Add ingredient metadata
      temp <- results %>%
        full_join(df)

        #Fix errors
        temp <- temp %>% mutate(database_ID = case_when(

          #Grains, nuts, seeds, legumes
          Ingredients == 'espresso bean coffee ground' ~ fixFoodMappingError(database = reference, 'coffee ground'),
          Ingredients == 'hazelnut' ~ fixFoodMappingError(database = reference, 'nut', 'hazel'),
          str_detect(Ingredients, 'lentils dried') ~ fixFoodMappingError(database = reference, 'lentil', 'dry'),
          Ingredients %in% c('peas green', 'stew peas') ~ fixFoodMappingError(database = reference, 'pea', 'garden'),
          Ingredients %in% c('bean green asparagus', 'bean green', 'bean broad') ~ fixFoodMappingError(database = reference, 'bean with pods', 'with'),
          str_detect(Ingredients, 'bean|chick pea') &
            !str_detect(Ingredients, 'canned|sprout|oil|flour|paste|coffee ground|white tomato|taco|salad') ~ fixFoodMappingError(database = reference, 'beans', 'dry'),
          str_detect(Ingredients, 'bean|chick pea|lentils') & str_detect(Ingredients, 'canned') |
            Ingredients %in% c('bean white tomato', 'chili beans', 'bean mixed', 'refried beans') ~ fixFoodMappingError(database = reference, 'bean', 'canned'),
          str_detect(Ingredients, 'noodle') ~ fixFoodMappingError(database = reference, 'noodle'),
          Ingredients == 'pistachio nut' ~ fixFoodMappingError(database = reference, 'pistachio'),
          Ingredients == 'dried soybeans' ~ fixFoodMappingError(database = reference, 'bean', 'soy'),
          Ingredients == 'pecan' ~ fixFoodMappingError(database = reference, 'tree', 'nut'),
          Ingredients == 'tahini' ~ fixFoodMappingError(database = reference, 'sesame', 'seed'),
          Ingredients %in% c('seed flax', 'flaxseed meal') ~ fixFoodMappingError(database = reference, 'linseed'),
          Ingredients == 'corn starch' ~ fixFoodMappingError(database = reference, 'corn', 'flour'), #Use as substitute
          Ingredients == 'chick pea flour' ~ fixFoodMappingError(database = reference, 'chick pea', 'flour'),
          Ingredients == 'bean salad' ~ fixFoodMappingError(database = reference, 'bean salad'),
          Ingredients == 'lasagna plate pasta' ~ fixFoodMappingError(database = reference, 'pasta'),

          #Veggies and fruit
          str_detect(Ingredients, 'pickled') & str_detect(Ingredients, 'ginger|sweet pepper|cucumber|onion|beetroot|chili|jalapeno') ~ fixFoodMappingError(database = reference, 'vegetables', 'pickled'),
          str_detect(Ingredients, 'canned') & str_detect(Ingredients, 'sweet pepper|sweet corn|artichoke') ~ fixFoodMappingError(database = reference, 'vegetables', 'canned'),
          str_detect(Ingredients, 'endive|chicory') ~ fixFoodMappingError(database = reference, 'curly', 'endives'),
          Ingredients == 'peach' ~ fixFoodMappingError(database = reference, 'peaches', 'other'),
          Ingredients == 'sorrel' ~ fixFoodMappingError(database = reference, 'lettuce', 'other'),
          str_detect(Ingredients, 'winter squash') ~ fixFoodMappingError(database = reference, 'pumpkin'),
          str_detect(Ingredients, 'eggplant') ~ fixFoodMappingError(database = reference, 'eggplant'),
          Ingredients == 'garlic chinese' ~ fixFoodMappingError(database = reference, 'garlic'),
          Ingredients %in% c('corn baby', 'corn cob') ~ fixFoodMappingError(database = reference, 'sweet', 'corn'),
          Ingredients == 'mangold' ~ fixFoodMappingError(database = reference, 'chard'),
          Ingredients == 'olive black' ~ fixFoodMappingError(database = reference, 'olives', 'canned'),
          Ingredients %in% c('olive green', 'of olives') ~ fixFoodMappingError(database = reference, 'olives', 'fresh'),
          Ingredients == 'olive green' ~ fixFoodMappingError(database = reference, 'olives', 'canned'),
          Ingredients %in% c('red chili', 'strong chili', 'chili peppers') ~ fixFoodMappingError(database = reference, 'chili', 'pepper'),
          Ingredients %in% c('tomato bunch', 'tomato beef') ~ fixFoodMappingError(database = reference, 'tomato'),
          Ingredients %in% c('salad', 'salad heart', 'salad lollo rosso') ~ fixFoodMappingError(database = reference, 'head', 'lettuce'),
          Ingredients == 'salad crispi' ~ fixFoodMappingError(database = reference, 'crisp', 'lettuce'),
          Ingredients == 'salsa tomato' ~ fixFoodMappingError(database = reference, 'chunky', 'salsa'), #Standard
          Ingredients %in% c('tomato sun dried', 'tomato sun dried in oil') ~ fixFoodMappingError(database = reference, 'tomato', 'sun-dried'),
          str_detect(Ingredients, 'tomato canned') & !str_detect(Ingredients, 'mackerel') ~ fixFoodMappingError(database = reference, 'preserved', 'tomato'),
          Ingredients == 'watermelon' ~ fixFoodMappingError(database = reference, 'watermelons'), #Fix ref query to water melon
          str_detect(Ingredients, 'the zest') ~ fixFoodMappingError(database = reference, 'citrus', 'fruit'), #Reference for citrus fruit peel
          Ingredients == 'clementine' ~ fixFoodMappingError(database = reference, 'mandarin'),
          Ingredients == 'black currant' ~ fixFoodMappingError(database = reference, 'blackcurrant'),
          Ingredients == 'currant' ~ fixFoodMappingError(database = reference, 'redcurrant'),
          Ingredients == 'tamarind juice' ~ fixFoodMappingError(database = reference, 'fruit', 'juice'),
          Ingredients == 'salsa' ~ fixFoodMappingError(database = reference, 'chunky', 'salsa'),
          Ingredients %in% c('syrup apple', 'syrup pear', 'syrup ginger', 'syrup currant',
                             'syrup chocolate', 'syrup caramel', 'glucose') ~ fixFoodMappingError(database = reference, 'syrup'),
          Ingredients == 'apricot nectar' ~ fixFoodMappingError(database = reference, 'fruit', 'nectars'),
          Ingredients %in% c(
            "cloud berry", "lingonberry", "goji berry", "physalis") ~ fixFoodMappingError(database = reference, 'berries'),
          Ingredients %in% c("grape fruit") ~ fixFoodMappingError(database = reference, 'grapefruit'),
          Ingredients == "melon honeydew" ~ fixFoodMappingError(database = reference, 'melon'),
          Ingredients == 'sprouts radish' ~ fixFoodMappingError(database = reference, 'sprout'),
          Ingredients %in% c("morel") ~ fixFoodMappingError(database = reference, 'cherries', 'sour'),
          Ingredients %in% c("jackfruit", "nectarine") ~ fixFoodMappingError(database = reference, 'fig'), #In the same family
          Ingredients %in% c("tangerine canned") ~ fixFoodMappingError(database = reference, 'fruit', 'canned'),
          Ingredients == 'starfruit' ~ fixFoodMappingError(database = reference, 'fruit', 'used'),
          Ingredients %in% c('blueberries dried', 'cranberries dried') ~ fixFoodMappingError(database = reference, 'fruit', 'dried'),

          #Red meat
          str_detect(Ingredients, 'reindeer|\\belk ') ~ fixFoodMappingError(database = reference, 'mammals', 'meat'),
          str_detect(Ingredients, 'pork') & !str_detect(Ingredients, 'lard') ~ fixFoodMappingError(database = reference, 'pork'),
          Ingredients %in% c('lamb sheep cabbage stew meat', 'lamb sheep head', 'lamb minced meat') ~ fixFoodMappingError(database = reference, 'lamb', 'fresh'),
          Ingredients == "meatballs in tomato sauce" ~ fixFoodMappingError(database = reference, "meatball", "tomato sauce"),
          Ingredients %in% c("beef minced meat", "beef minced meat 6") ~ fixFoodMappingError(database = reference, "beef"),

          #Poultry
          str_detect(Ingredients, 'turkey') & !str_detect(Ingredients, 'broth') ~ fixFoodMappingError(database = reference, 'turkey'),
          Ingredients %in% c('hen breast fillet grouse', 'hen', 'hen grouse') ~ fixFoodMappingError(database = reference, 'poultry', 'fresh'), #All poultry meats have the same CO2 and landuse in the db

          #Seafood
          Ingredients == 'scampi' ~ fixFoodMappingError(database = reference, 'prawn'),
          Ingredients %in% c('arctic char', 'char') ~ fixFoodMappingError(database = reference, 'trout'), #Look up other alternatives
          Ingredients %in% c('catfish', 'flounder') ~ fixFoodMappingError(database = reference, 'miscellaneous', 'demersal'), #Steinbit
          Ingredients %in% c('salmon roe', 'roe', 'roe salmon') ~ fixFoodMappingError(database = reference, 'fish', 'roe'),
          Ingredients == 'fish cakes coarse' ~ fixFoodMappingError(database = reference, 'fish cakes coarse'),
          Ingredients == 'cockles' ~ fixFoodMappingError(database = reference, 'scallop'),

          # Beverages
          Ingredients == "apple cider" ~ fixFoodMappingError(database = reference, 'cider'),
          str_detect(Ingredients, "water broth|ice cube") ~ fixFoodMappingError(database = reference, 'water'),
          Ingredients %in% c(
            "cranberry juice", "pomegranate juice", 'strawberry juice',
            "black currant juice", "currant juice") ~ fixFoodMappingError(database = reference, 'fruit', 'juice'),
          Ingredients == "aquavit" ~ fixFoodMappingError(database = reference, 'fortified wine'),
          Ingredients == "coffee liqueur" ~ fixFoodMappingError(database = reference, 'liqueur', 'coffee'),

          #Div
          str_detect(Ingredients, 'vinegar') & str_detect(Ingredients, 'wine') ~ fixFoodMappingError(database = reference, 'vinegar', 'wine'),
          Ingredients %in% c('vinegar rice', 'vinegar apple cider',
          'vinegar sherry', 'vinegar brown', 'vinegar raspberries') ~ fixFoodMappingError(database = reference, 'vinegar'),
          Ingredients == 'condensed cream of celery soup' ~ fixFoodMappingError(database = reference, 'condensed cream of celery soup'),
          Ingredients == 'condensed cream of chicken soup' ~ fixFoodMappingError(database = reference, 'condensed cream of chicken soup'),
          Ingredients == 'condensed cream of mushroom soup' ~ fixFoodMappingError(database = reference, 'condensed cream of mushroom soup'),
          str_detect(Ingredients, 'mushroom') & !str_detect(Ingredients, 'dried|canned|sauce') ~ fixFoodMappingError(database = reference, 'mushroom'),
          Ingredients %in% c('garlic oil', 'oil truffle') ~ fixFoodMappingError(database = reference, 'olive', 'oil'), #Garlic/truffle oil can be made by placing garlic in olive oil
          Ingredients == 'sauce hot' ~ fixFoodMappingError(database = reference, 'hot', 'pepper'),
          Ingredients == 'sesame oil' ~ fixFoodMappingError(database = reference, 'seed', 'oil'),
          Ingredients == 'hazelnut oil' ~ fixFoodMappingError(database = reference, 'walnut', 'oil'),
          Ingredients == 'sauce pasta' ~ fixFoodMappingError(database = reference, 'tomato', 'sauce'),
          Ingredients == 'sweet chili sauce' ~ fixFoodMappingError(database = reference, 'chili', 'sweet'),
          str_detect(Ingredients, 'cognac|kirsch') ~ fixFoodMappingError(database = reference, 'brandy'),
          str_detect(Ingredients, 'broth cube') ~ fixFoodMappingError(database = reference, 'stock', 'cubes'),
          Ingredients %in% c('nacho', 'taco shell') ~ fixFoodMappingError(database = reference, 'tortilla', 'corn'), #Similar ingredients just with more salt
          str_detect(Ingredients, 'broth cube') ~ fixFoodMappingError(database = reference, 'stock', 'cubes'),
          Ingredients == 'mango chutney' ~ fixFoodMappingError(database = reference, 'mango chutney'),
          Ingredients == 'soybean oil' ~ fixFoodMappingError(database = reference, 'soy', 'oil'),
          Ingredients == 'mustard honey' ~ fixFoodMappingError(database = reference, 'mustard'),
          Ingredients == 'refrigerated buttermilk biscuit dough' ~ fixFoodMappingError(database = reference, 'refrigerated buttermilk biscuit dough'),
          Ingredients == 'corn meal mix' ~ fixFoodMappingError(database = reference, 'corn flour', 'polenta'),
          Ingredients %in% c('jam blueberries', 'jam currant', 'cranberries jam') ~ fixFoodMappingError(database = reference, 'jam'),
          Ingredients == 'sweet green pickle relish' ~ fixFoodMappingError(database = reference, 'sweet green pickle relish'),
          Ingredients == 'goose fat' ~ fixFoodMappingError(database = reference, 'fats'),
          Ingredients == 'remulade' ~ fixFoodMappingError(database = reference, 'mayonnaise', 'sauce'),
          Ingredients == 'tabasco' ~ fixFoodMappingError(database = reference, 'chili', 'sauce'),
          Ingredients %in% c("aioli", "mayonnaise") ~ fixFoodMappingError(database = reference, 'mayonnaise', 'sauce'),
          Ingredients %in% c("aioli plant-based", "mayonnaise plant-based") ~ fixFoodMappingError(database = reference, 'mayonnaise', 'vegan'),

          #Dairy
          Ingredients == 'buttermilk' ~ fixFoodMappingError(database = reference, 'buttermilk'),
          str_detect(Ingredients,
                     'cheddar|romano|parmigiano-reggiano|parmesan|parmigiano-reggiano|parmigiano reggiano|cheese hard goat|cheese cotjia|gruyere') &
            !str_detect(Ingredients, 'salad') ~ fixFoodMappingError(database = reference, 'hard cheese'),
          str_detect(Ingredients,
                     'cheese pizza|halloumi|manchego|havarti|swiss|monterey jack|pepperjack|asiago|mozzarella|goat brown cheese|jarlsberg|cheese semi-hard|provolone|norvegia|emmentaler|cheese garlic|cheese brown') ~ fixFoodMappingError(database = reference, 'hard to semi-hard cheese'),
          str_detect(Ingredients,
                     'ricotta|cheese blue|camembert|chevre|neufchatel|port salut|brie|mascarpone|gorgonzola|cheese soft|cheese le crémier de chaumes') |
            Ingredients %in% c('cheese goat') ~ fixFoodMappingError(database = reference, 'soft-ripened cheese'),
          Ingredients %in% c('cheese american', 'cheese spread') ~ fixFoodMappingError(database = reference, 'processed cheese and spreads'),
          Ingredients == 'yoghurt greek' |
            Ingredients == 'kefir' |
            str_detect(Ingredients, 'quark|biola|yoghurt skyr|yoghurt berries flavored') ~ fixFoodMappingError(database = reference, 'yoghurt'),
          #Milk with cocoa powder
          str_detect(Ingredients, 'milk beverage chocolate') ~ fixFoodMappingError(database = reference, 'milk'),
          Ingredients %in% c("cheese plant-based", 'cheese, plant-based') ~ fixFoodMappingError(database = reference, "dairy", "imitate"),


          #Bread and rolls
          Ingredients %in% c(
            'bread', 'bread coarse', 'tortilla coarse', 'crisp bread coarse','bread crumb',
            'bread rye', 'bread polar', 'hamburger bread coarse', 'rolls coarse baguette garlic',
            'bread brown chapati', 'rolls coarse', 'rolls coarse baguette',
            "pita bread coarse") ~ fixFoodMappingError(database = reference, 'wheat bread and rolls', 'brown'),
          Ingredients %in% c(
            'hamburger bun', 'bread white', 'tortilla', 'crisp bread', 'breadstick', 'ciabatta',
            'hamburger bread', 'bread white foccacia',
            'rolls white', 'cracker cream', 'bread naan', 'bread flat hard', 'pita bread white',
            'bread sausage', 'bread paratha', "parata flat bread",
            'pizza crust', 'pizza crust italian') | str_detect(Ingredients, 'rolls white') ~ fixFoodMappingError(database = reference, 'wheat bread and rolls', 'white'),

          #Herbs and spices
          str_detect(
            Ingredients,
            'vanilla extract|vanilla pod|vanilla essence|vanilla powder|saffron|fenugreek seed|mint fresh herbs|mint dried|lemon balm|turmeric|anise|marjoram|sazon seasoning|ginger\\b|caraway|lemongrass|basil|rosemary|thyme|tarragon|pepper|sage|garam masala|oregano|spice mix|nutmeg|cloves|coriander|cumin|dill|fenugreek leaf|juniper berry|cinnamon|chives|chervil|cardamom|caper|allspice|bay leaf|paprika powder|fennel seed|garlic powder') &
            !str_detect(Ingredients, 'sauce|paste|sweet|chili|sausage') |
            str_detect(Ingredients, 'chili') & !str_detect(Ingredients, 'pepper|paste|sauce|sausage|carne|nut|canned|glaze|marinade') |
            Ingredients %in% c('herbs', 'different spices', 'spices', 'soup seasoning') ~ fixFoodMappingError(database = reference, 'mixed', 'herbs'),

          #Not in ref
          (Ingredients %in% c('yeast nutritional', 'paste chili', 'agar', 'gluten', 'corn meal mix', 'blueberries pie filling', 'onion seed',
                             'nori seaweed','salmon roe', "beans'n'pork canned", 'apricot preserve', 'apple sauce', 'chili bean paste sichuan',
                             'plantain', 'tabasco', 'tapioca', 'sake', 'wine rice', 'liquid smoke flavoring', 'carbonated beverage lemon-lime', 'sauce cranberry',
                             'pack high quality charcoal briquettes', 'cooking spray', 'quinoa', 'paste carrot', 'seed hemp', 'onion powder', 'rhubarb juice',
                             'red food coloring', 'toro greek moussaka', 'banana', 'can tomato soup', 'paste vanilla bean', 'lime leaf', 'sugar color',
                             'fish scraps for broth', 'fish soup base', 'paste garlic', 'vanillin', 'vanilla extract', 'toenjang soybean paste',
                             'pomegranate kernel', 'sauce white', 'celery seed', 'trout caviar', 'vanilla pod', 'condensed tomato soup', 'cream sauce base',
                             'sauce bearnaise', 'wine rice', 'soup onion instant', 'whip it stabilizer', 'butter-vanilla aroma', 'shake mixed spice',
                             'vanilla bean', 'sauce curry', 'gingerbread house', 'rice puffed', 'candy mixed', 'horn salt',
                             'dragon fruit', 'decorative glaze', 'lemon gel', 'sauce mushroom', 'lemon gel', 'prune marinade', 'roe salmon',
                             'chia seed', 'cornflakes', 'erythriol', 'flowers', 'firkorn', 'food coloring', 'food coloring', 'baking soda',
                             'gelatin', 'pavlova powder mix', 'quick lunch', 'psyllium husk', 'raspberry jelly', 'rice puffed', 'seltzer',
                             'spring roll paper', 'sprinkles', 'wakame', 'wasabi', 'weetabix', 'yeast', 'yeast dry', 'almond essence', 'lemonade') |
             str_detect(Ingredients, 'spice mix(?! taco)|powder mix|soup instant')
          )  &
            #If user have added these ingredients, keep
            !str_detect(database_ID, ".999") ~ 0,

          TRUE ~ database_ID
        ))

      #Add the new reference words
      #Add the new reference words
      results <- temp %>%
        left_join(reference, by = 'database_ID') %>%
        mutate(database_reference = paste0(first_word, ' ', second_word)) %>%
        #Remove unnecessary columns
        select(-c(first_word, second_word, .data$loop)) %>% unique() %>%
        left_join(db,
                  by = "database_ID") %>%
        mutate(database_reference = str_replace(database_reference, "NA NA", "Not in database"))


  } else {
      stop("Sorry, there are no added fixes for the reference used.")
    }

  } else {
    results
  }

  results %>% mutate(database_reference = str_replace(database_reference, '\\\\', ''))

}
