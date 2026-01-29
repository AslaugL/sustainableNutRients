#' Find food in reference database.
#' @title newFindFoodInDatabase
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
newFindFoodInDatabase <- function(df, database,
                                  additional_entries = NULL,
                                  fix_errors = TRUE,
                                  include_water = FALSE) {

  # Only look up relevant database hits
  temp <- function(singleIngredient) {

    ref <- reference %>%
      dplyr::mutate(second_word = str_replace(second_word, "\\\\", "\\\\\\\\"))

  if(isTRUE(str_count(singleIngredient, pattern = "\\w+") >= 2)) {

    tmp_db <- ref %>%
      dplyr::filter(str_detect(singleIngredient, paste0('\\b', first_word)) &
                      ((second_word != "\\\\" &
                          str_detect(singleIngredient, paste0('\\b', second_word)) ) |
                         second_word == "\\\\" )
      )

    if(isTRUE(any(str_detect(tmp_db$second_word, singleIngredient)) )) {

      tmp_db <- tmp_db %>%
        dplyr::filter(str_detect(tmp_db$second_word, singleIngredient))

    }

  } else if(isTRUE(str_count(singleIngredient, pattern = "\\w+") == 1)) {

    tmp_db <- ref %>%
      dplyr::filter(str_detect(singleIngredient, paste0('\\b', first_word)))

    if(isTRUE("\\\\" %in% tmp_db$second_word)) {

      tmp_db <- tmp_db %>%
        dplyr::filter(second_word == "\\\\")

    }

  } else {

    stop("Ingredient is likely empty, check for errors.")

  }

  # Arrange search words so the most complex are looked up first
  tmp_db <- tmp_db %>%
    dplyr::mutate(counts_1 = str_count(first_word),
                  counts_2 = str_count(second_word),
                  counts_3 = counts_1+counts_2) %>%
    group_by(first_word) %>%
    arrange(desc(counts_3), desc(counts_1), desc(counts_2)) %>% ungroup()

  # If no hits, ID is 0
  if(nrow(tmp_db) == 0) {
    ID <- tibble(
      database_ID = 0,
      database_reference = "No database match")

    # If there is a hit, find the best match
  } else {

    ID <- vector("list", 3)

    # Loop
    for (ref_words in 1:length(tmp_db)) {

      #Only look for the whole word found in the reference
      if(all(str_detect(singleIngredient, regex(paste0('\\b', tmp_db$first_word[ref_words]))),
             str_detect(singleIngredient, regex(paste0('\\b', tmp_db$second_word[ref_words]))))
      ) {

        #print("in loop 1")
        ID <- tibble(
          database_ID = tmp_db$database_ID[ref_words],
          database_reference = paste0(tmp_db$first_word[ref_words], " ", tmp_db$second_word[ref_words], "_bothRefWords"),
          loop = "1"
        )

        break
      }  else if(all(str_detect(singleIngredient, regex(paste0('\\b', tmp_db$first_word[ref_words]))),
                     tmp_db$second_word[ref_words] == "\\\\")
      ) {
        #print("in loop 2")

        if(str_count(singleIngredient, "\\w+") == str_count(tmp_db$first_word[[ref_words]] )) {

          ID <- tibble(
            database_ID = tmp_db$database_ID[ref_words],
            database_reference = paste0(tmp_db$first_word[ref_words], " ", tmp_db$second_word[ref_words], "_SingleWordNoSecondWordNeeded"),
            loop = "2"
          )

        } else {

          ID <- tibble(
            database_ID = tmp_db$database_ID[ref_words],
            database_reference = paste0(tmp_db$first_word[ref_words], " ", tmp_db$second_word[ref_words], "_SingleWordNoSecondWords"),
            loop = "2"
          )

        }

        break
      } else if(isTRUE(str_detect(singleIngredient, regex(paste0('\\b', tmp_db$first_word[ref_words])))
      )) {
        #print("in loop 3")
        ID <- tibble(
          database_ID = tmp_db$database_ID[ref_words],
          database_reference = paste0(tmp_db$first_word[ref_words], " ", tmp_db$second_word[ref_words], "_SingleWordSecondWords"),
          loop = "3"
        )
        break

      }

      else {
        ID <- tibble(
          database_ID = 999999999,
          database_reference = "For some reason not working",
          loop = "4")
      }
    }

    # ID <- tmp_db %>%
    #   dplyr::mutate(counts = str_count(first_word) + str_count(second_word)) %>%
    #   arrange(desc(counts)) %>%
    #   dplyr::mutate(
    #     new_ID = case_when(
    #       all(str_detect(df$Ingredients, paste0("\\b",first_word)),
    #           str_detect(df$Ingredients, paste0("\\b", second_word))
    #           ) ~ paste0(database_ID, "_bothRefWords"),
    #       (str_detect(df$Ingredients, paste0("\\b", first_word)) &
    #         second_word == "\\\\") ~ paste0(database_ID, "_bothRefWords"),
    #       str_detect(df$Ingredients, paste0("\\b", first_word)) ~ paste0(database_ID, "_onlyFirstWord")
    #     ),
    #     database_reference = case_when(
    #       str_detect(new_ID, "bothRefWords") & second_word != "\\\\" ~ paste0(first_word, " ", second_word),
    #       str_detect(new_ID, "bothRefWords") & second_word == "\\\\" ~ paste0(first_word),
    #       str_detect(new_ID, "onlyFirstWord") ~ paste0(first_word)
    #       )
    #   ) %>%
    #   dplyr::select(new_ID, database_reference)

  }

  ID <- ID %>%
    bind_rows() %>%
    dplyr::mutate(
      Original_ingredients = singleIngredient,
      database_reference = str_replace(database_reference, " \\\\\\\\", ""))

  ID

  }

  #Pull the reference database from sustainableNutRient internal data
  if(database == "volume_weight"){

    #Separate out ingredients with weight in kilo already, unnecessary to look through them
    if(length(unique(df$unit) > 1)) {

      ingredients_to_check <- df %>% dplyr::filter(unit != 'kg')

    } else if(unique(df$unit) == "kg") {

      stop("All the ingredients are in kilograms, no need to check against volume weight database.")

    }

    #No additional entries for the database
    if(is.null(additional_entries)){

      #Database query words
      reference <- unit_weights_query

      #The database
      db <- unit_weights %>%
        dplyr::select(-Ingredients)

    }else if(!is.null(additional_entries)){

      #Check if input has required format
      if(isTRUE(is.list(additional_entries) &
                all.equal(names(additional_entries), c('db', 'query_words')) &
                all.equal(names(additional_entries$db), c('unit', 'grams_per_unit', 'database_ID')) &
                all.equal(names(additional_entries$query_words), c('first_word', 'second_word', 'database_ID')))
      ) {

        #Database query words
        reference <- unit_weights_query %>%
          #Add user entries
          bind_rows(additional_entries$query_words) %>%
          distinct() %>%
          #Arrange by lexogographical order and number of words in the first search term
          dplyr::mutate(
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
          dplyr::select(-Ingredients) %>%
          bind_rows(additional_entries$db) %>% distinct()


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
          dplyr::filter(nutrient != "Water")
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
          dplyr::mutate(
            number_of_words1 = str_count(first_word),
            number_of_words2 = str_count(second_word),
            number_of_words2 = case_when(

              second_word == "\\" ~ 50,
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
            dplyr::filter(nutrient != "Water")
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
          dplyr::mutate(
            number_of_words1 = str_count(first_word),
            number_of_words2 = str_count(second_word),
            number_of_words2 = case_when(

              second_word == "\\" ~ 50,
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
  results <- sapply(ingredients_to_check$Ingredients %>% unique(),
                    temp, USE.NAMES = TRUE, simplify = FALSE) %>%
    map_dfr(as_tibble, .id = "Ingredients")

  #Fix some known errors

  #Fix some known errors
  if(isTRUE(fix_errors)){

    #Fix volume weight hits
    if(database == 'volume_weight'){

      #Add ingredient metadata
      temp <- results %>%
        # Split into food items to look up or not %>%
        dplyr::mutate(to_fix = case_when(
          Ingredients %in% sustainableNutRients:::databaseHitFixes$volumeWeight$Ingredients ~ "ToFix",
          TRUE ~ "NoFix"
        )) %>%
        split(.$to_fix)

      if(all(is.data.frame(temp$ToFix) & nrow(temp$ToFix > 0))) {

        print(head(results))

        # Use the fixes
        temp$ToFix <- temp$ToFix %>%
          left_join(sustainableNutRients:::databaseHitFixes$volumeWeight, by = "Ingredients") %>%
          dplyr::mutate(
            database_ID = map2(database_referenceword1, database_referenceword2, function(word1, word2) {

              fixFoodMappingError(database = reference, first_w = word1, second_w = word2)

            }),
            database_reference = paste0(database_referenceword1, " ", database_referenceword2, "_Fixed")
          ) %>%
          dplyr::select(Ingredients, database_ID, database_reference) %>%
          unnest(., database_ID, keep_empty = TRUE)

      }

      # bind together
      temp <- bind_rows(temp) %>%
        dplyr::select(-to_fix) %>%
        # Check for food items not i database
        dplyr::mutate(
          database_ID = case_when(
            !!!rlang::parse_exprs(sustainableNutRients:::not_in_database %>%
                                    dplyr::filter(database == "volumeWeight") %>% pull(expression)),
            TRUE ~ database_ID
          ))

      print("Finished fixing IDs")

      #Add the new reference words and join with the database
      results <- temp %>%
        full_join(df) %>%
        left_join(reference %>% dplyr::select(database_ID, contains("word")) %>%
                    dplyr::filter(database_ID %in% temp$database_ID)) %>%
        dplyr::mutate(database_reference = paste0(first_word, ' ', second_word)) %>%
        #Remove unnecessary columns
        select(-c(first_word, second_word, loop)) %>%
        distinct() %>%
        left_join(db,
                  by = c("database_ID", "unit")) %>%
        dplyr::mutate(
          grams_per_unit = case_when(unit == 'kg' ~ 1000, TRUE ~ grams_per_unit),
          database_reference = case_when(
            unit == 'kg' ~ "Not looked up in database as weight already given",
            is.na(Amounts) ~ "Not looked up in database as no amounts given",
            is.na(database_ID) | database_ID == 0 ~ "Not found in database",
            TRUE ~ database_reference
          )) %>%
        dplyr::select(-Original_ingredients)

    } else if(database == 'nutrients'){

      #Add ingredient metadata
      temp <- results %>%
        full_join(df) %>%
        # Split into food items to look up or not %>%
        dplyr::mutate(to_fix = case_when(
          Ingredients %in% sustainableNutRients:::databaseHitFixes$nutrients$Ingredients ~ "ToFix",
          TRUE ~ "NoFix"
        )) %>%
        split(.$to_fix)

      # Use the fixes
      temp$ToFix <- temp$ToFix %>%
        left_join(sustainableNutRients:::databaseHitFixes$nutrients) %>%
        dplyr::mutate(
          database_ID = map2(database_referenceword1, database_referenceword2, function(word1, word2) {

            fixFoodMappingError(database = reference, first_w = word1, second_w = word2)

          }),
          database_reference = paste0(database_referenceword1, " ", database_referenceword2, "_Fixed")
        ) %>% dplyr::select(Ingredients, database_ID, database_reference) %>%
        unnest(., database_ID, keep_empty = TRUE)

      # bind together
      temp <- bind_rows(temp) %>%
        dplyr::select(-to_fix) %>%
        # Check for food items not i database
        dplyr::mutate(
          database_ID = case_when(
            !!!rlang::parse_exprs(
            sustainableNutRients:::not_in_database %>%
              dplyr::filter(database == "nutrients") %>% pull(expression)),
            TRUE ~ database_ID)

        )

      print("Finished fixing IDs")

      #Add the new reference words
      results <- temp %>%
        left_join(reference, by = 'database_ID') %>%
        unique() %>%
        left_join(db,
                  by = "database_ID") %>%
        select(-from) %>%
        dplyr::mutate(database_reference = case_when(
          is.na(database_ID) | database_ID == 0 | database_reference == "NA NA" ~ "Not found in database",
          TRUE ~ database_reference)
          ) %>%
        dplyr::select(-Original_ingredients)


    } else if(database == 'sustainability'){

      #Add ingredient metadata
      temp <- results %>%
        full_join(df) %>%
        # Split into food items to look up or not %>%
        dplyr::mutate(to_fix = case_when(
          Ingredients %in% sustainableNutRients:::databaseHitFixes$sustainability$Ingredients ~ "ToFix",
          TRUE ~ "NoFix"
        )) %>%
        split(.$to_fix)

      # Use the fixes
      temp$ToFix <- temp$ToFix %>%
        left_join(sustainableNutRients:::databaseHitFixes$sustainability) %>%
        dplyr::mutate(
          database_ID = map2(database_referenceword1, database_referenceword2, function(word1, word2) {

            fixFoodMappingError(database = reference, first_w = word1, second_w = word2)

          }),
          database_reference = paste0(database_referenceword1, " ", database_referenceword2, "_Fixed")
        ) %>% dplyr::select(Ingredients, database_ID, database_reference) %>%
        unnest(., database_ID, keep_empty = TRUE)


      # bind together
      temp <- bind_rows(temp) %>%
        dplyr::select(-to_fix) %>%
        # Check for food items not i database
        dplyr::mutate(
          database_ID = case_when(
            !!!rlang::parse_exprs(sustainableNutRients:::not_in_database %>%
                                 dplyr::filter(database == "sustainability") %>%
                                 pull(expression)),
          TRUE ~ database_ID)
        )

      #Add the new reference words
      #Add the new reference words
      results <- temp %>%
        left_join(reference, by = 'database_ID') %>%
        unique() %>%
        left_join(db,
                  by = "database_ID") %>%
        dplyr::mutate(database_reference = case_when(
          is.na(database_ID) | database_ID == 0 | database_reference == "NA NA" ~ "Not found in database",
          TRUE ~ database_reference)
        ) %>%
        dplyr::select(-Original_ingredients)


    } else {
      stop("Sorry, there are no added fixes for the reference used.")
    }

  } else {
    results
  }

  results %>% dplyr::mutate(database_reference = str_replace_all(database_reference, '\\\\', ''))

}
