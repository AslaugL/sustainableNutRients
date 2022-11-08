#' Turn cooked ingredient amounts into their equivalents in raw form.
#' @title cookedToRaw
#'
#' @description Turn cooked ingredient amounts into their equivalents in raw form, using conversion factors from the Norwegian Directorate of Health.
#'
#' @param df A dataframe standardised ingredient and unit names.
#'
#' @return The dataframe wwhere cooked ingredients have been trned into their raw equivalents.
#'
#' @export
cookedToRaw <- function(df){

  df %>%
    #Do the conversion
    mutate(
      Amounts = case_when(
        Ingredients == 'bacon cooked' ~ Amounts/0.31,
        Ingredients %in% c('chicken breast cooked', 'chicken breast without skin cooked') ~ Amounts/0.8,
        Ingredients %in% c('chicken cooked', 'turkey meat cooked') ~ Amounts/0.4,
        Ingredients == 'lamb shoulder cooked' ~ Amounts/0.56,
        Ingredients == 'pork tenderloin cooked' ~ Amounts/0.75,
        Ingredients == 'pasta cooked' ~ Amounts/2.63,
        Ingredients == 'rice cooked' ~ Amounts/2.94,
        Ingredients == 'beetroot cooked' ~ Amounts/0.95, #Use values for parsley root
        Ingredients %in% c('lobster cooked', 'shrimp cooked') ~ Amounts/0.77, #Use values for Lean fish, with skin, simmered

        TRUE ~ Amounts),

      #Remove cooked from ingredient name
      Ingredients = str_replace(Ingredients, ' cooked', '')
    )
}
