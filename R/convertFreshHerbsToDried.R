#' Convert the amounts of herbs from fresh to dried if database only have the dried herbs.
#' @title convertFreshHerbsToDried
#'
#' @description Convert the amounts of herbs from fresh to dried / dried to fresh if database only have the dried or fresh herbs.
#'
#' @param df A dataframe with standardised ingredient names.
#'
#' @return A dataframe with the amounts of herbs converted to dried or fresh weight, and ingredient name changed from "fresh herbs" to "dried" or "dried" to "fresh herbs".
#'
#' @export
convertFreshHerbsToDried <- function(df){
  # Martha Steward rule fo thumb the conversion from fresh to dried is * by 1/3

  df %>%
    mutate(
      Amounts = case_when(
        Ingredients == "oregano fresh herbs" ~ Amounts*(1/3),
        Ingredients == "sage fresh herbs" ~ Amounts*(1/3),
        Ingredients == "chervil fresh herbs" ~ Amounts*(1/3),
        Ingredients == "tarragon fresh herbs" ~ Amounts*(1/3),
        Ingredients == "coriander dried" ~ Amounts*3,

        TRUE ~ Amounts),
      Ingredients = Ingredients %>%
        str_replace("oregano fresh herbs", "oregano dried")%>%
        str_replace("sage fresh herbs", "sage dried")%>%
        str_replace("chervil fresh herbs", "chervil dried")%>%
        str_replace("tarragon fresh herbs", "tarragon dried") %>%
        str_replace("coriander dried", "coriander fresh herbs")
    )
}
