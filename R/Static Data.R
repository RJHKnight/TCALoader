#' Add Spread
#'
#' Add an indicative spread column to the standard post trade.
#'
#' @param input standardised post trade
#'
#' @return post trade with spread column added.
#'
#' @export
add_spread <- function(input)
{
  library(magrittr)
  library(dplyr)

  if (!requireNamespace("TicR", quietly = TRUE)) {
    stop("Package \"TicR\" needed for this function to work. Please install it.",
         call. = FALSE)
  }

  universe <- unique(input$sym)
  batch_date <- format(max(input$date), "%Y.%m.%d")

  daily_data <- TicR::getBatchData(symList = universe, endDate = batch_date, columns = "daily")

  spread <- daily_data %>%
    select(sym, Avg20dSpread) %>%
    rename(spread = Avg20dSpread)

  input <- dplyr::left_join(input, spread, by = "sym")

  return (input)
}
