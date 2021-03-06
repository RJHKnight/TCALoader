

# Names in post trade
VWAP_COLS <- c("Vwap Deviation (bps)", "InLimitVwapDev")
ARRIVAL_COLS <- c("Arrival Deviation (bps)", "ArrivalDev")
DATE_COLS <- c("Date")
STRATEGY_COLS <- c("Algo Strategy", "Algorithm", "Strategy")
BATCH_COLS <- c("Batch")
SYM_COLS <- c("symbol", "sym", "Instrument", "ExchangeID")
SIDE_COLS <- c("Side")
ORDER_ID_COLS <- c("Order ID", "OrderID")

# Standardised names
DATE     <- "date"
STRATEGY <- "strategy"
BATCH    <- "batch"
SYM      <- "sym"
SIDE     <- "side"
VWAP     <- "vwap_dev"
ARRIVAL  <- "arrival_dev"
ORDER_ID <- "order_id"


#' Standardise post trade
#'
#' Attempt standardisation of column names and formats.
#'
#' @param input data.frame containing post trade to be standardised
#'
#' @return standardised data.frame
#'
#' @export
standardise_post_trade <- function(input)
{
  library(magrittr)
  library(dplyr)

  # Dates
  input <- handle_date(input, DATE_COLS)

  # Key columns
  input <- find_and_rename(input, SYM_COLS, SYM)
  input <- find_and_rename(input, STRATEGY_COLS, STRATEGY)
  input <- find_and_rename(input, BATCH_COLS, BATCH)
  input <- find_and_rename(input, SIDE_COLS, SIDE)
  input <- find_and_rename(input, VWAP_COLS, VWAP)
  input <- find_and_rename(input, ARRIVAL_COLS, ARRIVAL)
  input <- find_and_rename(input, ORDER_ID_COLS, ORDER_ID)

  # Generic column rename
  input <- generic_rename(input)

  return (input)

}

find_and_rename <- function(input, possible_columns, target_name)
{
  all_cols <- colnames(input)
  col_name <- get_col_name(possible_columns, all_cols)

  if (is.na(col_name)) {
    warning(paste("Unable to identify", target_name))
    return (input)
  }

  return (rename_col(input, col_name, target_name))
}

handle_date <- function(input, date_cols)
{
   all_cols <- colnames(input)
   col_name <- get_col_name(DATE_COLS, all_cols)

   if (is.na(col_name))
     return(input)

   date_class <- class(input %>% pull(col_name))

  if (date_class == "numeric")
  {
    input <- (mutate(input, !!DATE := handle_excel_date(!! as.name(col_name))))
    input <- select(input, -col_name)
  }
  else
  {
    input <- rename_col(input, DATE_COLS, DATE)
  }


  return (input)
}

rename_col <- function(input, col_name, new_name)
{
  if (col_name == new_name)
    return(input)

  print(paste("Renaming", col_name, "to", new_name ))
  return (rename(input, !!new_name := col_name ))
}

get_col_name <- function(name, columns)
{
  matching_cols <- columns[columns %in% name]

  # If multiple match, return first.
  return (matching_cols[1])
}

handle_excel_date <- function(dates)
{
  return (as.Date(dates, origin="1899-12-30"))
}
