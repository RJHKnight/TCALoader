java_data_format_regex <- "^([:alpha:]{3}) ([:alpha:]{3}) ([:digit:]{2}) ([:graph:]{8}) ([:alpha:]{3}) ([:digit:]{4})$"

#' Convert java date / time into a Date
#'
#'
#' @param java_date_strings list of datetimes
#'
#' @return parsed Dates
#'
#' @export
extract_date <- function(java_date_strings)
{
  bits <- str_match(java_date_strings, java_data_format_regex)
  date_strings <- paste(bits[,4], bits[,3], bits[,7])
  return (dmy(date_strings))
}

#' Convert java date / time into a PosixCT
#'
#'
#' @param java_date_strings list of datetimes
#' @param tz timezone to use
#'
#' @return parsed PosixCTs
#'
#' @export
extract_date_time <- function(java_date_strings, tz = "Australia/Sydney")
{
  # TODO: Handle tz properly.
  bits <- str_match(java_date_strings, java_data_format_regex)
  date_time_strings <- paste(bits[,4], bits[,3], bits[,7], bits[,5])
  return (dmy_hms(date_time_strings, tz = "Australia/Sydney "))
}
