#' Basic Analysis
#'
#' Simple performance analysis
#'
#' @param input data.frame containing the standardised post trade
#'
#' @return analysis data.frame
#'
#' @export
basic_analysis <- function(input, ...)
{
  library(magrittr)
  library(dplyr)
  library(rlang)

  group_var <- enquos(...)

  input <- input %>% group_by(!!! group_var)

  # Group names as string list for the join
  groups <- (sapply(group_var, quo_name))

  if (length(groups) > 0)
  {
  results <- tca_overview(input) %>%
    left_join(tca_summary(input, vwap_dev), by = groups) %>%
    left_join(tca_summary(input, arrival_dev), by = groups)
  }
  else
  {
    results <- bind_cols(tca_overview(input),
                         tca_summary(input, vwap_dev),
                         tca_summary(input, arrival_dev))
  }

  return (results)
}

tca_overview <- function(input)
{
  return (
    summarise(
      input,
      n = n(),
      value = sum(us_value)
    )
  )
}

tca_summary <- function(input, benchmark)
{
  expr <- enquo(benchmark)
  mean_name <- paste0("m_", quo_name(expr))
  weighted_mean_name <- paste0("wm_", quo_name(expr))
  median_name <- paste0("med_", quo_name(expr))

  return (
    summarise(
      input,
      !! mean_name          := mean(!!expr),
      !! weighted_mean_name := weighted.mean(!!expr, us_value),
      !! median_name        := median(!!expr)
    )
  )
}
