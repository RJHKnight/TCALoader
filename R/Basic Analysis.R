#' Basic Analysis
#'
#' Simple performance analysis
#'
#' @param input data.frame containing the standardised post trade
#' @param spread_normalise return benchmarks normalised by the average spread
#'
#' @return analysis data.frame
#'
#' @export
basic_analysis <- function(input, ..., spread_normalise = FALSE)
{
  library(magrittr)
  library(dplyr)
  library(rlang)

  group_var <- enquos(...)

  input <- input %>% group_by(!!! group_var)

  # Group names as string list for the join
  groups <- (sapply(group_var, quo_name))

  if (spread_normalise & !has_spread(input))
  {
    stop("No spread column found - add it using add_spread function.", call. = FALSE)
  }

  if (length(groups) > 0)
  {
  results <- tca_overview(input) %>%
    left_join(tca_summary(input, vwap_dev, spread_normalise), by = groups) %>%
    left_join(tca_summary(input, arrival_dev, spread_normalise), by = groups)
  }
  else
  {
    results <- bind_cols(tca_overview(input),
                         tca_summary(input, vwap_dev, spread_normalise),
                         tca_summary(input, arrival_dev, spread_normalise))
  }

  return (results)
}

#' Equal Freq Analysis
#'
#' Performance analysis with equal freq bins of a continuous variable
#'
#' @param input data.frame containing the standardised post trade
#' @param spread_normalise return benchmarks normalised by the average spread
#' @param break_var variable to break on
#' @param num_groups number of groups to split into
#'
#' @return analysis data.frame
#'
#' @export
equal_freq_analysis <- function(input, break_var, num_groups, spread_normalise = FALSE)
{
  library(magrittr)
  library(dplyr)
  library(rlang)

  group_var <- enquo(break_var)

  input <- input %>%
    mutate(group = Hmisc::cut2(!!group_var, g = num_groups)) %>%
    group_by(group)

  if (spread_normalise & !has_spread(input))
  {
    stop("No spread column found - add it using add_spread function.", call. = FALSE)
  }

  results <- tca_overview(input) %>%
    left_join(tca_summary(input, vwap_dev, spread_normalise), by = "group") %>%
    left_join(tca_summary(input, arrival_dev, spread_normalise), by = "group")

  return (results)
}



has_spread <- function(input)
{
  return (any(colnames(input) == "spread"))
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

tca_summary <- function(input, benchmark, spread_normalise)
{
  expr <- enquo(benchmark)
  mean_name <- paste0("m_", quo_name(expr))
  weighted_mean_name <- paste0("wm_", quo_name(expr))
  median_name <- paste0("med_", quo_name(expr))
  denom <- ifelse(spread_normalise, as.name("spread"), 1)

  return (
    summarise(
      input,
      !! mean_name          := mean(!!expr / !!denom),
      !! weighted_mean_name := weighted.mean(!!expr/ !!denom, us_value),
      !! median_name        := median(!!expr / !!denom)
    )
  )
}
