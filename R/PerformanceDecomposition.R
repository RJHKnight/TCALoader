#' Run profile decomposition for simulations.
#'
#' @param file_name path to the bin_data export
#' @param return_full return the raw profile bins
#'
#' @return decomposition of the slippage into historical, tracking and execution components.
#'
#' @export
performance_decomposition <- function(file_name, return_full = FALSE)
{
  tmp <- read_delim(file_name, ";", escape_double = FALSE, trim_ws = TRUE)
  tmp <- janitor::clean_names(tmp) %>%
    mutate(farmjob = as.character(farmjob))

  binned_execs <- load_simulator_bins(tmp)

  # Unique Sym / Dates
  order_date_syms <- tmp %>%
    group_by(farmjob) %>%
    summarise(
      sym = exchange_id [1],
      date = extract_date(date_time[1])
    ) %>%
    distinct()

  # Actual Vol Profile - full day
  actual_vol_profile <- load_actual_profile(order_date_syms$sym, order_date_syms$date)

  historical_vol_profile <- load_vol_profile(order_date_syms$sym, order_date_syms$date)
  # Historical Vol Profile - full day

  # All together
  combined_profile <- actual_vol_profile %>%
    left_join(binned_execs, by = (c("sym" = "exchange_id", "date", "startTime" = "bin_start", "endTime" = "bin_end"))) %>%
    left_join(historical_vol_profile, by = (c("sym", "date", "startTime", "endTime"))) %>%
    fill(farmjob, order_quantity , .direction = "down")

  # Fill down missing values
  combined_profile <- combined_profile %>%
    group_by(farmjob) %>%
    mutate(vwap = if_else(is.nan(vwap), NA_real_, vwap)) %>%
    mutate(average_price = if_else(is.nan(average_price), NA_real_, average_price)) %>%
    fill(vwap) %>%
    ungroup()

  if (return_full)
    return (combined_profile)


  results <- combined_profile %>%
    group_by(farmjob) %>%
    summarise(
      hist_price    = sum(hist_pct_vol * vwap, na.rm = TRUE),                       # Price from trading profile exactly
      market_price  = sum(actual_pct_vol * vwap, na.rm = TRUE),                     # Actual VWAP
      profile_price = sum(pct_order * vwap, na.rm = TRUE),                          # Price from trading our profile at bin vwap
      exec_price    = sum(pct_order * average_price, na.rm = TRUE),
      slippage      = 10000 * (market_price - exec_price) / market_price,
      hist_dev      = 10000 * (market_price - hist_price ) / market_price,
      tracking_dev  = 10000 * (hist_price - profile_price) / market_price,
      exec_dev      = 10000 * (profile_price - exec_price) / market_price
    )

  return (results)
}

component_to_qty <- function(component_string)
{
  bits <- str_remove_all(component_string, "\\{|\\}") %>%
    str_split(",")

  return (sapply(bits, function(x) sum(as.numeric(x))))
}


load_simulator_bins <- function(tmp)
{
  # Simulator Execs
  binned_execs <- tmp %>%
    mutate(date_time = extract_date_time(date_time)) %>%
    mutate(bin_start = floor_date(date_time, "5 minute")) %>%
    mutate(bin_end = ceiling_date(date_time, "5 minute")) %>%
    mutate(exec_price = as.numeric(exec_price)) %>%
    rename(order_quantity = quantity) %>%
    group_by(farmjob) %>%
    mutate(
      #order_start = min(bin_start),
      #order_end = max(bin_end),
      bin_qty = component_to_qty(quantities)
    ) %>%
    ungroup() %>%
    group_by(farmjob, bin_start, order_quantity, exchange_id) %>%
    summarise(
      average_price = weighted.mean(exec_price, bin_qty, na.rm = TRUE),
      quantity = sum(bin_qty, na.rm = TRUE),
      bin_end = max(bin_end)
    ) %>%
    mutate(pct_order = if_else(is.na(quantity), 0, quantity / order_quantity)) %>%
    mutate(date = as.Date(bin_start, tz = "Australia/Sydney")) %>%
    ungroup() %>%
    filter(!(hour(bin_end) == 16 & minute(bin_end) > 0 & is.na(average_price))) %>%
    mutate(bin_end = if_else(hour(bin_end) == 16 & minute(bin_end) > 0,
                             ymd_hms(paste(date, "16:05:00"), tz = "Australia/Sydney"),
                             bin_end),
           bin_start = if_else(hour(bin_end) == 16 & minute(bin_end) > 0,
                               ymd_hms(paste(date, "16:00:00"), tz = "Australia/Sydney"),
                               bin_start))

  return (binned_execs)
}
