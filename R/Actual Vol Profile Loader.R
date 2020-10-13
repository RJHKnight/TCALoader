#' Create a profile of the actual traded volume
#'
#' This will add in a dummy point for open and close auctions
#'
#' @param syms list of syms
#' @param dates list of dates
#'
#' @return volume profile per sym / date
#'
#' @export
load_actual_profile <- function(syms, dates)
{
  if (!inherits(dates, "Date"))
  {
    dates <- ymd(dates)
  }

  input <- data.frame(sym = syms, date = dates, stringsAsFactors = FALSE)
  unique_dates <- unique(input$date)

  all_profiles <- map_dfr(unique_dates, function(x){
    this_syms <- filter(input, date == x) %>% pull(sym)
    load_actual_vol_profile_one_date(this_syms, x)
  })

}

load_actual_vol_profile_one_date <- function(syms, date)
{
  date_string <- format(date, "%Y.%m.%d")
  cat(paste("Running for", length(syms), "syms on", date_string, "\n"))

  bin_data <- getBinData(symList = syms,
                              startDate = date_string,
                              endDate = date_string,
                              columns = c("volume", "vwap"))

  auctions <- getDailyData(symList = syms,
                           startDate = date_string,
                           endDate = date_string,
                           columns =  c("openauctvolume", "closeauctvolume", "open", "close"))


  start_times <- bin_data %>%
    group_by(sym, date) %>%
    summarise(startTime = min(startTime)) %>%
    ungroup()

  open_row <- start_times %>%
    left_join(auctions, by = c("sym", "date")) %>%
    mutate(endTime = startTime,
           startTime = startTime - minutes(5),
           volume = openauctvolume,
           vwap = open) %>%
    select(sym, date, startTime, endTime, volume, vwap)

  end_times <- bin_data %>%
    group_by(sym, date) %>%
    summarise(endTime = max(endTime)) %>%
    ungroup()

  close_row <- end_times %>%
    left_join(auctions, by = c("sym", "date")) %>%
    mutate(
      startTime = endTime,
      endTime = endTime + minutes(5),
      volume = closeauctvolume,
      vwap = close) %>%
      select(sym, date, startTime, endTime, volume, vwap)

  all_profiles <- rbind(as.data.frame(open_row),
                        bin_data,
                        as.data.frame(close_row)) %>%
    arrange(date, sym, startTime)

  # Add pct vol
  all_profiles <- all_profiles %>%
    group_by(date, sym) %>%
    mutate(pct_vol = volume / sum(volume)) %>%
    ungroup() %>%
    rename(actual_pct_vol = pct_vol)

  return (all_profiles)

}
