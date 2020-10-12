#' Load a volume profile using cached data
#'
#' This will adjust the batch volume profile, adding in a dummy point for open and close auctions
#'
#' @param syms list of syms
#' @param dates list of dates
#'
#' @return volume profile per sym / date
#'
#' @export
load_vol_profile <- function(syms, dates)
{
  if (!inherits(dates, "Date"))
  {
    dates <- ymd(dates)
  }

  input <- data.frame(sym = syms, date = dates, stringsAsFactors = FALSE)
  unique_dates <- unique(input$date)

  all_profiles <- map_dfr(unique_dates, function(x){
    this_syms <- filter(input, date == x) %>% pull(sym)
    load_vol_profile_one_date(this_syms, x)
  })

}

load_vol_profile_one_date <- function(syms, date)
{
  date_string <- format(date, "%Y.%m.%d")
  cat(paste("Running for", length(syms), "syms on", date_string, "\n"))

  vol_profile <- getBatchData(symList = syms,
                              endDate = date_string,
                              columns = "profile")

  vol_profile <- vol_profile %>%
    select(sym, date, startTime, endTime, Avg20dPctVolume) %>%
    rename(pct_vol = Avg20dPctVolume) %>%
    mutate(pct_vol = pct_vol / 100)

  volumes <- getBatchData(symList = syms,
                              endDate = date_string,
                              columns = "daily")

  volumes <- select(volumes, date, sym, Avg20dVolume, Avg20dOpenVolume, Avg20dCloseVolume ) %>%
    rename(volume = Avg20dVolume,
           openauctvolume = Avg20dOpenVolume,
           closeauctvolume = Avg20dCloseVolume)

  volumes <- volumes %>%
    mutate(
      pct_open = openauctvolume / volume,
      pct_close = closeauctvolume / volume,
      pct_cont = 1 - pct_open - pct_close
    ) %>%
    select(date, sym, pct_open, pct_close, pct_cont)

  vol_profile <- vol_profile %>%
    left_join(volumes, by = c("date", "sym")) %>%
    mutate(pct_vol = pct_vol * pct_cont) %>%
    select(sym, date, startTime, endTime, pct_vol)

  start_times <- vol_profile %>%
    group_by(sym, date) %>%
    summarise(startTime = min(startTime)) %>%
    ungroup()

  open_row <- start_times %>%
    left_join(volumes, by = c("sym", "date")) %>%
    mutate(endTime = startTime,
           startTime = startTime - minutes(15),
           pct_vol = pct_open) %>%
    select(-pct_close, -pct_cont, -pct_open)

  end_times <- vol_profile %>%
    group_by(sym, date) %>%
    summarise(endTime = max(endTime)) %>%
    ungroup()

  close_row <- end_times %>%
    left_join(volumes, by = c("sym", "date")) %>%
    mutate(
      startTime = endTime,
      endTime = endTime + minutes(15),
      pct_vol = pct_close) %>%
    select(-pct_close, -pct_cont, -pct_open)

  all_profiles <- rbind(as.data.frame(open_row),
                        vol_profile,
                        as.data.frame(close_row)) %>%
    arrange(date, sym, startTime)

  return (all_profiles)

}
