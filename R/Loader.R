DEFAULT_CSV_OFFSET   <- 0
DEFAULT_EXCEL_OFFSET <- 4
FILE_SEP <- "/"


#' Load Multiple Post Trades
#'
#' Load multiple post trade files (csv and excel supported) returning a single
#' data.frame.
#'
#' @param path path containing the files to read
#' @param pattern pattern for files to match
#' @param add_filename should the file name be added to the returned data.frame
#' @param sheet_name sheet name to load (excel only)
#' @param row_offset offset to use when loading data
#'
#' @return data.frame containing the loaded files.
#'
#' @export
load_multiple <- function(path, pattern = "*.xlsx", add_filename = FALSE, sheet_name = "Detail", row_offset = NA)
{
  files <- list.files(path = path,
                      pattern = pattern,
                      full.names = TRUE)


  all_results <- map_dfr(files, handle_one, add_filename, sheet_name, row_offset)

  return (all_results)

}

handle_one <- function(file_name, add_filename, sheet_name, row_offset)
{
  res <- NULL;

  if (stringr::str_detect(file_name, "*.csv"))
  {
    res <- load_from_csv(file_name, if_else(is.na(row_offset), DEFUALT_CSV_OFFSET, row_offset))
  }
  else
  {
    res <- load_from_excel(file_name, sheet_name, if_else(is.na(row_offset), DEFAULT_EXCEL_OFFSET, row_offset))
  }

  stripped_file_name <- get_stripped_file_name(file_name)

  if (add_filename){
    res <- res %>% mutate(file_name = stripped_file_name)
  }

  return (res)
}

# Warning - not vectorised
get_stripped_file_name <- function(file_name)
{
  tail(unlist(stringr::str_split(file_name, FILE_SEP)),1)
}

load_from_csv <- function(file_name, row_offset = 0)
{
  results <- readr::read_delim(
    file_name,
    ";",
    escape_double = FALSE,
    trim_ws = TRUE)

  return (results)
}

load_from_excel <- function(file_name, sheet_name, row_offset)
{
  result <- readxl::read_excel(file_name,
                       sheet = sheet_name, skip = row_offset)

  return (result)
}
