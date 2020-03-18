#' Pairwise Analysis
#'
#' Analysis of multiple runs.
#'
#' @param input data.frame containing the standardised post trade
#' @param benchmark_name name of the benchmark to analyse
#' @param group_variable list of varibles to uniquely identify a single run
#' @param batch column containing an indicator of the run
#' @param complete_cases should we only return data where results are present for all batches.
#' @param add_diff add a difference column (only for cases where there are two batches)
#'
#'
#' @return analysis data.frame
#'
#' @export
pairwise_analysis <- function(input, benchmark_name = "arrival_dev",
                              group_variables = c("date", "sym", "side", "order_size"),
                              complete_cases = TRUE, batch_id = "batch", add_diff = FALSE)
{

  batch_names <- unique(input %>% pull(batch_id))
  num_batches <- length(batch_names)

  if (complete_cases)
  {

    input <- input %>%
      group_by(!!! syms(group_variables)) %>%
      mutate(count = n()) %>%
      filter(count == num_batches) %>%
      ungroup()
  }

  all_variables <- c(group_variables, batch_id, benchmark_name)

  pairs_long <- input %>%
    select(!!! syms(all_variables))

  pairs_wide <- tidyr::pivot_wider(pairs_long, names_from = !!sym(batch_id), values_from = !! sym(benchmark_name))

  if (num_batches == 2 & add_diff)
  {
    pairs_wide <- mutate(pairs_wide, diff = !! sym(batch_names[1]) - !! sym(batch_names[2]))
  }

  return (pairs_wide)
}
