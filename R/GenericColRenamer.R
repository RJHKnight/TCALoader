
# Rename to snake case, handle special characters and generic phrases
generic_rename <- function(input)
{
  old_names <- colnames(input)
  new_names = get_new_names(old_names)

  for (i in 1:length(old_names))
  {
    input <- rename_col(input, old_names[i], new_names[i])
  }

  return (input)
}

get_new_names <- function(old_names)
{
  snake_case <- snakecase::to_snake_case(old_names, sep_in = "\\s|\\$")
  snake_case <- stringr::str_replace_all(snake_case, "\\%", "pct_")
  snake_case <- stringr::str_replace_all(snake_case, "deviation\\(bps\\)", "dev")
}
