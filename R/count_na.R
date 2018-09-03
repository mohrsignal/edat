#' Count NA values
#'
#' \code{count_na} counts NA values
#' 
#' Adapted from https://sebastiansauer.github.io/sum-isna/
count_na <- function(df) {
  
  df %>% purrr::map_df(~sum(is.na(.)))
}