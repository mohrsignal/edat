count_na <- function(df) {
  
  df %>% map_df(~sum(is.na(.)))
}