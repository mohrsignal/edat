central_measures <- function(df, x_var, group_var) {
  
  x_var <- enquo(x_var)
  group_var <- enquo(group_var)
  
  df %>% 
    group_by(!! group_var) %>% 
    summarize(med = median(!! x_var, na.rm = T),
              avg = mean(!! x_var, na.rm = T)) %>% 
    ungroup()
}