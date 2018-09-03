dotplot <- function(df, discrete_var, continuous_var) {

  discrete_var <- enquo(discrete_var)
  continuous_var <- enquo(continuous_var)
  x_lab <- quo_name(discrete_var)
  y_lab <- quo_name(continuous_var)

  df %>%
    select(!! discrete_var,
           !! continuous_var) %>%
    na.omit() %>%
    ggplot(aes(x = reorder(!! discrete_var, !! continuous_var),
               y = !! continuous_var)) +
    geom_point(color = rgb(93, 165, 218, max = 255),
               size = 2) +
    coord_flip() +
    scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
    labs(title = paste0(y_lab, " of ", x_lab),
         caption = NULL,
         x = x_lab,
         y = y_lab) +
    theme_minimal() +
    theme(panel.background = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.major.y = element_line(linetype = "dashed",
                                            color = "grey80"),
          axis.line = element_line(color = rgb(140, 140, 140, max = 255)),
          strip.text = element_blank(),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 11, angle = 0),
          axis.text = element_text(size = 11),
          axis.ticks = element_blank())
}
