plot_categorization_heatmaps <- function(tbl, n_cat) {
  #' means and modes of category responses in 2D heat map 
  #' 
  #' @description plots heat maps of by-participant avg and modal category responses
  #' @param tbl the aggregated tibble with avg and mode responses by true x1 and x2 values
  #' @param n_cat filter on number of categories (i.e., between participants)
  #' 
  #' @return the plot
  #' 
  
  # order participants in order of decreasing accuracy
  tbl <- tbl %>% arrange(desc(mean_accuracy)) %>%
    mutate(participant_id = fct_inorder(substr(participant_id, 1, 6), ordered = TRUE)) %>% 
    filter(n_categories == n_cat)
  
  ggplot(tbl, aes(x1_true, x2_true, group = value)) +
    geom_raster(aes(fill = value)) +
    geom_label(aes(
      10, 8, label = str_c(round(mean_accuracy, 2))
    ), size = 3) +
    facet_wrap(participant_id ~  name) +
    scale_fill_viridis_c(name = "Category\nResponse") +
    theme_bw() +
    labs(
      x = "Head Spikiness",
      y = "Belly Fill"
    )
}
