library(tidyverse)
library(gridExtra)

create_categories <- function(tbl, n_cat_per_feat) {
  #' create categories from feature space
  #' 
  #' @description cut two-dimensional feature space with equidistant spacing
  #' into equally large categories
  #' columns starting with x are assumed to be features
  #' @param tbl \code{tibble} containing each of the two features in a column
  #' @param n_cat_per_feat \code{integer} stating how many categories to create
  #' @return the \code{tibble} with an added column stating the category
  #' 
  col_nms <- names(tbl)[starts_with("x", ignore.case = FALSE, vars = names(tbl))]
  mappings <- map(tbl[, col_nms], cut, n_cat_per_feat, labels = FALSE) %>% as_tibble()
  names(mappings) <- str_c(names(mappings), "_cat")
  tbl <- cbind(tbl, mappings) %>% as_tibble()
  tbl$category <- interaction(tbl$x1_cat, tbl$x2_cat)
  levels(tbl$category) <- 1:nlevels(tbl$category)
  tbl$category <- as.numeric(tbl$category)
  return(tbl)
}


create_shepard_categories <- function(tbl, type, dim_anchor){
  #' create Shepard et al. categories with binary features
  #' 
  #' @description cut three-dimensional feature space into categories using Shepard's types
  #' @param tbl \code{tibble} containing each of the three features in a column
  #' @param type upper case Roman character stating the type number
  #' @param dim_anchor the "anchoring" dimension
  #' @return the \code{tibble} with an added column stating the category
  #' 
  if (type == "I") {
    thx <- colMeans(tbl[, dim_anchor])
    tbl$category <- cut(tbl$x2, c(0, thx, Inf), labels = FALSE)
  }
  return(tbl)
}


plot_clustered_grid <- function(tbl, stepsize_cat) {
  #' plot clusters in grid
  #' 
  #' @description plot categories in a 2d space with equidistanct spacing
  #' @param tbl \code{tibble} containing the two features and the category as columns
  #' @param stepsize_cat \code{double} the stepsize to be shown on the axes and on the grid
  #' @return the ggplot2 object
  #' 
  ggplot(tbl, aes(x1, x2, group = category)) +
    geom_raster(aes(fill = category)) +
    theme_bw() + 
    theme(
      panel.background = element_rect(fill = NA),
      panel.ontop = TRUE,
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(size = .1),
    ) +
    scale_fill_gradient(name = "Category\n",
                        low = "#FFFFFF",
                        high = "#012345") +
    scale_x_continuous(breaks = seq(0, max_x, stepsize_cat)) +
    scale_y_continuous(breaks = seq(0, max_x, stepsize_cat)) +
    labs(
      x = expression(X["1"]),
      y = expression(X["2"])
    )
}

plot_arrangement <- function(pl, n_cols = 2) {
  #' plot a list of plots on one page
  #' 
  #' @param pl all the ggplots
  #' @param n_cols nr columns of the page layout
  n_plots <- length(pl)
  n_rows <- ceiling(n_plots / n_cols)
  marrangeGrob(pl, nrow = n_rows, ncol = n_cols)
}

extract_posterior <- function(posterior, m_nb) {
  #' extract posterior of the true values
  #' 
  #' @param posterior posterior probabilities of all categories given data
  #' @param m_nb a trained naive bayes model
  tbl_post <- cbind(posterior, category = as.character(tbl_new$category)) %>% as_tibble()
  tbl_post[, levels(tbl_new$category)] <- map(tbl_post[, levels(tbl_new$category)], as.numeric)
  tbl_post_long <- tbl_post %>%
    pivot_longer(cols = levels(tbl_new$category)) %>%
    filter(category == name)
  l_post <- tbl_post_long %>%
    summarize(sum(log(value)))
  l_out <- list(tbl_post_long, l_post)
  return(l_out)
}


agg_and_join <- function(tbl, timepoint_str, tbl_centers){
  #' aggregate prior and/or sampled values
  #' 
  #' @param tbl tibble with prior and/or samples
  #' @param timepoint_str a string stating the timepoint of learning
  #' @param tbl_centers a tibble with the category centroids
  #' 
  tbl_tmp <- tbl %>% mutate(
    timepoint == timepoint_str) %>% group_by(
      stim_id, category, timepoint
    ) %>% summarize(
      x1 = mean(x1),
      x2 = mean(x2)
    ) %>% ungroup() %>%
    relocate(x1, x2, .after = stim_id)
  tbl_results <- left_join(
    tbl_tmp, 
    tbl_centers, 
    by = c("category", "timepoint"),
    suffix = c("_data", "_center")
  )
  tbl_results$timepoint <- factor(
    tbl_results$timepoint, 
    levels = c("Before Training", "After Training")
  )
  return(tbl_results)
}


center_of_category <- function(m, timepoint_str){
  #' model-based category centroids
  #' 
  #' @param m the fitted naive Bayes model
  #' @param timepoint_str a string stating the timepoint of learning
  #' 
  map(m$tables, function(x) x[1, ]) %>% 
    as_tibble() %>%
    mutate(
      category = categories,
      timepoint = timepoint_str
    ) 
}


add_centers <- function(tbl_new, nstart, m_nb_initial, m_nb_update, categories) {
  #' add category centers
  #' 
  #' @param tbl_new a tibble with all the accepted samples
  #' @param nstart nr of different stimuli
  #' @param m_nb_initial the originally fitted naive Bayes model (i.e., prior model)
  #' @param m_nb_update the most recently updated naive Bayes model
  #' @param categories vector with unique categories
  #' 
  nnew <- nrow(tbl_new) - nstart
  tbl_new$timepoint <- c(rep("Before Training", nstart), rep("After Training", nnew))
  tbl_centers <- rbind(
    center_of_category(m_nb_initial, "Before Training"),
    center_of_category(m_nb_update, "After Training")
    )
  tbl_before <- tbl_new %>% filter(timepoint == "Before Training")
  l <- map2(
    list(tbl_before = tbl_before, tbl_all = tbl_new),
    list("Before Training", "After Training"),
    agg_and_join,
    tbl_centers = tbl_centers
  )
  return(l)
}

plot_moves <- function(tbl_results) {
  ggplot(tbl_results, aes(x1_data, x2_data, group = as.numeric(category))) +
    geom_point(aes(color = as.numeric(category))) +
    geom_point(aes(x1_center, x2_center, color = as.numeric(category)), size = 3) +
    geom_segment(
      aes(
        x = x1_data, y = x2_data, xend = x1_center, yend = x2_center, 
        color = as.numeric(category)
      ),
      arrow = arrow(length = unit(.1, "inches"))
    ) +
    facet_wrap(~ timepoint, scales = "free") +
    theme_bw() +
    coord_cartesian(xlim = c(thx[1] - 1, thx[2] + 1), ylim = c(thx[1] - 1, thx[2] + 1)) +
    scale_color_viridis_c(name = "Category") +
    labs(
      x = "X1",
      y = "X2"
    )
}

plot_cat_probs <- function(tbl_posteriors) {
  ggplot(tbl_posteriors, aes(value)) +
    geom_histogram(aes(y = ..density..), alpha = .5, color = "black") +
    geom_density(aes(y = ..density..), color = "purple", size = 1) +
    facet_wrap(~ timepoint) +
    theme_bw() +
    labs(
      x = "Posterior Probability",
      y = "Probability Density"
    )
}

