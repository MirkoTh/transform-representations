fix_data_types <- function(tbl, fs, ns) {
  #' fix data types of columns of tbl
  #' 
  #' @description factors as factors and numerics as numerics, leave characters
  #' 
  #' @param tbl the columns of this tibble are changed
  #' @param fs vector of factors
  #' @param ns vector of numerics
  #' @return the tibble with the changed data types
  #' 
  cols <- colnames(tbl)
  fs_available <- intersect(fs, cols)
  ns_available <- intersect(ns, cols)
  tbl[, fs_available] <- map(tbl[, fs_available], as.factor)
  tbl[, ns_available] <- map(tbl[, ns_available], as.numeric)
  return(tbl)
}

load_data <- function() {
  #' load continuous reproduction ("cr") and category learning ("cat") data
  #' 
  #' @description loads data and declares factor and numeric columns in the two tibbles
  #' 
  #' @return a list with the two tibbles
  #' 
  # read individual performance
  js_cr_txt <- read_file("experiments/2022-02-category-learning/data/cr-participant-2.json")
  js_cr_txt <- str_c("[", str_replace(js_cr_txt, ",$", ""), "]")
  tbl_cr <- jsonlite::fromJSON(js_cr_txt) %>% as_tibble()
  
  
  # only pilot data have to be corrected currently...
  tbl_cr$session <- as.numeric(tbl_cr$session)
  tbl_cr[148:nrow(tbl_cr), "session"] <- 2 + tbl_cr[148:nrow(tbl_cr), "session"]
  
  
  js_cat_txt <- read_file("experiments/2022-02-category-learning/data/cat-participant-2.json")
  js_cat_txt <- str_c("[", str_replace(js_cat_txt, ",$", ""), "]")
  tbl_cat <- jsonlite::fromJSON(js_cat_txt) %>% as_tibble()
  
  factors <- c("participant_id", "session")
  numerics <- c("trial_id", "x1_true", "x2_true", "x1_response", "x2_response", "rt")
  tbl_cr <- fix_data_types(tbl_cr, factors, numerics)
  tbl_cat <- fix_data_types(tbl_cat, factors, numerics)
  # add stim_id
  tbl_cr <- tbl_cr %>% group_by(participant_id) %>% arrange(x1_true, x2_true) %>% 
    mutate(stim_id = row_number(x1_true)) %>% ungroup()
  
  l_data <- list(tbl_cr, tbl_cat)
  return(l_data)
}



plot_marginals_one_session <- function(idx_session, tbl) {
  #' scatter plot of 2D deviations with marginals
  #' 
  #' @description makes a scatter plot of deviation of responses and adds marginal distributions
  #' @param idx_session which session should be plotted (i.e. before or after categorization task)
  #' @param tbl the tibble with the by-trial responses
  #' 
  #' @return the complete ggMarginal plot object
  #' 
  # read individual performance
  idx_color <- ifelse(idx_session == 1, 1, 2)
  title <- c("Before Category Learning", "After Category Learning")[idx_color]
  col <- c("#3399FF", "#990099")[idx_color]
  tbl_plot <- tbl %>% filter(session == idx_session)
  pl <- ggplot(tbl_plot, aes(x1_deviation, x2_deviation)) +
    geom_point(color = col, shape = 1, size = 2) +
    geom_density2d() +
    theme_bw() +
    theme(plot.title = element_text(size=20)) +
    scale_color_brewer(palette = "Set1") +
    # somehow ggMarginal does not like coord_cartesian...
    # the following excludes some of the responses, though
    scale_x_continuous(limits = c(-50, 50)) +
    scale_y_continuous(limits = c(-50, 50)) +
    labs(
      x = bquote(x[1]),
      y = bquote(x[2]),
      title = title
    )# + coord_cartesian(xlim = c(-50, 50), ylim = c(-50, 50))
  
  pl_marginals <- ggMarginal(pl, fill = col, type = "histogram", bins = 15)
  return(pl_marginals)
}


plot_2d_binned_heatmaps <- function(tbl, n_agg_x) {
  #' 2D heat maps of average deviation (L2 norm) from true values
  #' 
  #' @description plots heat maps of avg deviation before and after the categorization task
  #' @param tbl the tibble with the by-trial responses
  #' @param n_agg_x into how many bins should x1 and x2 be cut?
  #' 
  #' @return the plot
  #' 
  # read individual performance
  lims <- tbl %>% 
    summarise(min_x = min(x1_true), max_x = max(x2_true)) %>%
    mutate(min_x = min_x - 1, max_x = max_x + 1) %>%
    as_vector()
  cutpoints <- seq(lims[1], lims[2], length.out = n_agg_x + 1)
  tbl_cr_agg <- tbl %>%
    filter(session %in% c(1, 3)) %>%
    mutate(
      x1_true_binned = cut(x1_true, cutpoints, labels = FALSE),
      x2_true_binned = cut(x2_true, cutpoints, labels = FALSE)
    ) %>% group_by(participant_id, session, x1_true_binned, x2_true_binned) %>%
    summarise(avg_deviation_x1x2 = mean(sqrt(x1_deviation^2 + x2_deviation^2))) %>%
    ungroup()
  pl <- ggplot(tbl_cr_agg, aes(x1_true_binned, x2_true_binned)) +
    geom_tile(aes(fill = avg_deviation_x1x2)) +
    scale_fill_gradient(name = "Avg. Deviation", low = "#009966", high = "#FF6666") +
    labs(
      x = "Spikiness of Head (Binned)",
      y = "Fill of Belly (Binned)"
    ) + facet_wrap(~ session) + theme_bw()
  
  return(pl)
}



plot_1d_marginals <- function(tbl) {
  #' histograms with freq polys overlaid of 1D deviation (i.e., distance) from true values
  #' 
  #' @description plots histograms and freq polys of 1D distance from true values
  #' facets for x1/x2 and before/after category learning are teased apart
  #' @param tbl the tibble with the by-trial responses
  #' 
  #' @return the plot
  #' 
  # read individual performance
  tbl %>% filter(session %in% c(1, 3)) %>%
    pivot_longer(c(x1_deviation, x2_deviation), names_to = "var_deviation", values_to = "val_deviation") %>%
    mutate(
      var_deviation = factor(var_deviation, labels = c("Head Spikiness", "Belly Fill")),
      session = factor(session, labels = c("Before Cat. Learning", "After Cat. Learning"))
    ) %>%
    ggplot(aes(val_deviation, group = session)) +
    geom_histogram(aes(fill = session), bins = 10, alpha = .5, color = "black") +
    geom_freqpoly(aes(color = session), bins = 10) +
    facet_wrap(~ var_deviation + session) +
    theme_bw() +
    scale_color_brewer(name = "Timepoint", palette = "Set1") +
    scale_fill_brewer(name = "Timepoint", palette = "Set1") +
    labs(
      x = "Deviation from True Value",
      y = "Nr. Responses"
    )
}


category_centers <- function() {
  #' helper function to define category centers
  #' 
  #' @description returns x1 and x2 means of the ellipse categories (n_categories - 1)
  #' 
  #' @return the list with the centers of the 2 and 4 category conditions
  #' 
  # read individual performance
  x1 <- seq(0, 11, by = 1)
  x2 <- seq(0, 11, by = 1)
  tbl_tmp <- crossing(x1, x2)
  tbl_tmp <- tbl_tmp %>% mutate(stim_id = seq(1, 144, by = 1))
  l_ellipses <- map(c(2, 4), create_ellipse_categories, tbl = tbl_tmp)
  cat_boundaries_2 <- l_ellipses[[1]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * 8 - 2, y_rotated = (y_rotated + 1) * 8 - 2)
  cat_boundaries_4 <- l_ellipses[[2]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * 8 - 2, y_rotated = (y_rotated + 1) * 8 - 2)
  
  category_means <- function(tbl) {
    tbl %>% group_by(category) %>%
      summarize(x_mn = mean(x_rotated), y_mn = mean(y_rotated)) %>% ungroup()
  }
  cat_2_mns <- category_means(cat_boundaries_2)
  cat_4_mns <- category_means(cat_boundaries_4)
  l_cat_mns <- list(cat_2_mns, cat_4_mns)
  return(l_cat_mns)
}
