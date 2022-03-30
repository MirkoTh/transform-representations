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
txt_ex <- "this is a bad example }how can you add something? Especially, when there is several examples of the thing } to be replaced?"
str_replace_all(txt_ex, "\\}", "\\},")
load_data <- function(path_data) {
  #' load continuous reproduction ("cr") and category learning ("cat") data
  #' 
  #' @description loads data and declares factor and numeric columns in the two tibbles
  #' 
  #' @return a list with the two tibbles
  #' 
  # read individual performance
  files_dir <- dir(path_data)
  paths_cat <- str_c(path_data, files_dir[startsWith(files_dir, "cat")])
  paths_cr <- str_c(path_data, files_dir[startsWith(files_dir, "cr")])
  
  json_to_tibble <- function(path_file) {
    js_txt <- read_file(path_file)
    js_txt <-str_c("[", str_replace_all(js_txt, "\\}", "\\},"), "]")
    js_txt <- str_replace(js_txt, ",\n]", "]")
    tbl_cr <- jsonlite::fromJSON(js_txt) %>% as_tibble()
    return(tbl_cr)
  }
  tbl_cr <- reduce(map(paths_cr, json_to_tibble), rbind) %>% filter(session %in% c(1, 2))
  tbl_cat <- reduce(map(paths_cat, json_to_tibble), rbind)
  
  
  # only pilot data have to be corrected currently...
  tbl_cr$session <- as.numeric(tbl_cr$session)
  #tbl_cr[148:nrow(tbl_cr), "session"] <- 2 + tbl_cr[148:nrow(tbl_cr), "session"]
  
  factors <- c("participant_id", "session", "cat_true")
  numerics <- c("trial_id", "x1_true", "x2_true", "x1_response", "x2_response", "rt")
  tbl_cr <- fix_data_types(tbl_cr, factors, numerics)
  tbl_cat <- fix_data_types(tbl_cat, factors, numerics)
  # add stim_id
  tbl_cr <- tbl_cr %>% group_by(participant_id) %>% arrange(x1_true, x2_true) %>% 
    mutate(stim_id = rep(seq(1, 100, by = 1), each = 2*length(unique(tbl_cr$participant_id)))) %>% ungroup()
  
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
  tbl %>% filter(session %in% c(1, 2)) %>%
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
  l_ellipses <- map(c(2, 3), create_ellipse_categories, tbl = tbl_tmp)
  cat_boundaries_2 <- l_ellipses[[1]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * 8 - 2, y_rotated = (y_rotated + 1) * 8 - 2)
  cat_boundaries_3 <- l_ellipses[[2]][[2]] %>% as_tibble() %>% mutate(x_rotated = (x_rotated + 1) * 8 - 2, y_rotated = (y_rotated + 1) * 8 - 2)
  
  category_means <- function(tbl) {
    tbl %>% group_by(category) %>%
      summarize(x_mn = mean(x_rotated), y_mn = mean(y_rotated)) %>% ungroup()
  }
  cat_2_mns <- category_means(cat_boundaries_2)
  cat_3_mns <- category_means(cat_boundaries_3)
  l_cat_mns <- list(cat_2_mns, cat_3_mns)
  return(list(l_cat_mns, l_ellipses))
}


add_distance_to_nearest_center <- function(tbl_cr) {
  #' add distance to closest category centroid
  #' 
  #' @description calculates distances to all possible category centroids and returns min of those
  #' @param tbl_cr the tibble with the by-trial responses
  #' 
  #' @return the tibble with the min distance as added column
  #' 
  # read individual performance
  l_tmp <- category_centers()
  l_cat_mns <- l_tmp[[1]]
  l_ellipses <- l_tmp[[2]]
  euclidian_distance_to_center <- function(x_mn, y_mn, tbl, is_response) {
    if(is_response) {
      sqrt((tbl$x1_response - x_mn)^2 + (tbl$x2_response - y_mn)^2)
    } else {
      sqrt((tbl$x1_true - x_mn)^2 + (tbl$x2_true - y_mn)^2)
    }
    
  }
  # split by nr of categories
  l_tbl_cr <- split(tbl_cr, tbl_cr$n_categories)
  # uncomment only when data from category 2 condition is available
  # as only one category center, can directly compute distance from response to that center
  # tbl_d2 <- pmap(l_cat_mns[[1]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["2"]], is_response = TRUE) %>% 
  #   unlist() %>% matrix(ncol = 1) %>% as.data.frame() %>% tibble()
  # l_tbl_cr[["2"]] <- l_tbl_cr[["2"]] %>% cbind(tbl_d2) %>% left_join(l_ellipses[[1]][[1]] %>% select(stim_id, category), by = c("stim_id"))
  # colnames(tbl_d2) <- c("dmin")
  
  # here, we first have to compute what the closest center of a given stimulus is and then index using that id
  tbl_d3_true <- pmap(l_cat_mns[[2]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["3"]], is_response = FALSE) %>% 
    unlist() %>% matrix(ncol = 2) %>% as.data.frame() %>% tibble()
  colnames(tbl_d3_true) <- c("d1", "d2")
  tbl_d3_response <- pmap(l_cat_mns[[2]][, c("x_mn", "y_mn")], euclidian_distance_to_center, tbl = l_tbl_cr[["3"]], is_response = TRUE) %>% 
    unlist() %>% matrix(ncol = 2) %>% as.data.frame() %>% tibble()
  colnames(tbl_d3_response) <- seq(1:2)
  col_idx_closest <- apply(tbl_d3_true, 1, function(x) which(x == min(x)))
  tbl_d3_response$col_idx_closest <- col_idx_closest
  tbl_d3_response$d_closest <- apply(tbl_d3_response, 1, function(x) x[1:2][x[3]])
  d_closest <- as_vector(tbl_d3_response$d_closest) %>% unname()
  
  l_tbl_cr[["3"]] <- l_tbl_cr[["3"]] %>% cbind(d_closest) %>%
    left_join(l_ellipses[[2]][[1]] %>% select(stim_id, category), by = c("stim_id"))
  
  tbl_cr <- rbind(l_tbl_cr[["3"]], l_tbl_cr[["2"]])
  return(tbl_cr)
}
