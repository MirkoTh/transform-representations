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
  idx_session <- 1
  idx_color <- ifelse(idx_session == 1, 1, 2)
  col <- c("#3399FF", "#990099")[idx_color]
  pl <- ggplot(tbl %>% filter(session == idx_session), aes(x1_deviation, x2_deviation, group = session)) +
    geom_point(color = col, shape = 1, size = 2) +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = bquote(x[1]),
      y = bquote(x[2])
    ) + coord_cartesian(xlim = c(-50, 50), ylim = c(-50, 50))
  
  pl_marginals <- ggMarginal(pl, groupColor = TRUE, fill = col, type = "densigram")
  return(pl_marginals)
}

