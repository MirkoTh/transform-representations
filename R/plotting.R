
plot_clustered_grid <- function(tbl, stepsize_cat) {
  #' plot clusters in grid
  #' 
  #' @description plot categories in a 2d space with equidistanct spacing
  #' @param tbl \code{tibble} containing the two features and the category as columns
  #' @param stepsize_cat \code{double} the stepsize to be shown on the axes and on the grid
  #' @return the ggplot2 object
  #' 
  max_x <- max(tbl$x1, tbl$x2)
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


plot_arrangement <- function(pl, n_cols = 2, n_rows = NULL) {
  #' plot a list of plots on one page
  #' 
  #' @param pl all the ggplots
  #' @param n_cols nr columns of the page layout
  n_plots <- length(pl)
  if (is.null(n_rows)) {
    n_rows <- ceiling(n_plots / n_cols)
  }
  marrangeGrob(pl, nrow = n_rows, ncol = n_cols, top = quote(paste("")))
}


plot_moves <- function(tbl_results, l_info) {
  if (l_info$category_shape == "ellipses") {
    tbl_results2 <- tbl_results %>% filter(category != 1)
  } else if (l_info$category_shape == "squares") {
    tbl_results2 <- tbl_results
  }
  space_edges <- l_info$space_edges
  ggplot(tbl_results, aes(x1_data, x2_data, group = as.numeric(category))) +
    geom_point(aes(color = as.numeric(category))) +
    geom_point(aes(x1_center, x2_center, color = as.numeric(category)), size = 3) +
    geom_segment(
      data = tbl_results2, aes(
        x = x1_data, y = x2_data, xend = x1_center, yend = x2_center, 
        color = as.numeric(category)
      ),
      arrow = arrow(length = unit(.1, "inches"))
    ) +
    facet_wrap(~ timepoint, scales = "free") +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    coord_cartesian(
      xlim = c(space_edges[1] - 1, space_edges[2] + 1), 
      ylim = c(space_edges[1] - 1, space_edges[2] + 1)
    ) +
    scale_color_viridis_c(name = "Category") +
    labs(
      x = bquote(x[1]),
      y = bquote(x[2]),
      title = str_c(
        "Prior SD = ", l_info$prior_sd, ", ",
        "Sampling = ", l_info$sampling, "\n",
        "Constrain Space = ", l_info$constrain_space, ", ",
        "Model = ", l_info$cat_type
      )
    )
}


plot_cat_probs <- function(tbl_posteriors, l_info) {
  bw <- 10/nrow(tbl_posteriors)
  ggplot(tbl_posteriors, aes(value)) +
    geom_histogram(aes(y = ..density..), alpha = .5, color = "black", binwidth = bw) +
    geom_density(aes(y = ..density..), color = "purple", size = 1) +
    facet_wrap(~ timepoint) +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    labs(
      x = "Posterior Probability",
      y = "Probability Density",
      title = str_c(
        "Prior SD = ", l_info$prior_sd, "\n",
        "Sampling = ", l_info$sampling, "\n",
        "Constrain Space = ", l_info$constrain_space
      )
    )
}


plot_stimulus_movements <- function(l_tbl_stimulus) {
  #' plot prior and posterior means of a stimulus and connect them using colored arrows
  #' 
  #' @param tbl_stimulus tibble with the pivoted x1 and x2 coordinates
  #' 
  
  plot_tmp <- function(tbl_stimulus, title) {
    ggplot(tbl_stimulus) +
      geom_segment(aes(
        x = x1_bef, y = x2_bef, xend = x1_aft, yend = x2_aft,
        color = n_categories
      ), arrow = arrow(type = "closed", length = unit(.1, "inches"))) +
      geom_point(aes(x1_bef, x2_bef), size = 8, color = "white") +
      geom_point(aes(x1_bef, x2_bef), color = "black", size = 3) +
      #geom_point(aes(x1_aft, x2_aft, color = n_categories), position = position_dodge(.1)) +
      theme_bw() + 
      theme(plot.title = element_text(size=10)) +
      facet_grid(prior_sd ~ cat_type) +
      scale_color_brewer(palette = "Set1", name = "Nr. Categories") +
      labs(
        x = bquote(x[1]),
        y = bquote(x[2]),
        title = title
      )
  }
  map2(l_tbl_stimulus, names(l_tbl_stimulus), plot_tmp)
}


save_plots <- function(l_results, pl_stimulus_movement) {
  #' save required figures in figures dir
  #' 
  #' @param l_results nested result list
  #' @param pl_stimulus_movement plot visualizing movements for different nr cats
  #' 
  tbl_save_pl <- tibble(
    pl = c(
      expr(l_results[[1]][[2]][[1]]), expr(l_results[[2]][[2]][[1]]),
      expr(l_results[[3]][[2]][[1]]), expr(l_results[[4]][[2]][[1]]),
      expr(pl_stimulus_movement)
    ),
    filename = c(
      "4-Cats-Prototype-All-Movements.png", "4-Cats-Rule-All-Movements.png",
      "9-Cats-Prototype-All-Movements.png", "9-Cats-Rule-All-Movements.png",
      "Diverging-Movement-One-Stimulus.png"
    ),
    width = c(10, 10, 10, 10, 4),
    height = c(5, 5, 5, 5, 4)
  )
  save_figure_png <- function(pl, filename, width, height) {
    png(
      filename = str_c("figures/", filename), width = width, height = height,
      units = "in", res = 200
    )
    print(eval(pl))
    dev.off()
  }
  pwalk(tbl_save_pl, save_figure_png)
}


plot_marginals <- function(tbl_new, l_info) {
  #' plots marginal distributions of x1 and x2 for prior and posterior
  #' 
  #' @param tbl_new tbl with prior and samples values during category learning
  #' @param l_info parameter list
  #' @return the plot
  #' 
  if (nrow(tbl_new) == l_info$n_stimuli){
    nice_showcase <- round(max(tbl_new$x1[1:l_info$n_stimuli]) / 2 + 1)
  } else {
    tmp <- round(max(tbl_new$x1[1:l_info$n_stimuli]) / 2 + 1)
    nice_showcase <- sort(tbl_new$stim_id[tbl_new$stim_id >= tmp])[1]
  }
  
  
  n_accept_stimuli <- tbl_new %>% arrange(stim_id) %>%
    group_by(stim_id) %>% mutate(rwn = row_number(x1)) %>% 
    ungroup() %>% arrange(desc(rwn))
  n_accepted <- n_accept_stimuli %>% filter(stim_id == nice_showcase) %>% 
    select(rwn) %>% as_vector()
  if (!is_empty(n_accepted)) {n_accepted <- n_accepted %>% max()} else {n_accepted <- 0}
  if(n_accepted > 2) {
    showcase <- nice_showcase
  } else {
    showcase <- n_accept_stimuli %>% head(1) %>% select(stim_id) %>% as_vector()
  }
  tbl_tmp <- tbl_new %>% filter(stim_id == showcase) %>%
    group_by(timepoint) %>%
    summarize(
      x1_mean = mean(x1),
      x2_mean = mean(x2),
      x1_sd = sd(x1),
      x2_sd = sd(x2)
    ) %>% replace_na(list(x1_sd = 0, x2_sd = 0))
  if (nrow(tbl_tmp) == 1) {
    tbl_tmp[2, ] <- list("After Training", 0, 0, l_info$prior_sd, l_info$prior_sd)
  }
  
  tbl_samples <- normal_quantiles_given_pars(tbl_tmp, l_info)
  
  tbl_plt <- tbl_samples %>% group_by(Variable, Timepoint) %>% 
    mutate(rwn = row_number(Value)) %>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
    select(-rwn) %>%
    mutate(Timepoint = recode_factor(Timepoint, prior = "Prior", posterior = "Posterior"))
  
  pl <- ggplot(tbl_plt, aes(x1, x2, group = Timepoint)) +
    geom_point(aes(color = Timepoint), shape = 1) +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = bquote(x[1]),
      y = bquote(x[2]),
      title = str_c(
        "Prior SD = ", l_info$prior_sd, "\n",
        "Sampling = ", l_info$sampling, "\n",
        "Constrain Space = ", l_info$constrain_space
      )
    )
  
  pl_marginals <- ggMarginal(pl, groupColour = TRUE, type="density", size=10)
  return(pl_marginals)
}


diagnostic_plots <- function(l_categorization) {
  #' plots showing qualitative predictions from different models
  #' 
  #' @param l_categorization list with category learning results
  #' @return a list with samples (a) from the prior, 
  #' (b) from the category learning period, and (c) with (a) and (b) together
  #' 
  env <- rlang::current_env()
  list2env(l_categorization, env)
  l_results <- add_centers(tbl_new, l_m, l_info)
  tbl_posterior_long <- extract_posterior(posterior, tbl_new)[[1]]
  tbl_posteriors <- tbl_prior_long %>%
    mutate(timepoint = "Before Training") %>%
    rbind(
      tbl_posterior_long %>%
        mutate(timepoint = "After Training")
    ) %>%
    mutate(
      timepoint = fct_relevel(timepoint, "Before Training", "After Training")
    )
  
  # add some fields that function from analysis script can be re-used
  l_results$tbl_posterior$n_categories <- max(as.numeric(as.character(l_results$tbl_posterior$category)))
  l_results$tbl_posterior$x1_response <- l_results$tbl_posterior$x1_data
  l_results$tbl_posterior$x2_response <- l_results$tbl_posterior$x2_data
  l_results$tbl_posterior <- add_distance_to_nearest_center(l_results$tbl_posterior, is_simulation = TRUE)
  l_results$tbl_posterior$participant_id <- 1
  l_results$tbl_posterior$session <- l_results$tbl_posterior$timepoint
  
  # add prior to stimulus_ids, which have not been sampled, as posterior = prior for these stimuli
  tbl_bef_aft <- l_results$tbl_posterior %>%
    filter(timepoint == "Before Training") %>%
    left_join(
      l_results$tbl_posterior %>% 
        filter(timepoint == "After Training") %>%
        select(stim_id, x1_data, x2_data), by = "stim_id", suffix = c("_bef", "_aft")) %>%
    filter(!is.na(x1_data_aft))
  samples_posterior_stim_id <- unique(tbl_bef_aft$stim_id)
  tbl_after_complete <- l_results$tbl_posterior %>% 
    filter(timepoint == "Before Training" & !(stim_id %in% samples_posterior_stim_id)) %>%
    rbind(l_results$tbl_posterior %>% 
            filter(timepoint == "After Training") %>% 
            group_by(across(-c(x1_data, x2_data))) %>%
            summarize(x1_data = mean(x1_data), x2_data = mean(x2_data))) %>% ungroup() %>%
    mutate(timepoint = "After Training") %>% arrange(stim_id)
  l_results$tbl_posterior <- rbind(
    l_results$tbl_posterior %>% filter(timepoint == "Before Training"),
    tbl_after_complete)
  
  pl_avg_move <- plot_distance_to_category_center(l_results$tbl_posterior)
  
  # movement of stimulus representations before vs. after
  pl_centers <- plot_moves(l_results$tbl_posterior, l_info)
  # histograms of posterior category probabilities before vs. after
  pl_post <- plot_cat_probs(tbl_posteriors, l_info)
  # x1 and x2 marginals for one stimulus before vs. after
  pl_marginals <- plot_marginals(tbl_new, l_info)
  # plotting list
  l_plots <- list(
    pl_centers, pl_post, pl_marginals, pl_avg_move
  )
  # list with results and plots
  l_out <- list(
    l_results, l_plots
  )
  return(l_out)
}


save_results_plots <- function(tbl_info, l_results_plot, p_sd, n_cat) {
  #' helper function to plot and save prior means alongside posterior means
  #' a pdf with model type as columns (exemplar, prototype, rule from l.t.r.)
  #' and the variable 'constrain space' in the rows
  #' two different pages are printed for the two sampling algorithms
  #' 

  # select prior means vs. posterior means plots
  select_nested_plot <- function(l, idx) {
    l[[idx]][[2]][[1]]
  }
  
  tbl_summary <- tbl_info %>%
    filter(
      prior_sd == p_sd,
      n_categories == n_cat
    ) %>% 
    arrange(
      sampling, cat_type, constrain_space
    ) %>%
    select(condition_id)
  
  l_pl_summary <- map(tbl_summary$condition_id, select_nested_plot, l = l_results_plots)
  l_pl_out <- plot_arrangement(l_pl_summary, n_cols = 3, n_rows = 2)
  
  file_nm <- str_c(
    "figures/",
    "prior-sd-", p_sd, "-n_cats-", n_cat, ".pdf"
  )
  return(list(l_pl_out, file_nm))
}

