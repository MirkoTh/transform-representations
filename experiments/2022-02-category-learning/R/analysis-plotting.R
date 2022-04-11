dg <- position_dodge(width = .8)


plot_2d_binned_heatmaps <- function(tbl_checker, tbl_avg) {
  #' 2D heat maps of average deviation (L2 norm) from true values
  #' 
  #' @description plots heat maps of avg deviation before and after the categorization task
  #' @param tbl_checker the already aggregated checkerboard tibble
  #' @param tbl_avg tbl with the average deviation in tbl_checker
  #' 
  #' @return the plot
  #' 
  pl <- ggplot(data = tbl_checker, aes(x1_true_binned, x2_true_binned)) +
    geom_tile(aes(fill = avg_deviation_x1x2)) +
    scale_fill_gradient2(name = "Avg. Deviation", low = "#009966", high = "#FF6666", midpoint = 25.5) +
    geom_label(data = tbl_avg, aes(2, 2, label = str_c("Avg. Dev. = ", round(avg_deviation, 0)))) +
    labs(
      x = "Spikiness of Head (Binned)",
      y = "Fill of Belly (Binned)"
    ) + facet_wrap(~ participant_id) + theme_bw()
  
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
  
  
  plot_2d_points_marginal <- function(tbl, participant) {
    tbl <- tbl %>% filter(participant_id == participant)
    pl <- ggplot(tbl, aes(x1_deviation, x2_deviation)) +
      geom_point(color = col, shape = 1, size = 2) +
      geom_density2d() +
      theme_bw() +
      theme(plot.title = element_text(size = 10)) +
      scale_color_brewer(palette = "Set1") +
      # somehow ggMarginal does not like coord_cartesian...
      # the following excludes some of the responses, though
      scale_x_continuous(limits = c(-84, 84)) +
      scale_y_continuous(limits = c(-84, 84)) +
      labs(
        x = "Head Spikiness",
        y = "Belly Size",
        title = substr(participant, 1, 6)
      )# + coord_cartesian(xlim = c(-50, 50), ylim = c(-50, 50))
    
    pl_marginals <- ggMarginal(pl, fill = col, type = "histogram", bins = 15)
    return(pl_marginals)
  }
  participants <- unique(tbl_cr$participant_id)
  l_pl <- map(participants, plot_2d_points_marginal, tbl = tbl_plot)
  
  pages_plots <- marrangeGrob(l_pl, ncol = 4, nrow = 4)  
  return(pages_plots)
}


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


histograms_overall_accuracy <- function(tbl_cat_overview, tbl_chance2) {
  ggplot() + 
    geom_histogram(
      data = tbl_cat_overview, aes(mean_accuracy, group = participant_id), 
      fill = "black", color = "white"
    ) +
    geom_segment(
      data = tbl_chance2, aes(
        x = p_chance, xend = p_chance, y = 0, yend = 3, group = n_categories
      ), linetype = "dotdash", color = "red") +
    facet_wrap(~ n_categories) +
    theme_dark() +
    labs(
      x = "Overall Accuracy",
      y = "Participant Counts"
    )
}


plot_categorization_accuracy_against_blocks <- function(tbl_cat) {
  tbl_cat_agg <- tbl_cat %>% group_by(participant_id, n_categories, cat_true, trial_id_binned) %>%
    summarize(
      accuracy_mn_participant = mean(accuracy)
    ) %>%  ungroup()
  
  tbl_cat_agg_ci <- summarySEwithin(
    tbl_cat_agg, "accuracy_mn_participant", c("n_categories"), 
    c("cat_true", "trial_id_binned"), "participant_id"
  ) %>% as_tibble()
  tbl_cat_agg_ci$trial_id_binned <- as.numeric(as.character(tbl_cat_agg_ci$trial_id_binned))
  
  tbl_chance <- chance_performance_cat(tbl_cat)
  tbl_chance$block <- as.numeric(as.character(tbl_chance$block))
  
  ggplot() + 
    geom_errorbar(data = tbl_cat_agg_ci, aes(
      trial_id_binned, ymin = accuracy_mn_participant - ci, 
      ymax = accuracy_mn_participant + ci, color = cat_true
    ), width = .25, position = dg) +
    geom_line(data = tbl_cat_agg_ci, aes(
      trial_id_binned, accuracy_mn_participant, group = cat_true, color = cat_true
    ), position = dg) +
    geom_point(data = tbl_cat_agg_ci, aes(
      trial_id_binned, accuracy_mn_participant, group = cat_true
    ), position = dg, color = "white", size = 3) +
    geom_point(data = tbl_cat_agg_ci, aes(
      trial_id_binned, accuracy_mn_participant, group = cat_true, color = cat_true
    ), position = dg) +
    geom_line(
      data = tbl_chance, aes(block, prop_chance, group = 1), 
      linetype = "dotdash", size = .5) +
    facet_wrap(~ n_categories) +
    coord_cartesian(ylim = c(0, 1)) +
    scale_color_brewer(name = "Category", palette = "Set1") +
    scale_x_continuous(breaks = seq(2, 10, by = 2)) +
    labs(
      x = "Block of 20 Trials",
      y = "Categorization Accuracy"
    ) + theme_bw()
}
