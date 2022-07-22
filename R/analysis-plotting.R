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
    scale_fill_gradient2(
      name = "Avg. Deviation", low = "#009966", high = "#FF6666", midpoint = 25.5
    ) + geom_label(size = 3, 
                   data = tbl_avg, aes(2.5, 2, label = str_c(
                     "AvgDev=", round(avg_deviation, 0), ", Tr=", n_trials, " NCat=", n_categories))
    ) +
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


plot_marginals_one_session <- function(idx_session, tbl, n_samples = 4) {
  #' scatter plot of 2D deviations with marginals
  #' 
  #' @description makes a scatter plot of deviation of responses and adds marginal distributions
  #' @param idx_session which session should be plotted (i.e. before or after categorization task)
  #' @param tbl the tibble with the by-trial responses
  #' @param n_samples the number of sampled participants to plot; has to be
  #' an even integer as half of n_samples participants are plotted per group
  #' 
  #' @return the complete ggMarginal plot object
  #' 
  if(n_samples %% 2 != 0) {
    stop("The number of samples to plot n_samples has to be even; please adjust accordingly")
  }
  # read individual performance
  idx_color <- ifelse(idx_session == 1, 1, 2)
  title <- c("Before Category Learning", "After Category Learning")[idx_color]
  col <- c("#3399FF", "#990099")[idx_color]
  tbl_plot <- tbl %>% filter(session == idx_session)
  
  
  plot_2d_points_marginal <- function(tbl, participant) {
    tbl <- tbl %>% filter(participant_id == participant)
    grp <- tbl$n_categories[1]
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
        title = str_c(substr(participant, 1, 6), ", ", grp)
      )# + coord_cartesian(xlim = c(-50, 50), ylim = c(-50, 50))
    
    pl_marginals <- ggMarginal(pl, fill = col, type = "histogram", bins = 15)
    return(pl_marginals)
  }
  
  participant_sample_groups <- tbl_cr %>% group_by(participant_id, n_categories) %>%
    count() %>% group_by(n_categories) %>% 
    mutate(
      id_random = substr(participant_id, sample(1:4, 1), sample(5:8, 1)),
      rwn = row_number(participant_id)) %>%
    filter(rwn <= n_samples / 2)
  
  l_pl <- map(participant_sample_groups$participant_id, plot_2d_points_marginal, tbl = tbl_plot)
  
  n_cols <- 4
  n_rows <- ceiling(n_samples / n_cols)
  pages_plots <- marrangeGrob(l_pl, ncol = n_cols, nrow = n_rows, top = title)  
  return(pages_plots)
}


plot_categorization_heatmaps <- function(tbl, n_cat, f_agg = "Mode") {
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
    filter(n_categories %in% n_cat, name == f_agg)
  
  ggplot() +
    geom_raster(data = tbl, aes(x1_true, x2_true, group = value, fill = value)) +
    geom_label(data = tbl, aes(
      10, 8, label = str_c(round(mean_accuracy, 2)), group = value
    ), size = 3) +
    facet_wrap(~ participant_id, ncol = 4) +
    scale_fill_viridis_c(name = "Category\nResponse") +
    theme_bw() +
    labs(
      x = "Head Spikiness",
      y = "Belly Fill"
    )
}


histograms_accuracies_rts <- function(tbl_cat_overview) {
  #' histogram of by-participant categorization accuracy and
  #' categorization rts
  #' 
  tbl_cat_overview %>% arrange(mean_accuracy) %>%
    mutate(participant_id = fct_inorder(factor(substr(participant_id, 1, 6)))) %>%
    pivot_longer(c(mean_accuracy, mean_rt)) %>%
    mutate(
      name = factor(name),
      name = fct_relabel(name, ~ c("Mean Accuracy", "Mean RT (s)"))
    ) %>%
    ggplot(aes(value, group = participant_id)) +
    geom_histogram(aes(group = participant_id, fill = participant_id), color = "white") +
    facet_grid(n_categories ~ name, scale = "free_x") +
    scale_fill_viridis_d(name = "Participant ID")  +
    theme_dark() +
    labs(
      x = "Overall Accuracy",
      y = "Participant Counts"
    )
}


plot_categorization_accuracy_against_blocks <- function(tbl_cat, show_errorbars = TRUE) {
  #' 
  #' 
  #' @description overall categorization accuracy against blocks with 95% ci and
  #' individual categorization accuracies against blocks
  #' @param tbl_cat the tbl with by-trial category learning responses
  #' @param show_errorbars shoul 95% cis be displayed
  #' 
  #' @return a list with two plots
  #' 
  
  tbl_cat_agg <- tbl_cat %>% group_by(participant_id, n_categories, cat_true, trial_id_binned) %>%
    summarize(
      accuracy_mn_participant = mean(accuracy),
      n_categories = factor(n_categories)
    ) %>%  ungroup()
  levels(tbl_cat_agg$n_categories) <- str_c(levels(tbl_cat_agg$n_categories), " Categories")
  
  tbl_cat_agg_ci <- summarySEwithin(
    tbl_cat_agg, "accuracy_mn_participant", c("n_categories"), 
    c("cat_true", "trial_id_binned"), "participant_id"
  ) %>% as_tibble()
  tbl_cat_agg_ci$trial_id_binned <- as.numeric(as.character(tbl_cat_agg_ci$trial_id_binned))
  tbl_cat_agg_ci <- tbl_cat_agg_ci %>%
    mutate(cat_true = factor(cat_true, labels = c("Bukil", "Venak", "Monus", "Ladiv")))
  
  tbl_chance <- chance_performance_cat(tbl_cat)
  tbl_chance$block <- as.numeric(as.character(tbl_chance$block))
  
  pl_agg <- ggplot() + 
    geom_line(data = tbl_cat_agg_ci, aes(
      trial_id_binned, accuracy_mn_participant, group = cat_true, color = cat_true
    ), position = dg) +
    geom_point(data = tbl_cat_agg_ci, aes(
      trial_id_binned, accuracy_mn_participant, group = cat_true
    ), position = dg, color = "white", size = 3) +
    geom_point(data = tbl_cat_agg_ci, aes(
      trial_id_binned, accuracy_mn_participant, group = cat_true, color = cat_true
    ), position = dg) +
    # geom_line(
    #   data = tbl_chance, aes(block, prop_chance, group = 1), 
    #   linetype = "dotdash", size = .5) +
    facet_wrap(~ n_categories, scales = "free_x") +
    coord_cartesian(ylim = c(.25, 1)) +
    scale_color_brewer(name = "Category", palette = "Set1") +
    scale_x_continuous(breaks = seq(2, 14, by = 2)) +
    labs(
      x = "Block of 20 Trials",
      y = "Categorization Accuracy",
      caption = "Note. x-axes differ between panels"
    ) + theme_bw()
  
  if(show_errorbars) {
    pl_agg <- pl_agg + geom_errorbar(data = tbl_cat_agg_ci, aes(
      trial_id_binned, ymin = accuracy_mn_participant - ci, 
      ymax = accuracy_mn_participant + ci, color = cat_true
    ), width = .25, position = dg)
  }
  
  pl_indiv <- ggplot(tbl_cat_agg, aes(
    trial_id_binned, accuracy_mn_participant, group = as.numeric(participant_id)
  )) + geom_line(aes(color = as.numeric(participant_id))) +
    geom_point(color = "white", size = 3) +
    geom_point(aes(color = as.numeric(participant_id))) +
    facet_grid(n_categories ~ cat_true) +
    theme_bw() +
    scale_color_viridis_c(guide = "none") +
    labs(
      x = "Block of 20 Trials",
      y = "Categorization Accuracy",
      title = "By Participant Categorization Trajectories"
    )
  
  return(list(pl_agg, pl_indiv, tbl_cat_agg))
}


movement_towards_category_center <- function(tbl_cat_sim, tbl_cr, d_measure) {
  #' 
  #' @description calculate and plot average movement of reproduction
  #' responses towards category center
  #' @param tbl_cat_sim the tbl with by-trial category learning and 
  #' similarity judgment responses
  #' @param tbl_cr the tbl with by-trial reproduction responses
  #' 
  #' @return a list containing [[1]] a tbl with the average deviation and
  #' [[2]] a list of plots showing movement towards center against 
  #' (a) task2 (cat/sim) accuracy in last block
  #' (b) task2 improvement aka delta from first block to last block
  #' (c) histograms of (a) and (b)
  #' 
  
  # note. do not use by category accuracy to average over potential response biases
  tbl_cat_sim_last <- tbl_cat_sim %>% 
    mutate(category = cat_true) %>%
    group_by(participant_id) %>%
    filter(trial_id_binned %in% c(
      max(as.numeric(as.character(trial_id_binned))),
      min(as.numeric(as.character(trial_id_binned)))
    )) %>%
    grouped_agg(c(participant_id, n_categories, trial_id_binned), accuracy) %>%
    mutate(
      mean_accuracy_before = lag(mean_accuracy, 1),
      mean_delta_accuracy = mean_accuracy - mean_accuracy_before
    ) %>% filter(
      trial_id_binned == max(as.numeric(as.character(trial_id_binned)))
    )
  # similarity condition gets a dummy accuracy of .5
  tbl_cat_sim_last$mean_accuracy[tbl_cat_sim_last$n_categories == 1] <- .5
  # movement across square categories can be collapsed (not as in ellipse condition)
  # because movement should be towards closest category center for all categories
  tbl_cr_no_sq <- tbl_cr %>% filter(n_categories != 4)
  tbl_cr_sq <- tbl_cr %>% filter(n_categories == 4) 
  tbl_cr_sq$category <- 2
  tbl_cr <- rbind(tbl_cr_no_sq, tbl_cr_sq)
  tbl_movement <- grouped_agg(
    tbl_cr, c(participant_id, n_categories, session, category), d_measure
  ) %>% rename(mean_distance = str_c("mean_", d_measure)) %>%
    select(participant_id, n_categories, session, category, mean_distance) %>%
    left_join(
      tbl_cat_sim_last[, c("participant_id", "mean_accuracy", "mean_delta_accuracy")], 
      by = c("participant_id")
    ) %>% ungroup() %>% arrange(participant_id, category, session) %>%
    group_by(participant_id, category) %>%
    mutate(
      mean_distance_before = lag(mean_distance),
      movement = mean_distance_before - mean_distance,
      category = fct_relabel(
        category, ~ ifelse(.x == 1, "Residual Category", "Closed Category")
      ),
      n_categories = fct_relabel(
        n_categories, ~ ifelse(.x == 1, "Control (Similarity)", str_c("Nr. Categories = ", .x))
      )
    ) %>%
    filter(!is.na(mean_distance_before))
  
  pl_last <- ggplot() + 
    geom_point(data = tbl_movement, aes(mean_accuracy, movement, group = category, color = category), shape = 1) +
    geom_smooth(data = tbl_movement, method = "lm", se = FALSE, aes(mean_accuracy, movement, color = category), size = .5) +
    geom_hline(yintercept = 0, linetype = "dotdash", alpha = .5) +
    facet_grid(n_categories ~ category) +
    scale_color_brewer(palette = "Set1", name = "Category") +
    theme_bw() +
    labs(
      x = "Categorization Accuracy Last Block",
      y = "Movement (Euclidian Distance)"
    )
  
  pl_delta <- ggplot() + 
    geom_point(data = tbl_movement, aes(mean_delta_accuracy, movement, group = category, color = category), shape = 1) +
    geom_smooth(data = tbl_movement, method = "lm", se = FALSE, aes(mean_delta_accuracy, movement, color = category), size = .5) +
    geom_hline(yintercept = 0, linetype = "dotdash", alpha = .5) +
    facet_grid(n_categories ~ category) +
    scale_color_brewer(palette = "Set1", name = "Category") +
    theme_bw() +
    labs(
      x = "Delta Categorization Accuracy",
      y = "Movement (Euclidian Distance)"
    )
  
  tbl_data <- tbl_movement %>% 
    filter(n_categories != "Control (Similarity)") %>%
    group_by(participant_id, n_categories, session) %>%
    summarize(
      mean_distance = mean(mean_distance),
      mean_accuracy = mean(mean_accuracy),
      mean_delta_accuracy = mean(mean_delta_accuracy)
    ) %>%
  pivot_longer(c(mean_accuracy, mean_delta_accuracy)) %>%
    mutate(name = fct_inorder(name), name = fct_relabel(name, ~ c("Mean Accuracy (Last Block)", "Delta Mean Accuracy\nFirst vs. Last Block")))
  tbl_label <- tbl_data %>%
    filter(name == "Delta Mean Accuracy\nFirst vs. Last Block") %>%
    group_by(n_categories) %>% 
    summarize(no_improvement = str_c(sum(value < 0), "/", length(value)))
  
  hist_delta_last <- ggplot() +
    geom_histogram(data = tbl_data, aes(value, group = name, fill = name), alpha = .5, color = "black") +
    geom_label(data = tbl_label, aes(.425, 10, label = str_c(no_improvement, " Participants\nWithout Improvement"))) +
    facet_grid(~ n_categories) +
    theme_bw() +
    coord_cartesian(ylim = c(0, 13)) +
    scale_fill_brewer(name = "Variable", palette = "Set1") +
    labs(x = "Proportion Correct / Proportion Change", 
         y = "Nr. Participants")
  
  l_pl <- list(pl_last = pl_last, pl_delta = pl_delta, hist_delta_last = hist_delta_last)
  
  return(list(tbl_movement, l_pl))
}


plot_distance_to_category_center <- function(tbl_cr, l_info = NULL) {
  tbl_cr_agg <- tbl_cr %>% group_by(n_categories, participant_id, session, category) %>%
    summarize(dmin_mn_participant = mean(d_closest)) %>%
    group_by(n_categories, session, category) %>%
    summarize(dmin_mn = mean(dmin_mn_participant),
              dmin_se = sd(dmin_mn_participant)/sqrt(length(unique(tbl_cr$participant_id)))) %>%
    ungroup() %>%
    mutate(
      session = factor(session, labels = c("Before Cat. Learning", "After Cat. Learning")),
      category = factor(category, labels = c("Non-Target", "Target"))
    )
  
  pl <- ggplot() +
    geom_col(data = tbl_cr_agg, aes(
      category, dmin_mn, group = session, fill = session
    ), position = dg, alpha = .5) +
    geom_point(data = tbl_cr_agg, aes(
      category, dmin_mn, color = session
    ), position = dg, show.legend = FALSE) +
    geom_errorbar(data = tbl_cr_agg, aes(
      category, ymin = dmin_mn - 1.96 * dmin_se, 
      ymax = dmin_mn + 1.96 * dmin_se, color = session
    ), position = dg, width = .25, show.legend = FALSE) +
    facet_wrap(~ n_categories) +
    theme_bw() +
    scale_fill_brewer(name = "Session", palette = "Set1") +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = "Category",
      y = "Distance to Closest Category Center"
    ) +
    theme(plot.title = element_text(size=14, face = "bold"))
  
  if(!is.null(l_info)) {
    pl <- pl  +
      theme(legend.position = c(.7, .7))
    labs(
      title = str_c(l_info$cat_type, ", ", l_info$sampling)
    ) +
      theme(plot.title = element_text(size=14, face = "bold"))
  }
  
  return(pl)
}

by_participant_coefs <- function(tbl_df, iv_str, dv_str, title_str) {
  #' 
  #' @description scatterplot of by-participant intercepts and slopes
  #' @param tbl_df the tbl with aggregated responses by participant and
  #' by iv_str
  #' @param iv_str the independent variable as a string
  #' @param dv_str the dependent variable as a string
  #' @param title_str the title of the plot as a string
  #' 
  
  tbl_df_nested <- tbl_df %>% group_by(participant_id, n_categories) %>% nest()
  lm_participant <- function(tbl_df) {
    lm(str_c(dv_str, " ~ ", iv_str), data = tbl_df)
  }
  tbl_df_nested <- tbl_df_nested %>%
    mutate(
      model = map(data, lm_participant),
      bs = map(model, coefficients),
      bi = unlist(map(bs, 1)),
      btime = unlist(map(bs, 2))
    )
  x_label <- min(tbl_df_nested$bi) + .5*sd(tbl_df_nested$bi)
  y_label <- max(tbl_df_nested$btime) - .5*sd(tbl_df_nested$btime)
  tbl_corr <- tibble(corr = cor(tbl_df_nested$bi, tbl_df_nested$btime))
  ggplot(tbl_df_nested, aes(bi, btime)) +
    geom_hline(yintercept = 0,
               color = "tomato",
               size = 1) +
    geom_point(shape = 1, aes(color = n_categories)) +
    geom_smooth(method = "lm", se = FALSE, aes(color = n_categories)) +
    geom_label(data = tbl_corr, aes(x_label, y_label, label = str_c("r = ", round(corr, 2)))) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    labs(title = title_str, x = "Intercept", y = "Slope per Bin")
}


plot_distance_from_decision_boundary <- function(tbl_cr, nbins) {
  #' 
  #' @description scatter plot of distance from category center and
  #' distance from decision boundary before and after category learning
  #' @param tbl_cr the tbl with by-trial cr responses
  #' @param nbins nr of bins to cut the distances from decision boundary into
  #' 
  #' @return the scatter plot
  #' 
  tbl_cr$d2boundary_stim_cut <- cut(tbl_cr$d2boundary_stim, nbins, labels = FALSE)
  tbl_cr <- tbl_cr %>% mutate(
    session = factor(session, labels = c("Before Cat. Learning", "After Cat. Learning")),
    category = factor(category, labels = c("Residual", "Closed 1", "Closed 2", "Closed 3"))
  )
  dg <- position_dodge(width = .2)
  ggplot(
    grouped_agg(tbl_cr, c(session, n_categories, category, d2boundary_stim_cut), d_closest), 
    aes(d2boundary_stim_cut, mean_d_closest, group = session)) +
    geom_point(aes(color = category, shape = session), position = dg) +
    facet_wrap(~ n_categories) +
    theme_bw() +
    scale_color_brewer(palette = "Set1", name = "") +
    scale_shape_discrete(name = "") +
    labs(
      x = "Distance from Decision Boundary",
      y = "Distance from Category Center"
    )
}
