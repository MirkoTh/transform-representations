dirs_homegrow <- c("R/normDataWithin.R", "R/summarySE.R", "R/summarySEwithin.R")
walk(dirs_homegrow, source)

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
  pl <-
    ggplot(data = tbl_checker, aes(x1_true_binned, x2_true_binned)) +
    geom_tile(aes(fill = avg_deviation_x1x2)) +
    scale_fill_gradient2(
      name = "Avg. Deviation",
      low = "#009966",
      high = "#FF6666"
    ) + geom_label(size = 3,
                   data = tbl_avg, aes(
                     2.5,
                     2,
                     label = str_c(
                       "AvgDev=",
                       round(avg_deviation, 0),
                       ", Tr=",
                       n_trials,
                       " NCat=",
                       n_categories
                     )
                   )) +
    labs(x = "Spikiness of Head (Binned)",
         y = "Fill of Belly (Binned)") + facet_wrap( ~ participant_id) + theme_bw()
  
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
  tbl %>% filter(session %in% c(1, 2)) %>% ungroup() %>%
    pivot_longer(c(x1_deviation, x2_deviation),
                 names_to = "var_deviation",
                 values_to = "val_deviation") %>%
    mutate(
      var_deviation = factor(var_deviation, labels = c("Head Spikiness", "Belly Fill")),
      session = factor(
        session,
        labels = c("Before Cat. Learning", "After Cat. Learning")
      )
    ) %>%
    ggplot(aes(val_deviation, group = session)) +
    geom_histogram(
      aes(fill = session),
      bins = 40,
      alpha = .5,
      color = "black"
    ) +
    geom_freqpoly(aes(color = session), bins = 40) +
    facet_wrap( ~ var_deviation + session) +
    theme_bw() +
    scale_color_brewer(name = "Timepoint", palette = "Set1") +
    scale_fill_brewer(name = "Timepoint", palette = "Set1") +
    labs(x = "Deviation from True Value",
         y = "Nr. Responses")
}


plot_marginals_one_session <-
  function(idx_session, tbl, n_samples = 4) {
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
    if (n_samples %% 2 != 0) {
      stop("The number of samples to plot n_samples has to be even; please adjust accordingly")
    }
    # read individual performance
    idx_color <- ifelse(idx_session == 1, 1, 2)
    title <-
      c("Before Category Learning", "After Category Learning")[idx_color]
    col <- c("#3399FF", "#990099")[idx_color]
    tbl_plot <- tbl %>% filter(session == idx_session)
    
    plot_2d_points_marginal <- function(tbl, participant) {
      tbl_all <- tbl
      tbl <- tbl %>% filter(participant_id == participant)
      grp <- tbl$n_categories[1]
      pl <- ggplot(tbl, aes(x1_deviation, x2_deviation)) +
        geom_point(color = col,
                   shape = 1,
                   size = 2) +
        geom_density2d() +
        theme_bw() +
        theme(plot.title = element_text(size = 10)) +
        scale_color_brewer(palette = "Set1") +
        # somehow ggMarginal does not like coord_cartesian...
        # the following excludes some of the responses, though
        scale_x_continuous(limits = c(min(tbl_all$x1_deviation), max(tbl_all$x1_deviation))) +
        scale_y_continuous(limits = c(min(tbl_all$x2_deviation), max(tbl_all$x2_deviation))) +
        labs(x = "Head Spikiness",
             y = "Belly Size",
             title = str_c(substr(participant, 1, 6), ", ", grp))# + coord_cartesian(xlim = c(-50, 50), ylim = c(-50, 50))
      
      pl_marginals <-
        ggMarginal(pl,
                   fill = col,
                   type = "histogram",
                   bins = 15)
      return(pl_marginals)
    }
    
    participant_sample_groups <-
      tbl_cr %>% group_by(participant_id, n_categories) %>%
      count() %>% group_by(n_categories) %>%
      filter(n == 200) %>%
      mutate(
        id_random = substr(participant_id, sample(1:4, 1), sample(5:8, 1)),
        rwn = row_number(participant_id)
      ) %>%
      filter(rwn <= n_samples / 2)
    
    l_pl <-
      map(participant_sample_groups$participant_id,
          plot_2d_points_marginal,
          tbl = tbl_plot)
    
    n_cols <- 4
    n_rows <- ceiling(n_samples / n_cols)
    pages_plots <-
      marrangeGrob(l_pl,
                   ncol = n_cols,
                   nrow = n_rows,
                   top = title)
    return(pages_plots)
  }


plot_categorization_heatmaps <-
  function(tbl, n_cat, f_agg = "Mode") {
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
      geom_label(data = tbl,
                 aes(5, 1.5, label = str_c(round(mean_accuracy, 2)), group = value),
                 size = 3) +
      facet_wrap( ~ participant_id, ncol = 4) +
      scale_fill_viridis_c(name = "Category\nResponse") +
      theme_bw() +
      labs(x = "Head Spikiness",
           y = "Belly Fill")
  }


histograms_accuracies_rts <- function(tbl_cat_overview) {
  #' histogram of by-participant categorization accuracy and
  #' categorization rts
  #'
  
  hist_acc <- tbl_cat_overview %>% arrange(mean_accuracy) %>%
    mutate(participant_id = fct_inorder(factor(substr(
      participant_id, 1, 6
    ))),
    "Mean Accuracy" = mean_accuracy) %>% ggplot(aes(`Mean Accuracy`, group = participant_id)) +
    geom_histogram(aes(fill = n), color = "white") +
    facet_grid( ~ n_categories) +
    coord_cartesian(xlim = c(0, 1)) +
    scale_x_continuous(breaks = seq(0, 1, by = .05)) +
    scale_fill_viridis_c(guide = "none")  +
    theme_dark() +
    theme(axis.text.x = element_text(
      angle = 45,
      vjust = 0.5,
      hjust = 1
    )) +
    labs(x = "Mean Accuracy Overall",
         y = "Participant Counts")
  hist_rt <- tbl_cat_overview %>% arrange(mean_rt) %>%
    mutate(participant_id = fct_inorder(factor(substr(
      participant_id, 1, 6
    ))),
    "Mean RT" = mean_rt) %>% ggplot(aes(`Mean RT`, group = participant_id)) +
    geom_histogram(aes(fill = n), color = "white") +
    facet_grid( ~ n_categories) +
    scale_fill_viridis_c(name = "Nr. Trials")  +
    theme_dark() +
    labs(x = "Mean RT Overall",
         y = "Participant Counts")
  l_hist <- list(hist_acc, hist_rt)
  
  return(l_hist)
}


plot_categorization_accuracy_against_blocks <-
  function(tbl_cat, show_errorbars = TRUE) {
    #'
    #'
    #' @description overall categorization accuracy against blocks with 95% ci and
    #' individual categorization accuracies against blocks
    #' @param tbl_cat the tbl with by-trial category learning responses
    #' @param show_errorbars shoul 95% cis be displayed
    #'
    #' @return a list with two plots
    #'
    
    tbl_cat_agg <-
      tbl_cat %>% group_by(participant_id, n_categories, cat_true, trial_id_binned) %>%
      summarize(
        accuracy_mn_participant = mean(accuracy),
        n_categories = factor(n_categories)
      ) %>%  ungroup()
    levels(tbl_cat_agg$n_categories) <-
      str_c(levels(tbl_cat_agg$n_categories), " Categories")
    
    tbl_cat_agg_ci <- summarySEwithin(
      tbl_cat_agg,
      "accuracy_mn_participant",
      c("n_categories"),
      c("cat_true", "trial_id_binned"),
      "participant_id"
    ) %>% as_tibble()
    max_categories <- length(unique(tbl_cat_agg_ci$cat_true))
    tbl_cat_agg_ci$trial_id_binned <-
      as.numeric(as.character(tbl_cat_agg_ci$trial_id_binned))
    tbl_cat_agg_ci <- tbl_cat_agg_ci %>%
      mutate(cat_true = factor(cat_true, labels = c("Bukil", "Venak", "Monus", "Ladiv")[1:max_categories]))
    tbl_cat_agg_ci <- tbl_cat_agg_ci %>%
      mutate(cat_true = factor(cat_true, labels = seq(1, 4, by = 1)[1:max_categories]))
    
    tbl_chance <- chance_performance_cat(tbl_cat)
    tbl_chance$block <- as.numeric(as.character(tbl_chance$block))
    
    levels(tbl_cat_agg_ci$cat_true) <- c("Bukil", "Venak", "Monus", "Ladiv")
    pl_agg <- ggplot() +
      geom_line(
        data = tbl_cat_agg_ci,
        aes(
          trial_id_binned,
          accuracy_mn_participant,
          group = cat_true,
          color = cat_true
        ),
        position = dg
      ) +
      geom_point(
        data = tbl_cat_agg_ci,
        aes(trial_id_binned, accuracy_mn_participant, group = cat_true),
        position = dg,
        color = "white",
        size = 2
      ) +
      geom_point(
        data = tbl_cat_agg_ci,
        aes(
          trial_id_binned,
          accuracy_mn_participant,
          group = cat_true,
          color = cat_true
        ), position = dg
      ) +
      # geom_line(
      #   data = tbl_chance, aes(block, prop_chance, group = 1),
      #   linetype = "dotdash", size = .5) +
      #facet_wrap( ~ n_categories, scales = "free_x") +
      #coord_cartesian(ylim = c(.25, 1)) +
      scale_color_viridis_d(name = "Category") +
      scale_x_continuous(breaks = seq(2, 14, by = 2)) +
      labs(x = "Block of 20 Trials",
           y = "Categorization Accuracy",
           #caption = "Note. x-axes differ between panels"
      ) + theme_bw()
    
    if (show_errorbars) {
      pl_agg <- pl_agg + geom_errorbar(
        data = tbl_cat_agg_ci,
        aes(
          trial_id_binned,
          ymin = accuracy_mn_participant - ci,
          ymax = accuracy_mn_participant + ci,
          color = cat_true
        ),
        width = .25,
        position = dg
      )
    }
    
    pl_indiv <- ggplot(tbl_cat_agg,
                       aes(
                         trial_id_binned,
                         accuracy_mn_participant,
                         group = as.numeric(participant_id)
                       )) + geom_line(aes(color = as.numeric(participant_id))) +
      geom_point(color = "white", size = 3) +
      geom_point(aes(color = as.numeric(participant_id))) +
      facet_grid(n_categories ~ cat_true) +
      theme_bw() +
      scale_color_viridis_c(guide = "none") +
      labs(x = "Block of 20 Trials",
           y = "Categorization Accuracy",
           title = "By Participant Categorization Trajectories")
    
    return(list(pl_agg, pl_indiv, tbl_cat_agg))
  }


movement_towards_category_center <-
  function(tbl_cat_sim, tbl_cr, d_measure, sim_center) {
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
    
    tbl_cat_sim_last <- tbl_cat_sim %>%
      mutate(category = cat_true) %>%
      group_by(participant_id) %>%
      filter(trial_id_binned %in% c(max(as.numeric(
        as.character(trial_id_binned)
      )),
      min(as.numeric(
        as.character(trial_id_binned)
      )))) %>%
      grouped_agg(c(participant_id, n_categories, trial_id_binned), accuracy) %>%
      mutate(
        mean_accuracy_before = lag(mean_accuracy, 1),
        mean_delta_accuracy = mean_accuracy - mean_accuracy_before
      ) %>% filter(trial_id_binned == max(as.numeric(as.character(trial_id_binned))))
    # similarity condition gets a dummy accuracy of .5
    tbl_cat_sim_last$mean_accuracy[tbl_cat_sim_last$n_categories == 1] <-
      .5
    # movement across square categories can be collapsed (not as in ellipse condition)
    # because movement should be towards closest category center for all categories
    if (sim_center == "ellipses") {
      tbl_cr_no_sq <- tbl_cr %>% filter(n_categories != 4)
      tbl_cr_sq <- tbl_cr %>% filter(n_categories == 4)
      
    } else if (sim_center == "square") {
      tbl_cr_no_sq <- tbl_cr %>% filter(n_categories == 2)
      tbl_cr_sq <- tbl_cr %>% filter(n_categories %in% c(1, 4))
    }
    tbl_cr_sq$category <- 2
    tbl_cr_plot <- rbind(tbl_cr_no_sq, tbl_cr_sq)
    
    tbl_lookup_category <- tbl_cr_plot %>% filter(n_categories != 1) %>% ungroup() %>% count(stim_id, category) %>% select(-n)
    tbl_cr_plot <- tbl_cr_plot %>% select(-category) %>% left_join(tbl_lookup_category, by = "stim_id")
    
    tbl_movement <- grouped_agg(tbl_cr_plot,
                                c(participant_id, n_categories, session, category),
                                all_of(d_measure)) %>% rename(mean_distance = str_c("mean_", d_measure)) %>%
      select(participant_id,
             n_categories,
             session,
             category,
             mean_distance) %>%
      left_join(tbl_cat_sim_last[, c("participant_id", "mean_accuracy", "mean_delta_accuracy")],
                by = c("participant_id")) %>% ungroup() %>% arrange(participant_id, category, session) %>%
      group_by(participant_id, category) %>%
      mutate(
        mean_distance_before = lag(mean_distance),
        movement = mean_distance_before - mean_distance,
        category = fct_inseq(factor(category)),
        # category = fct_relabel(
        #   category, ~ ifelse(.x == 1, "Residual Category", "Closed Category")
        # ),
        n_categories = fct_inseq(factor(n_categories)),
        n_categories = fct_relabel(n_categories, ~ ifelse(
          .x == 1, "Control (Similarity)", str_c("Nr. Categories = ", .x)
        ))
      ) %>%
      filter(!is.na(mean_distance_before))
    if (sim_center == "ellipses") {
      tbl_movement$category <- factor(tbl_movement$category, labels = c("Bukil", "Venak"))
      if (length(unique(tbl_movement$n_categories)) == 1){
        levels(tbl_movement$n_categories <- "Category Learning")
      }
      levels(tbl_movement$n_categories) <- c("Sequential Comparison", "Category Learning")
      
    } else if (sim_center == "square") {
      tbl_movement$category <- factor(tbl_movement$category, labels = c("Any Category"))
      levels(tbl_movement$n_categories) <- c("Category Learning", "Sequential Comparison")
      
    }
    
    pl_last <- ggplot() +
      geom_point(
        data = tbl_movement %>% 
          filter(n_categories %in% c("Category Learning", "Nr. Categories = 4")),
        aes(mean_accuracy, movement, group = category, color = category)
      ) +
      geom_smooth(
        data = tbl_movement %>% 
          filter(n_categories == "Category Learning"),
        method = "lm",
        se = TRUE, level = .95,
        aes(mean_accuracy, movement, color = category),
        size = .5, alpha = .25
      ) +
      geom_hline(yintercept = 0,
                 linetype = "dotdash",
                 alpha = .5) +
      facet_wrap( ~ category) +
      scale_color_manual(values = c("skyblue2", "tomato3"), name = "Category") +
      theme_bw() +
      labs(x = "Categorization Accuracy Last Block",
           y = "Movement Towards Center") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = expansion(add = c(.0005, .0005))) +
      theme(
        strip.background = element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        text = element_text(size = 16)) +
      guides(color = guide_legend(nrow=2,byrow=TRUE))
    
    pl_delta <- ggplot() +
      geom_point(
        data = tbl_movement %>% 
          filter(n_categories %in% c("Category Learning", "Nr. Categories = 4")),
        aes(
          mean_delta_accuracy,
          movement,
          group = category,
          color = category
        )
      ) +
      geom_smooth(
        data = tbl_movement %>% 
          filter(n_categories %in% c("Category Learning", "Nr. Categories = 4")),
        method = "lm",
        se = TRUE, level = .95, 
        aes(mean_delta_accuracy, movement, color = category),
        size = .5, alpha = .25
      ) +
      geom_hline(yintercept = 0, linetype = "dotdash", alpha = .5) +
      facet_wrap( ~ category) +
      scale_color_manual(values = c("skyblue2", "tomato3"), name = "Category") +
      theme_bw() +
      labs(x = "Delta Categorization Accuracy",
           y = "Movement Towards Center") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = expansion(add = c(.0005, .0005))) +
      theme(
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        text = element_text(size = 16)) +
      guides(color = guide_legend(nrow=2,byrow=TRUE))
    
    
    hist_movements <- ggplot(tbl_movement, aes(movement, group = fct_rev(n_categories), drop = TRUE)) +
      geom_histogram(aes(
        fill = fct_rev(n_categories), y=(..count..)/tapply(..count..,..PANEL..,sum)[..PANEL..]
      ), color = "white", binwidth = sd(tbl_movement$movement)/2) +
      geom_vline(xintercept = 0, linetype = "dotdash", color = "grey", size = 1) +
      scale_fill_viridis_d(name = "Group") +
      facet_grid(fct_rev(n_categories) ~ category) +
      theme_bw() +
      labs(x = "Movement Towards Center", y = "Prop. Participants per Group") +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = expansion(add = c(.0005, .0005))) +
      theme(
        strip.background =element_rect(fill="white"),
        strip.text = element_text(colour = 'black'),
        legend.position = "bottom",
        text = element_text(size = 16)) +
      guides(fill = guide_legend(nrow=2,byrow=TRUE))
    
    
    tbl_data_improve <- tbl_movement %>%
      filter(n_categories != "Control (Similarity)") %>%
      group_by(participant_id, n_categories, session) %>%
      summarize(
        mean_distance = mean(mean_distance),
        mean_accuracy = mean(mean_accuracy),
        mean_delta_accuracy = mean(mean_delta_accuracy)
      ) %>%
      pivot_longer(c(mean_accuracy, mean_delta_accuracy)) %>%
      mutate(name = fct_inorder(name), name = fct_relabel(
        name,
        ~ c(
          "Mean Accuracy (Last Block)",
          "Delta Mean Accuracy\nFirst vs. Last Block"
        )
      ))
    tbl_label <- tbl_data_improve %>%
      filter(name == "Delta Mean Accuracy\nFirst vs. Last Block") %>%
      group_by(n_categories) %>%
      summarize(no_improvement = str_c(sum(value < 0), "/", length(value)))
    
    hist_delta_last <- ggplot() +
      geom_histogram(
        data = tbl_data_improve,
        aes(value, group = name, fill = name),
        alpha = .5,
        color = "black"
      ) +
      geom_label(data = tbl_label, aes(
        .425,
        10,
        label = str_c(no_improvement, " Participants\nWithout Improvement")
      )) +
      facet_wrap( ~ n_categories) +
      theme_bw() +
      #coord_cartesian(ylim = c(0, 13)) +
      scale_fill_brewer(name = "Variable", palette = "Set1") +
      labs(x = "Proportion Correct / Proportion Change",
           y = "Nr. Participants")    +  theme(
             strip.background =element_rect(fill="white"),
             strip.text = element_text(colour = 'black'),
             legend.position = "bottom")
    
    l_pl <- list(
      pl_last = pl_last,
      pl_delta = pl_delta,
      hist_delta_last = hist_delta_last,
      hist_movements = hist_movements
    )
    
    return(list(tbl_movement, l_pl))
  }


plot_distance_to_category_center <-
  function(
    tbl_cr, sim_center, l_info = NULL, 
    center_or_boundary = "center", yttl = "Dist. to Closest Center"
  ) {
    if (sim_center == "ellipses") {
      tbl_cr_sq <-
        tbl_cr %>% filter(n_categories %in% c("4 Categories", 4))
      tbl_ell <-
        tbl_cr %>% 
        filter(n_categories %in% c("Similarity", "2 Categories", 2, 1))
    } else if (sim_center == "square") {
      tbl_cr_sq <-
        tbl_cr %>% filter(n_categories %in% c("Similarity", "4 Categories", 4))
      tbl_ell <-
        tbl_cr %>% filter(n_categories %in% c("2 Categories", 2))
    }
    
    tbl_lookup <- tbl_ell %>% filter(n_categories %in% c("2 Categories", 2)) %>%
      group_by(stim_id, category) %>% count() %>% select(-n)
    tbl_ell <- tbl_ell %>% filter(n_categories == "Similarity") %>%
      select(-category) %>% left_join(tbl_lookup, by = "stim_id") %>%
      rbind(tbl_ell %>% filter(n_categories %in% c("2 Categories", 2)))
    
    tbl_cr_sq$category <- 2
    tbl_ell[tbl_ell$n_categories == "Similarity", ]
    tbl_cr <- rbind(tbl_ell, tbl_cr_sq)
    if (center_or_boundary == "center") {
      tbl_cr$d_closest_sqrt <- sqrt(tbl_cr$d_closest)
    } else if (center_or_boundary == "boundary") {
      tbl_cr$d_closest_sqrt <- sqrt(tbl_cr$d_boundary)
    }
    
    tbl_cr_agg <- summarySEwithin(
      tbl_cr, "d_closest_sqrt", "n_categories", 
      c("session", "category"), idvar = "participant_id"
    ) %>%
      mutate(session = factor(
        session,
        labels = c("Before", "After")
      ))
    dg <- position_dodge(width = .9)
    pl <- ggplot() +
      geom_col(
        data = tbl_cr_agg,
        aes(session, d_closest_sqrt, group = n_categories, fill = n_categories),
        position = dg,
        alpha = .5
      ) +
      geom_point(
        data = tbl_cr_agg,
        aes(session, d_closest_sqrt, color = n_categories),
        position = dg,
        show.legend = FALSE
      ) +
      geom_errorbar(
        data = tbl_cr_agg,
        aes(
          session,
          ymin = d_closest_sqrt - ci,
          ymax = d_closest_sqrt + ci,
          color = n_categories
        ),
        position = dg,
        width = .25,
        show.legend = FALSE
      ) +
      theme_bw() +
      scale_fill_viridis_d(name = "Group") +
      scale_color_viridis_d() +
      # scale_fill_brewer(name = "Group", palette = "Set1") +
      # scale_color_brewer(palette = "Set1") +
      scale_x_discrete(expand = c(0.01, 0)) +
      scale_y_continuous(expand = c(0.01, 0)) +
      labs(x = "Time Point", y = yttl) + 
      theme(
        strip.background = element_rect(fill = "white"), 
        text = element_text(size = 22)
      )
    
    if (!is.null(l_info)) {
      pl <- pl  +
        theme(legend.position = c(.7, .7))
      labs(title = str_c(l_info$cat_type, ", ", l_info$sampling)) +
        theme(plot.title = element_text(size = 14, face = "bold"))
    }
    
    return(list(pl = pl, tbl_cr_agg = tbl_cr_agg))
  }

by_participant_coefs <-
  function(tbl_df, iv_str, dv_str, title_str) {
    #'
    #' @description scatterplot of by-participant intercepts and slopes
    #' @param tbl_df the tbl with aggregated responses by participant and
    #' by iv_str
    #' @param iv_str the independent variable as a string
    #' @param dv_str the dependent variable as a string
    #' @param title_str the title of the plot as a string
    #'
    
    tbl_df_nested <-
      tbl_df %>% group_by(participant_id, n_categories) %>% nest()
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
    x_label <- min(tbl_df_nested$bi) + .5 * sd(tbl_df_nested$bi)
    y_label <- max(tbl_df_nested$btime) - .5 * sd(tbl_df_nested$btime)
    tbl_corr <-
      tibble(corr = cor(tbl_df_nested$bi, tbl_df_nested$btime))
    ggplot(tbl_df_nested, aes(bi, btime)) +
      geom_hline(yintercept = 0,
                 color = "tomato",
                 size = 1) +
      geom_point(shape = 1, aes(color = n_categories)) +
      geom_smooth(method = "lm", se = TRUE, aes(color = n_categories)) +
      geom_label(data = tbl_corr, aes(x_label, y_label, label = str_c("r = ", round(corr, 2)))) +
      scale_color_brewer(name = "", palette = "Set1") +
      theme_bw() +
      labs(title = title_str, x = "Intercept", y = "Slope per Bin")
  }


plot_distance_from_decision_boundary <- function(tbl_cr_d, nbins, sim_center) {
  #'
  #' @description scatter plot of distance from category center and
  #' distance from decision boundary before and after category learning
  #' @param tbl_cr_d the tbl with by-trial cr responses
  #' @param nbins nr of bins to cut the distances from decision boundary into
  #' @param sim_center ellipse or square category structure
  #'
  #' @return the scatter plot
  #'
  tbl_cr_d$d2boundary_stim_cut <-
    cut(tbl_cr_d$d2boundary_stim, nbins, labels = FALSE)
  if (sim_center == "square") {
    tbl_cr_d$d2boundary_stim_cut <- tbl_cr_d$d2boundary_stim
    tbl_cr_d$category[tbl_cr_d$n_categories != 2] <- 2
  } else if (sim_center == "ellipses") {
    tbl_cr_d <- tbl_cr_d %>% 
      filter(
        n_categories == "Similarity" | 
          n_categories == "2 Categories" & category == 2
      )
  }
  tbl_cr_d <- tbl_cr_d %>% 
    mutate(session = factor(
      session, labels = c("Before Cat. Learning", "After Cat. Learning")),
      category = factor(category)
    )
  # all items in square groups and in similarity groups are treated in the same way
  tbl_cr_d <- grouped_agg(
    tbl_cr_d, 
    c(session, n_categories, category, d2boundary_stim_cut), d_closest
  ) %>%
    group_by(n_categories, category, d2boundary_stim_cut) %>%
    arrange(session) %>%
    mutate(
      mean_d_closest_before = lag(mean_d_closest),
      mean_d_delta = mean_d_closest_before - mean_d_closest
    ) %>% filter(!is.na(mean_d_delta))
  
  
  dg <- position_dodge(width = .2)
  ggplot(tbl_cr_d,
         aes(d2boundary_stim_cut, mean_d_delta, group = n_categories)) +
    geom_line(aes(color = n_categories), position = dg) +
    geom_point(color = "white",
               size = 4,
               position = dg) +
    geom_point(aes(color = n_categories, size = n), position = dg) +
    theme_bw() +
    scale_color_brewer(palette = "Set1", name = "") +
    scale_size_continuous(guide = "none") +
    scale_shape_discrete(name = "") +
    labs(x = "Distance from Decision Boundary",
         y = "Movement towards Category Center")
}

mean_against_delta_cat_accuracy <- function(tbl_movement) {
  #' plot average categorization accuracy against delta
  #' categorization accuracy
  #'
  ggplot(tbl_movement,
         aes(mean_delta_accuracy, mean_accuracy, group = n_categories)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ n_categories) +
    theme_bw() +
    labs(x = "Delta Accuracy", y = "Mean Accuracy")
}

plot_movement_against_precision <- function(tbl_precision) {
  #' plot movements towards true category centers and movements towards
  #' representational centers against precision of representations
  #'
  tbl_precision %>% pivot_longer(cols = c(movement_gt, movement_representation)) %>% mutate(name = factor(name,
                                                                                                          labels = c(
                                                                                                            "True Center", "Representational Center"
                                                                                                          ))) %>%
    ggplot(aes(v_precision_representation, value, group = name)) +
    geom_point(aes(color = name)) +
    geom_smooth(aes(color = name),
                method = "lm",
                se = FALSE,
                size = .5) +
    scale_color_brewer(palette = "Set1", name = "Movement to") +
    theme_bw() +
    labs(x = "Representational Precision", y = "Movement")
}


plot_heatmaps_with_representations <- function(l_nb, sample_ids, tbl_df) {
  #' plot heat maps of category learning responses for some sample
  #' participants and overlay representations from nb model
  #'
  tbl_preds_nb <- reduce(map(l_nb, 2), rbind) %>%
    mutate(participant_id = fct_inorder(substr(participant_id, 1, 6), ordered = TRUE))
  plot_categorization_heatmaps(tbl_df %>% filter(participant_id %in% sample_ids),
                               4,
                               "Mode") + geom_contour(
                                 data = tbl_preds_nb %>%
                                   filter(participant_id %in% substr(sample_ids, 1, 6)),
                                 aes(x1, x2, group = category, z = density),
                                 color = "black"
                               )
}


plot_similarity_against_distance <-
  function(tbl_sim, tbl_sim_ci, sample_ids_sim = NULL, sim_edges = c(1, 4)) {
    #' plot similarity ratings against euclidean distance
    #' @description individual and aggregated plots of similarity against distance
    #' @param tbl_sim by-trial responses in similarity task
    #' @param tbl_sim_ci aggregated responses in similarity task
    #' @param sample_ids_sim list with subset of participants to plot in the
    #' individual plot
    #' @param sim_edges
    #' @return list with the two plots
    #'
    tbl_sample <- tbl_sim %>%
      group_by(participant_id, n_categories, distance_binned)
    if (!is.null(sample_ids_sim)) {
      tbl_sample <-
        tbl_sample %>% filter(participant_id %in% sample_ids_sim)
    }
    
    # scatter plot and line plot of sample
    pl_sample <- tbl_sample %>%
      summarize(response_mn = mean(response), n = n()) %>%
      ggplot(aes(distance_binned, response_mn, group = as.numeric(participant_id))) +
      geom_line(aes(color = as.numeric(participant_id))) +
      geom_point(aes(color = as.numeric(participant_id), size = n)) +
      theme_bw() +
      scale_color_viridis_c(name = "Participant ID") +
      scale_size_continuous(name = "Nr. Similarity Judgments") +
      labs(x = "Euclidean Distance (Binned)",
           y = "Average Similarity",
           title = "Sample Participants")
    
    # aggregate scatter plot and line plot
    pl_agg <- ggplot() +
      geom_smooth(
        data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
        aes(distance_binned, response),
        color = "#440154",
        method = "lm"
      ) + geom_errorbar(
        data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
        aes(
          distance_binned,
          ymin = response - ci,
          ymax = response + ci,
          width = .2
        )
      ) +  geom_point(size = 3, color = "white") +
      geom_point(data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
                 aes(distance_binned, response)) +
      theme_bw() +
      scale_x_continuous(breaks = seq(2, 10, by = 2)) +
      coord_cartesian(ylim = c(sim_edges[1], sim_edges[2])) +
      labs(x = "Euclidean Distance (Binned)",
           y = "Average Similarity") # ,title = "Mean Effect"
    
    tbl_tmp <- tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13)))
    tbl_tmp$n_categories <- factor(tbl_tmp$n_categories)
    levels(tbl_tmp$n_categories) <- "Similarity"
    pl_agg_lines <- ggplot() +
      geom_line(
        data = tbl_tmp,
        aes(distance_binned, response, color = n_categories)
      ) + geom_errorbar(
        data = tbl_tmp,
        aes(
          distance_binned,
          ymin = response - ci,
          ymax = response + ci,
          width = .2,
          color = n_categories
        )
      ) + geom_point(data = tbl_tmp,
                     aes(distance_binned, response), size = 2, color = "white") +
      geom_point(data = tbl_tmp,
                 aes(distance_binned, response, color = n_categories)) +
      theme_bw() +
      theme(legend.position = "bottom") +
      scale_x_continuous(breaks = seq(2, 10, by = 2)) +
      scale_color_viridis_d(name = "Group") +
      coord_cartesian(ylim = c(sim_edges[1], sim_edges[2])) +
      labs(x = "Euclidean Distance (Binned)",
           y = "Average Similarity")
    
    return(list(pl_sample = pl_sample, pl_agg = pl_agg, pl_agg_lines = pl_agg_lines))
  }



plot_deviations_from_stimulus <- function(tbl_cr_agg) {
  pl_density <- ggplot(tbl_cr_agg %>% mutate(session = factor(
    session,
    levels = c(1, 2),
    labels = c("Before\nCategory Learning", "After\nCategory Learning")
  )),
  aes(mean_eucl_deviation, group = session)) +
    geom_density(aes(color = session)) +
    facet_wrap( ~ n_categories) +
    theme_bw() +
    scale_color_brewer(palette = "Set1", name = "") +
    labs(x = "Mean Euclidean Deviation",
         y = "Probability Density")
  
  pdg <- position_dodge(width = .15)
  pl_agg <-
    summarySEwithin(
      tbl_cr_agg,
      "mean_eucl_deviation",
      betweenvars = "n_categories",
      withinvars = "session"
    ) %>%
    mutate(session = factor(
      session,
      labels = c("Before\nCategory Learning", "After\nCategory Learning")
    )) %>%
    ggplot(aes(session, mean_eucl_deviation, group = n_categories)) +
    geom_point(aes(color = n_categories), position = pdg) +
    geom_line(aes(color = n_categories),
              show.legend = FALSE,
              position = pdg) +
    geom_errorbar(
      aes(
        ymin = mean_eucl_deviation - 1.96 * se,
        ymax = mean_eucl_deviation + 1.96 * se,
        color = n_categories
      ),
      width = .15,
      show.legend = FALSE,
      position = pdg
    ) +
    scale_fill_brewer(name = "", palette = "Set1") +
    scale_color_brewer(name = "Group", palette = "Set1") +
    theme_bw() +
    labs(x = "",
         y = "Mean Euclidean Deviation")
  
  pl_accuracy_against_deviation <- tbl_cr_agg %>%
    left_join(tbl_movement_gt[, c("participant_id", "mean_accuracy", "mean_delta_accuracy")],
              by = "participant_id") %>% select(
                participant_id,
                session,
                n_categories,
                mean_eucl_deviation,
                mean_accuracy,
                mean_delta_accuracy
              ) %>% filter(n_categories == "4 Categories") %>%
    pivot_longer(c(mean_accuracy, mean_delta_accuracy)) %>%
    mutate(name = factor(
      name,
      labels = c(
        "Final Categorization Accuracy",
        "Delta Categorization Accuracy"
      )
    )) %>%
    ggplot(aes(mean_eucl_deviation, value, group = name)) +
    facet_wrap(~ name) +
    geom_point(aes(color = name), show.legend = FALSE) +
    geom_smooth(method = "lm", aes(color = name), show.legend = FALSE) +
    scale_color_brewer(palette = "Set1") +
    theme_bw() +
    labs(x = "Euclidean Deviation Reproduction",
         y = "(Delta) Categorization Accuracy")
  
  return(
    list(
      pl_density = pl_density,
      pl_agg = pl_agg,
      pl_accuracy_against_deviation = pl_accuracy_against_deviation
    )
  )
  
}


plot_distances_to_centers <- function(tbl_cr) {
  tbl_cr %>% mutate(
    d_closest_mc = scale(d_closest, scale = FALSE),
    d_closest_sqrt_mc = scale(d_closest_sqrt, scale = FALSE)
  ) %>%
    pivot_longer(c(d_closest, d_closest_sqrt, d_closest_mc, d_closest_sqrt_mc)) %>%
    filter(name == c("d_closest", "d_closest_sqrt")) %>%
    mutate(name = factor(name, labels = c("Not Transformed", "Square Root"))) %>%
    ggplot(aes(value)) +
    geom_histogram(bins = 50,
                   fill = "#66CCFF",
                   color = "white") +
    facet_wrap( ~ name, scales = "free") +
    theme_bw() +
    labs(x = "Value", y = "Nr. Responses")
}


plot_groupmeans_against_session <- function(tbl_cr, sim_center = "square", yttl = "Distance to Closest Center") {
  pd <- position_dodge(width = .9)
  if(sim_center == "square") {
    vars <- c("n_categories")
  } else if (sim_center == "ellipses") {
    vars <- c("n_categories", "category")
  }
  pl_default <- summarySEwithin(tbl_cr, "d_closest_sqrt", vars, "session") %>%
    mutate(session = factor(
      session,
      labels = c("Before Category Learning", "After Category Learning")
    )) %>%
    ggplot(aes(session, d_closest_sqrt, group = n_categories)) +
    geom_col(aes(fill = n_categories), position = pd, show.legend = FALSE) +
    geom_errorbar(aes(
      ymin = d_closest_sqrt - ci,
      ymax = d_closest_sqrt + ci,
    ), color = "black", width = .2, position = pd
    ) + geom_point(size = 3, color = "white", position = pd) +
    geom_point(aes(color = n_categories), position = pd) +
    scale_color_viridis_d(name = "Group") +
    scale_fill_viridis_d() +
    theme_bw() +
    labs(x = "",
         y = yttl) +
    scale_x_discrete(expand = c(0.01, 0)) +
    scale_y_continuous(expand = c(0.01, 0)) +
    theme(
      strip.background = element_rect(fill = "white"), text = element_text(size = 22)
    )
  if(sim_center == "ellipses"){
    pl_default <- pl_default + facet_wrap(~ category)
  }
  return(pl_default)
}



plot_mean_deltas <- function(tbl_cr) {
  tbl_cr %>% group_by(participant_id, session, n_categories) %>%
    summarize(
      d_closest_mn_sqrt = mean(d_closest_sqrt),
      d_closest_mn_abs = mean(d_closest)
    ) %>%
    group_by(participant_id) %>%
    mutate(
      d_closest_before_sqrt = lag(d_closest_mn_sqrt),
      d_move_sqrt = d_closest_before_sqrt - d_closest_mn_sqrt,
      d_closest_before_abs = lag(d_closest_mn_abs),
      d_move_abs = d_closest_before_abs - d_closest_mn_abs
    ) %>%
    ungroup() %>%
    mutate(n_categories = factor(n_categories, labels = c("Similarity Judgment", "4 Categories"))) %>%
    dplyr::filter(!is.na(d_closest_before_abs)) %>%
    pivot_longer(c(d_move_sqrt, d_move_abs)) %>%
    mutate(name = factor(name, labels = c("Not Transformed", "Square Root"))) %>%
    ggplot(aes(value)) +
    geom_histogram(bins = 60,
                   fill = "#66CCFF",
                   color = "white") + # "dodgerblue"
    geom_vline(
      xintercept = 0,
      color = "darkred",
      size = 1,
      linetype = "dashed"
    ) +
    facet_wrap(name ~ n_categories, scales = "free_x") +
    theme_bw() +
    labs(x = "Movement Towards Center", y = "Nr. Participants")
}

plot_mean_deltas_ellipse <- function(tbl_cr, nbins = 60) {
  tbl_cr %>% group_by(participant_id, session, n_categories, category) %>%
    summarize(
      d_closest_mn_sqrt = mean(d_closest_sqrt),
      d_closest_mn_abs = mean(d_closest)
    ) %>%
    group_by(participant_id, category) %>%
    arrange(participant_id, category, session) %>%
    mutate(
      d_closest_before_sqrt = lag(d_closest_mn_sqrt),
      d_move_sqrt = d_closest_before_sqrt - d_closest_mn_sqrt,
      d_closest_before_abs = lag(d_closest_mn_abs),
      d_move_abs = d_closest_before_abs - d_closest_mn_abs
    ) %>%
    ungroup() %>%
    mutate(
      n_categories = factor(n_categories, labels = c("Similarity Judgment", "2 Categories")),
      category = factor(category, labels = c("Bukil", "Venak"))
    ) %>%
    dplyr::filter(!is.na(d_closest_before_abs)) %>%
    ggplot(aes(d_move_abs)) +
    geom_histogram(bins = nbins,
                   fill = "#440154",
                   color = "white") + # "dodgerblue"
    geom_vline(
      xintercept = 0,
      color = "grey",
      size = 1,
      linetype = "dashed"
    ) +
    facet_grid(n_categories ~ category, scales = "free_x") +
    theme_bw() +
    labs(x = "Movement Towards Center", y = "Nr. Participants") +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0))
}



plot_movement_outliers <-
  function(tbl_outliers,
           tbl_labels,
           ttl,
           nrcols = 6,
           as_outlier = TRUE) {
    if (!as_outlier) {
      tbl_labels$participant_id <- factor(tbl_labels$participant_id)
      levels(tbl_labels$participant_id) <- str_c(seq(1, length(unique(tbl_labels$participant_id))), str_match(levels(tbl_labels$participant_id), ", [a-z A-Z 0-9]*")[, 1])
      levels(tbl_outliers$participant_id) <- str_c(seq(1, length(unique(tbl_outliers$participant_id))), str_match(levels(tbl_outliers$participant_id), ", [a-z A-Z 0-9]*")[, 1])
      
      pl <- ggplot(
        tbl_outliers %>% 
          filter(name == "Not Transformed"),
        aes(value)) +
        geom_histogram(bins = 30,
                       color = "white", fill = "#440154") #,fill = "#66CCFF"
    } else if (as_outlier) {
      pl <- ggplot(
        tbl_outliers %>% filter(name == "Not Transformed"),
        aes(value, group = flag_outlier)
      ) +
        geom_histogram(bins = 30,
                       color = "white",
                       aes(fill = flag_outlier))
    }
    if (is.null(tbl_labels$mean)) tbl_labels$mean <- 99.999
    pl +
      geom_vline(
        xintercept = 0,
        color = "darkred",
        size = 1,
        linetype = "dashed"
      ) +
      geom_label(data = tbl_labels, label.padding = unit(0.1, "lines"), aes(
        x = 2,
        y = 18,
        label = str_c("Avg. Move = ", round(avg_move, 1), "\nMAP Gamma = ", round(mean, 2))
      ), size = 5) +
      facet_wrap( ~ participant_id, ncol = nrcols) +
      theme_bw() +
      scale_fill_brewer(palette = "Set1", name = "Outlier") +
      labs(x = "Movement Towards Center", y = "Nr. Stimuli", title = ttl)
  }



plot_group_rts_against_session <- function(tbl_cr, nr_cats = 4) {
  pd <- position_dodge(width = .2)
  grouped_agg(tbl_cr, c(n_categories, session), rt) %>% ungroup() %>%
    mutate(
      n_categories = factor(n_categories, labels = c("Similarity", str_c(nr_cats, " Categories"))),
      mean_rt = mean_rt / 1000,
      se_rt = se_rt / 1000
    ) %>%
    ggplot(aes(session, mean_rt, group = n_categories)) +
    geom_errorbar(
      aes(
        ymin = mean_rt - 2 * se_rt,
        ymax = mean_rt + 2 * se_rt,
        color = n_categories
      ),
      width = .2,
      position = pd
    ) +
    geom_line(aes(color = n_categories), position = pd) +
    geom_point(size = 3,
               color = "white",
               position = pd) +
    geom_point(aes(color = n_categories), position = pd) +
    scale_color_brewer(palette = "Set1", name = "Group") +
    theme_bw() +
    labs(x = "Timepoint", y = "RT (s)")
}


plot_predictions_with_data_mixture <- function(
    tbl_empirical, tbl_post_preds, facet_by = "participant"
) {
  if (facet_by == "participant") {  
    pl <- ggplot() + facet_wrap( ~ participant_id) +
      scale_fill_brewer(palette = "Set1", name = "Outlier") +
      geom_histogram(
        data = tbl_empirical,
        color = "white",
        binwidth = 5,
        aes(d_move_abs, y = ..density.., fill = outlier)
      )} else if (facet_by == "group") {
        pl <- ggplot() + facet_wrap( ~ n_categories) +
          scale_fill_brewer(palette = "Set1", name = "Group") +
          geom_histogram(
            data = tbl_empirical,
            color = "white",
            binwidth = 5,
            aes(d_move_abs, y = ..density.., fill = n_categories)
          ) +
          scale_fill_brewer(palette = "Set1", name = "Group")
      }
  
  pl +  
    geom_vline(
      xintercept = 0, color = "darkred", size = 1, linetype = "dashed") + 
    theme_bw() +
    coord_cartesian(xlim = c(-50, 75), ylim = c(0, .045)) +
    labs(x = "Movement to Center", y = "Probability Density") + 
    geom_freqpoly(
      data = tbl_post_preds, color = "#a9a9a9", size = 1.5, binwidth = 5,
      aes(value, y = ..density..),
    )
}


save_my_tiff <- function(pl, path_fl, w, h) {
  tiff(path_fl, w, h, "in", res = 300)
  grid.draw(pl)
  dev.off()
}


save_my_pdf <- function(pl, path_fl, w, h) {
  pdf(path_fl, w, h, paper = "special")
  grid.draw(pl)
  dev.off()
}

save_my_pdf_and_tiff <- function(pl, path_fl, w, h) {
  save_my_pdf(pl, str_c(path_fl, ".pdf"), w, h)
  save_my_tiff(pl, str_c(path_fl, ".tiff"), w, h)
}


plot_distance_psychonomics <- function(tbl_cr_agg) {
  dg <- position_dodge(width = .9)
  ggplot(
    tbl_cr_agg %>% filter(category == 2), 
    aes(session, d_closest_sqrt, group = n_categories)
  ) + 
    geom_col(aes(fill = n_categories), position = dg, alpha = .5) +
    geom_point(
      aes(color = n_categories), position = dg
    ) +
    geom_errorbar(
      aes(
        ymin = d_closest_sqrt - ci,
        ymax = d_closest_sqrt + ci,
        color = n_categories
      ),
      position = dg,
      width = .25,
    ) +
    theme_bw() +
    theme(legend.position = "bottom") +
    scale_fill_viridis_d(name = "Group") +
    scale_color_viridis_d(guide = "none") +
    labs(x = "Time Point",
         y = "Distance to Closest Category Center") +
    theme(plot.title = element_text(size = 14, face = "bold"))
}


ribbon_plot <- function(tbl_simult_move) {
  
  tmp <- tbl_simult_move %>% 
    group_by(d_euclidean_cut, comparison_pool_binary, n_categories) %>% 
    summarize(
      response = mean(move_response),
      n_responses = length(move_response)) 
  
  tbl_ribbon <- tmp %>%
    pivot_wider(
      id_cols = c(d_euclidean_cut, n_categories), 
      names_from = comparison_pool_binary, values_from = response, names_prefix = "move_"
    ) %>% left_join(
      tmp %>% 
        pivot_wider(
          id_cols = c(d_euclidean_cut, n_categories), 
          names_from = comparison_pool_binary, values_from = n_responses, names_prefix = "n_"
        ), by = c("d_euclidean_cut", "n_categories")
    )
  
  colors <- c(
    "Same" = "#fde725", "Different" = "#440154"
  )
  
  pl_ribbon <- ggplot(tbl_ribbon %>% rename(Same = move_Same, Different = move_Different), aes(group = n_categories)) +
    geom_ribbon(aes(d_euclidean_cut, ymin = Different, ymax = Same), fill = "grey", alpha = .25, outline.type = "lower") +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "grey70") +
    geom_line(aes(d_euclidean_cut, Same, color = "Same"), ) +
    geom_line(aes(d_euclidean_cut, Different, color = "Different")) +
    geom_point(aes(d_euclidean_cut, Same), color = "white", size = 5) +
    geom_point(aes(d_euclidean_cut, Same, size = n_Same, color = "Same")) +
    geom_point(aes(d_euclidean_cut, Different), color = "white", size = 5) +
    geom_point(aes(d_euclidean_cut, Different, size = n_Different, color = "Different")) +
    facet_wrap(~ n_categories) +
    theme_bw() +
    scale_size_continuous(name = "Nr. Responses", range = c(2, 4)) +
    scale_color_manual(name = "Category", values = colors) +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Euclidean Distance", y = "Move After - Before")
  
  pl_no_ribbon <- ggplot(
    tbl_simult_move %>% 
      group_by(d_euclidean_cut, comparison_pool_binary, n_categories) %>% 
      summarize(
        response = mean(move_response),
        n_responses = length(move_response)),
    aes(d_euclidean_cut, response, group = comparison_pool_binary)) +
    geom_hline(yintercept = 0, linetype = "dotdash", color = "grey70") +
    geom_line(aes(color = comparison_pool_binary)) +
    geom_point(color = "white", size = 5) +
    geom_point(aes(color = comparison_pool_binary, size = n_responses)) +
    facet_wrap(~ n_categories) +
    scale_color_viridis_d(name = "Category") +
    scale_size_continuous(range = c(1, 4), guide = "none") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = "Euclidean Distance", y = "Move After - Before")
  
  l_plots <- list(pl_ribbon, pl_no_ribbon)
  
  return(l_plots)
}


plot_2d_distributions <- function(tbl_combined, save) {
  d1 <- tbl_combined %>% filter(session == "Before")
  d2 <- tbl_combined %>% filter(session == "After")
  
  pl1 <- ggplot(data = d1, aes(x1_deviation, x2_deviation)) +
    geom_point(shape = 1, size = 0, color = "white", fill = "white") +
    geom_bin2d(bins = 40) + scale_fill_viridis_c(name = "Nr. Responses", limits = c(0, 160)) +
    # somehow ggMarginal does not like coord_cartesian...
    # the following excludes some of the responses, though
    scale_x_continuous(limits = c(-40, 40), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-40, 40), expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      x = "Head Spikiness",
      y = "Belly Size",
      caption = "Session 1"
    )
  
  pl2 <- ggplot(data = d2, aes(x1_deviation, x2_deviation)) +
    geom_point(shape = 1, size = 0, color = "white", fill = "white") +
    #geom_density2d(data = d1, color = "#440154", aes(x1_deviation, x2_deviation)) +
    geom_bin2d(bins = 40) + scale_fill_viridis_c(name = "Nr. Responses", limits = c(0, 160)) +
    scale_x_continuous(limits = c(-40, 40), expand = c(0, 0)) +
    scale_y_continuous(limits = c(-40, 40), expand = c(0, 0)) +
    theme_bw() +
    theme(legend.position = "bottom") +
    labs(
      x = "Head Spikiness",
      y = "Belly Size",
      caption = "Session 2"
    )
  pl_precision <- arrangeGrob(
    ggMarginal(pl1, type = "density", fill = "#440154", size = 3), 
    ggMarginal(pl2, type = "density", fill = "#fde725", size = 3),
    nrow = 1)
  
  
  tbl_sds_agg <- tbl_combined %>% 
    group_by(n_categories, session) %>% 
    summarize(m_x1 = mean(x1_deviation), m_x2 = mean(x2_deviation),
              sd_x1 = sd(x1_deviation), sd_x2 = sd(x2_deviation), 
              cov_x1x2 = cor(x1_deviation, x2_deviation),
    ) %>%
    ungroup()
  
  knitr::kable(tbl_sds_agg)
  
  pl_marginal_x1 <- ggplot(tbl_combined, aes(x1_deviation, group = session)) +
    geom_density(aes(color = session), size = 1) +
    geom_label(data = tbl_sds_agg, aes(20, .03 + (3 - as.numeric(session)) * .0055, label = str_c("sd = ", round(sd_x1, 1)), group = session, color = session), size = 5) +
    facet_wrap(~ n_categories) +
    coord_cartesian(xlim = c(-38, 38), ylim = c(0, .047)) + 
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "", y = "Probability Density", title = "Head Spikiness") + 
    theme(strip.background = element_rect(fill = "white"), text = element_text(size = 16)) + 
    scale_color_manual(values = c("skyblue2", "tomato4"), name = "Session")
  
  pl_marginal_x2 <- ggplot(tbl_combined, aes(x2_deviation, group = session)) +
    geom_density(aes(color = session), size = 1) +
    geom_label(data = tbl_sds_agg, aes(20, .03 + (3 - as.numeric(session)) * .0055, label = str_c("sd = ", round(sd_x2, 1)), group = session, color = session), size = 5) +
    facet_wrap(~ n_categories) +
    coord_cartesian(xlim = c(-38, 38), ylim = c(0, .047)) +
    theme_bw() +
    scale_x_continuous(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "", y = "Probability Density", title = "Belly Size") + 
    theme(strip.background = element_rect(fill = "white"), text = element_text(size = 16)) + 
    scale_color_manual(values = c("skyblue2", "tomato4"), name = "Session")
  
  pl_marginals <- arrangeGrob(pl_marginal_x1, pl_marginal_x2, nrow = 2)
  
  if (save) {
    save_my_pdf_and_tiff(
      pl_precision,
      "figures/precision-e1-e2-combined",
      9, 5
    )
    save_my_pdf_and_tiff(
      pl_marginals,
      "figures/marginal-densities",
      6, 7.5
    )
    
  }
  
  
  return(list(pl_precision, pl_marginals))
}
