library(tidyverse)
library(gridExtra)
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)


categorize_stimuli <- function(l_params) {
  #' main categorization function
  #' 
  #' @description categorize stimuli, store accepted samples, and visualize results
  #' 
  #' @param l_params parameter list with fields n_stimuli, n_categories, prior_sd, nruns
  #' @return a list with prior, samples, and posterior in [[1]] and some
  #' visualizations in [[2]]
  #' 
  # unpack parameters
  env <- rlang::current_env()
  list2env(l_params, env)

  thx <- c(0, sqrt(n_stimuli) - 1)
  x1 <- seq(thx[1], thx[2], by = 1)
  x2 <- seq(thx[1], thx[2], by = 1)
  features <- crossing(x1, x2)
  tbl <- tibble(stim_id = seq(1, nrow(features)), features)
  
  tbl <- create_categories(tbl, sqrt(n_categories)) %>% select(-c(x1_cat, x2_cat))

  feature_names <- c("x1", "x2")
  label <- "category"
  tbl$category <- as.factor(tbl$category)
  categories <- levels(tbl$category)
  fml <- formula(str_c(label, " ~ ", str_c(feature_names, collapse = " + ")))
  m_nb_initial <- naive_bayes(fml, data = tbl)
  m_nb_update <- m_nb_initial
  
  tbl_new <- tbl
  posterior_prior <- predict(m_nb_initial, tbl[, c("x1", "x2")], "prob")
  l_prior_prep <- extract_posterior(posterior_prior, m_nb_initial, tbl_new)
  tbl_prior_long <- l_prior_prep[[1]]
  l_prior <- l_prior_prep[[2]]
  posterior <- posterior_prior
  
  
  # Categorization Simulation -----------------------------------------------
  
  pb <- txtProgressBar(min = 1, max = nruns, initial = 1, char = "*", style = 2)
  for (i in 1:nruns) {
    # randomly move random observation 
    idx <- sample(n_stimuli, 1)
    cat_cur <- tbl_new$category[idx]
    stim_id_cur <- tbl_new$stim_id[idx]
    X_new <-  tibble(
      tbl_new[idx, "x1"] + rnorm(1, 0, prior_sd), 
      tbl_new[idx, "x2"] + rnorm(1, 0, prior_sd)
    )
    X_old <- tbl_new[, c("x1", "x2")]
    X <- rbind(X_old, X_new)
    # create new X matrix
    colnames(X) <- feature_names
    posterior_new <- predict(m_nb_update, X, "prob")
    post_x_new <- tail(posterior_new[, cat_cur], 1)
    # compare to average prediction given previously perceived stimuli
    idxs_stim <- which(tbl_new$stim_id == stim_id_cur)
    post_x_old <- mean(posterior[idxs_stim, cat_cur])
    p_thx <- runif(1)
    if (
      # (post_x_new > post_x_old) & 
      p_thx < min(1, post_x_new/post_x_old) &
      between(X_new$x1, thx[1], thx[2]) & 
      between(X_new$x2, thx[1], thx[2])
    ) {
      #cat("accepted\n")
      tbl_new <- rbind(
        tbl_new, tibble(stim_id = stim_id_cur, X_new, category = cat_cur)
      )
      posterior <- posterior_new
      # refit model
      m_nb_update <- naive_bayes(fml, data = tbl_new)
    }
    
    setTxtProgressBar(pb,i)
  }
  close(pb)

  # Post Processing ---------------------------------------------------------
  
  nstart <- nrow(tbl)
  nnew <- nrow(tbl_new) - nstart
  tbl_new$timepoint <- c(rep("Before Training", nstart), rep("After Training", nnew))
  l_results <- add_centers(tbl_new, m_nb_initial, m_nb_update, categories)
  
  
  tbl_posterior_long <- extract_posterior(posterior, m_nb_update, tbl_new)[[1]]
  tbl_posteriors <- tbl_prior_long %>%
    mutate(timepoint = "Before Training") %>%
    rbind(
      tbl_posterior_long %>%
        mutate(timepoint = "After Training")
    ) %>%
    mutate(
      timepoint = fct_relevel(timepoint, "Before Training", "After Training")
    )
  
  pl_centers <- plot_moves(l_results$tbl_posterior, thx)
  pl_post <- plot_cat_probs(tbl_posteriors)
  
  
  # Inspect Prior and Samples ---------------------------------------------
  # the following only works when nruns is large enough
  nice_showcase <- max(tbl$x1) + 1
  n_accept_stimuli <- tbl_new %>% arrange(stim_id) %>%
    group_by(stim_id) %>% mutate(rwn = row_number(x1)) %>% 
    ungroup() %>% arrange(desc(rwn))
  if(n_accept_stimuli %>% filter(stim_id == nice_showcase) %>% 
     select(rwn) %>% as_vector() > 2) {
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
    )
  
  tbl_samples <- normal_quantiles_given_pars(tbl_tmp, prior_sd)
  
  tbl_plt <- tbl_samples %>% group_by(Variable, Timepoint) %>% 
    mutate(rwn = row_number(Value)) %>%
    pivot_wider(names_from = Variable, values_from = Value) %>%
    select(-rwn) %>%
    mutate(Timepoint = recode_factor(Timepoint, prior = "Prior", posterior = "Posterior"))
  
  pl <- ggplot(tbl_plt, aes(x1, x2, group = Timepoint)) +
    geom_point(aes(color = Timepoint), shape = 1) +
    theme_bw() +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = bquote(x[1]),
      y = bquote(x[2])
    )
  
  pl_marginals <- ggMarginal(pl, groupColour = TRUE, type="density", size=10)
  
  l_plots <- list(
    pl_centers, pl_post, pl_marginals
  )
  
  l_out <- list(
    l_results, l_plots
  )
  
  return(l_out)
}


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
  marrangeGrob(pl, nrow = n_rows, ncol = n_cols, top = quote(paste("")))
}

extract_posterior <- function(posterior, m_nb, tbl_new) {
  #' extract posterior of the true values
  #' 
  #' @param posterior posterior probabilities of all categories given data
  #' @param m_nb a trained naive bayes model
  #' @param tbl_new the original tbl with the accepted samples
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
  #' aggregate prior and/or sampled values and join with model centroids
  #' 
  #' @param tbl tibble with prior and/or samples
  #' @param timepoint_str a string stating the time point of learning
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


center_of_category <- function(m, timepoint_str, categories){
  #' model-based category centroids
  #' 
  #' @param m the fitted naive Bayes model
  #' @param timepoint_str a string stating the time point of learning
  #' @param categories unique available categories
  #' 
  map(m$tables, function(x) x[1, ]) %>% 
    as_tibble() %>%
    mutate(
      category = categories,
      timepoint = timepoint_str
    ) 
}


add_centers <- function(tbl_new, m_nb_initial, m_nb_update, categories) {
  #' add category centers
  #' 
  #' @param tbl_new a tibble with all the accepted samples
  #' @param m_nb_initial the originally fitted naive Bayes model (i.e., prior model)
  #' @param m_nb_update the most recently updated naive Bayes model
  #' @param categories vector with unique categories
  #' 
  tbl_centers <- rbind(
    center_of_category(m_nb_initial, "Before Training", categories),
    center_of_category(m_nb_update, "After Training", categories)
    )
  tbl_prior <- tbl_new %>% filter(timepoint == "Before Training")
  tbl_samples <- tbl_new %>% filter(timepoint == "After Training")
  l <- map2(
    list(tbl_prior = tbl_prior, tbl_samples = tbl_samples, tbl_posterior = tbl_new),
    list("Before Training", "Only Samples", "After Training"),
    agg_and_join,
    tbl_centers = tbl_centers
  )
  return(l)
}

plot_moves <- function(tbl_results, thx) {
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
      x = bquote(x[1]),
      y = bquote(x[2])
    )
}

plot_cat_probs <- function(tbl_posteriors) {
  bw <- 10/nrow(tbl_posteriors)
  ggplot(tbl_posteriors, aes(value)) +
    geom_histogram(aes(y = ..density..), alpha = .5, color = "black", binwidth = bw) +
    geom_density(aes(y = ..density..), color = "purple", size = 1) +
    facet_wrap(~ timepoint) +
    theme_bw() +
    labs(
      x = "Posterior Probability",
      y = "Probability Density"
    )
}

normal_quantiles_given_pars <- function(tbl, prior_sd) {
  #' calculate quantiles given prior mean and sd, 
  #' as well as empirical mean and sd from accepted samples
  #' 
  #' @param tbl tibble with the aggregated values
  #' 
  tmp_mean <- tbl %>% filter(timepoint == "After Training") %>%
    select(ends_with("_mean")) %>%
    unlist()
  tmp_sd <- tbl %>% filter(timepoint == "After Training") %>%
    select(ends_with("_sd")) %>%
    unlist()
  tmp_mean_prior <- tbl %>% filter(timepoint == "Before Training") %>% 
    select(ends_with("_mean")) %>%
    unlist()
  names(tmp_mean_prior) <- str_c(names(tmp_mean_prior), "_prior")
  tbl_parms <- tibble(
    mean = c(tmp_mean_prior, tmp_mean),
    sd = append(rep(prior_sd, length(tmp_mean)), tmp_sd)
  )
  tbl_out <- pmap(tbl_parms, qnorm, p = seq(.01, .99, by = .01)) %>% as_tibble()
  names(tbl_out) <- str_c(
    rep(str_match(names(tmp_mean), "(^.+)_")[, 2], 2),
    rep(c("_prior", "_posterior"), each = 2)
  )
  tbl_out_long <- tbl_out %>% pivot_longer(
    names(tbl_out), names_to = "Variable", values_to = "Value"
  ) %>% separate(Variable, c("Variable", "Timepoint"), "_")
  
  return(tbl_out_long)
}


check_categories <- function(n_categories) {
  #' check that n_categories is a usable value
  #' 
  #' @param n_categories an integer 2^x with any integer as x
  #' 
  if (!is_integer(n_categories)) stop("Make sure n_categories is an integer")
  
  if (!(sqrt(n_categories) %% 1 == 0)) {
    stop("Make sure n_categories is 2^x")
  }
}


prior_posterior_for_stim_id <- function(l, l_id, s_id) {
  #' extract prior and posterior means from nested results list
  #' 
  #' @param l the nested list
  #' @param l_id the first level of the list's hierarchy to be selected
  #' @param s_id the stimulus_id to be extracted
  #' 
  l[[l_id]][[1]]$tbl_posterior %>%
    mutate(n_categories = factor(max(as.numeric(category)))) %>%
    filter(stim_id == s_id)
}
