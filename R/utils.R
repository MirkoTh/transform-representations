library(tidyverse)
library(gridExtra)
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)
library(assertthat)
library(mvtnorm)

categorize_stimuli <- function(l_info) {
  #' main categorization function
  #' 
  #' @description categorize stimuli, store accepted samples, and visualize results
  #' 
  #' @param l_info parameter list with fields n_stimuli, n_categories, prior_sd, nruns
  #' @return a list with prior, samples, and posterior in [[1]] and some
  #' visualizations in [[2]]
  #' 
  
  l_tmp <- make_stimuli(l_info)
  tbl <- l_tmp[[1]]
  l_info <- l_tmp[[2]]
  # compute priors
  l_m <- priors(l_info, tbl)
  
  # save prior for later and copy original tbl
  l_prior_prep <- extract_posterior(l_m$posterior_prior, tbl)
  tbl_prior_long <- l_prior_prep[[1]]
  l_prior <- l_prior_prep[[2]]
  posterior <- l_m$posterior_prior
  tbl_new <- tbl
  
  unique_boundaries <- boundaries(tbl, l_info)
  thx_grt <- thxs(unique_boundaries)
  
  
  # Categorization Simulation -----------------------------------------------
  
  pb <- txtProgressBar(min = 1, max = l_info$nruns, initial = 1, char = "*", style = 2)
  for (i in 1:l_info$nruns) {
    # perceive a randomly sampled stimulus
    # while(l_x$stim_id_cur != 1){}
    l_x <- perceive_stimulus(tbl_new, l_info)
    
    # propose a new posterior
    if (l_info$cat_type == "rule") {
      posterior_new <- pmap(
        thx_grt, grt_cat_probs, tbl_stim = l_x$X_new, l_info = l_info
      ) %>% unlist() %>%
        matrix(nrow = 1) %>%
        as_tibble(.name_repair = ~ l_info$categories)
    } else if (l_info$cat_type == "prototype") {
      posterior_new <- tail(predict(l_m$m_nb_update, l_x$X, "prob"), 1)
    } else if (l_info$cat_type == "exemplar") {
      l_info$sens <- l_m$gcm_params[1]
      l_info$wgh <- l_m$gcm_params[2]
      posterior_new <- predict_gcm(tbl_new, l_x$X_new, l_info)
      colnames(posterior_new) <- l_info$categories
    }
    
    post_x_new <- round(as_vector(tail(posterior_new[, l_x$cat_cur], 1)), 3)
    # compare to average prediction given previously perceived stimuli
    idxs_stim <- which(tbl_new$stim_id == l_x$stim_id_cur)
    post_x_old <- round(mean(as_vector(posterior[idxs_stim, l_x$cat_cur])), 3)
    p_thx <- runif(1)
    # decide on whether to accept or reject a proposition
    is_improvement <- post_x_new > post_x_old
    mh_is_accepted <- p_thx < min(1, post_x_new/post_x_old)
    is_in_shown_space <- (
      between(l_x$X_new$x1, l_info$space_edges[1], l_info$space_edges[2]) & 
        between(l_x$X_new$x2, l_info$space_edges[1], l_info$space_edges[2])
    )
    if (
      ifelse(l_info$sampling == "improvement", is_improvement, mh_is_accepted) & 
      ifelse(l_info$constrain_space, is_in_shown_space, TRUE)
    ) {
      #cat("accepted\n")
      onehots <- one_hot(l_info, l_x$cat_cur)
      m_onehots <- as_tibble(t(as.matrix(onehots)))
      tbl_new <- rbind(
        tbl_new, tibble(
          stim_id = l_x$stim_id_cur, l_x$X_new, 
          m_onehots,
          category = l_x$cat_cur, cat_type = l_info$cat_type
        )
      )
      posterior <- rbind(posterior, posterior_new)
      # refit prototype and exemplar models when sample was accepted
      # if (l_info$cat_type == "prototype") {
      #   l_m$m_nb_update <- naive_bayes(l_m$fml, data = tbl_new)
      # }
      # if (l_info$cat_type == "exemplar") {
      #   fit_gcm <- optim(
      #     f = ll_gcm, c(3, .5), lower = c(0, 0), upper = c(10, 1),
      #     l_info = l_info, tbl_stim = tbl_new, method = "L-BFGS-B"
      #   )
      #   l_m$gcm_params <- fit_gcm$par
      # }
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  # Post Processing ---------------------------------------------------------
  
  nstart <- nrow(tbl)
  nnew <- nrow(tbl_new) - nstart
  tbl_new$timepoint <- c(rep("Before Training", nstart), rep("After Training", nnew))
  
  l_out <- list(
    tbl_new = tbl_new, tbl_prior_long = tbl_prior_long, l_m = l_m,
    posterior = posterior, l_info = l_info
  )
  return(l_out)
}


make_stimuli <- function(l_info) {
  #' create stimuli from 2D feature space
  #' 
  #' @param l_info list with parameters
  #' @return a list with a tbl containing the stimulus set and 
  #' the parameter list with the added infos
  #' 
  l_info$space_edges <- c(0, sqrt(l_info$n_stimuli) - 1)
  x1 <- seq(l_info$space_edges[1], l_info$space_edges[2], by = 1)
  x2 <- seq(l_info$space_edges[1], l_info$space_edges[2], by = 1)
  features <- crossing(x1, x2)
  tbl <- tibble(stim_id = seq(1, nrow(features)), features)
  if (l_info$category_shape == "square") {
    tbl <- create_categories(tbl, sqrt(l_info$n_categories)) %>% 
      select(-c(x1_cat, x2_cat))
  } else if (l_info$category_shape == "ellipses") {
    l <- create_ellipse_categories(tbl, l_info$n_categories)
    tbl <- l[[1]]
    l_info$tbl_ellipses <- l[[2]]
  }
  
  l_info$feature_names <- c("x1", "x2")
  l_info$label <- "category"
  tbl$category <- as.factor(tbl$category)
  l_info$categories <- levels(tbl$category)
  tbl$cat_type <- l_info$cat_type
  tbl <- tbl %>% mutate(observed = 1) %>%
    pivot_wider(
      names_from = category, values_from = observed,
      names_sort = TRUE, names_prefix = "cat"
    ) %>%
    mutate(category = tbl$category)
  tbl[is.na(tbl)] <- 0
  if (l_info$is_reward) {
    tbl$prior_sd <- l_info$prior_sd
  }
  
  return(list(tbl, l_info))
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


create_ellipse_categories <- function(tbl, n_categories) {
  #' create ellipse categories from feature space
  #' 
  #' @description create one ellipse or three ellipses within feature space
  #' creating different categories; assign all points to those one/three
  #' categories or to a baseline category (i.e., points not within any
  #' of the ellipses)
  #' @param tbl \code{tibble} containing each of the two features in a column
  #' @param n_categories \code{integer} stating how many categories to create
  #' @return a list with the \code{tibble} with an added column stating 
  #' the category and another \code{tibble} with the ellipse contours
  #' 
  thxs <- c(0, apply(tbl[, c("x1")], 2, function(x) (min(x) + max(x))/2))
  theta_deg <- 45
  fctr_mid <- list(
    "squash_all" = .9, "squash_y" = 1, "squash_x" = .45, 
    "move_x" = 0, "move_y" = 0, "category" = 2
  )
  fctr_hi <- list(
    "squash_all" = .85, "squash_y" = .75, "squash_x" = .225,
    "move_x" = 3, "move_y" = -3, "category" = 3
  )
  fctr_lo <- list(
    "squash_all" = .85, "squash_y" = .75, "squash_x" = .225,
    "move_x" = -3, "move_y" = 3, "category" = 4
  )
  fctr_mid_hi <- list(
    "squash_all" = .9, "squash_y" = .75, "squash_x" = .45, 
    "move_x" = -1.5, "move_y" = 1.5, "category" = 2
  )
  fctr_mid_lo <- list(
    "squash_all" = .9, "squash_y" = .75, "squash_x" = .45, 
    "move_x" = 1.5, "move_y" = -1.5, "category" = 3 
  )
  if (n_categories == 4) {
    l_map <- list(fctr_mid, fctr_hi, fctr_lo)
  } else if (n_categories == 3) {
    l_map <- list(fctr_mid_hi, fctr_mid_lo)
  } else if (n_categories == 2) {
    l_map <- list(fctr_mid)
  }
  
  
  l <- map(l_map, assign_grid_points, tbl = tbl, thxs = thxs, theta_deg = theta_deg)
  tbl_all_cats <- reduce(
    map(l, 2), function(x, y) inner_join(x, y, by = c("stim_id", "x1", "x2"))
  )
  tbl_ellipses <- reduce(map(l, 1), rbind)
  cat_tmp <- apply(tbl_all_cats[, 4: ncol(tbl_all_cats)], 1, max)
  tbl_all_cats <- tbl_all_cats[, 1:3]
  tbl_all_cats$category <- as.factor(cat_tmp)
  
  return(list(tbl_all_cats, tbl_ellipses))
}

assign_grid_points <- function(fctrs, tbl, thxs, theta_deg) {
  #' define whether 2D points are inside or outside an ellipse
  #' 
  #' @description take all integer pairs from a 2D space and decide whether
  #' each pair is inside or outside a given ellipse
  #' @param fctrs squashing and moving of ellipse
  #' @param tbl tibble with 2D points (x1 and x2)
  #' @param thxs min and max vals on x and y axis
  #' @param theta_deg rotation angle in degrees
  #' @return list with tbl defining ellipse and tbl with all 2D points
  #' 
  tbl_ellipse <- ellipse(thxs, fctrs, theta_deg)
  # https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
  elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
  is_within_ellipse <- function(x1, x2, tbl_ellipse) {
    tryCatch(
      warning = function(cnd) FALSE,
      {
        y_range <- tbl_ellipse %>% filter(elementwise.all.equal(x_rotated, x1)) %>%
          summarize(y_min = min(y_rotated), y_max = max(y_rotated))
        return(between(x2, y_range$y_min, y_range$y_max))
      }
    )
  }
  in_ellipse <- pmap_lgl(tbl[, c("x1", "x2")], is_within_ellipse, tbl_ellipse = tbl_ellipse)
  tbl$category <- 1
  tbl$category[in_ellipse] <- fctrs[["category"]]
  tbl_ellipse$category <- fctrs[["category"]]
  return(list(tbl_ellipse, tbl))
}

sample_ellipse_space <- function(fctrs, n, thxs, theta_deg) {
  #' uniformly sample from ellipse
  #' 
  #' @description randomly sample from a space defined by an ellipse
  #' @param fctrs squashing and moving of ellipse
  #' @param n number of samples
  #' @param thxs min and max vals on x and y axis
  #' @param theta_deg rotation angle in degrees
  #' @return x and y values of samples
  #' 
  tbl_ellipse <- ellipse(thxs, fctrs, theta_deg)
  min_max <- apply(tbl_ellipse[, c("x_rotated", "y_rotated")], 2, function(x) c(min(x), max(x)))
  x1 <- round(runif(n, min_max[1, 1], min_max[2, 1]), 1)
  # https://stackoverflow.com/questions/9508518/why-are-these-numbers-not-equal
  elementwise.all.equal <- Vectorize(function(x, y) {isTRUE(all.equal(x, y))})
  
  sample_y_uniform <- function(x_val) {
    y_bounds <- tbl_ellipse %>% filter(elementwise.all.equal(x_rotated, x_val)) %>% 
      summarize(min_y = min(y_rotated), max_y = max(y_rotated)) %>% 
      as_vector() %>% sort() %>% unname()
    return(runif(1, y_bounds[1], y_bounds[2]))
  }
  x2 <- map_dbl(x1, sample_y_uniform)
  tbl <- tibble(x1, x2)
  return(tbl_ellipse)
}

ellipse <- function(thxs, fctrs, theta_deg) {
  #' create an ellipse and rotate it
  #' 
  #' @description create an ellipse within 2D range
  #' @param thxs min and max vals on x and y axis
  #' @param fctrs squashing and moving of ellipse
  #' @param theta_deg rotation angle in degrees
  #' @return fine grained 2D values of a rotated ellipse
  #' 
  x_prep <- seq(0, 2*pi, by = .01)
  tbl_circle <- tibble(
    x = thxs[2] * fctrs[["squash_all"]] * fctrs[["squash_x"]] * sin(x_prep),
    y = thxs[2] * fctrs[["squash_all"]] * fctrs[["squash_y"]] * cos(x_prep)
  )
  tbl_circle <- cbind(
    tbl_circle, 
    t(tbl_circle %>% as.matrix()) %>% rotate_points(theta_deg) %>% t() %>%
      round(1)
  ) %>% rename(x_rotated = `1`, y_rotated = `2`) %>% 
    mutate(
      x_rotated = x_rotated + thxs[2],
      y_rotated = y_rotated + thxs[2]
    )
  tbl_circle$x_rotated <- tbl_circle$x_rotated + fctrs[["move_x"]]
  tbl_circle$y_rotated <- tbl_circle$y_rotated + fctrs[["move_y"]]
  return(tbl_circle)
}



rotate_points <- function(x, theta_deg) {
  #' rotate 2D points in clockwise direction
  #' according to theta_deg (rotation angle in degrees)
  #' 
  #' @description rotate 2D points
  #' @param x a matrix of 2D points
  #' @param theta_deg rotation angle in degrees
  #' @return the rotate x matrix
  #' 
  theta_rad <- (theta_deg * pi) / 180
  theta_sin <- sin(theta_rad)
  theta_cos <- cos(theta_rad)
  m_rotate <- matrix(c(theta_cos, -theta_sin, theta_sin, theta_cos), ncol = 2, byrow = FALSE)
  x_rotated <- apply(x, 2, function(a) m_rotate %*% a)
  x_rotated
  return(x_rotated)
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


priors <- function(l_info, tbl) {
  #' calculate category priors for one of the three different categorization types
  #' 
  #' @param l_info parameter list
  #' @param tbl \code{tibble} containing the two features and the category as columns
  #' @return the stimuli priors
  #' 
  if (l_info$cat_type == "rule"){
    l_info$n_categories <- length(l_info$categories)
    unique_boundaries <- boundaries(tbl, l_info)
    thx_grt <- thxs(unique_boundaries)
    posterior_prior <- pmap(
      thx_grt, grt_cat_probs, tbl_stim = tbl, l_info = l_info
    ) %>% unlist() %>%
      matrix(nrow = nrow(tbl)) %>%
      as_tibble(.name_repair = ~ l_info$categories)
    l_out <- list(posterior_prior = posterior_prior)
    
  } else if (l_info$cat_type == "prototype") {
    fml <- formula(str_c(l_info$label, " ~ ", str_c(l_info$feature_names, collapse = " + ")))
    m_nb_initial <- naive_bayes(fml, data = tbl)
    m_nb_update <- m_nb_initial
    posterior_prior <- predict(m_nb_initial, tbl[, c("x1", "x2")], "prob")
    l_out <- list(
      posterior_prior = posterior_prior, fml = fml,
      m_nb_initial = m_nb_initial, m_nb_update = m_nb_update
    )
    
  } else if (l_info$cat_type == "exemplar"){
    fit_gcm <- optim(
      f = ll_gcm, c(3, .5), lower = c(0, 0), upper = c(10, 1),
      l_info = l_info, tbl_stim = tbl, method = "L-BFGS-B"
    )
    l_info$sens <- fit_gcm$par[1]
    l_info$wgh <- fit_gcm$par[2]
    posterior_prior <- predict_gcm(tbl, tbl, l_info) %>%
      as_tibble(.name_repair = ~ l_info$categories)
    l_out <- list(
      posterior_prior = posterior_prior,
      gcm_params = c(l_info$sens, l_info$wgh))
  }
  return(l_out)
}


perceive_stimulus <- function(tbl_new, l_info, is_reward = FALSE) {
  #' simulate noisy perception of a 2D stimulus
  #' 
  #' @param tbl_new \code{tibble} containing the stimulus id, two features,
  #' and the category as columns
  #' @param l_info parameter list
  #' @return three different X tbls (before, after, together)
  #' 
  # randomly move random observation
  idx <- sample(l_info$n_stimuli, 1)
  cat_cur <- tbl_new$category[idx]
  stim_id_cur <- tbl_new$stim_id[idx]
  reward_magnitude <- 1
  if (is_reward) {
    prior_sd <- as_vector(tbl_new[idx, "prior_sd"])
    reward_magnitude <- tbl_new[idx, "reward"]
    X_new <-  tibble(
      tbl_new[idx, "x1"] + rnorm(1, 0, prior_sd), 
      tbl_new[idx, "x2"] + rnorm(1, 0, prior_sd)
    )
  } else {
    prior_sd <- l_info$prior_sd
    X_new <-  tibble(
      tbl_new[idx, "x1"] + rnorm(1, 0, l_info$prior_sd), 
      tbl_new[idx, "x2"] + rnorm(1, 0, l_info$prior_sd)
    )
  }
  
  X_old <- tbl_new[, c("x1", "x2")]
  # create new X matrix
  X <- rbind(X_old, X_new)
  colnames(X) <- l_info$feature_names
  l_out <- list(
    X_old = X_old, X_new = X_new, X = X,
    cat_cur = cat_cur, stim_id_cur = stim_id_cur,
    prior_sd = prior_sd, reward_magnitude = as_vector(reward_magnitude)
  )
  return(l_out)
}


boundaries <- function(tbl, l_info) {
  #' calculate decision boundaries
  #' 
  #' @param tbl \code{tibble} containing the stimulus id, two features,
  #' and the category as columns
  #' @param l_info parameter list
  #' @return the unique boundaries in a vector
  #' 
  x_vals <- sort(unique(tbl$x1))
  stepsize <- length(x_vals) / sqrt(l_info$n_categories)
  cutoffs_prep <- seq(0, length(x_vals), stepsize) - .5
  unique_boundaries <- cutoffs_prep[between(cutoffs_prep, min(tbl$x1), max(tbl$x1))]
  return(unique_boundaries)
}


thxs <- function(cutoff_vals) {
  #' create required thresholds for rule-based categorization model
  #' 
  #' @param cutoff_vals unique cut-off values that have to be the same across
  #' the two dimensions
  #' @return a list with all the thresholds for the categories
  #' 
  #' assuming uncorrelated feature dimensions
  #' assuming prior variance is fixed across dimensions
  prep_lower <- c(-Inf, cutoff_vals)
  prep_upper <- c(cutoff_vals, Inf)
  n_thx <- length(prep_lower)
  l <- list()
  l[["x1_lower"]] <- rep(prep_lower, n_thx)
  l[["x2_lower"]] <- rep(prep_lower, each = n_thx)
  l[["x1_upper"]] <- rep(prep_upper, n_thx)
  l[["x2_upper"]] <- rep(prep_upper, each = n_thx)
  return(l)
}


extract_posterior <- function(posterior, tbl_new) {
  #' extract posterior of the true values
  #' 
  #' @param posterior posterior probabilities of all categories given data
  #' @param tbl_new the original tbl with the accepted samples
  #' 
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
    suffix = c("_true", "_center")
  )
  tbl_results$timepoint <- factor(
    tbl_results$timepoint, 
    levels = c("Before Training", "After Training")
  )
  return(tbl_results)
}


center_of_category <- function(l_info, l_m, timepoint_str, tbl_new){
  #' calculate category centroids
  #' model-based centroids for prototype model
  #' trial averages for rule-based model
  #' 
  #' @param l_info the parameter list
  #' @param l_m a list with model-based info (i.e., posterior, model object)
  #' @param timepoint_str a string stating the time point of learning
  #' @param tbl_new tbl with the prior the accepted samples
  #' 
  if (l_info$cat_type == "prototype") {
    if(timepoint_str == "Before Training") m <- l_m$m_nb_initial
    if(timepoint_str == "After Training") m <- l_m$m_nb_update
    map(m$tables, function(x) x[1, ]) %>% 
      as_tibble() %>%
      mutate(
        category = l_info$categories,
        timepoint = timepoint_str
      ) 
  } else {
    tbl_new %>% 
      filter(timepoint == timepoint_str) %>%
      group_by(category, timepoint) %>%
      summarize(
        x1 = mean(x1),
        x2 = mean(x2)
      ) %>% ungroup()
  }
}


add_centers <- function(tbl_new, l_m, l_info) {
  #' add category centers
  #' 
  #' @param tbl_new a tibble with all the accepted samples
  #' @param l_m predictions and model info if available
  #' @param l_info parameter and experimental infos
  #' 
  # model-based prototypes
  tbl_centers <- rbind(
    center_of_category(l_info, l_m, "Before Training", tbl_new),
    center_of_category(l_info, l_m, "After Training", tbl_new)
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



normal_quantiles_given_pars <- function(tbl, l_info) {
  #' calculate quantiles given prior mean and sd, 
  #' as well as empirical mean and sd from accepted samples
  #' 
  #' @param tbl tibble with the aggregated values
  #' @param l_info parameter list
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
    sd = append(rep(l_info$prior_sd, length(tmp_mean)), tmp_sd)
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

check_cat_types <- function(cat_type) {
  #' check that cat_type is one of the available options
  #' 
  #' @param cat_type a character vector
  #' 
  assert_that(are_equal(
    sum(str_detect(cat_type, c("^prototype$", "^rule$", "^exemplar$"))), 1
  ), msg = "Make sure cat_type is one of prototype, rule, exemplar")
}


prior_posterior_for_stim_id <- function(l, l_id, s_id) {
  #' extract prior and posterior means from nested results list
  #' 
  #' @param l the nested list
  #' @param l_id the first level of the list's hierarchy to be selected
  #' @param s_id the stimulus_id to be extracted
  #' 
  l$tbl_new %>%
    mutate(n_categories = factor(max(as.numeric(category)))) %>%
    filter(stim_id == s_id)
}


stimulus_before_after <- function(l_results, stim_id) {
  #' prior and posterior 2D means of a stimulus
  #' 
  #' @param l_results the list with the simulation results
  #' @param stim_id the stimulus_id to be extracted
  #' 
  prior_posterior_for_stim_id <- function(l, s_id) {
    tmp <- l$tbl_new %>%
      mutate(
        n_categories = factor(max(as.numeric(category))),
        prior_sd = str_c("Prior SD = ", l$l_info$prior_sd),
        sampling = str_c("Sampling = ", l$l_info$sampling),
        space = str_c("Space Constrained = ", l$l_info$constrain_space)
      ) %>%
      filter(stim_id == s_id)
    cols_keep <- !str_detect(colnames(tmp), pattern = "^cat[0-9]+$")
    tmp[, cols_keep]
  }
  tbl_stimulus <- reduce(
    map(l_results, prior_posterior_for_stim_id, s_id = stim_id), 
    rbind
  ) %>% group_by(
    stim_id, cat_type, timepoint, n_categories, prior_sd, sampling, space
  ) %>%
    summarize(x1 = mean(x1), x2 = mean(x2)) %>% ungroup()
  tbl_stimulus <- tbl_stimulus %>%
    select(c(x1, x2, cat_type, prior_sd, sampling, space, timepoint, n_categories)) %>%
    pivot_wider(
      id_cols = c(cat_type, prior_sd, sampling, space, n_categories), # 5 + 4 = 9
      names_from = timepoint, values_from = c(x1, x2),
      names_sort = TRUE
    )
  # in case no samples were stored at all:
  if(ncol(tbl_stimulus) < 9) { # 9 see above
    tbl_stimulus <- cbind(
      tbl_stimulus, 
      x1_aft = rep(tbl_stimulus$x1_bef[1], nrow(tbl_stimulus)), 
      x2_aft = rep(tbl_stimulus$x2_bef[1], nrow(tbl_stimulus))
    ) %>% relocate(x1_aft, .after = n_categories) %>%
      relocate(x2_aft, .after = `x1_Before Training`)
  }
  names(tbl_stimulus) <- c(
    "cat_type", "prior_sd", "sampling", "space", "n_categories", 
    "x1_aft", "x1_bef", "x2_aft", "x2_bef"
  )
  tbl_stimulus$x1_aft[is.na(tbl_stimulus$x1_aft)] <- tbl_stimulus$x1_bef[1]
  tbl_stimulus$x2_aft[is.na(tbl_stimulus$x2_aft)] <- tbl_stimulus$x2_bef[1]
  tbl_stimulus$cat_type <- as.factor(tbl_stimulus$cat_type)
  upper_first <- function(s) {
    s <- str_c(str_to_upper(substr(s, 1, 1)), substr(s, 2, str_length(s)))
    s
  }
  levels(tbl_stimulus$cat_type) <- map_chr(levels(tbl_stimulus$cat_type), upper_first)
  return(tbl_stimulus)
}


grt_cat_probs <- function(x1_lower, x2_lower, x1_upper, x2_upper, tbl_stim, l_info){
  #' categorize using two-dimensional general recognition theory model
  #' 
  #' @description compute category probability given category thresholds (thx), 
  #' 2d stimulus feature values, and prior variance of stimulus
  #' 
  #' @param x1_lower lower thx on x1 dimension
  #' @param x2_lower lower thx on x2 dimension
  #' @param x1_upper upper thx on x1 dimension
  #' @param x2_upper upper thx on x2 dimension
  #' @param tbl_stim 
  #' @param l_info parameter list
  #' @return a list with prior, samples, and posterior in [[1]] and some
  #' visualizations in [[2]]
  #' 
  #' assuming uncorrelated feature dimensions
  #' assuming prior variance is fixed across dimensions
  m_cov <- matrix(c(l_info$prior_sd ^ 2, 0, 0, l_info$prior_sd ^ 2), nrow = 2, byrow = TRUE)
  f_pred <- function(x1, x2) { 
    pmvnorm(
      lower = c(x1_lower, x2_lower), 
      upper = c(x1_upper, x2_upper), 
      mean = c(x1, x2), 
      sigma = m_cov
    )
  }
  pmap_dbl(tbl_stim[, c("x1", "x2")], f_pred)
}


predict_gcm <- function(tbl_train, tbl_test, l_info) {
  #' categorize using nosofsky's gcm (2011) implemented in catlearn (Wilks)
  #' 
  #' @param tbl_train train data as tbl
  #' @param tbl_test test data as tbl
  #' @param l_info parameter list
  #' @return a matrix with category probabilities for tbl_test given tbl_train
  #' 
  l_st <- list(
    training_items = tbl_train[, c(
      l_info$feature_names, str_c("cat", l_info$categories)
    )], 
    tr = tbl_test[, c(l_info$feature_names)], 
    nCats = l_info$n_categories, 
    nFeat = length(l_info$feature_names), 
    sensitivity = l_info$sens, 
    weights = l_info$wgh, 
    choice_bias = rep(1 / l_info$n_categories, l_info$n_categories - 1), 
    p = 1, # 1 exponential, 2 gaussian
    r_metric = 2, # 1 city block, 2 euclidian
    # mp, optional memory strength parameter; i.e., should certain items receive higher memory strength?
    gamma = 1
  )
  stsimGCM(l_st)
}

ll_gcm <- function(params, l_info, tbl_stim) {
  #' gcm fitting wrapper
  #' 
  l_info$sens <- params[1]
  l_info$wgh <- params[2]
  m_preds <- predict_gcm(tbl_stim, tbl_stim, l_info)
  ll <- sum(log(m_preds[cbind(seq(1, nrow(m_preds), by = 1), tbl_stim$category)]))
  -2*ll
}


one_hot <- function(l_info, idx) {
  #' make one-hot encoded vector with 1 at idx
  #' 
  v_zeros <- rep(0, l_info$n_categories)
  names(v_zeros) <- str_c("cat", l_info$categories)
  v_zeros[idx] <- 1
  return(v_zeros)
}

tt_split_rewards <- function(tbl, l_info) {
  #' prioritize 5 items from two categories close to the category boundary
  #' 
  #' @description given one ellipse category and second category around
  #' create 5 examples from each category to be high-reward items
  #' 
  #' @param tbl tibble with all 2d location spanning the grid
  #' @param l_info list of simulation settings
  #' @return list containing 
  #' a tibble with new columns reward and timepoint (i.e., train or test)
  #' the updated list with the parameter settings
  #' 
  n_by_cat <- tbl %>% count(category)
  tmp <- ceiling(n_by_cat$n * l_info$prop_train)
  # n train per category should be an even number for symmetry reasons
  n_by_cat$n_train <- ifelse(tmp %% 2 == 1, tmp + 1, tmp)
  n_by_cat$n_test <- n_by_cat$n - n_by_cat$n_train
  l_info$n_train <- sum(n_by_cat$n_train)
  
  # close-to-boundary train examples
  tbl_outside_hi <- tibble(
    x1 = seq(4, 10, by = 1),
    x2 = seq(1, 7, by = 1),
    reward = rep(5, 7),
    timepoint = "train"
  )
  tbl_inside_hi <- tibble(
    x1 = seq(2, 7, by = 1),
    x2 = seq(4, 9, by = 1),
    reward = rep(5, 6),
    timepoint = "train"
  )
  # randomly select remaining train examples given proportion
  tbl_inside_lo <- tbl %>% 
    filter(category == 2 & x2 >= x1) %>%
    left_join(tbl_inside_hi, by = c("x1", "x2")) %>%
    filter(is.na(reward)) %>%
    select(x1, x2) %>%
    mutate(reward = 1)
  
  tbl_inside_lo <- tbl_inside_lo[sample(
    1:nrow(tbl_inside_lo), size = nrow(tbl_inside_lo), replace = FALSE
  ), ]
  
  tbl_outside_lo_1 <- tbl %>%
    filter(x1 >= x2 + 4 | (x1 <= 3 & x2 <= 3))  %>%
    left_join(tbl_outside_hi, by = c("x1", "x2")) %>%
    filter(is.na(reward)) %>%
    select(x1, x2) %>%
    mutate(reward = 1)
  tbl_outside_lo_2 <- tbl %>%
    filter(x1 <= x2 - 4 | (x1 >= 8 & x2 >= 8))  %>%
    left_join(tbl_outside_hi, by = c("x1", "x2")) %>%
    filter(is.na(reward)) %>%
    select(x1, x2) %>%
    mutate(reward = 1)
  tbl_outside_lo <- rbind(tbl_outside_lo_1, tbl_outside_lo_2)
  tbl_outside_lo <- tbl_outside_lo[sample(
    1:nrow(tbl_outside_lo), size = nrow(tbl_outside_lo), replace = FALSE
  ), ]
  
  
  n_by_cat$n_train_hi <- c(nrow(tbl_outside_hi), nrow(tbl_inside_hi))
  n_by_cat$n_train_lo <- n_by_cat$n_train - n_by_cat$n_train_hi
  tbl_outside_lo$timepoint <- "test"
  tbl_outside_lo$timepoint[1:n_by_cat$n_train_lo[n_by_cat$category == 1]] <- "train"
  tbl_inside_lo$timepoint <- "test"
  tbl_inside_lo$timepoint[1:n_by_cat$n_train_lo[n_by_cat$category == 2]] <- "train"
  
  tbl_reward <- rbind(
    tbl_outside_hi, tbl_outside_lo,
    tbl_inside_hi, tbl_inside_lo
  )
  tbl <- tbl %>% left_join(tbl_reward, by = c("x1", "x2"))
  tbl$reward[is.na(tbl$reward)] <- 1
  tbl$timepoint[is.na(tbl$timepoint)] <- "test"
  
  return(list(tbl, l_info))
}

reward_categorization <- function(l_info) {
  #' main reward-learning categorization function
  #' 
  #' @description categorize stimuli, store accepted samples, and visualize results
  #' 
  #' @param l_info parameter list with fields n_stimuli, n_categories, prior_sd, nruns
  #' @return a list with prior, samples, and posterior in [[1]] and some
  #' visualizations in [[2]]
  #' 
  
  l_tmp <- make_stimuli(l_info)
  l_split <- tt_split_rewards(l_tmp[[1]], l_tmp[[2]])
  tbl <- l_split[[1]]
  l_info <- l_split[[2]]
  tbl_train <- tbl %>% filter(timepoint == "train")
  tbl_test <- tbl %>% filter(timepoint == "test")
  # compute priors
  l_m <- priors(l_info, tbl_train)
  
  
  # save prior for later and copy original tbl
  l_prior_prep <- extract_posterior(l_m$posterior_prior, tbl_train)
  tbl_prior_long <- l_prior_prep[[1]]
  l_prior <- l_prior_prep[[2]]
  posterior <- l_m$posterior_prior
  tbl_new <- tbl_train
  tbl_new$rwn <- row_number(tbl_new$stim_id)
  
  unique_boundaries <- boundaries(tbl_train, l_info)
  thx_grt <- thxs(unique_boundaries)
  
  
  # Categorization Simulation -----------------------------------------------
  
  pb <- txtProgressBar(min = 1, max = l_info$nruns, initial = 1, char = "*", style = 2)
  for (i in 1:l_info$nruns) {
    # perceive a randomly sampled stimulus
    # while(l_x$stim_id_cur != 1){}
    l_x <- perceive_stimulus(tbl_new, l_info, is_reward = TRUE)
    
    # propose a new posterior
    if (l_info$cat_type == "rule") {
      posterior_new <- pmap(
        thx_grt, grt_cat_probs, tbl_stim = l_x$X_new, l_info = l_info
      ) %>% unlist() %>%
        matrix(nrow = 1) %>%
        as_tibble(.name_repair = ~ l_info$categories)
    } else if (l_info$cat_type == "prototype") {
      posterior_new <- tail(predict(l_m$m_nb_update, l_x$X, "prob"), 1)
    } else if (l_info$cat_type == "exemplar") {
      l_info$sens <- l_m$gcm_params[1]
      l_info$wgh <- l_m$gcm_params[2]
      posterior_new <- predict_gcm(tbl_new, l_x$X_new, l_info)
      colnames(posterior_new) <- l_info$categories
    }
    
    post_x_new <- round(as_vector(tail(posterior_new[, l_x$cat_cur], 1)), 3)
    
    # has the right category be chosen on that trial?
    
    category_chosen <- c(1:l_info$n_categories)[as.logical(rmultinom(1, 1, posterior_new))]
    is_correct <- category_chosen == l_x$cat_cur
    
    # has the average prediction given previously perceived stimuli improved?
    idxs_stim <- which(tbl_new$stim_id == l_x$stim_id_cur)
    post_x_old <- round(mean(as_vector(posterior[idxs_stim, l_x$cat_cur])), 3)
    p_thx <- runif(1)
    # decide on whether to accept or reject a proposition
    is_improvement <- post_x_new > post_x_old
    mh_is_accepted <- p_thx < min(1, post_x_new/post_x_old)
    is_in_shown_space <- (
      between(l_x$X_new$x1, l_info$space_edges[1], l_info$space_edges[2]) & 
        between(l_x$X_new$x2, l_info$space_edges[1], l_info$space_edges[2])
    )
    if (
      is_correct &
      ifelse(l_info$sampling == "improvement", is_improvement, mh_is_accepted) & 
      ifelse(l_info$constrain_space, is_in_shown_space, TRUE)
    ) {
      #cat("accepted\n")
      onehots <- one_hot(l_info, l_x$cat_cur)
      m_onehots <- as_tibble(t(as.matrix(onehots)))
      tbl_new <- rbind(
        tbl_new, tibble(
          stim_id = l_x$stim_id_cur, l_x$X_new,
          cat_type = l_info$cat_type, 
          m_onehots, category = l_x$cat_cur, 
          prior_sd = l_x$prior_sd*as_vector(l_info$nudge_prior[str_c(l_x$reward)]),
          reward = l_x$reward,
          timepoint = "train", rwn = max(tbl_new$rwn) + 1
        )
      )
      tbl_new_unchanged <- tbl_new %>% filter(stim_id != l_x$stim_id_cur)
      tbl_new_prior <- tbl_new %>% filter(stim_id == l_x$stim_id_cur) %>% 
        mutate(prior_sd = max(prior_sd))
      tbl_new <- rbind(tbl_new_unchanged, tbl_new_prior) %>% arrange(rwn)
      posterior <- rbind(posterior, posterior_new)
      # refit prototype and exemplar models when sample was accepted
      # if (l_info$cat_type == "prototype") {
      #   l_m$m_nb_update <- naive_bayes(l_m$fml, data = tbl_new)
      # }
      # if (l_info$cat_type == "exemplar") {
      #   fit_gcm <- optim(
      #     f = ll_gcm, c(3, .5), lower = c(0, 0), upper = c(10, 1),
      #     l_info = l_info, tbl_stim = tbl_new, method = "L-BFGS-B"
      #   )
      #   l_m$gcm_params <- fit_gcm$par
      # }
    }
    setTxtProgressBar(pb,i)
  }
  close(pb)
  tbl_new <- tbl_new %>% select(-rwn)
  
  # Post Processing ---------------------------------------------------------
  
  nstart <- nrow(tbl_train)
  nnew <- nrow(tbl_new) - nstart
  tbl_new$timepoint <- c(rep("Before Training", nstart), rep("After Training", nnew))
  
  l_out <- list(
    tbl_new = tbl_new, tbl_prior_long = tbl_prior_long, l_m = l_m,
    posterior = posterior, l_info = l_info
  )
  return(l_out)
}


adapt_posterior_to_empirical_analysis <- function(tbl) {
  #' fill up not sampled stim_ids with prior
  #' 
  #' @description adds prior to posterior for those stimuli that have not been sampled in simulation
  #' 
  #' @param tbl tibble with prior and posterior samples
  #' @return the same tbl but with prior added to posterior where necessary
  #' 
  tbl_bef_aft <- tbl %>%
    filter(timepoint == "Before Training") %>%
    left_join(
      tbl %>% 
        filter(timepoint == "After Training") %>%
        select(stim_id, x1_true, x2_true), by = "stim_id", suffix = c("_bef", "_aft")) %>%
    filter(!is.na(x1_true_aft))
  samples_posterior_stim_id <- unique(tbl_bef_aft$stim_id)
  tbl_after_complete <- tbl %>% 
    filter(timepoint == "Before Training" & !(stim_id %in% samples_posterior_stim_id)) %>%
    rbind(tbl %>% 
            filter(timepoint == "After Training") %>% 
            group_by(across(-c(x1_true, x2_true))) %>%
            summarize(x1_true = mean(x1_true), x2_true = mean(x2_true))) %>% ungroup() %>%
    mutate(timepoint = "After Training") %>% arrange(stim_id)
  tbl <- rbind(
    tbl %>% filter(timepoint == "Before Training"),
    tbl_after_complete)
  return(tbl)
}

distance_to_closest_center_simulation <- function(tbl, sim_center, is_simulation) {
  #' add distance to closest center for sampled representations
  #' 
  #' @description maps simulation results to empirical results and computes distance to closest category center
  #' 
  #' @param tbl tibble with prior and posterior samples
  #' @param is_simulation is the function used for simulations or empirical data
  #' @return the same tbl with distance column added
  #' 
  tbl$n_categories <- max(as.numeric(as.character(tbl$category)))
  tbl$x1_response <- tbl$x1_true
  tbl$x2_response <- tbl$x2_true
  l_centers <- category_centers(f_stretch = 1, f_shift = 0)
  l_centers[[3]] <- category_centers_squares(n_cats = c(4), is_simulation)
  tbl <- add_distance_to_nearest_center(tbl, l_centers, is_simulation = TRUE, sim_center = sim_center)
  tbl$participant_id <- 1
  tbl$session <- tbl$timepoint 
  return(tbl)
}


