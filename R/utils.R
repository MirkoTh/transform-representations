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
  
  if(l_info$category_shape == "ellipses") {
    predict_pt <- function(x) tail(predict(l_m$m_nb_update, x[, c("x1", "x2")], "prob"), 1)
    predict_gcm <- category_probs_2
  } else if(l_info$category_shape == "square") {
    predict_pt <- function(x) category_probs_pt(l_m$params_pt_tf, x, l_m$tbl_pt, 2, l_m$lo, l_m$hi)
    predict_gcm <- category_probs
  }
  
  # Categorization Simulation -----------------------------------------------
  
  pb <- txtProgressBar(min = 1, max = l_info$nruns, initial = 1, char = "*", style = 2)
  for (i in 1:l_info$nruns) {
    # perceive a randomly sampled stimulus
    # while(l_x$stim_id_cur != 1){}
    l_x <- perceive_stimulus(tbl_new, l_info)
    # irrelevant
    l_x$X_new$category <- 1
    l_x$X_new$response <- 1
    
    # manual code for modeling figure
    # l_x$stim_id_cur <- 58
    # l_x$X[nrow(l_x$X), ] <- tibble(x1=5.45, x2=6.9)# tibble(x1=4.55, x2=7.1) #
    # l_x$X_new <-tibble(x1=5.45, x2=6.9)# tibble(x1=4.55, x2=7.1) # 
    # l_x$cat_cur <- 4
    
    # propose a new posterior
    if (l_info$cat_type == "rule") {
      posterior_new <- category_probs_rb(l_m$params_rb_tf, l_x$X_new, l_m$tbl_rules, l_m$lo, l_m$hi)
      colnames(posterior_new)[1:l_info$n_categories] <- l_info$categories
    } else if (l_info$cat_type == "prototype") {
      posterior_new <- predict_pt(l_x$X_new[, c("x1", "x2")] %>% mutate(response = 1))
      colnames(posterior_new)[1:l_info$n_categories] <- l_info$categories
    } else if (l_info$cat_type == "exemplar") {
      posterior_new <- predict_gcm(l_m$params_gcm_tf, l_x$X_new, tbl_new %>% mutate(response = 1, trial_id = 1:nrow(tbl_new)), 2, 2, l_m$lo, l_m$hi)
      colnames(posterior_new)[1:l_info$n_categories] <- l_info$categories
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
      between(l_x$X_new$x1, l_info$space_edges$x1[1], l_info$space_edges$x1[2]) & 
        between(l_x$X_new$x2, l_info$space_edges$x2[1], l_info$space_edges$x2[2])
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
          stim_id = l_x$stim_id_cur, l_x$X_new[, c("x1", "x2")], 
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



compare_subsequent_stimuli <- function(l_info) {
  #' main sequential comparison function
  #' 
  #' @description compare stimuli, store accepted samples, and visualize results
  #' 
  #' @param l_info parameter list with fields n_stimuli, n_categories, prior_sd, nruns
  #' @return a list with prior, samples, and posterior in [[1]] and
  #' information about simulation condition in [[2]] and
  #' distances between sequentially presented stimuli in [[3]]
  #' 
  
  l_tmp <- make_stimuli(l_info)
  tbl <- l_tmp[[1]]
  l_info <- l_tmp[[2]]
  
  tbl_new <- tbl
  
  my_dmvnorm <- function(x1, x2, x_data, m_vcov) {
    dmvnorm(x_data, mean = c(x1, x2), sigma = m_vcov)
  }
  
  # Seq Comparison Simulation -----------------------------------------------
  
  pb <- txtProgressBar(min = 1, max = l_info$nruns, initial = 1, char = "*", style = 2)
  l_x_previous <- list()
  m_vcov <- matrix(c(l_info$prior_sd, 0, 0, l_info$prior_sd), nrow = 2)
  tbl_ds <- tibble(d_city = c(), d_euclidean = c())
  
  
  for (i in 1:l_info$nruns) {
    # perceive a randomly sampled stimulus
    # while(l_x$stim_id_cur != 1){}
    l_x <- perceive_stimulus(tbl, l_info)
    
    # every stimulus is accepted, but assigned to the stimulus id, which is most likely
    # most likely can be (a) according to prior means and sds
    # or (b) according to sampling algorithm
    is_in_shown_space <- (
      between(l_x$X_new$x1, l_info$space_edges$x1[1], l_info$space_edges$x1[2]) & 
        between(l_x$X_new$x2, l_info$space_edges$x2[1], l_info$space_edges$x2[2])
    )
    
    v_priormean <- as_vector(l_x$X_old[l_x$stim_id_cur, ])
    v_current <- as_vector(l_x$X_new)
    d_current <- dmvnorm(v_current, mean = v_priormean, sigma = m_vcov)
    onehots <- one_hot(l_info, l_x$cat_cur)
    m_onehots <- as_tibble(t(as.matrix(onehots)))
    ds_stim <- pmap_dbl(l_x$X_old[, c("x1", "x2")], my_dmvnorm, x_data = v_current, m_vcov = m_vcov)
    idx_max <- which.max(ds_stim)
    
    if (l_info$sampling == "improvement" & idx_max == l_x$stim_id_cur & ifelse(l_info$constrain_space, is_in_shown_space, TRUE)) {
      cat_current <- tbl$category[tbl$stim_id == idx_max]
      tbl_new <- rbind(tbl_new, tibble(
        stim_id = idx_max, x1 = v_current[[1]], x2 = v_current[[2]],
        cat_type = l_info$cat_type, m_onehots, category = cat_current
      ))
    } else if (l_info$sampling != "improvement" & ifelse(l_info$constrain_space, is_in_shown_space, TRUE)) {
      
      csum_ps <- cumsum(ds_stim / d_current)
      p_thx <- runif(1)
      thxs <- csum_ps / max(csum_ps)
      idx_sampled <- sum(p_thx > thxs) + 1
      cat_current <- tbl$category[tbl$stim_id == idx_sampled]
      tbl_new <- rbind(tbl_new, tibble(
        stim_id = idx_sampled, x1 = v_current[[1]], x2 = v_current[[2]],
        cat_type = l_info$cat_type, m_onehots, category = cat_current
      ))
    }
    
    if(!is_null(l_x_previous)) {
      d_city <- sum(abs(l_x_previous$X_new - l_x$X_new))
      d_euclidean <- sqrt(sum((l_x_previous$X_new - l_x$X_new)^2))
      tbl_ds <- rbind(tbl_ds, tibble(d_city, d_euclidean))
    }
    l_x_previous <- l_x
    
    setTxtProgressBar(pb,i)
  }
  close(pb)
  
  # Post Processing ---------------------------------------------------------
  
  nstart <- nrow(tbl)
  nnew <- nrow(tbl_new) - nstart
  tbl_new$timepoint <- c(rep("Before Training", nstart), rep("After Training", nnew))
  
  l_out <- list(
    tbl_new = tbl_new, l_info = l_info,
    tbl_ds = tbl_ds
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
  l_info$space_edges <- list(x1 = c(0, sqrt(l_info$n_stimuli) - 1), x2 = c(0, sqrt(l_info$n_stimuli) - 1))
  x1 <- seq(l_info$space_edges$x1[1], l_info$space_edges$x1[2], by = 1)
  x2 <- seq(l_info$space_edges$x2[1], l_info$space_edges$x2[2], by = 1)
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
  
  if (l_info$use_exptl_stimuli) {
    tbl$x1 <- 9 * (tbl$x1 + 1) + 1
    tbl$x2 <- 9 * (tbl$x2 + 1) + 1
    l_info$space_edges <- list(x1 = c(0, 100), x2 = c(0, 100))
    if (l_info$representation == "psychological-representation") {
      tbl_psych <- readRDS("data/psych-representations.rds")
      tbl <- tbl %>% 
        left_join(tbl_psych, by = c("x1" = "x1_obj", "x2" = "x2_obj")) %>%
        select(-c(x1, x2)) %>% rename(x1 = x1_psych, x2 = x2_psych) %>%
        relocate(c(x1, x2), .after = stim_id)
      l_info$space_edges <- list(x1 = c(-0.5758281, 7.1811028), x2 = c(-0.7996195, 7.9763013))
    }
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


get_fitted_params <- function(l_info, tbl_cat) {
  #' @description get model parameters and model objects
  #' either use avg fitted parameters or use optimal parameters
  
  l_rb <- list()
  l_pt <- list()
  l_gcm <- list()
  params_pt <- c(.5, .5, .5) # initialize with any values
  params_gcm <- rep(.5, 6)
  params_rb <- rep(99, 2)
  if (l_info$category_shape == "square") {
    n_cat <- 4
  } else if (l_info$category_shape == "ellipses") {
    n_cat <- 2
  }
  
  # pts as the average feature value
  tbl_pt <- tbl_cat %>% group_by(category) %>% summarize(x1_pt = mean(x1), x2_pt = mean(x2))
  
  # rules depend on feature dimensions (i.e., 0-9 or 0-100)
  if (l_info$use_exptl_stimuli) {
    tbl_rules <- tibble(
      category = 1:4,
      lo = list(c(-Inf, -Inf), c(50, -Inf), c(-Inf, 50), c(50, 50)),
      hi = list(c(50, 50), c(Inf, 50), c(50, Inf), c(Inf, Inf))
    )
    if (l_info$informed_by_data) {
      tbl_rules <- tibble(
        category = 1:4,
        lo = list(c(-Inf, -Inf), c(3.9377975, -Inf), c(-Inf, 3.6487456), c(3.9377975, 3.6487456)),
        hi = list(c(3.9377975, 3.6487456), c(Inf, 3.6487456), c(3.9377975, Inf), c(Inf, Inf))
      )
    }
    
  } else if (!l_info$use_exptl_stimuli) {
    unique_boundaries <- boundaries(tbl, l_info)
    thx_grt <- thxs(unique_boundaries)
    tbl_rules <- tibble(
      category = 1:4,
      lo = list(
        c(thx_grt$x1_lower[1], thx_grt$x2_lower[1]), c(thx_grt$x1_lower[2], thx_grt$x2_lower[2]), 
        c(thx_grt$x1_lower[3], thx_grt$x2_lower[3]), c(thx_grt$x1_lower[4], thx_grt$x2_lower[4])
      ),
      hi = list(
        c(thx_grt$x1_upper[1], thx_grt$x2_upper[1]), c(thx_grt$x1_upper[2], thx_grt$x2_upper[2]), 
        c(thx_grt$x1_upper[3], thx_grt$x2_upper[3]), c(thx_grt$x1_upper[4], thx_grt$x2_upper[4])
      )
    )
  }
  
  if (l_info$informed_by_data) {
    # load average fitted parameter values for the three models
    tbl_avg_params_obj <- readRDS("data/avg-params-all-catlearn-models-physical-properties.rds")
    tbl_avg_params_psych <- readRDS("data/avg-params-all-catlearn-models-psychological-representation.rds")
    tbl_avg_params <- rbind(tbl_avg_params_obj, tbl_avg_params_psych)
    
    if (l_info$category_shape == "square") {
      params_pt <- tbl_avg_params %>% 
        filter(model == "PT" & cat_structure ==  "square" & representation == l_info$representation) %>% 
        select(val) %>% as_vector()
      params_rb <- tbl_avg_params %>% 
        filter(model == "RB" & cat_structure == "square" & representation == l_info$representation) %>% 
        select(val) %>% as_vector()
      params_gcm <- tbl_avg_params %>%
        filter(model == "GCM" & cat_structure == "square" & representation == l_info$representation) %>% 
        select(val) %>% as_vector()
      
    } else if (l_info$category_shape == "ellipses") {
      m_nb_obj <- readRDS("data/e1-nb-pt-avg-fit.rds")
      m_nb_psych <- readRDS("data/e1-nb-pt-avg-fit-psych.rds")
      if (l_info$representation == "psychological-representation") {
        m_nb <- m_nb_psych
      } else if (l_info$representation == "object-properties") {
        m_nb <- m_nb_obj
      }
      
      l_pt$fml <- formula(str_c(l_info$label, " ~ ", str_c(l_info$feature_names, collapse = " + ")))
      l_pt$m_nb_initial <- m_nb
      l_pt$m_nb_update <- l_pt$m_nb_initial
      
      params_gcm <- tbl_avg_params %>%
        filter(model == "GCM" & cat_structure == "ellipses" & representation == l_info$representation) %>% 
        select(val) %>% as_vector()
    }
    
  } else if (!l_info$informed_by_data) {
    cat("extract parameters for RB model\n")
    params_rb <- rep(l_info$prior_sd, 2)
    names(params_rb) <- c("sd_x1", "sd_x2")
    
    if (l_info$cat_type == "prototype") {
      # fit pt model on ground truth
      if (l_info$category_shape == "square") {
        cat("fitting similarity-based prototype model\n")
        l_pt_fit <- fit_prototype_one_participant(tbl_cat %>% mutate(response = category), tbl_pt)
        params_pt <- l_pt_fit$params
      } else if (l_info$category_shape == "ellipses") {
        cat("fitting naive Bayes prototype model\n")
        l_pt$fml <- formula(str_c(l_info$label, " ~ ", str_c(l_info$feature_names, collapse = " + ")))
        l_pt$m_nb_initial <- naive_bayes(l_pt$fml, data = tbl_cat)
        l_pt$m_nb_update <- l_pt$m_nb_initial
      }
    }
    
    if (l_info$cat_type == "exemplar") {
      # fit gcm model on ground truth
      cat("fitting GCM\n")
      l_gcm_fit <- fit_gcm_one_participant(tbl_cat %>% mutate(response = category, trial_id = 1:nrow(tbl)), n_cat = n_cat)
      params_gcm <- l_gcm_fit$params
    }
  }
  
  lo_pt <- c(0, 0, 0)
  hi_pt <- c(10, 1, 1)
  params_pt_tf <- pmap(list(params_pt, lo_pt, hi_pt), upper_and_lower_bounds) %>% as_vector()
  l_pt$params_pt_tf <- params_pt_tf
  l_pt$lo <- lo_pt
  l_pt$hi <- hi_pt
  l_pt$tbl_pt <- tbl_pt
  
  params_gcm_ul <- unlist(params_gcm)
  lo_gcm <- rep(0, 6)[1:length(params_gcm_ul)]
  hi_gcm <- c(10, rep(1, 5))[1:length(params_gcm_ul)]
  params_gcm_tf <- pmap(list(params_gcm_ul, lo_gcm, hi_gcm), upper_and_lower_bounds) %>% as_vector()
  l_gcm$params_gcm_tf <- params_gcm_tf
  l_gcm$params_gcm <- params_gcm
  l_gcm$lo <- lo_gcm
  l_gcm$hi <- hi_gcm
  
  lo_rb <- c(0, 0)
  hi_rb <- c(100, 100)
  params_rb_tf <- pmap(list(params_rb, lo_rb, hi_rb), upper_and_lower_bounds) %>% as_vector()
  l_rb <- list(
    params_rb = params_rb,
    params_rb_tf = params_rb_tf,
    tbl_rules = tbl_rules,
    lo = lo_rb,
    hi = hi_rb
  )
  
  return(list(
    l_rb = l_rb, l_pt = l_pt, l_gcm = l_gcm
  ))
}

priors <- function(l_info, tbl) {
  #' use average parameters from fitted models as model parameters
  #' or parameters from models fitted on ground truth (!informed_by_data)
  #' 
  #' @param l_info parameter list
  #' @param tbl \code{tibble} containing the two features and the category as columns
  #' @return the stimuli priors
  #' 
  
  tbl$response <- tbl$category
  tbl$trial_id <- 1:nrow(tbl)
  l_params <- get_fitted_params(l_info, tbl)
  l_rb <- l_params$l_rb
  l_pt <- l_params$l_pt
  l_gcm <- l_params$l_gcm
  n_cat <- length(unique(tbl$category))
  if (n_cat == 2 & l_info$cat_type == "rule") {
    stop("rule model not available for ellipse category structure")
  }
  
  if (l_info$cat_type == "rule"){
    
    posterior_prior <- category_probs_rb(l_rb$params_rb_tf, tbl, l_rb$tbl_rules, l_rb$lo, l_rb$hi)#[, c(1:4)]
    l_out <- list(
      posterior_prior = posterior_prior,
      params_rb = l_rb$params_rb,
      params_rb_tf = l_rb$params_rb_tf,
      lo = l_rb$lo, hi = l_rb$hi,
      tbl_rules = l_rb$tbl_rules
      )
    
  } else if (l_info$cat_type == "prototype") {
    
    # use nb model on ellipse structure, but similarity-based one on square structure
    if (l_info$category_shape == "ellipses") {
      posterior_prior <- predict(l_pt$m_nb_initial, tbl[, c("x1", "x2")], "prob")
      l_out <- list(
        posterior_prior = posterior_prior, 
        fml = l_pt$fml,
        m_nb_initial = l_pt$m_nb_initial,
        m_nb_update = l_pt$m_nb_update
      )
    } else if (l_info$category_shape == "square") {
      posterior_prior <- category_probs_pt(l_pt$params_pt_tf, tbl, l_pt$tbl_pt, 2, l_pt$lo, l_pt$hi)
      l_out <- list(
        posterior_prior = posterior_prior, 
        params_pt_tf = l_pt$params_pt_tf,
        tbl_pt = l_pt$tbl_pt, 
        lo = l_pt$lo, hi = l_pt$hi
      )
    }
    
  } else if (l_info$cat_type == "exemplar"){
    
    l_info$sens <- l_gcm$params_gcm[1]
    l_info$wgh <- l_gcm$params_gcm[2]
    
    if (n_cat == 4) {
      posterior_prior <- category_probs(l_gcm$params_gcm_tf, tbl, tbl, 2, 2, l_gcm$lo, l_gcm$hi)
    } else if (n_cat == 2) {
      posterior_prior <- category_probs_2(l_gcm$params_gcm_tf, tbl, tbl, 2, 2, l_gcm$lo, l_gcm$hi)
    }
    
    l_out <- list(
      posterior_prior = posterior_prior,
      params_gcm = l_gcm$params_gcm,
      params_gcm_tf = l_gcm$params_gcm_tf,
      lo = l_gcm$lo, hi = l_gcm$hi
    )
  }
  colnames(l_out$posterior_prior)[1:n_cat] <- l_info$categories
  
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
  if (l_info$cat_type == "prototype" & l_info$category_shape == "ellipses") {
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
      between(l_x$X_new$x1, l_info$space_edges$x1[1], l_info$space_edges$x1[2]) & 
        between(l_x$X_new$x2, l_info$space_edges$x2[1], l_info$space_edges$x2[2])
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

distance_to_closest_center_simulation <- function(tbl, sim_center, is_simulation, l_info) {
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
  tbl$session <- tbl$timepoint
  l_centers <- category_centers(f_stretch = 1, f_shift = 0, l_info)
  l_centers[[3]] <- category_centers_squares(c(4), tbl)
  tbl <- add_distance_to_nearest_center(tbl, l_centers, is_simulation = TRUE, sim_center = sim_center)
  tbl$participant_id <- 1
  tbl$session <- tbl$timepoint 
  return(list(tbl = tbl, l_centers = l_centers))
}

add_distance_to_boundary <- function(tbl, l_centers, sim_center, l_info) {
  #' @description compute distance to closest category boundary
  #' @param tbl tibble with x1/x and x2/y values
  #' @param l_centers list with category boundary for ellipse structure
  #' @param sim_center ellipse or square cat structure
  #' @param l_info simulation information/parameters
  #' 
  if (sim_center == "ellipses") {
    pmap_dbl(
      tbl[, c("x1_response", "x2_response")], 
      ~ min(sqrt(
        (.x - l_centers[[2]][[1]][[2]]$x_rotated) ^ 2 +
          (.y - l_centers[[2]][[1]][[2]]$y_rotated) ^ 2
      )
      ))
  } else if (sim_center == "square") {
    if (l_info$representation == "physical-properties"){
      boundaries_grid <- tibble(
        x = rep(50, 1000),
        y = seq(0, 100, length.out = 1000)
      ) %>% rbind(
        tibble(
          x = seq(0, 100, length.out = 1000),
          y = rep(50, 1000)
        )
      )
    } else if (l_info$representation == "psychological-representation") {
      boundaries_grid <- tibble(
        x = rep(3.9377975, 1000),
        y = seq(0, 7, length.out = 1000)
      ) %>% rbind(
        tibble(
          x = seq(0, 7, length.out = 1000),
          y = rep(3.6487456, 1000)
        )
      )
    }
    pmap_dbl(
      tbl[, c("x1_response", "x2_response")], 
      ~ min(sqrt(
        (.x - boundaries_grid$x) ^ 2 +
          (.y - boundaries_grid$y) ^ 2
      )
      ))
  }
}



outer <- function(x11_true, x21_true, tbl_df) {
  #' @description maps over xs from second tbl
  pmap(tbl_df[, c("x1_true", "x2_true")], inner, x11_true, x21_true)
}

inner <- function(x1_true, x2_true, x11_true, x21_true) {
  #' @description computes distance for a pair of xs
  sqrt((x11_true - x1_true)^2 + (x21_true - x2_true)^2)
}

sum_of_distances <- function(tbl_1, tbl_2, cat1, cat2) {
  #' @description maps over xs from first tbl
  tmp <- pmap(
    tbl_1 %>% filter(category %in% cat1) %>% select(x1_true, x2_true) %>%
      rename(x11_true = x1_true, x21_true = x2_true),
    outer,
    tbl_df = tbl_2 %>% filter(category %in% cat2)
  ) %>% unlist()
  return(c(n = length(tmp), s = sum(tmp)))
}

wrap_sum_of_distances <- function(tp, cat1, cat2, tbl_df) {
  #' @description convenience wrapper of sum_of_distances
  tbl_select <- tbl_df  %>% filter(timepoint == tp)
  sum_of_distances(tbl_select, tbl_select,  cat1, cat2)
}


sum_of_pairwise_distances <- function(tbl_posterior, sim_center) {
  #' compute sum of pairwise distances
  #' 
  #' @description sum of pairwise distances for within-category and between-category pairs
  #' before and after training (aka category learning)
  #' 
  #' @param tbl_posterior tibble with prior and posterior samples
  #' @return a tbl with the sum of distances and the absolute change and its proportion
  #' 
  # design tbl to compute distances before and after
  if (sim_center == "square") {
    tbl_comparisons <- tibble(
      tp = factor(
        rep(c("Before Training", "After Training"), 8), 
        levels = c("Before Training", "After Training"), ordered = TRUE
      ),
      cat1 = rep(1:4, each = 4),
      cat2 = c(
        1, 1, list(c(2, 3, 4)), list(c(2, 3, 4)),
        2, 2, list(c(1, 3, 4)), list(c(1, 3, 4)),
        3, 3, list(c(1, 2, 4)), list(c(1, 2, 4)),
        4, 4, list(c(1, 2, 3)), list(c(1, 2, 3))
      ),
      comparison = rep(rep(c("Within", "Between"), each = 2), 4)
    )
  } else if (sim_center == "ellipses") {
    tbl_comparisons <- tibble(
      tp = factor(
        rep(c("Before Training", "After Training"), 4), 
        levels = c("Before Training", "After Training"), ordered = TRUE
      ),
      cat1 = rep(1:2, each = 4),
      cat2 = c(1, 1, 2, 2, 2, 2, 1, 1),
      comparison = rep(rep(c("Within", "Between"), each = 2), 2)
    )
  }
  
  # compute actual distances
  tmp <- pmap(
    tbl_comparisons %>% select(-comparison), 
    wrap_sum_of_distances, 
    tbl_df = tbl_posterior %>% mutate(timepoint = as.character(timepoint))
  ) %>% reduce(rbind) %>% as.data.frame()
  
  tbl_comparisons$n_comparisons <- tmp$n
  tbl_comparisons$distances_sum <- tmp$s
  # compute 
  tbl_comparisons %>% 
    group_by(comparison, tp, n_comparisons) %>% 
    summarize(ds_sum = sum(distances_sum)) %>%
    group_by(comparison) %>%
    mutate(
      ds_abs = ds_sum - lag(ds_sum, 1),
      ds_prop = ds_sum / lag(ds_sum, 1)
    ) %>% ungroup()
}


upper_and_lower_bounds <- function(par, lo, hi) {
  log(((par - lo) / (hi - lo)) / (1 - (par - lo) / (hi - lo)))
}

upper_and_lower_bounds_revert <- function(par, lo, hi) {
  lo + ((hi - lo) / (1 + exp(-par)))
}

upper_and_lower_constrain_bias <- function(bias) {
  bias[4] <- .9999999 - (bias[1] + bias[2] + bias[3])
  bias_out <- c()
  bias_out[1] <- upper_and_lower_bounds(bias[1], 0, 1)
  bias_out[2] <- upper_and_lower_bounds(bias[2], 0, 1 - bias[1])
  bias_out[3] <- upper_and_lower_bounds(bias[3], 0, 1 - (bias[1] + bias[2]))
  bias_out[4] <- upper_and_lower_bounds(bias[4], 0, 1 - (bias[1] + bias[2] + bias[3]))
  return(bias_out)
}


upper_and_lower_unconstrain_bias <- function(bias_constrained) {
  bias_remap <- c()
  bias_remap[1] <- upper_and_lower_bounds_revert(bias_constrained[1], 0, 1)
  bias_remap[2] <- upper_and_lower_bounds_revert(bias_constrained[2], 0, (1 - bias_remap[1]))
  bias_remap[3] <- upper_and_lower_bounds_revert(bias_constrained[3], 0, (1 - (bias_remap[1] + bias_remap[2])))
  bias_remap[4] <- upper_and_lower_bounds_revert(bias_constrained[4], 0, (1 - (bias_remap[1] + bias_remap[2] + bias_remap[3])))
  return(bias_remap)
}


gcm_likelihood_no_forgetting <- function(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi, n_cat = 4) {
  #' @description -2 * negative log likelihood of transfer set given training data
  #' and a gcm without a forgetting parameter (i.e., forgetting set to 0)
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_x training data
  #' @param n_feat number of features
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  if (n_cat == 4) {
    tbl_probs <- category_probs(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi)
  } else if (n_cat == 2) {
    tbl_probs <- category_probs_2(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi)
  }
  ll <- log(tbl_probs$prob_correct)
  neg2llsum <- -2 * sum(ll)
  return(neg2llsum)
}

category_probs <- function(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi) {
  #' @description calculate category probabilities for every stimulus in the transfer set
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_x training data
  #' @param n_feat number of features
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  params_untf <- pmap(list(x[1:2], lo[1:2], hi[1:2]), upper_and_lower_bounds_revert)
  names(params_untf) <- c("c", "w")
  bias_untf <- upper_and_lower_unconstrain_bias(x[3:6])
  params_untf$bias <- bias_untf
  params_untf$w[2] <- 1 - params_untf$w[1]
  
  l_transfer_x <- split(tbl_transfer[, c("x1", "x2")], 1:nrow(tbl_transfer))
  l_category_probs <- map(
    l_transfer_x, gcm_base, 
    tbl_x = tbl_x, 
    n_feat = n_feat, 
    c = params_untf[["c"]], 
    w = params_untf[["w"]], 
    bias = params_untf[["bias"]], 
    delta = ifelse(is.null(params_untf[["delta"]]), 0, params_untf[["delta"]]),
    d_measure = d_measure
  )
  tbl_probs <- as.data.frame(matrix(reduce(l_category_probs, rbind), ncol = length(params_untf$bias))) %>% 
    mutate(response = tbl_transfer$response)
  tbl_probs$prob_correct <- pmap_dbl(
    tbl_probs, ~ c(..1, ..2, ..3, ..4)[as.numeric(as.character(..5))]
  )
  return(tbl_probs)
}

category_probs_2 <- function(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi) {
  #' @description calculate category probabilities for every stimulus in the transfer set
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_x training data
  #' @param n_feat number of features
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  params_untf <- pmap(list(x, lo, hi), upper_and_lower_bounds_revert)
  names(params_untf) <- c("c", "w", "bias")
  params_untf$bias[2] <- 1 - params_untf$bias
  params_untf$w[2] <- 1 - params_untf$w[1]
  
  l_transfer_x <- split(tbl_transfer[, c("x1", "x2")], 1:nrow(tbl_transfer))
  l_category_probs <- map(
    l_transfer_x, gcm_base, 
    tbl_x = tbl_x, 
    n_feat = n_feat, 
    c = params_untf[["c"]], 
    w = params_untf[["w"]], 
    bias = params_untf[["bias"]], 
    delta = ifelse(is.null(params_untf[["delta"]]), 0, params_untf[["delta"]]),
    d_measure = d_measure
  )
  tbl_probs <- as.data.frame(matrix(reduce(l_category_probs, rbind), ncol = length(params_untf$bias))) %>% 
    mutate(response = tbl_transfer$response)
  tbl_probs$prob_correct <- pmap_dbl(
    tbl_probs, ~ c(..1, ..2)[as.numeric(as.character(..3))]
  )
  return(tbl_probs)
}

gcm_base <- function(x_new, tbl_x, n_feat, c, w, bias, delta, d_measure = 1) {
  #' compute class probabilities with the GCM model
  #' 
  #' @description summed similarity computation with gcm;
  #' using sensitivity, attentional weighting, and response bias;
  #' @param x_new the x coordinates of the new item
  #' @param tbl_x the tbl with all memory exemplars,
  #' including a column "category" denoting the category of that item
  #' @param n_feat number of feature dimensions
  #' @param c sensitivity
  #' @param w attentional weighting
  #' @param bias response bias / category prior
  #' @param delta forgetting rate (if delta == 0, no forgetting)
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @return a vector with the class probabilities for the new item
  l_x_cat <- split(tbl_x, tbl_x$category)
  sims_cat <- map(l_x_cat, f_similarity_cat, w, c, delta, x_new, d_measure)
  sims_cat_sum <- map_dbl(sims_cat, sum)
  sims_cat_sum_biased <- sims_cat_sum * bias
  map_dbl(sims_cat_sum_biased, ~ .x/sum(sims_cat_sum_biased))
}


pt_likelihood <- function(x, tbl_transfer, tbl_pt, d_measure, lo, hi) {
  #' @description -2 * negative log likelihood of transfer set given training data
  #' and a gcm without a forgetting parameter (i.e., forgetting set to 0)
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_pt prototypes
  #' @param n_feat number of features
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  tbl_probs <- category_probs_pt(x, tbl_transfer, tbl_pt, d_measure, lo, hi)
  ll <- log(tbl_probs$prob_correct)
  neg2llsum <- -2 * sum(ll)
  return(neg2llsum)
}

f_similarity_pt <- function(x_pt, x_new, w, c, d_measure) {
  #' @description calculate similarity from single stimulus to prototype
  #' using d_measure (1=city-block) (2=euclidean) distance metric
  distance <- sum(w * (abs(x_pt - x_new)^d_measure))^(1/d_measure)
  sim <- exp(-c * distance)
  return(sim)
}

prototype_base <- function(x_new, tbl_pt, c, w, g, d_measure = 1) {
  #' compute class probabilities with the multiplicative prototype model
  #' (Nosofsky et al., 1987, 1992, 2002) 
  #' 
  #' @description summed similarity computation;
  #' using sensitivity, attentional weighting, and guessing parameter;
  #' @param x_new the x coordinates of the new item
  #' @param tbl_pt the tbl with the category prototypes
  #' including a column "category" denoting the true category of that item
  #' @param c sensitivity
  #' @param w attentional weighting
  #' @param g guessing parameter
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @return a vector with the class probabilities for the new item
  #' 
  cols <- colnames(tbl_pt)[str_starts(colnames(tbl_pt), "x[0-9]+_")]
  n_cat <- nrow(tbl_pt)
  sims_pt <- apply(tbl_pt[, cols], 1, FUN = f_similarity_pt, x_new, w, c, d_measure)
  g / n_cat + (1 - g) * map_dbl(sims_pt, ~ .x/sum(sims_pt))
}

category_probs_pt <- function(x, tbl_transfer, tbl_pt, d_measure, lo, hi) {
  #' @description calculate category probabilities for every stimulus
  #' in the transfer set for the multiplicative prototype model
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_pt feature values of prototype
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  params_untf <- pmap(list(x, lo, hi), upper_and_lower_bounds_revert)
  names(params_untf) <- c("c", "w", "g")
  params_untf$w[2] <- 1 - params_untf$w[1] 
  l_transfer_x <- split(tbl_transfer[, c("x1", "x2")], 1:nrow(tbl_transfer))
  l_category_probs <- map(
    l_transfer_x, prototype_base, 
    tbl_pt = tbl_pt, 
    c = params_untf[["c"]], 
    w = params_untf[["w"]], 
    g = params_untf[["g"]], 
    d_measure = d_measure
  )
  tbl_probs <- as.data.frame(matrix(reduce(l_category_probs, rbind), ncol = nrow(tbl_pt))) %>% 
    mutate(response = tbl_transfer$response)
  tbl_probs$prob_correct <- pmap_dbl(
    tbl_probs, ~ c(..1, ..2, ..3, ..4)[as.numeric(as.character(..5))]
  )
  return(tbl_probs)
  
}

f_similarity <- function(x1, x2, w, c, x_new, d_measure) {
  #' @description helper function calculating similarity for one item
  #' given a currently presented item to be classified
  d <- (
    w[1]*abs((x_new$x1 - x1))^d_measure + w[2]*abs((x_new$x2 - x2))^d_measure
  )^(1/d_measure)
  exp(-d*c)
}


f_similarity_cat <- function(x, w, c, delta, x_new, d_measure) {
  #' @description helper function calculating similarities for all items
  #' within a given category
  x$lag <- abs(x$trial_id - max(x$trial_id))
  x$prop_decay <- exp(- (delta * x$lag))
  sims <- pmap_dbl(x[, c("x1", "x2")], f_similarity, w, c, x_new, d_measure)
  return(sims*x$prop_decay)
}



fit_gcm_one_participant <- function(tbl_1p, n_cat = 4) {
  #' fit plain-vanilla GCM to data from one participant
  #' 
  #' @description classical GCM implementation; for current purposes,
  #' uses same data set as train set and test set (i.e., no generalization)
  #' @param tbl_1p a tibble with by-trial data from one participant
  #' @return list with elements -2*ll, fitted params, and if converged
  
  if (n_cat == 4) {
    params <- c(1, .5)
    lo_bd <- c(0, .0001)
    hi_bd <- c(10, .9999)
    params <- pmap_dbl(list(params, lo_bd, hi_bd), upper_and_lower_bounds)
    bias <- rep(.2499999, 3)
    bias_constrained <- upper_and_lower_constrain_bias(bias)
    params <- c(params, bias_constrained)
    params_init_tf <- x <- params
  } else if (n_cat == 2) {
    params <- c(1, .5, .5)
    lo_bd <- c(0, 0, .0001)
    hi_bd <- c(10, 1, .9999)
    params_init_tf <- pmap_dbl(list(params, lo_bd, hi_bd), upper_and_lower_bounds)
  }
  
  
  n_feat <- 2
  d_measure <- 2
  
  results <- optim(
    params_init_tf,
    gcm_likelihood_no_forgetting,
    tbl_transfer = tbl_1p,
    tbl_x = tbl_1p, 
    n_feat = n_feat,
    d_measure = d_measure,
    lo = lo_bd,
    hi = hi_bd,
    n_cat = n_cat,
    control = list(maxit = 1000)
  )
  
  if (n_cat == 4) {
    ps <- unconstrain_all_params(results, lo_bd, hi_bd)
  } else if (n_cat == 2) {
    ps <- pmap_dbl(list(results$par, lo_bd, hi_bd), upper_and_lower_bounds_revert)
  }
  
  l_out <- list(
    params = ps,
    neg2ll = results$value,
    is_converged = results$convergence == 0
  )
  
  return(l_out)
  
}


fit_prototype_one_participant <- function(tbl_1p, tbl_pt) {
  #' fit multiplicative prototype model to data from one participant
  #' 
  #' @description implementation by Nosofsky et al. (1987, 1992, 2002);
  #' for current purposes, uses same data set as train set and test set
  #' (i.e., no generalization)
  #' @param tbl_1p a tibble with by-trial data from one participant
  #' @return list with elements -2*ll, fitted params, and if converged
  
  params <- c(1, .5, .1)
  lo_bd <- c(0, 0, 0)
  hi_bd <- c(10, 1, 1)
  params_init_tf <- x <- pmap_dbl(list(params, lo_bd, hi_bd), upper_and_lower_bounds)
  
  d_measure <- 2
  t_start <- Sys.time()
  results <- optim(
    params_init_tf,
    pt_likelihood,
    tbl_transfer = tbl_1p,
    tbl_pt = tbl_pt, 
    d_measure = d_measure,
    lo = lo_bd,
    hi = hi_bd,
    control = list(maxit = 1000)
  )
  t_end <- Sys.time()
  t_end - t_start
  
  l_out <- list(
    params = upper_and_lower_bounds_revert(results$par, lo_bd, hi_bd),
    neg2ll = results$value,
    is_converged = results$convergence == 0
  )
  
  return(l_out)
  
}

unconstrain_all_params <- function(r, lo, hi) {
  #' @description bring all parameters back into unconstrained space
  #' ordering is: c, w, bias
  c_and_w_unconstrained <- pmap_dbl(list(r$par[1:2], lo, hi), upper_and_lower_bounds_revert)
  bias_unconstrained <- upper_and_lower_unconstrain_bias(r$par[3:6])
  l_out <- list(
    c = c_and_w_unconstrained[1],
    w = c_and_w_unconstrained[2],
    bias = bias_unconstrained
  )
  return(l_out)
}

post_process_gcm_fits <- function(l_results) {
  n_converged <- map_df(
    map(l_results, "result"), 
    ~ list(c = sum(.x$is_converged), not_c = length(.x$is_converged) - sum(.x$is_converged))
  ) %>% colSums()
  cat("converged ", n_converged[1])
  cat("\nnot converged ", n_converged[2])
  cat("\n")
  
  l_params <- map(map(l_results, "result"), "params")
  df_bias <- reduce(map(l_params, "bias"), rbind)
  colnames(df_bias) <- c("bias1", "bias2", "bias3", "bias4")
  df_c_w <- reduce(map(l_params, ~ c(.x$c, .x$w)), rbind)
  colnames(df_c_w) <- c("c", "w")
  tbl_params <- as_tibble(cbind(df_c_w, df_bias))
  participant_ids <- names(l_params)
  tbl_params$participant_id <- participant_ids
  
  return(tbl_params)
}


rule_base <- function(x_new, sds, tbl_rules) {
  #' compute class probabilities with the rule-based model
  #' 
  #' @description integrates a bivariate normal with means set to the
  #' feature values between lower and upper bounds for every rule
  #' parameters of the model are sd_x1 and sd_x2
  #' @param x_new the x coordinates of the new item
  #' @param sds standard deviation of the representational distribution
  #' @param tbl_rules tibble with decision bounds
  #' @return a vector with the class probabilities for the new item
  
  n_dim <- length(sds)
  m_cov <- diag(n_dim) * sds^2
  
  f_pred <- function(lo, hi) { 
    pmvnorm(
      lower = lo, upper = hi, 
      mean = x_new, sigma = m_cov
    )[1]
  }
  
  cat_probs <- pmap_dbl(tbl_rules[, c("lo", "hi")], f_pred)
  
  return(cat_probs)
}

category_probs_rb <- function(x, tbl_transfer, tbl_rules, lo, hi) {
  #' @description calculate category probabilities for every stimulus
  #' in the transfer set for the multiplicative prototype model
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_rules decision bounds of categories
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  params_untf <- pmap(list(x, lo, hi), upper_and_lower_bounds_revert)
  names(params_untf) <- c("sd_x1", "sd_x2")
  l_transfer_x <- map(
    split(tbl_transfer[, c("x1", "x2")], 1:nrow(tbl_transfer)),
    as_vector
  )
  l_category_probs <- map(
    l_transfer_x, rule_base, 
    sds = c(params_untf[["sd_x1"]], params_untf[["sd_x2"]]),
    tbl_rules = tbl_rules
  )
  tbl_probs <- as.data.frame(matrix(reduce(l_category_probs, rbind), ncol = nrow(tbl_rules))) %>%
    mutate(response = tbl_transfer$response)
  tbl_probs$prob_correct <- pmap_dbl(
    tbl_probs, ~ c(..1, ..2, ..3, ..4)[as.numeric(as.character(..5))]
  )
  return(tbl_probs)
  
}

rb_likelihood <- function(x, tbl_transfer, tbl_rules, lo, hi) {
  #' @description -2 * negative log likelihood of transfer set given training data
  #' and a rule-based category learning model
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_rules decision bounds for the different categories
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  tbl_probs <- category_probs_rb(x, tbl_transfer, tbl_rules, lo, hi)
  ll <- log(pmax(tbl_probs$prob_correct, 1e-10))
  neg2llsum <- -2 * sum(ll)
  return(neg2llsum)
}


fit_rb_one_participant <- function(tbl_1p, tbl_rules) {
  #' fit rule-based category learning model to data from one participant
  #' 
  #' @description implementation as in Donkin et al. (2015) Identifying Strat. Use
  #' @param tbl_1p a tibble with by-trial data from one participant
  #' @param tbl_rules upper and lower decision bounds for every category and dimension
  #' @return list with elements -2*ll, fitted params, and if converged
  
  params <- c(5, 5)
  lo_bd <- c(0, 0)
  hi_bd <- c(100, 100)
  params_init_tf <- x <- pmap_dbl(list(params, lo_bd, hi_bd), upper_and_lower_bounds)
  
  results <- optim(
    params_init_tf,
    rb_likelihood,
    tbl_transfer = tbl_1p,
    tbl_rules = tbl_rules, 
    lo = lo_bd,
    hi = hi_bd,
    control = list(maxit = 1000)
  )
  
  l_out <- list(
    params = upper_and_lower_bounds_revert(results$par, lo_bd, hi_bd),
    neg2ll = results$value,
    is_converged = results$convergence == 0
  )
  
  return(l_out)
  
}


extract_from_results <- function(l, what, cnames) {
  #' @description extract n2ll or parameter values from model fits
  
  tmp <- reduce(map(map(l, "result"), what), rbind) %>%
    as.data.frame() %>%
    as_tibble()
  colnames(tmp) <- cnames
  return(tmp)
}


aic_and_bic <- function(tbl_ll_gcm, tbl_ll_pt, tbl_ll_rb, ntrials) {
  #' @description compute aic and bic, and winning models 
  #' according to aic and bic
  
  tbl_ll <- cbind(tbl_ll_gcm, tbl_ll_pt, tbl_ll_rb)
  colnames(tbl_ll) <- c("gcm", "pt", "rb")
  tbl_ll$id <- 1:nrow(tbl_ll)
  
  tbl_ll <- tbl_ll %>%
    mutate(
      bic_gcm = gcm + 5*log(ntrials),
      bic_pt = pt + 3*log(ntrials),
      bic_rb = rb + 2*log(ntrials),
      aic_gcm = gcm + 10,
      aic_pt = pt + 6,
      aic_rb = rb + 4
    )
  tbl_ll$winner_bic <- c("GCM", "PT", "RB")[
    pmap_dbl(
      tbl_ll[, c("bic_gcm", "bic_pt", "bic_rb")], ~ which.min(c(..1, ..2, ..3))
    ) %>% as.factor()
  ]
  tbl_ll$winner_aic <- c("GCM", "PT", "RB")[
    pmap_dbl(
      tbl_ll[, c("aic_gcm", "aic_pt", "aic_rb")], ~ which.min(c(..1, ..2, ..3))
    )
  ]
  return(tbl_ll)
}

plot_grouped_and_ranked_models <- function(tbl_ll, v, win, ttl, lg_pos = "bottom") {
  #' @description plot heatmap of aic or bic
  #' grouped by winning models and rank-ordered 
  #' according to aic/bic within each group
  
  tbl_ll_plot <- tbl_ll %>% 
    pivot_longer({{v}}) %>%
    group_by({{win}}) %>%
    arrange({{win}}, value)
  
  lbls <- toupper(str_match(unique(tbl_ll_plot$name), "_([a-z]*)$")[, 2])
  tbl_ll_plot$name <- factor(
    tbl_ll_plot$name, 
    labels = lbls#c("GCM", "PT", "RB")
  )
  
  tbl_lookup <- tibble(
    id = unique(tbl_ll_plot$id),
    id_sorted = 1:nrow(tbl_ll)
  )
  
  tbl_ll_plot %>% left_join(tbl_lookup, by = c("id")) %>%
    arrange({{win}}) %>%
    ggplot(aes(name, id_sorted)) +
    geom_tile(aes(fill = {{win}}, alpha = value)) +
    guides(alpha = "none") +
    theme_bw() +
    scale_x_discrete(expand = c(0, 0)) +
    scale_y_continuous(expand = c(0, 0)) +
    labs(x = "Model", y = "Participant ID", title = ttl) + 
    theme(
      strip.background = element_rect(fill = "white"),
      text = element_text(size = 22),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 15),
      legend.position = lg_pos,
      title = element_blank()
    ) + 
    scale_fill_manual(values = c("skyblue2", "tomato4", "forestgreen"), name = ttl)
  
}


read_out_params_3e <- function(tbl_e2, tbl_e3, tbl_e4) {
  #' @description read out tibbles with fitted parameters for all three experiments
  #' 
  tbl_e2 %>%
    mutate(E = 2, id = 1:nrow(tbl_e2)) %>%
    rbind(
      tbl_e3 %>%
        mutate(E = 3, id = 1:nrow(tbl_e3))
    ) %>%
    rbind(
      tbl_e4 %>%
        mutate(E = 4, id = 1:nrow(tbl_e4))
    ) 
}


horizontal_and_vertical_moves <- function(tbl_secondary) {
  #' @description add variables coding for horizontal and vertical category confusions
  #' nb. ignores cross category confusions, because both feature values are mis-attributed
  #' 
  tbl_secondary$is_move_horizontal <- 0
  tbl_secondary$is_move_vertical <- 0
  
  tbl_secondary$is_move_horizontal[tbl_secondary$category == 1 & tbl_secondary$response == 3] <- 1
  tbl_secondary$is_move_horizontal[tbl_secondary$category == 3 & tbl_secondary$response == 1] <- 1
  tbl_secondary$is_move_horizontal[tbl_secondary$category == 2 & tbl_secondary$response == 4] <- 1
  tbl_secondary$is_move_horizontal[tbl_secondary$category == 4 & tbl_secondary$response == 2] <- 1
  
  tbl_secondary$is_move_vertical[tbl_secondary$category == 1 & tbl_secondary$response == 2] <- 1
  tbl_secondary$is_move_vertical[tbl_secondary$category == 2 & tbl_secondary$response == 1] <- 1
  tbl_secondary$is_move_vertical[tbl_secondary$category == 3 & tbl_secondary$response == 4] <- 1
  tbl_secondary$is_move_vertical[tbl_secondary$category == 4 & tbl_secondary$response == 3] <- 1
  
  return(tbl_secondary)
}


compare_models <- function(l_results_gcm, l_results_pt, l_results_rb, ntrials, expt_nr, lg_pos = "bottom") {
  #' @description compare gcm, pt, and rb using aic and bic
  #' @return list with comparison metrics, plot of metrics, and
  #' list with parameters for each model
  #' 
  filter_cat <- map_lgl(l_results_gcm, ~ !(is.null(.x$result)))
  l_results_gcm <- l_results_gcm[filter_cat]
  
  tbl_params_gcm <- post_process_gcm_fits(l_results_gcm) %>% as_tibble()
  tbl_params_rb <- extract_from_results(l_results_rb, "params", c("sd_x1", "sd_x2"))
  tbl_params_pt <- extract_from_results(l_results_pt, "params", c("c", "w", "g"))
  
  
  tbl_ll_gcm <- extract_from_results(l_results_gcm, "neg2ll", "neg2ll")
  tbl_ll_pt <- extract_from_results(l_results_pt, "neg2ll", "neg2ll")
  tbl_ll_rb <- extract_from_results(l_results_rb, "neg2ll", "neg2ll")
  
  tbl_ll <- aic_and_bic(tbl_ll_gcm, tbl_ll_pt, tbl_ll_rb, ntrials)
  
  pl_hm_bic <- plot_grouped_and_ranked_models(
    tbl_ll, c(bic_gcm, bic_pt, bic_rb), winner_bic, str_c("Winner BIC"), lg_pos
  )
  pl_hm_aic <- plot_grouped_and_ranked_models(
    tbl_ll, c(aic_gcm, aic_pt, aic_rb), winner_aic, str_c("Winner AIC"), lg_pos
  )
  
  pl_comp <- arrangeGrob(pl_hm_bic, pl_hm_aic, nrow = 1)
  
  return(list(
    tbl_ll = tbl_ll, 
    pl_comp = pl_comp, 
    l_tbl_params = list(
      tbl_params_gcm = tbl_params_gcm,
      tbl_params_pt = tbl_params_pt,
      tbl_params_rb = tbl_params_rb
    )
  ))
}
