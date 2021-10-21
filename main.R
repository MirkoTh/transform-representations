# Load Packages and Home-Grown Stuff --------------------------------------

rm(list = ls())
library(mclust)
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)


files <- c("R/utils.R")
walk(files, source)


# Create Categories -------------------------------------------------------
n_categories <- 16L
check_categories(n_categories)
prior_sd <- .75
nruns <- 1000
l_params <- list(
  n_categories = n_categories,
  prior_sd = prior_sd,
  nruns = nruns
)

thx <- c(0, n_categories - 1)
x1 <- seq(thx[1], thx[2], by = 1)
x2 <- seq(thx[1], thx[2], by = 1)
features <- crossing(x1, x2)
tbl <- tibble(stim_id = seq(1, nrow(features)), features)

l_results <- categorize_stimuli(tbl, l_params)


categorize_stimuli <- function(tbl, l_params) {
  list2env(l_params)
  tbl <- create_categories(tbl, sqrt(n_categories)) %>% select(-c(x1_cat, x2_cat))
  nstim <- nrow(tbl)
  
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
  
  pb <- txtProgressBar(min = 1, max = nruns, initial = 1)
  for (i in 1:nruns) {
    # randomly move random observation 
    idx <- sample(nstim, 1)
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
    if (
      (post_x_new > post_x_old) & 
      between(X_new$x1, thx[1], thx[2]) & 
      between(X_new$x2, thx[1], thx[2])
    ) {
      cat("accepted\n")
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
  
  pl_centers <- plot_moves(l_results$tbl_posterior)
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



