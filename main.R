# Load Packages and Home-Grown Stuff --------------------------------------

rm(list = ls())
library(mclust)
library(naivebayes)
library(tidyverse)
library(docstring)

files <- c("R/utils.R")
walk(files, source)


# Create Categories -------------------------------------------------------

thx <- c(0, 15)
x1 <- seq(thx[1], thx[2], by = 1)
x2 <- seq(thx[1], thx[2], by = 1)
features <- crossing(x1, x2)
tbl <- tibble(stim_id = seq(1, nrow(features)), features)
tbl <- create_categories(tbl, 4) %>% select(-c(x1_cat, x2_cat))
nstim <- nrow(tbl)

# this has to be changed. the initial model is not trained on the prior means
# but on random samples from the stimuli priors, reflecting some initial contact
# with the stimuli by participants

feature_names <- c("x1", "x2")
label <- "category"
tbl$category <- as.factor(tbl$category)
categories <- levels(tbl$category)
fml <- formula(str_c(label, " ~ ", str_c(feature_names, collapse = " + ")))
m_nb_initial <- naive_bayes(fml, data = tbl)
m_nb_update <- m_nb_initial

nruns <- 1000
tbl_new <- tbl
posterior_prior <- predict(m_nb_initial, tbl[, c("x1", "x2")], "prob")
l_prior_prep <- extract_posterior(posterior_prior, m_nb_initial)
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
    tbl_new[idx, "x1"] + rnorm(1, 0, 0.75), 
    tbl_new[idx, "x2"] + rnorm(1, 0, 0.75)
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


l_results <- add_centers(tbl_new, nrow(tbl), m_nb_initial, m_nb_update, categories)
tbl_results <- l_results[[1]]
tbl_results_agg <- l_results[[2]]


tbl_posterior_long <- extract_posterior(posterior, m_nb_update)[[1]]
tbl_posteriors <- tbl_prior_long %>%
  mutate(timepoint = "Before Training") %>%
  rbind(
    tbl_posterior_long %>%
      mutate(timepoint = "After Training")
  ) %>%
  mutate(
    timepoint = fct_relevel(timepoint, "Before Training", "After Training")
  )

pl_centers <- plot_moves(rbind(tbl_results, tbl_results_agg))
pl_post <- plot_cat_probs(tbl_posteriors)
plot_arrangement(list(pl_centers, pl_post), n_cols = 1)


