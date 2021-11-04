# Load Packages and Home-Grown Stuff --------------------------------------

rm(list = ls())
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)
library(furrr)
library(catlearn)

files <- c("R/utils.R", "R/plotting.R")
walk(files, source)


# Simulation Parameters ---------------------------------------------------

n_stimuli <- 144L
prior_sd <- .75
nruns <- 100

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  prior_sd = prior_sd,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(4L, 9L), cat_type = c("rule", "prototype", "exemplar")
)
l_info <- pmap(
  tbl_vary, ~ append(l_info_prep, list(n_categories = .x, cat_type = .y))
)


# sanity checks of parameters
walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)


# Run Category Learning Task ----------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))
suppressWarnings(
  suppressMessages(
    l_category_results <- future_map(l_info, categorize_stimuli, .progress = TRUE)
  )
)

# Post Processing & Plotting ----------------------------------------------

l_results_plots <- map(l_category_results, diagnostic_plots)
tbl_stimulus <- stimulus_before_after(l_category_results, 21)
pl_stimulus_movement <- plot_stimulus_movements(tbl_stimulus)

save_plots(l_results_plots, pl_stimulus_movement)

# GCM: Playing Around -----------------------------------------------------

l <- make_stimuli(l_info[[1]])
tbl_stim <- l[[1]]
l_info_selected <- l[[2]]
tbl_stim <- tbl_stim %>% mutate(observed = 1) %>%
  pivot_wider(
    names_from = category, values_from = observed,
    names_sort = TRUE,
    names_prefix = "cat"
  ) %>%
  mutate(category = tbl_stim$category)
tbl_stim[is.na(tbl_stim)] <- 0



optim(
  f = ll_gcm, 
  c(3, .5),
  lower = c(0, 0),
  upper = c(10, 1),
  l_info = l_info_selected, 
  tbl_stim = tbl_stim,
  method = "L-BFGS-B"
)
m_preds <- predict_gcm(tbl_stim, tbl_stim, l_info_selected, 10, .5315122)
m_preds[cbind(seq(1, nrow(m_preds), by = 1), tbl_stim$category)]

l_test <- perceive_stimulus(tbl_stim, l_info_selected)
m_preds <- predict_gcm(tbl_stim, l_test$X_new, l_info_selected, 10, .5315122)
m_preds[cbind(seq(1, nrow(m_preds), by = 1), c(tbl_stim$category, l_test$cat_cur))]







