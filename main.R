# Load Packages and Home-Grown Modules ------------------------------------

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
nruns <- 10000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(4L, 9L), cat_type = c("rule", "prototype", "exemplar"),
  prior_sd = c(.1, .75)
)
l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(n_categories = ..1, cat_type = ..2, prior_sd = ..3)
    )
)

# sanity checks of parameters
walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)


# Run Category Learning Task ----------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))
l_category_results <- future_map(
  l_info, categorize_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

# approx. 10 min using 10'000 samples when gcm is not re-fitted every time sample is accepted
# Post Processing & Plotting ----------------------------------------------

# 1 & 2: exemplar -> 1 empty
# 5 & 6: rule -> 5 empty

l_results_plots <- map(l_category_results, diagnostic_plots)
stim_ids <- c(72, 1)
l_tbl_stimuli <- map(stim_ids, stimulus_before_after, l_results = l_category_results)
l_plt_stimulus_movements <- map(l_tbl_stimuli, plot_stimulus_movements)

l_plt_stimulus_movements[[2]]

save_plots(l_results_plots, pl_stimulus_movement)



