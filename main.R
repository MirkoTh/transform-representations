# Load Packages and Home-Grown Stuff --------------------------------------

rm(list = ls())
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)
library(furrr)

files <- c("R/utils.R")
walk(files, source)


# Create Categories -------------------------------------------------------

n_categories <- 9L
n_stimuli <- 144L
prior_sd <- .75
nruns <- 10000

l_params <- list(
  n_stimuli = n_stimuli,
  n_categories = n_categories,
  prior_sd = prior_sd,
  nruns = nruns
)
l2_params <- list(
  l_params,
  l_params
)
l2_params[[2]]$n_categories <- 4L

# sanity check of parameters
walk(map(l2_params, .f = function(x) x$n_categories), check_categories)

# Run Categorization ------------------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l2_params)))
suppressWarnings(
  suppressMessages(
    l_results <- future_map(l2_params, categorize_stimuli, .progress = TRUE)
  )
)
# visualize movement from prior mean to two different posterior means
# two arrows pointing into the respective directions in a 2d grid


# Visualize Stimulus Movement For Different Nr. Categories ----------------

tbl_stimulus <- stimulus_before_after(l_results, 48)
pl_stimulus_movement <- plot_stimulus_movements(tbl_stimulus)


# Save Required Plots -----------------------------------------------------

save_plots(l_results, pl_stimulus_movement)
