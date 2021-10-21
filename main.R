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

n_categories <- 16L
n_stimuli <- n_categories ^ 2
prior_sd <- .75
nruns <- 500

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
l_results <- future_map(l2_params, categorize_stimuli)
l_results[[2]][[2]][[1]]
l_results[[1]][[2]][[1]]
