# Load Packages and Home-Grown Stuff --------------------------------------

rm(list = ls())
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)
library(furrr)

files <- c("R/utils.R", "R/plotting.R")
walk(files, source)


# Create Categories -------------------------------------------------------

n_stimuli <- 144L
prior_sd <- .75
nruns <- 1000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  prior_sd = prior_sd,
  nruns = nruns
)

# variable
tbl_vary <- crossing(n_categories = c(4L, 9L), cat_type = c("rule", "prototype"))
l_info <- pmap(tbl_vary, ~ append(l_info_prep, list(n_categories = .x, cat_type = .y)))


# sanity checks of parameters
walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)


# Run Categorization ------------------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))
suppressWarnings(
  suppressMessages(
    l_results <- future_map(l_info, categorize_stimuli, .progress = TRUE)
  )
)


# Post Processing & Plotting ----------------------------------------------

l_results <- map(l_results[c(1, 3)], diagnostic_plots)

