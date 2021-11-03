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
nruns <- 100

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

if (l_info$cat_type == "prototype") {
  # post processing
  l_results <- map(l_results, postprocess_prototype)
  
  # visualize movement from prior mean to two different posterior means
  # two arrows pointing into the respective directions in a 2d grid
  tbl_stimulus <- stimulus_before_after(l_results, 48)
  
  # Visualize Stimulus Movement For Different Nr. Categories
  pl_stimulus_movement <- plot_stimulus_movements(tbl_stimulus)
  
  # Save Required Plots
  save_plots(l_results, pl_stimulus_movement)
}

if (l_info$cat_type == "rule") {
  ## tb filled with post-processing for rule-based categorization
}

if (l_info$cat_type == "exemplar") {
  ## tb filled with post-processing for exemplar-based categorization
}
