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
nruns <- 20000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(4L, 9L), cat_type = c("rule", "prototype", "exemplar"),
  prior_sd = c(.1, .75), sampling = c("improvement", "metropolis-hastings"),
  constrain_space = c(TRUE, FALSE)
)
l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5
    )
  )
)
tbl_info <- tibble(do.call(rbind.data.frame, l_info)) %>%
  mutate(condition_id = seq(1:length(l_info))) %>%
  relocate(condition_id, .before = n_stimuli)

# sanity checks of parameters
walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)


# Run Category Learning Task ----------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))
l_category_results <- future_map(
  l_info, categorize_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)


saveRDS(l_category_results, file = "data/xxxx-xx-xx-grid-search.rds")
l_category_results <- readRDS(file = "data/2021-11-05-grid-search.rds")
# approx. 10 min using 10'000 samples when gcm is not re-fitted every time sample is accepted
# Post Processing & Plotting ----------------------------------------------

l_results_plots <- map(l_category_results, diagnostic_plots)


# Plot Prior Means & Posterior Means --------------------------------------

l_tmp <- save_results_plots(tbl_info, l_results_plots, .1, 4)
ggsave(l_tmp[[2]], l_tmp[[1]], width = 15, height = 7, units = "in")


# Plot Movements of Individual Points -------------------------------------

stim_ids <- c(72, 1)
l_tbl_stimuli <- map(stim_ids, stimulus_before_after, l_results = l_category_results)
split_my_vars <- function(tbl){
  tbl %>% split(list(.$sampling, .$space))
}
l_tbl_stimuli_split <- map(l_tbl_stimuli, split_my_vars)
l_plt_stimulus_movements <- map(l_tbl_stimuli_split, plot_stimulus_movements)

l_plt_stimulus_movements[[1]][[1]]
