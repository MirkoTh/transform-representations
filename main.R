# Load Packages and Home-Grown Stuff --------------------------------------


# visualize movement from prior mean to two different posterior means
# two arrows pointing into the respective directions in a 2d grid

tbl_stimulus <- reduce(
  map(1:2, prior_posterior_for_stim_id, l = l_results, s_id = 64),
  rbind
)
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

tbl_stimulus <- tbl_stimulus %>%
  select(c(x1_data, x2_data, timepoint, n_categories)) %>%
  pivot_wider(
    id_cols = c(n_categories), names_from = timepoint, values_from = c(x1_data, x2_data)
    )
names(tbl_stimulus) <- c("n_categories", "x1_aft", "x1_bef", "x2_aft", "x2_bef")
pl_move_different <- ggplot(tbl_stimulus) +
  geom_point(aes(x1_bef, x2_bef), size = 5, color = "white") +
  geom_point(aes(x1_bef, x2_bef, color = n_categories), size = 3, position = position_dodge(.1)) +
  geom_point(aes(x1_aft, x2_aft, color = n_categories), position = position_dodge(.1)) +
  geom_segment(aes(
    x = x1_bef - .005, y = x2_bef - .025, xend = x1_aft + .005, yend = x2_aft + .025,
    color = n_categories
    ), arrow = arrow(type = "closed")) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "Nr. Categories") +
  labs(
    x = bquote(x[1]),
    y = bquote(x[2])
  )
