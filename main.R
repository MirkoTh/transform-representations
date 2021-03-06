# Load Packages and Home-Grown Modules ------------------------------------

rm(list = ls())
library(naivebayes)
library(tidyverse)
library(docstring)
library(ggExtra)
library(furrr)
library(catlearn)

files <- c(
  "R/utils.R", "R/plotting.R", 
  "experiments/2022-02-category-learning/R/analysis-utils.R",
  "experiments/2022-02-category-learning/R/analysis-plotting.R"
  )
walk(files, source)


# Simulation Parameters ---------------------------------------------------

n_stimuli <- 100L
nruns <- 5000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(2L), cat_type = c("prototype", "exemplar"), # "rule", 
  prior_sd = c(.75), sampling = c("improvement", "metropolis-hastings"),
  constrain_space = c(TRUE, FALSE), category_shape = c("ellipses"),
  is_reward = FALSE
)
l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6, is_reward = ..7
    )
  )
)
tbl_info <- tibble(do.call(rbind.data.frame, l_info)) %>%
  mutate(condition_id = seq(1:length(l_info))) %>%
  relocate(condition_id, .before = n_stimuli)

# sanity checks of parameters
# makes only sense with square categories:
# walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)


# Run Category Learning Task ----------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))
l_category_results <- future_map(
  l_info, categorize_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

td <- lubridate::today()

saveRDS(l_category_results, file = str_c("data/", td, "-grid-search-vary-constrain-space.rds"))
l_category_results <- readRDS(file = "data/2022-06-13-grid-search-vary-constrain-space.rds")
# approx. 10 min using 10'000 samples when gcm is not re-fitted every time sample is accepted
# Post Processing & Plotting ----------------------------------------------

l_results_plots <- map(l_category_results, diagnostic_plots)


tbl_bef_aft$x1_aft[is.na(tbl_bef_aft$x1_aft)] <- tbl_bef_aft$x1_bef[is.na(tbl_bef_aft$x1_aft)]
tbl_bef_aft$x2_aft[is.na(tbl_bef_aft$x2_aft)] <- tbl_bef_aft$x2_bef[is.na(tbl_bef_aft$x2_aft)]

ggplot(tbl_bef_aft, aes(x1_aft, x2_aft, group = category)) + geom_point(aes(color = category))

marrangeGrob(
  list(l_results_plots[[3]][[2]][[1]], l_results_plots[[3]][[2]][[4]], l_results_plots[[5]][[2]][[1]], l_results_plots[[5]][[2]][[4]]),
  nrow = 2, ncol = 2, layout_matrix = matrix(c(1,2,3,4), byrow = TRUE, nrow = 2, ncol = 2)
)

# Plot Prior Means & Posterior Means --------------------------------------

l_tmp <- save_results_plots(tbl_info, l_results_plots, .75, 2)
ggsave(l_tmp[[2]], l_tmp[[1]], width = 15, height = 7, units = "in")
l_tmp <- save_results_plots(tbl_info, l_results_plots, .75, 4)
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



l_tmp <- make_stimuli(l_info[[1]])
tbl_all_cats <- l_tmp[[1]]
tbl_ellipses <- l_tmp[[2]]$tbl_ellipses
ggplot() + geom_point(data = tbl_all_cats, aes(x1, x2, color = category), size = 2) + 
  geom_point(data = tbl_ellipses, aes(x_rotated, y_rotated, group = category), color = "grey", size = .5) +
  scale_color_brewer(palette = "Set1", name = "Category") +
  coord_cartesian(xlim = c(0, 12), ylim = c(0, 12)) +
  theme_bw() +
  # theme(plot.background = element_rect(fill = "black"),
  #       panel.background = element_rect(fill = "black")) +
  labs(
    x = expression(X["1"]),
    y = expression(X["2"])
  )


## evaluate results from one simulation condition
l_category_results <- readRDS(file = "data/2021-11-05-grid-search.rds")
add_shape <- function(x) {x$l_info[["category_shape"]] <- "squares"; return(x)}
l_category_results <- map(l_category_results, add_shape)

# look at simulation conditions
map(l_category_results, function(x) c(x$l_info$sampling, x$l_info$cat_type, x$l_info$prior_sd))

tbl_test <- l_category_results[[35]]$tbl_new %>% arrange(stim_id)

ggplot(tbl_test, aes(x1, x2, group = stim_id)) +
  geom_point(aes(color = stim_id)) +
  scale_color_continuous()









