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
nruns <- 5000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns,
  prop_train = .5,
  is_reward = TRUE
)

# variable
tbl_vary <- crossing(
  n_categories = c(2L), cat_type = c("prototype", "exemplar"), # "rule", 
  prior_sd = c(.75), sampling = c("improvement", "metropolis-hastings"),
  constrain_space = c(FALSE), category_shape = c("ellipses")
)

l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6
    )
  )
)
tbl_info <- tibble(do.call(rbind.data.frame, l_info)) %>%
  mutate(condition_id = seq(1:length(l_info))) %>%
  relocate(condition_id, .before = n_stimuli)

# how much should sd of prior be widened by a large reward?
l_info <- map(l_info, function(x) {x$nudge_prior = list("1" = 1, "5" = 1.025); return(x)})

# sanity checks of parameters
# makes only sense with square categories:
# walk(map(l_info, .f = function(x) x$n_categories), check_categories)
walk(map(l_info, .f = function(x) x$cat_type), check_cat_types)

# Run Category Learning Task ----------------------------------------------

plan(multisession, workers = min(future::availableCores() - 2, length(l_info)))
l_category_results <- future_map(
  l_info, reward_categorization, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

td <- lubridate::today()

saveRDS(l_category_results, file = str_c("data/", td, "-grid-search.rds"))
# l_category_results <- readRDS(file = "data/2021-11-05-grid-search.rds")
# approx. 10 min using 10'000 samples when gcm is not re-fitted every time sample is accepted
# Post Processing & Plotting ----------------------------------------------



l_results_plots <- map(l_category_results, diagnostic_plots)

# Plot Prior Means & Posterior Means --------------------------------------

l_tmp <- save_results_plots(tbl_info, l_results_plots, .75, 2)
ggsave(l_tmp[[2]], l_tmp[[1]], width = 15, height = 7, units = "in")
l_tmp <- save_results_plots(tbl_info, l_results_plots, .75, 4)
ggsave(l_tmp[[2]], l_tmp[[1]], width = 15, height = 7, units = "in")



tbl <- l_category_results[[2]]$tbl_new

tbl_movement <- tbl %>% filter(timepoint == "After Training") %>% left_join(
  tbl %>% filter(timepoint == "Before Training") %>% select(stim_id, x1, x2), 
  by = "stim_id", suffix = c("_after", "_before"))

tbl_movement_agg <- tbl_movement %>%
  mutate(deviation = sqrt((x1_after - x1_before)^2 + (x2_after - x2_before)^2)) %>%
  mutate(x1_before, x2_before) %>%
  group_by(stim_id, x1_before, x2_before, reward) %>%
  summarize(
    deviation_avg = mean(deviation),
    x1_after_avg = mean(x1_after),
    x2_after_avg = mean(x2_after)
    ) %>% ungroup()

ggplot() +
  geom_point(data = tbl_movement_agg, aes(
      x1_before, x2_before, color = as.factor(stim_id), size = deviation_avg, shape = as.factor(reward)
      )) +
  geom_point(data = tbl_movement_agg, aes(
    x1_after_avg, x2_after_avg, group = stim_id, color = as.factor(stim_id), shape = as.factor(reward)
    ), size = 1.5) +
  geom_segment(data = tbl_movement_agg, aes(
    x = x1_before, xend = x1_after_avg, y = x2_before, yend = x2_after_avg, color = as.factor(stim_id)
    ), arrow = arrow(type = "open", length = unit(0.05, "inches"))) +
  theme_bw() +
  geom_point(data = l_category_results[[2]]$l_info$tbl_ellipses, aes(
    x_rotated, y_rotated, group = category
    ), color = "grey", size = .75) + 
  scale_color_viridis_d(guide = "none") +
  scale_size_continuous(name = "Avg. Deviation", range = c(1, 5)) +
  scale_shape_discrete(name = "Reward Magnitude") +
  labs(
    x = expression(X["1"]),
    y = expression(X["2"])
  )

# Plot Movements of Individual Points -------------------------------------

stim_ids <- c(72, 1)
l_tbl_stimuli <- map(stim_ids, stimulus_before_after, l_results = l_category_results)
split_my_vars <- function(tbl){
  tbl %>% split(list(.$sampling, .$space))
}
l_tbl_stimuli_split <- map(l_tbl_stimuli, split_my_vars)
l_plt_stimulus_movements <- map(l_tbl_stimuli_split, plot_stimulus_movements)

l_plt_stimulus_movements[[1]][[1]]

# make that plot nicer to look at for presentation
ggplot(tbl, aes(x1, x2, group = category)) + geom_point(aes(color = category, shape = timepoint))

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



# Visualize the conditions with training and testing examples -------------

l_stim <- make_stimuli(l_info[[1]])
tbl_stim <- l_stim[[1]]
l_info <- l_stim[[2]]
tbl_stim %>% ggplot(aes(x1, x2, group = category)) + geom_point(aes(color = category))


l_stim <- tt_split_rewards(tbl_stim, l_info)
tbl_stim <- l_stim[[1]]
l_info <- l_stim[[2]]

tbl_stim %>% ggplot(aes(x1, x2, group = category)) + 
  geom_point(aes(color = category, size = as.factor(reward), shape = timepoint)) +
  theme_bw() +
  scale_size_discrete(range = c(1.5, 3), name = "Reward") +
  scale_color_brewer(palette = "Set1", name = "Category") +
  scale_shape_discrete(name = "Timepoint") +
  labs(
    x = expression(X["1"]),
    y = expression(X["2"])
  )


# maybe useful code at a later point in time
# 
# 
# min_distance_from_boundary <- function(x1, x2) {
#   min(sqrt(
#     (x1 - l_info$tbl_ellipses$x_rotated)^2 +
#       (x2 - l_info$tbl_ellipses$y_rotated)^2
#   ))
# }
# tbl$distance_bd <- pmap_dbl(tbl[, c("x1", "x2")], min_distance_from_boundary)