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
  "R/analysis-utils.R",
  "R/analysis-plotting.R"
)
walk(files, source)


# Simulation Parameters ---------------------------------------------------

n_stimuli <- 100L
nruns <- 500

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(4L), cat_type = c("prototype", "exemplar", "rule"), #, 
  prior_sd = c(.75), sampling = c("improvement", "metropolis-hastings"),
  constrain_space = c(TRUE, FALSE), category_shape = c("square"),
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

l_info_seq <- pmap(
  tbl_vary %>% filter(cat_type == "exemplar") %>% mutate(cat_type == "no category") %>% unique(), ~ append(
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

tbl_info_seq <- tibble(do.call(rbind.data.frame, l_info_seq)) %>%
  mutate(condition_id = seq(1:length(l_info_seq))) %>%
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

l_seq_results <- future_map(
  l_info_seq, compare_subsequent_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)


td <- lubridate::today()


saveRDS(l_category_results, file = str_c("data/", td, "-grid-search-vary-constrain-space.rds"))
l_category_results <- readRDS(file = "data/2022-08-24-grid-search-vary-constrain-space.rds")
saveRDS(l_seq_results, file = str_c("data/", td, "-grid-search-sequential-comparison.rds"))
l_seq_results <- readRDS(file = "data/2023-01-12-grid-search-sequential-comparison.rds")
# approx. 10 min using 10'000 samples when gcm is not re-fitted every time sample is accepted



# Post Processing & Plotting ----------------------------------------------




diagnostics_seq <- function(l, sim_center, is_simulation) {
  
  env <- rlang::current_env()
  list2env(l, env)
  
  l_results <- add_centers(tbl_new, l_m, l_info)
  l_results$tbl_posterior <- distance_to_closest_center_simulation(
    l_results$tbl_posterior, sim_center, is_simulation
    )
  pl_avg_move <- plot_distance_to_category_center(l_results$tbl_posterior, sim_center, l_info)
  
  tmp <- l$tbl_new
  l_tmp <- split(tbl_new, tbl_new$timepoint)
  tmp_after <- l_tmp[[1]] %>% left_join(
    l_tmp[[2]][, c("stim_id", "x1", "x2")], 
    by = "stim_id", suffix = c("_after", "_before")
  )
  tmp_before <- l_tmp[[2]] %>% left_join(
    l_tmp[[2]][, c("stim_id", "x1", "x2")], 
    by = "stim_id", suffix = c("_after", "_before")
  )
  tmp_both <- rbind(tmp_after, tmp_before)
  tmp_after_agg <- tmp_after %>% group_by(stim_id, timepoint) %>%
    summarize(x1_after = mean(x1_after), x2_after = mean(x2_after))
  
  pl_movement <- ggplot(tmp_both, aes(x1_before, x2_before)) +
    geom_point() +
    geom_segment(aes(
      x = x1_before, xend = x1_after, y = x2_before, yend = x2_after
    ), arrow = arrow(angle = 15, length = unit(.05, "inches")
    ), alpha = .15) + geom_point(
      data = tmp_after_agg, aes(x1_after, x2_after), size = 3, color = "#0099FF"
    ) + facet_wrap(~ timepoint) +
    theme_bw() +
    labs(x = expr(x[1]), y = expr(x[2])) +
    scale_x_continuous(breaks = seq(0, 10, by = 2)) +
    scale_y_continuous(breaks = seq(0, 10, by = 2))
  
  l_out <- list(
    pl_movement = pl_movement, pl_avg_move = pl_avg_move$pl,
    tbl_avg_move = pl_avg_move$tbl_cr_agg
    )
  
  return(l_out)
}


l_results_plots <- map(l_category_results, diagnostic_plots, sim_center = "square", is_simulation = TRUE)
l_results_plots_seq <- map(l_seq_results, diagnostics_seq, sim_center = "square", is_simulation = TRUE)


tbl_bef_aft$x1_aft[is.na(tbl_bef_aft$x1_aft)] <- tbl_bef_aft$x1_bef[is.na(tbl_bef_aft$x1_aft)]
tbl_bef_aft$x2_aft[is.na(tbl_bef_aft$x2_aft)] <- tbl_bef_aft$x2_bef[is.na(tbl_bef_aft$x2_aft)]

ggplot(tbl_bef_aft, aes(x1_aft, x2_aft, group = category)) + geom_point(aes(color = category))

marrangeGrob(list(
  l_results_plots[[3]][[2]][[1]], l_results_plots[[3]][[2]][[4]], 
  l_results_plots[[5]][[2]][[1]], l_results_plots[[5]][[2]][[4]],
  l_results_plots[[9]][[2]][[1]], l_results_plots[[9]][[2]][[4]]
), nrow = 3, ncol = 2, 
layout_matrix = matrix(seq(1, 6, by = 1), byrow = TRUE, nrow = 3, ncol = 2)
)
pl_pred <- l_results_plots[[5]][[2]][[1]] +
  labs(title = "Prototype Model & Improvement Sampling")
tbl_preds <- crossing(
  group = c("Category Learning", "Similarity Judgment"),
  timepoint = factor(c("Before Training", "After Training"), c("Before Training", "After Training"), ordered = TRUE)
)
tbl_preds$d_closest <- c(2, 1.75, 2, 2)
dg <- position_dodge(width = .8)
pl_pred_delta <- ggplot(tbl_preds, aes(timepoint, d_closest, group = group)) +
  geom_col(aes(fill = group), position = dg) +
  theme_bw() +
  scale_fill_viridis_d(name = "Group") +
  labs(x = "Time Point", y = "Distance to Closest Center")
save_my_tiff(arrangeGrob(pl_pred, pl_pred_delta), "figures/model-predictions.tiff", 5, 7)

# extract sum of distances between prior and posterior
l_posteriors <- map(map(l_results_plots, 1), "tbl_posterior")
l_sum_of_distances <- map(l_posteriors, sum_of_pairwise_distances)

for (i in 1:nrow(tbl_info)) {
  l_sum_of_distances[[i]] <- cbind(
    l_sum_of_distances[[i]], 
    tbl_info[i, c("cat_type", "prior_sd", "sampling", "constrain_space")]
  )
}
tbl_sum_of_distances <- reduce(l_sum_of_distances, rbind)
tbl_ds_agg <- tbl_sum_of_distances %>%
  filter(tp == "After Training") %>%
  group_by(cat_type, comparison) %>%
  summarize(avg_ds_abs = mean(ds_abs), avg_ds_prop = mean(ds_prop)) %>%
  ungroup()
tbl_ds_agg$cat_type <- factor(tbl_ds_agg$cat_type, labels = c("Exemplar", "Prototype", "Rule"))
tbl_ds_agg$sampling <- factor(tbl_ds_agg$sampling, labels = c("Improvement", "Metropolis-Hastings"))

dg <- position_dodge(width = .9)
pl_preds_ds <- ggplot(tbl_ds_agg, aes(comparison, avg_ds_prop, group = cat_type)) +
  geom_col(aes(fill = cat_type), position = dg, color = "black") +
  geom_hline(yintercept = 1, size = 1, color = "grey", linetype = "dotdash") +
  #facet_grid(constrain_space ~ sampling) +
  theme_bw() +
  scale_fill_viridis_d(name = "Category Learning Model") +
  labs(x = "Comparison", y = "Prop. Change of Pairwise Distances")

save_my_tiff(pl_preds_ds, "figures/model-predictions-distances.tiff", 7, 4)

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









