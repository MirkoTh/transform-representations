# Load Packages and Home-Grown Modules ------------------------------------

rm(list = ls())
library(naivebayes)
library(tidyverse)
library(docstring)
library(grid)
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
nruns <- 5000

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

plan(multisession, workers = min(future::availableCores() - 4, length(l_info)))


l_category_results <- future_map(
  l_info, categorize_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

l_seq_results <- future_map(
  l_info_seq, compare_subsequent_stimuli, 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)



read_write <- "read"

td <- lubridate::today()

if (read_write == "write") {
  saveRDS(l_category_results, file = str_c("data/", td, "-grid-search-vary-constrain-space.rds"))
  saveRDS(l_seq_results, file = str_c("data/", td, "-grid-search-sequential-comparison.rds"))
} else if (read_write == "read") {
  l_category_results <- readRDS(file = "data/2023-01-27-grid-search-vary-constrain-space.rds")
  l_seq_results <- readRDS(file = "data/2023-01-27-grid-search-sequential-comparison.rds")
}

# approx. 10 min using 10'000 samples when gcm is not re-fitted every time sample is accepted



# Post Processing & Plotting ----------------------------------------------
if (read_write == "write") {
  l_results_plots <- map(l_category_results, diagnostic_plots, sim_center = "square", is_simulation = TRUE)
  l_results_plots_seq <- map(l_seq_results, diagnostics_seq, sim_center = "square", is_simulation = TRUE)
  saveRDS(l_results_plots, str_c("data/", td, "-category-learning-result-plots.RDS"))
  saveRDS(l_results_plots_seq, str_c("data/", td, "-sequential-comparison-result-plots.RDS"))
} else if (read_write == "read") {
  l_results_plots <- readRDS(str_c("data/2023-01-28-category-learning-result-plots.RDS"))
  l_results_plots_seq <- readRDS(str_c("data/2023-01-28-sequential-comparison-result-plots.RDS"))
}

dg <- position_dodge(width = .9)

pl_pred_delta <- l_results_plots[[1]][[2]][[4]]$tbl_cr_agg %>% mutate(condition = "Category Learning") %>% rbind(
  l_results_plots_seq[[1]]$tbl_avg_move %>% mutate(condition = "Sequential Comparison")
) %>% ggplot(aes(session, d_closest_sqrt, group = condition)) +
  geom_col(aes(fill = condition), position = dg) +
  scale_fill_viridis_d(name = "Condition") +
  theme_bw() +
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))+
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Time Point", y = "Distance To Closest Center")


# tbl_bef_aft$x1_aft[is.na(tbl_bef_aft$x1_aft)] <- tbl_bef_aft$x1_bef[is.na(tbl_bef_aft$x1_aft)]
# tbl_bef_aft$x2_aft[is.na(tbl_bef_aft$x2_aft)] <- tbl_bef_aft$x2_bef[is.na(tbl_bef_aft$x2_aft)]
# 
# ggplot(tbl_bef_aft, aes(x1_aft, x2_aft, group = category)) + geom_point(aes(color = category))

grid.draw(arrangeGrob(
  l_results_plots[[3]][[2]][[1]], l_results_plots[[3]][[2]][[4]]$pl, 
  l_results_plots[[5]][[2]][[1]], l_results_plots[[5]][[2]][[4]]$pl,
  l_results_plots[[9]][[2]][[1]], l_results_plots[[9]][[2]][[4]]$pl, nrow = 3, ncol = 2, 
  layout_matrix = matrix(seq(1, 6, by = 1), byrow = TRUE, nrow = 3, ncol = 2)
))
pl_pred <- l_results_plots[[5]][[2]][[1]] +
  theme(strip.background = element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))

tbl_preds <- crossing(
  group = c("Category Learning", "Similarity Judgment"),
  timepoint = factor(c("Before Training", "After Training"), c("Before Training", "After Training"), ordered = TRUE)
)
tbl_preds$d_closest <- c(2, 1.75, 2, 2)
dg <- position_dodge(width = .8)
# pl_pred_delta <- ggplot(tbl_preds, aes(timepoint, d_closest, group = group)) +
#   geom_col(aes(fill = group), position = dg) +
#   theme_bw() +
#   scale_fill_viridis_d(name = "Group") +
#   labs(x = "Time Point", y = "Distance to Closest Center")
save_my_tiff(arrangeGrob(pl_pred + theme(plot.title = element_blank()), pl_pred_delta, nrow = 1), "figures/model-predictions.tiff", 12, 3.5)
save_my_pdf(arrangeGrob(pl_pred + theme(plot.title = element_blank()), pl_pred_delta, nrow = 1), "figures/model-predictions.pdf", 12, 3.5)

# extract sum of distances between prior and posterior
l_posteriors_cat <- map(map(l_results_plots, 1), "tbl_posterior")
l_sum_of_distances_cat <- map(l_posteriors_cat, sum_of_pairwise_distances)


## todos
# why does category change for some stimuli in sequential comparison task?
# fix that and then plot within vs. between distances before and after

# l_posteriors_seq <- map(
#   l_seq_results, ~ .x$tbl_new %>% 
#     group_by(stim_id, cat_type, category, timepoint) %>%
#     summarize(x1_true = mean(x1), x2_true = mean(x2)) %>% ungroup()
#   )
temporary_correction <- function(l) {
  tbl_df <- l$tbl_new
  tmp_before <- tbl_df %>% filter(timepoint == "Before Training")
  tmp_after <- tbl_df %>% filter(timepoint == "After Training")
  tmp_after <- tmp_before[, c("stim_id", "category")] %>% 
    left_join(tmp_after, by = c("stim_id", "category"))
  rbind(tmp_before, tmp_after) %>% 
    group_by(stim_id, cat_type, category, timepoint) %>%
    summarize(x1_true = mean(x1), x2_true = mean(x2)) %>% ungroup()
}

l_posteriors_seq <- map(l_seq_results, temporary_correction)
l_sum_of_distances_seq <- map(l_posteriors_seq, sum_of_pairwise_distances)



l_conds <- split(tbl_info[, c("cat_type", "prior_sd", "sampling", "constrain_space")], tbl_info$condition_id)
l_conds_rep <- map(l_conds, ~ slice(.x, rep(1:n(), each = 4)))

l_sum_of_distances_cat <- map2(
  l_sum_of_distances_cat, 
  l_conds_rep,
  ~ cbind(tibble(.x), tibble(.y))
)


l_conds <- split(tbl_info_seq[, c("cat_type", "prior_sd", "sampling", "constrain_space")], tbl_info_seq$condition_id)
l_conds_rep <- map(l_conds, ~ slice(.x, rep(1:n(), each = 4)))

l_sum_of_distances_seq <- map2(
  l_sum_of_distances_seq,
  l_conds_rep,
  ~ cbind(tibble(.x), tibble(.y))
)

tbl_sum_of_distances <- reduce(l_sum_of_distances_cat, rbind) %>% as_tibble()
tbl_sum_of_distances$task <- "Category Learning"

tmp <- reduce(l_sum_of_distances_seq, rbind) %>% as_tibble()
tmp$task <- "Sequential Comparison"
tbl_sum_of_distances <- rbind(tbl_sum_of_distances, tmp)

tbl_ds_agg <- tbl_sum_of_distances %>%
  filter(tp == "After Training") %>%
  group_by(task, cat_type, comparison, sampling, constrain_space, n_comparisons) %>%
  summarize(avg_ds_abs = mean(ds_abs), avg_ds_prop = mean(ds_prop)) %>%
  ungroup()
tbl_ds_agg$cat_type <- factor(tbl_ds_agg$cat_type, labels = c("Exemplar", "Prototype", "Rule"))
tbl_ds_agg$sampling <- factor(tbl_ds_agg$sampling, labels = c("Improvement", "Metropolis-Hastings"))

dg <- position_dodge(width = .9)
pl_preds_ds <- ggplot(tbl_ds_agg, aes(comparison, avg_ds_prop, group = cat_type)) +
  geom_col(aes(fill = cat_type), position = dg, color = "black") +
  geom_hline(yintercept = 1, size = 1, color = "grey", linetype = "dotdash") +
  #facet_grid(interaction(constrain_space, sampling) ~ task) +
  facet_wrap(~ task) +
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









