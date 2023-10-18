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
  cat_type = c("prototype", "exemplar", "rule"), #, 
  prior_sd = c(.75), sampling = c("improvement", "metropolis-hastings"),
  constrain_space = c(TRUE, FALSE), category_shape = c("square", "ellipses"),
  is_reward = FALSE
)

tbl_vary$n_categories <- rep(c(2, 4), nrow(tbl_vary)/2)
tbl_vary <- tbl_vary %>% relocate(n_categories, .before = cat_type)

# rule-based strategy does not work well with ellipse category
filter_out <- tbl_vary$category_shape == "ellipses" & tbl_vary$cat_type == "rule"
tbl_vary <- tbl_vary[!filter_out, ]

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

tbl_vary_seq <- tbl_vary %>% filter(cat_type == "exemplar") %>% mutate(cat_type = "no category")
l_info_seq <- pmap(
  tbl_vary_seq %>% unique(), ~ append(
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
  l_category_results <- readRDS(file = "data/2023-02-08-grid-search-vary-constrain-space.rds")
  l_seq_results <- readRDS(file = "data/2023-02-08-grid-search-sequential-comparison.rds")
}


# Post Processing & Plotting ----------------------------------------------
if (read_write == "write") {
  l_results_plots <- map2(l_category_results, tbl_vary$category_shape, diagnostic_plots, is_simulation = TRUE)
  l_results_plots_seq <- map2(l_seq_results, tbl_info_seq$category_shape, diagnostics_seq, is_simulation = TRUE)
  saveRDS(l_results_plots, str_c("data/", td, "-category-learning-result-plots.RDS"))
  saveRDS(l_results_plots_seq, str_c("data/", td, "-sequential-comparison-result-plots.RDS"))
} else if (read_write == "read") {
  l_results_plots <- readRDS(str_c("data/2023-02-08-category-learning-result-plots.RDS"))
  l_results_plots_seq <- readRDS(str_c("data/2023-02-08-sequential-comparison-result-plots.RDS"))
}

dg <- position_dodge(width = .9)

# ellipse category structure example prediction
pl_pred_delta_ellipse <- l_results_plots[[1]][[2]][[4]]$tbl_cr_agg %>% mutate(condition = "Category Learning") %>% rbind(
  l_results_plots_seq[[1]]$tbl_avg_move %>% mutate(condition = "Sequential Comparison")
) %>% mutate(category = c("Bukil", "Venak")[category]) %>%
  ggplot(aes(session, d_closest_sqrt, group = condition)) +
  geom_col(aes(fill = condition), position = dg) +
  scale_fill_viridis_d(name = "Condition") +
  theme_bw()  +
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16),
    legend.text = element_text(size = 9),
    legend.title = element_text(size = 10),
    legend.position = c(.75, .84)
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Time Point", y = "Distance To Closest Center") + facet_wrap(~ category)

pl_pred_ellipse <- l_results_plots[[5]][[2]][[1]] +
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16)
  ) +
  scale_color_gradient(guide = "none", low = "lightskyblue2", high = "tomato3")

save_my_pdf_and_tiff(arrangeGrob(pl_pred_ellipse + theme(plot.title = element_blank()), pl_pred_delta_ellipse, nrow = 1), "figures/model-predictions-ellipses", 12, 3.5)


# square category example prediction
pl_pred_delta_square <- l_results_plots[[2]][[2]][[4]]$tbl_cr_agg %>% mutate(condition = "Category Learning") %>% rbind(
  l_results_plots_seq[[2]]$tbl_avg_move %>% mutate(condition = "Sequential Comparison")
) %>% mutate(category = c("Any Category")[category]) %>%
  ggplot(aes(session, d_closest_sqrt, group = condition)) +
  geom_col(aes(fill = condition), position = dg) +
  scale_fill_viridis_d(name = "Condition")  +
  theme_bw() + 
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16),
    legend.position = "none"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Time Point", y = "Distance To Closest Center") + facet_wrap(~ category)

pl_pred_square <- l_results_plots[[6]][[2]][[1]]  +
  theme(
    strip.background = element_rect(fill="white"),
    strip.text = element_text(colour = 'black'),
    text = element_text(size = 16)
  ) +
  scale_color_gradient(guide = "none", low = "lightskyblue2", high = "tomato3")


# get plot build
build <- ggplot_build(pl_pred_square)

# select hex value and x-value data
hex_df <- build$data[[1]][, c("colour", "x")]
unique(hex_df$colour)

save_my_pdf_and_tiff(arrangeGrob(pl_pred_square + theme(plot.title = element_blank()), pl_pred_delta_square, nrow = 1), "figures/model-predictions-squares", 12, 3.5)

save_my_pdf_and_tiff(arrangeGrob(
  pl_pred_ellipse + theme(plot.title = element_blank()), pl_pred_delta_ellipse,
  pl_pred_square + theme(plot.title = element_blank()), pl_pred_delta_square,
  nrow = 2
), "figures/model-predictions-both-designs", 11.25, 7.5)
# extract sum of distances between prior and posterior
# in category learning
l_posteriors_cat <- map(map(l_results_plots, 1), "tbl_posterior")
l_sum_of_distances_cat <- map2(l_posteriors_cat, tbl_vary$category_shape, sum_of_pairwise_distances)

# in sequential comparison
l_posteriors_seq <- map(
  l_seq_results, ~ .x$tbl_new %>%
    group_by(stim_id, cat_type, category, timepoint) %>%
    summarize(x1_true = mean(x1), x2_true = mean(x2)) %>% ungroup()
)
l_sum_of_distances_seq <- map2(l_posteriors_seq, tbl_vary_seq$category_shape, sum_of_pairwise_distances)


l_conds <- split(tbl_info[, c("cat_type", "prior_sd", "sampling", "constrain_space", "category_shape")], tbl_info$condition_id)
l_conds_rep <- map2(l_conds, l_sum_of_distances_cat, ~ slice(.x, rep(1:n(), each = nrow(.y))))

l_sum_of_distances_cat <- map2(
  l_sum_of_distances_cat, 
  l_conds_rep,
  ~ cbind(tibble(.x), tibble(.y))
)


l_conds <- split(tbl_info_seq[, c("cat_type", "prior_sd", "sampling", "constrain_space", "category_shape")], tbl_info_seq$condition_id)
l_conds_rep <- map2(l_conds, l_sum_of_distances_seq, ~ slice(.x, rep(1:n(), each = nrow(.y))))

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
  group_by(task, cat_type, comparison, sampling, constrain_space, category_shape, n_comparisons) %>%
  summarize(avg_ds_abs = mean(ds_abs), avg_ds_prop = mean(ds_prop)) %>%
  ungroup()
tbl_ds_agg$cat_type <- factor(tbl_ds_agg$cat_type, labels = c("Exemplar", "None", "Prototype", "Rule"))
tbl_ds_agg$cat_type <- fct_inorder(tbl_ds_agg$cat_type)
tbl_ds_agg$sampling <- factor(tbl_ds_agg$sampling, labels = c("Improvement", "Metropolis-Hastings"))
tbl_ds_agg$constrain_space <- factor(tbl_ds_agg$constrain_space, labels = c("No Space Constraints", "Space Constrained"))


# predictions for pairwise comparisons only have to be made for the squared category structure

dg <- position_dodge(width = .9)
pl_preds_ds <- ggplot(
  tbl_ds_agg %>% 
    filter(category_shape == "square" & constrain_space == "No Space Constraints" & sampling == "Improvement")
  , aes(comparison, avg_ds_prop, group = cat_type)) +
  geom_col(aes(fill = cat_type), position = dg, color = "black") +
  geom_hline(yintercept = 1, size = 1, color = "grey", linetype = "dotdash") +
  facet_grid(task ~ interaction(constrain_space, sampling, sep = " & \n")) +
  #facet_wrap(~ task) +
  theme_bw() +
  theme(
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom"
  ) +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  scale_fill_viridis_d(name = "Category Learning Model") +
  labs(x = "Comparison", y = "Prop. Change of Pairwise Distances")

save_my_tiff(pl_preds_ds, "figures/model-predictions-distances.tiff", 5.5, 4.5)
save_my_pdf(pl_preds_ds, "figures/model-predictions-distances.pdf", 5.5, 4.5)


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









