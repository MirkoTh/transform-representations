rm(list = ls())
# Import Packages ---------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(docstring)
library(rutils)
library(cmdstanr)


# Import Home-Grown Modules -----------------------------------------------

files <- c(
  "R/utils.R",
  "R/plotting.R",
  "R/analysis-utils.R",
  "R/analysis-plotting.R",
  "R/summarySEwithin.R",
  "R/summarySE.R",
  "R/normDataWithin.R",
  "R/stan-models.R"
)
walk(files, source)


# Load Data ---------------------------------------------------------------

tbl_cr <- read_rds("experiments/2022-02-category-learning/data/tbl_cr.rds")
tbl_cat_sim <- read_rds("experiments/2022-02-category-learning/data/tbl_cat_sim.rds")
tbl_cat <- tbl_cat_sim %>% filter(n_categories == 2)
tbl_sim <- read_rds("experiments/2022-02-category-learning/data/tbl_sim.rds")


# Category Learning -------------------------------------------------------


tbl_cat_binned <- tbl_cat %>% 
  group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(n_trials = n(), n_correct = sum(accuracy)) %>%
  ungroup() %>% rename(category = cat_true)

ggplot(
  tbl_cat_binned %>% 
    group_by(category, trial_id_binned) %>% 
    summarize(n_correct_avg = mean(n_correct)),
  aes(trial_id_binned, n_correct_avg, group = category)) +
  geom_line(aes(color = category)) +
  geom_point(size = 4, color = "white") +
  geom_point(shape = 1, aes(color = category)) +
  theme_bw() +
  scale_color_viridis_d(name = "Category") +
  labs(
    x = "Trial ID (Binned)",
    y = "Nr. Correct (Out of 80)"
  )

cat_model <- stan_cat_by_category()
mod_cat <- cmdstan_model(cat_model)

mm_cat <- model.matrix(
  n_correct ~ as.numeric(trial_id_binned) + category, data = tbl_cat_binned
) %>% as_tibble()
colnames(mm_cat) <- c("ic", "trial_id_binned", "category")
mm_cat$trial_id_binned <- scale(mm_cat$trial_id_binned)[, 1]
mm_cat$category <- mm_cat$category - .5
mm_cat$ia <- mm_cat$trial_id_binned * mm_cat$category

l_data <- list(
  n_data = nrow(tbl_cat_binned),
  n_subj = length(unique(tbl_cat_binned$participant_id)),
  n_trials = tbl_cat_binned$n_trials,
  n_correct = tbl_cat_binned$n_correct,
  subj = as.numeric(factor(
    tbl_cat_binned$participant_id, 
    labels = 1:length(unique(tbl_cat_binned$participant_id))
  )),
  x = as.matrix(mm_cat)
)

fit_cat <- mod_cat$sample(
  data = l_data, iter_sampling = 4000, iter_warmup = 2000, chains = 3, parallel_chains = 3
)

file_loc <- str_c("experiments/2022-02-category-learning/data/cat-model.RDS")
fit_cat$save_object(file = file_loc)
pars_interest <- c("mu_tf")
tbl_draws <- fit_cat$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_cat$summary()#variables = pars_interest)

lbls <- c("Intercept", "Trial (Binned)", "Category", "Trial x Category")
tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = lbls))

l <- sd_bfs(tbl_posterior, lbls, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(lbls), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], nrow = 1, ncol = 4)


# Similarity Ratings ------------------------------------------------------

ggplot(
  tbl_sim %>% 
    group_by(distance_binned) %>% 
    summarize(response_mn = mean(response)),
  aes(distance_binned, response_mn, group = 1)) +
  geom_line() +
  geom_point(size = 4, color = "white") +
  geom_point(shape = 1) +
  theme_bw() +
  labs(
    x = "Euclidean Distance (Binned)",
    y = "Avg. Similarity Rating (1-4)"
  )

sim_model <- stan_sim()
mod_sim <- cmdstan_model(sim_model)

mm_sim <- model.matrix(
  response ~ distance_euclidian, data = tbl_sim
) %>% as_tibble()
colnames(mm_sim) <- c("ic", "d_euclidean")
mm_sim$d_euclidean <- scale(mm_sim$d_euclidean)[, 1]

l_data <- list(
  n_data = nrow(tbl_sim),
  n_subj = length(unique(tbl_sim$participant_id)),
  response = tbl_sim$response,
  subj = as.numeric(factor(
    tbl_sim$participant_id, 
    labels = 1:length(unique(tbl_sim$participant_id))
  )),
  x = as.matrix(mm_sim)
)

fit_sim <- mod_sim$sample(
  data = l_data, iter_sampling = 1000, iter_warmup = 1000, chains = 1
)

file_loc <- str_c("experiments/2022-07-category-learning-II/data/sim-model.RDS")
fit_sim$save_object(file = file_loc)
pars_interest <- c("mu_tf")
tbl_draws <- fit_sim$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_sim$summary(variables = pars_interest)


params_bf <- c("Intercept", "Euclidean Distance")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))

l <- sd_bfs(tbl_posterior, params_bf, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)


# Movements Towards Centers -----------------------------------------------

# sqrt tf on d_closest
tbl_cr$d_closest_sqrt <- sqrt(tbl_cr$d_closest)

plot_distances_to_centers(tbl_cr) + facet_wrap(name ~ category)

pl_groupmeans <- plot_groupmeans_against_session(
  tbl_cr %>% mutate(n_categories = factor(n_categories, labels = c("Similarity Judgment", "Category Learning")))
  ) + theme(legend.position = "bottom")
save_my_tiff(pl_groupmeans, "experiments/2022-02-category-learning/data/means-sqrt-tf.tiff", 5, 4)

# analysis of the means suggests that there may be an interaction effect
# note, cis are within-participant cis

# the following plots suggest a different option
# i.e., deltas could represent a mixture between visuo-spatial responses
# and categorical respones
# what should be analyzed is whether the proportion of categorical respones
# differs between groups


plot_mean_deltas(tbl_cr)
tbl_cr_moves <- after_vs_before(tbl_cr, 2)
l_outliers <- extract_movement_outliers(tbl_cr_moves, 1, "Not Transformed")
tbl_outliers <- l_outliers$tbl_outliers
tbl_labels <- l_outliers$tbl_labels
pl_outliers_prior <- plot_movement_outliers(
  tbl_outliers, 
  tbl_labels, 
  "Outliers above/below 1 sd of the Mean"
)
pl_outliers_prior


# make sure tbl_cr_d1 and tbl_cr_moves are ordered in exactly the same way
tbl_cr_d1 <- tbl_cr %>% filter(session == 1)
tbl_cr_d1 <- tbl_cr_d1 %>% arrange(participant_id, trial_id)
tbl_cr_moves <- tbl_cr_moves %>% arrange(participant_id, trial_id)

tbl_participants_lookup <- tbl_cr_moves %>% group_by(participant_id, n_categories) %>%
  count() %>% ungroup() %>% select(-n) %>%
  mutate(participant_id_num = as.numeric(fct_inorder(factor(participant_id))))

plot_group_rts_against_session(tbl_cr, 2)


l_data_mixture_groups <- list(
  n_data = nrow(tbl_cr_moves),
  n_subj = length(unique(tbl_cr_moves$participant_id)),
  n_groups = length(unique(tbl_cr_moves$n_categories)),
  d_moved = tbl_cr_moves$d_move_abs,
  subj = fct_inorder(factor(tbl_cr_moves$participant_id)) %>% as.numeric(),
  group = as.numeric(tbl_participants_lookup$n_categories)
)

# model having a parameter shifting group mean of categorization group 
# whereas mean of similarity group is only fixed at zero


move_model_shift_normal <- stan_move_shift_normal()
move_model_shift_normal <- cmdstan_model(move_model_shift_normal)

fit_move_shift_normal <- move_model_shift_normal$sample(
  data = l_data_mixture_groups, iter_sampling = 500, iter_warmup = 200,
  chains = 3, parallel_chains = 3,
  save_warmup = FALSE
)

file_loc_move_shift_normal <- str_c(
  "experiments/2022-02-category-learning/data/cr-move-shift-normal-model.RDS"
)

fit_or_read <- "fit"
if (fit_or_read == "fit") {
  fit_move_shift_normal$save_object(file = file_loc_move_shift_normal, compress = "gzip")
} else if (fit_or_read == "read") {
  fit_move_shift_normal <- readRDS(file_loc_move_shift_normal)
}

pars_interest <- c("sigma_subject", "mu", "posterior_prediction") # 
tbl_draws <- fit_move_shift_normal$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_move_shift_normal$summary(variables = pars_interest)

params_bf <- c("Mean Similarity", "Mean Categorization", "Mean Difference")
tbl_posterior <- tbl_draws %>% 
  dplyr::select(`mu[1]`, `mu[2]`, .chain) %>%
  mutate(mu_diff = `mu[2]` - `mu[1]`) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))
l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 1, ncol = 3)

