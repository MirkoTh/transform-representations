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

tbl_cr <- read_rds("experiments/2022-02-category-learning/data/tbl_cr-psychological-representation.rds")
tbl_cat_sim <- read_rds("experiments/2022-02-category-learning/data/tbl_cat_sim-psychological-representation.rds")
tbl_cat <- tbl_cat_sim %>% filter(n_categories == 2)
tbl_sim <- read_rds("experiments/2022-02-category-learning/data/tbl_sim-psychological-representation.rds")
tbl_moves_agg <- read_csv("experiments/2022-02-category-learning/data/movements-catacc-gt.csv")


# should models be run or posteriors be loaded?
is_fit <- FALSE


# Category Learning -------------------------------------------------------


tbl_cat_binned <- tbl_cat %>% 
  group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(n_trials = n(), n_correct = sum(accuracy)) %>%
  ungroup() %>% rename(category = cat_true)

tbl_cat_binned %>% filter(trial_id_binned == max(as.numeric(trial_id_binned))) %>%
  group_by(category) %>%
  summarize(n_trials = sum(n_trials), n_correct = sum(n_correct), prop_correct = n_correct/n_trials)

ggplot(
  tbl_cat_binned %>% 
    mutate(category = factor(category)) %>%
    group_by(category, trial_id_binned) %>% 
    summarize(n_correct_avg = mean(n_correct/n_trials)),
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

tbl_cat_binned$trial_id_binned_z <- scale(as.numeric(as.character(tbl_cat_binned$trial_id_binned)))[, 1]
mm_cat <- model.matrix(
  n_correct ~ trial_id_binned_z + as.factor(category), data = tbl_cat_binned
) %>% as_tibble()
colnames(mm_cat) <- c("ic", "trial_id_binned", "category")
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

file_loc <- str_c("experiments/2022-02-category-learning/data/cat-model-posterior.RDS")
pars_interest <- c("mu_tf")


if (is_fit) {
  fit_cat <- mod_cat$sample(
    data = l_data, iter_sampling = 10000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  tbl_draws <- fit_cat$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc)
  
} else if (!is_fit) {
  tbl_draws <- read_rds(file_loc)
}


tbl_summary <- fit_cat$summary(variables = pars_interest)
tbl_summary %>% arrange(desc(rhat))

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
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], nrow = 2)


# Similarity Ratings ------------------------------------------------------

ggplot(
  tbl_sim %>% 
    group_by(distance_binned) %>% 
    summarize(response_mn = mean(response), n = n()),
  aes(distance_binned, response_mn, group = 1)) +
  geom_line() +
  geom_point(size = 4, color = "white") +
  geom_point(aes(size = n)) +
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

file_loc_sim <- str_c("experiments/2022-02-category-learning/data/sim-model-posteriors.rds")

if (is_fit) {
  fit_sim <- mod_sim$sample(
    data = l_data, iter_sampling = 10000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  
  pars_interest <- c("mu_tf")
  tbl_draws <- fit_sim$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file = file_loc_sim)
  
} else if (!is_fit) {
  tbl_draws <- read_rds(file_loc_sim)
}

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
# equal categories across groups
# the following assigns "category labels" to stimuli also in the sequential comparison group (even though there aren't)
tbl_category_lookup <- tbl_cr %>% filter(n_categories == 2) %>%
  count(stim_id, category) %>% select(-n)

tbl_cr <- tbl_cr %>% select(-category)
tbl_cr <- tbl_cr %>% left_join(tbl_category_lookup, by = "stim_id")


plot_mean_deltas_ellipse(tbl_cr, 15)
tbl_cr_moves <- after_vs_before(tbl_cr, 2)

plot_groupmeans_against_session(
  tbl_cr %>% 
    mutate(
      n_categories = factor(n_categories, labels = c("Similarity Judgment", "Category Learning")),
      n_categories = fct_rev(n_categories), 
      category = factor(category, labels = c("Bukil", "Venak"))
    ), sim_center = "ellipses"
) + theme(legend.position = "bottom") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(.02, .02)))

# make sure tbl_cr_d1 and tbl_cr_moves are ordered in exactly the same way
tbl_cr_d1 <- tbl_cr %>% filter(session == 1)
tbl_cr_d1 <- tbl_cr_d1 %>% arrange(participant_id, trial_id)
tbl_cr_moves <- tbl_cr_moves %>% arrange(participant_id, trial_id)

tbl_participants_lookup <- tbl_cr_moves %>% group_by(participant_id, n_categories) %>%
  count() %>% ungroup() %>% select(-n) %>%
  mutate(participant_id_num = as.numeric(fct_inorder(factor(participant_id))))

plot_group_rts_against_session(tbl_cr, 2)


# prediction can be formulated as 3-way ia on ds or 2-way ia on movements
# ds ~ timepoint x group x category
# dmove ~ group x category


mm <- model.matrix(d_move_abs ~ category + n_categories, data = tbl_cr_moves)
mm[, 2] <- mm[, 2] - 1.5
mm[, 3] <- mm[, 3] - .5
mm <- cbind(mm, mm[, 2] * mm[, 3])


l_data_moves <- list(
  n_data = nrow(tbl_cr_moves),
  n_subj = length(unique(tbl_cr_moves$participant_id)),
  d_moved = tbl_cr_moves$d_move_abs,
  subj = fct_inorder(factor(tbl_cr_moves$participant_id)) %>% as.numeric(),
  x = mm
)

move_model <- stan_move_e1()
move_model <- cmdstan_model(move_model)


file_loc_move <- str_c(
  "experiments/2022-02-category-learning/data/cr-move-model-posterior.RDS"
)
pars_interest <- c("mu_tf") # 
if (is_fit) {
  fit_move <- move_model$sample(
    data = l_data_moves, iter_sampling = 5000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  tbl_draws <- fit_move$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc_move)
  
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc_sim)
}


tbl_summary <- fit_move$summary(variables = pars_interest)

params_bf <- c("Intercept", "Ellipse Category", "Group", "Ellipse Category x Group")
tbl_posterior <- tbl_draws %>% 
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))
l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], nrow = 2, ncol = 2)


# individual differences in movements -------------------------------------

tbl_moves_agg <- tbl_moves_agg %>% filter(n_categories == "Category Learning")

tbl_moves_agg %>%
  pivot_longer(cols = c(mean_accuracy, mean_delta_accuracy)) %>%
  mutate(name = factor(name, labels = c("Average Final Accuracy", "Average Improvement"))) %>%
  ggplot(aes(value, movement, group = name)) +
  geom_point(aes(color = name)) +
  geom_smooth(aes(color = name), method = "lm") +
  theme_bw() +
  facet_grid(category ~ name, scales = "free") +
  scale_color_viridis_d(name = "Measurement") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Value", y = "Movement Towards Category Center")

ggplot(tbl_moves_agg, aes(movement)) + geom_histogram() + facet_wrap(~ category)
ggplot(tbl_moves_agg, aes(mean_accuracy)) + geom_histogram() + facet_wrap(~ category)

move_model <- stan_move_by_success()
mod_move <- cmdstan_model(move_model)

# use final accuracy
mm_move_finalacc <- model.matrix(
  movement ~ category, data = tbl_moves_agg
) %>% as_tibble()
mm_move_finalacc[, 2] <- mm_move_finalacc[, 2] - .5
mm_move_finalacc[, 3] <- scale(tbl_moves_agg$mean_accuracy, scale = FALSE)[, 1]
mm_move_finalacc[, 4] <- mm_move_finalacc[, 2] * mm_move_finalacc[, 3]

l_data <- list(
  n_data = nrow(tbl_moves_agg),
  response = tbl_moves_agg$movement,
  x = as.matrix(mm_move_finalacc)
)

fit_move_finalacc <- mod_move$sample(
  data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
)

file_loc <- str_c("experiments/2022-02-category-learning/data/move-model-finalacc.RDS")
fit_move_finalacc$save_object(file = file_loc)
pars_interest <- c("mu")
tbl_draws <- fit_move_finalacc$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_move_finalacc$summary(variables = pars_interest)


params_bf <- c(
  "Intercept", "Category", 
  "Final Accuracy", "Category x Final Accuracy"
)

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))

l <- sd_bfs(tbl_posterior, params_bf[c(2, 3, 4)], 1)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf[c(2, 3, 4)]), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 2, ncol = 2)



# use delta in categorization accuracy
mm_move_deltaacc <- model.matrix(
  movement ~ category, data = tbl_moves_agg
) %>% as_tibble()
mm_move_deltaacc[, 2] <- mm_move_deltaacc[, 2] - .5
mm_move_deltaacc[, 3] <- scale(tbl_moves_agg$mean_delta_accuracy, scale = FALSE)[, 1]
mm_move_deltaacc[, 4] <- mm_move_deltaacc[, 2] * mm_move_deltaacc[, 3]

l_data <- list(
  n_data = nrow(tbl_moves_agg),
  response = tbl_moves_agg$movement,
  x = as.matrix(mm_move_deltaacc)
)

fit_move_deltaacc <- mod_move$sample(
  data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3
)

file_loc <- str_c("experiments/2022-02-category-learning/data/move-model-deltaacc.RDS")
fit_move_deltaacc$save_object(file = file_loc)
pars_interest <- c("mu")
tbl_draws <- fit_move_deltaacc$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_move_deltaacc$summary(variables = pars_interest)


params_bf <- c("Intercept", "Category", "Delta Accuracy", "Category x Delta Accuracy")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))


l <- sd_bfs(tbl_posterior, params_bf[c(2, 3, 4)], 1)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf[c(2, 3, 4)]), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 2, ncol = 2)


# Boundary Aversion -------------------------------------------------------


mm <- model.matrix(d_move_boundary ~ category + n_categories, data = tbl_cr_moves)
mm[, 2] <- mm[, 2] - 1.5
mm[, 3] <- mm[, 3] - .5
mm <- cbind(mm, mm[, 2] * mm[, 3])


l_data_moves_boundary <- list(
  n_data = nrow(tbl_cr_moves),
  n_subj = length(unique(tbl_cr_moves$participant_id)),
  d_moved = tbl_cr_moves$d_move_boundary,
  subj = fct_inorder(factor(tbl_cr_moves$participant_id)) %>% as.numeric(),
  x = mm
)

move_model <- stan_move_e1()
move_model <- cmdstan_model(move_model)

file_loc_aversion <- str_c(
  "experiments/2022-02-category-learning/data/cr-boundary-aversion-posterior.RDS"
)
pars_interest <- c("mu_tf") # 

if (is_fit) {
  fit_move_boundary <- move_model$sample(
    data = l_data_moves_boundary, iter_sampling = 5000, iter_warmup = 1000, #5000, 1000
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  tbl_draws <- fit_move_boundary$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc_aversion)
  tbl_summary <- fit_move_boundary$summary(variables = pars_interest)
} else if (!is_fit) {
  tbl_draws <- read_rds(file_loc_aversion)
}


params_bf <- c("Intercept", "Ellipse Category", "Group", "Ellipse Category x Group")
tbl_posterior <- tbl_draws %>% 
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))
l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], nrow = 2, ncol = 2)


