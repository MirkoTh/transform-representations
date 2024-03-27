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

tbl_simult <- read_rds("experiments/2022-09-category-learning-similarity/data/tbl_simult-treps-psychological-representation.rds")
tbl_simult_move <- read_rds("experiments/2022-09-category-learning-similarity/data/tbl_simult_move-treps-psychological-representation.rds")
tbl_cat <- read_rds("experiments/2022-09-category-learning-similarity/data/tbl_cat-treps-psychological-representation.rds")
tbl_seq <- read_rds("experiments/2022-09-category-learning-similarity/data/tbl_seq-treps-psychological-representation.rds")


is_fit <- TRUE

# Category Learning -------------------------------------------------------


tbl_cat_binned <- tbl_cat %>% 
  group_by(participant_id, trial_id_binned) %>%
  summarize(n_trials = n(), n_correct = sum(accuracy))

ggplot(
  tbl_cat_binned %>% 
    group_by(trial_id_binned) %>% 
    summarize(n_correct_avg = mean(n_correct)),
  aes(trial_id_binned, n_correct_avg, group = 1)) +
  geom_line() +
  geom_point(size = 4, color = "white") +
  geom_point(shape = 1) +
  theme_bw() +
  labs(
    x = "Trial ID (Binned)",
    y = "Nr. Correct (Out of 80)"
  )

cat_model <- stan_cat()
mod_cat <- cmdstan_model(cat_model)

mm_cat <- model.matrix(
  n_correct ~ as.numeric(trial_id_binned), data = tbl_cat_binned
) %>% as_tibble()
colnames(mm_cat) <- c("ic", "trial_id_binned")
mm_cat$trial_id_binned <- scale(mm_cat$trial_id_binned)[, 1]

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


file_loc <- str_c("experiments/2022-09-category-learning-similarity/data/cat-model-posterior.RDS")
pars_interest <- c("mu_tf")


if (is_fit) {
  fit_cat <- mod_cat$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  
  tbl_draws <- fit_cat$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_cat$summary()#variables = pars_interest)
  tbl_summary %>% arrange(desc(rhat))
  tbl_summary %>% arrange(rhat)
  
  saveRDS(tbl_draws, file_loc)
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
}


tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = c("Intercept", "Trial (Binned)")))

params_bf <- c("Intercept", "Trial (Binned)")
l <- sd_bfs(tbl_posterior, params_bf, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)


# Sequential Similarity Ratings ------------------------------------------------------

ggplot(
  tbl_seq %>% 
    group_by(distance_binned) %>% 
    summarize(response_mn = mean(response), n = n()),
  aes(distance_binned, response_mn, group = 1)) +
  geom_line() +
  geom_point(size = 4, color = "white") +
  geom_point(shape = 1, aes(size = n)) +
  theme_bw() +
  labs(
    x = "Euclidean Distance (Binned)",
    y = "Avg. Similarity Rating (1-4)"
  )

sim_model <- stan_sim()
mod_sim <- cmdstan_model(sim_model)

mm_sim <- model.matrix(
  response ~ distance_euclidian, data = tbl_seq
) %>% as_tibble()
colnames(mm_sim) <- c("ic", "d_euclidean")
mm_sim$d_euclidean <- scale(mm_sim$d_euclidean)[, 1]

l_data <- list(
  n_data = nrow(tbl_seq),
  n_subj = length(unique(tbl_seq$participant_id)),
  response = tbl_seq$response,
  subj = as.numeric(factor(
    tbl_seq$participant_id, 
    labels = 1:length(unique(tbl_seq$participant_id))
  )),
  x = as.matrix(mm_sim)
)

file_loc <- str_c("experiments/2022-07-category-learning-II/data/sim-model-posterior.RDS")
pars_interest <- c("mu_tf")

if (is_fit) {
  fit_sim <- mod_sim$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 2000, chains = 3, parallel_chains = 3
  )
  tbl_draws <- fit_sim$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_sim$summary(variables = pars_interest)
  saveRDS(tbl_draws, file_loc)
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
}

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


# Attraction vs. Repulsion -----------------------------------------------


tbl_simult_agg <- summarySEwithin(
  tbl_simult_move, "move_response", "n_categories", 
  "comparison_pool_binary", "participant_id"
)
tbl_simult_agg$n_categories <- fct_relevel(tbl_simult_agg$n_categories, "4 Categories", "Similarity")
dg <- position_dodge(width = .2)
pl_groupmeans <- ggplot(tbl_simult_agg, aes(comparison_pool_binary, move_response, group = n_categories)) +
  geom_hline(yintercept = 0, color = "grey", size = 1, linetype = "dotdash") +
  geom_errorbar(aes(ymin = move_response - ci, ymax = move_response + ci, color = n_categories), width = .2, position = dg) +
  geom_line(aes(color = n_categories), position = dg) +
  geom_point(color = "white", size = 4, position = dg) +
  geom_point(aes(color = n_categories), position = dg) +
  scale_color_viridis_d(name = "Group") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = .01, .01)) +
  theme_bw() +
  labs(
    x = "Category Comparison",
    y = "Rating After - Before"
  )
save_my_tiff(pl_groupmeans, "experiments/2022-09-category-learning-similarity/data/figures/mean-moves.tiff", 6, 4)


# Mean Effects ------------------------------------------------------------


# random slopes on same vs. different category

simult_move_model_rs <- stan_simult_rs()
mod_simult_move_rs <- cmdstan_model(simult_move_model_rs)

# only random intercept
simult_move_model_ri <- stan_simult_ri()
mod_simult_move_ri <- cmdstan_model(simult_move_model_ri)


mm_simult_move <- model.matrix(
  move_response ~ as.factor(comparison_pool_binary) + as.factor(n_categories), data = tbl_simult_move
) %>% as_tibble()
colnames(mm_simult_move) <- c("ic", "comp_pool", "ncat")
mm_simult_move$comp_pool <- mm_simult_move$comp_pool - .5
mm_simult_move$ncat <- mm_simult_move$ncat - .5
mm_simult_move$ia <- mm_simult_move$comp_pool * mm_simult_move$ncat

l_data <- list(
  n_data = nrow(tbl_simult_move),
  n_subj = length(unique(tbl_simult_move$participant_id)),
  move_response = scale(tbl_simult_move$move_response, scale = FALSE)[, 1],
  subj = as.numeric(factor(
    tbl_simult_move$participant_id, 
    labels = 1:length(unique(tbl_simult_move$participant_id))
  )),
  x = as.matrix(mm_simult_move)
)

file_loc_rs <- str_c("experiments/2022-09-category-learning-similarity/data/simult-move-rs-model-posterior.RDS")
file_loc_loo_rs <- str_c("experiments/2022-09-category-learning-similarity/data/simult-move-rs-loo.RDS")
file_loc_ri <- str_c("experiments/2022-09-category-learning-similarity/data/simult-move-ri-model-posterior.RDS")
file_loc_loo_ri <- str_c("experiments/2022-09-category-learning-similarity/data/simult-move-ri-loo.RDS")
pars_interest <- c("b", "mu")

if (is_fit) {
  # random slopes
  fit_simult_move_rs <- mod_simult_move_rs$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  tbl_draws_rs <- fit_simult_move_rs$draws(variables = pars_interest, format = "df")
  tbl_summary_rs <- fit_simult_move_rs$summary(variables = pars_interest)
  saveRDS(tbl_draws_rs, file_loc_rs)
  
  # fit_simult_move_rs <- readRDS(file_loc_rs)
  loo_rs <- fit_simult_move_rs$loo(variables = "log_lik_pred")
  saveRDS(loo_rs, file = file_loc_loo_rs)
  
  # only random intercept
  fit_simult_move_ri <- mod_simult_move_ri$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3
  )
  tbl_summary_ri <- fit_simult_move_ri$summary(variables = pars_interest)
  tbl_draws_ri <- fit_simult_move_ri$draws(variables = pars_interest, format = "df")
  
  saveRDS(tbl_draws_ri, file_loc_ri)
  loo_ri <- fit_simult_move_ri$loo(variables = "log_lik_pred")
  saveRDS(loo_ri, file = file_loc_loo_ri)
} else if (!is_fit) {
  loo_rs <- readRDS(file_loc_loo_rs)
  loo_ri <- readRDS(file_loc_loo_ri)
  tbl_draws_rs <- readRDS(file_loc_rs)
  tbl_draws_ri <- readRDS(file_loc_ri)
}


loo::loo_model_weights(list(loo_ri, loo_rs), method = "stacking")

tbl_draws <- tbl_draws_rs
params_bf <- c("Intercept", "Category Comparison", "Group", "Category Comparison x Group")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))

l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
pl_arrangement <- grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], nrow = 2, ncol = 2)
save_my_tiff(pl_arrangement, "experiments/2022-09-category-learning-similarity/data/figures/posteriors-rs.tiff", 10, 6)

pl_arrangement2 <- grid.arrange(
  pl_preds_ds + ggtitle("Predictions"),
  pl_groupmeans + ggtitle("Mean Differences"),
  pl_move_mass + ggtitle("Distribution of Differences"), nrow = 1, ncol = 3
)
save_my_tiff(pl_arrangement2, "experiments/2022-09-category-learning-similarity/data/figures/arrangement-psychonomics-2022.tiff", 16.5, 3.35)
