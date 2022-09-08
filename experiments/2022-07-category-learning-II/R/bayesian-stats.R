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
  "experiments/2022-07-category-learning-II/R/stan-models.R"
)
walk(files, source)


# Load Data ---------------------------------------------------------------

tbl_cr <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri.rds")
tbl_cat <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri.rds")
tbl_sim <- read_rds("experiments/2022-07-category-learning-II/data/tbl_sim-treps-long-ri.rds")


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

fit_cat <- mod_cat$sample(
  data = l_data, iter_sampling = 4000, iter_warmup = 2000, chains = 1
)

file_loc <- str_c("experiments/2022-07-category-learning-II/data/cat-model.RDS")
fit_cat$save_object(file = file_loc)
pars_interest <- c("mu_tf")
tbl_draws <- fit_cat$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_cat$summary()#variables = pars_interest)

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

plot_distances_to_centers(tbl_cr)
plot_groupmeans_against_session(tbl_cr)
# analysis of the means suggests that there may be an interaction effect
# note, cis are within-participant cis

# random slopes on session seem reasonable
# results from frequentist comparison of ri and rs show that rs are necessary

cr_model_rs <- stan_cr_rs()
mod_cr_rs <- cmdstan_model(cr_model_rs)

cr_model_ri <- stan_cr_ri()
mod_cr_ri <- cmdstan_model(cr_model_ri)

mm_cr <- model.matrix(
  d_closest ~ session + n_categories, data = tbl_cr
) %>% as_tibble()
colnames(mm_cr) <- c("ic", "session", "ncat")
mm_cr$session <- mm_cr$session - .5
mm_cr$ncat <- mm_cr$ncat - .5
mm_cr$ia <- mm_cr$session * mm_cr$ncat

l_data <- list(
  n_data = nrow(tbl_cr),
  n_subj = length(unique(tbl_cr$participant_id)),
  d_closest = scale(sqrt(tbl_cr$d_closest), scale = FALSE)[, 1],
  subj = as.numeric(factor(
    tbl_cr$participant_id, 
    labels = 1:length(unique(tbl_cr$participant_id))
  )),
  x = as.matrix(mm_cr)
)

# random slopes
fit_cr_rs <- mod_cr_rs$sample(
  data = l_data, iter_sampling = 2000, iter_warmup = 1000,
  chains = 1, parallel_chains = 3,
  save_warmup = FALSE
)
file_loc_rs <- str_c("experiments/2022-07-category-learning-II/data/cr-rs-model.RDS")
fit_cr_rs$save_object(file = file_loc_rs, compress = "gzip")

# fit_cr_rs <- readRDS(file_loc_rs)
file_loc_loo_rs <- str_c("experiments/2022-07-category-learning-II/data/cr-rs-loo.RDS")
loo_rs <- fit_cr_rs$loo(variables = "log_lik_pred")
saveRDS(loo_rs, file = file_loc_loo_rs)
# loo_rs <- readRDS(file_loc_loo_rs)

# only random intercept
fit_cr_ri <- mod_cr_ri$sample(
  data = l_data, iter_sampling = 2000, iter_warmup = 1000,
  chains = 1, parallel_chains = 3
)
file_loc_ri <- str_c("experiments/2022-07-category-learning-II/data/cr-ri-model.RDS")
fit_cr_ri$save_object(file = file_loc_ri)

# fit_cr_ri <- readRDS(file_loc_ri)
file_loc_loo_ri <- str_c("experiments/2022-07-category-learning-II/data/cr-ri-loo.RDS")
loo_ri <- fit_cr_ri$loo(variables = "log_lik_pred")
saveRDS(loo_ri, file = file_loc_loo_ri)
# loo_ri <- readRDS(file_loc_loo_ri)

loo::loo_model_weights(list(loo_ri, loo_rs), method = "stacking")


pars_interest <- c("b")
tbl_draws <- fit_cr_rs$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_cr_rs$summary(variables = pars_interest)

params_bf <- c("Intercept", "Timepoint", "Group", "Timepoint x Group")

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
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], nrow = 2, ncol = 2)

# frequentist framework with random intercept and random slopes on session
library(nlme)
m_ri <- lme(
  sqrt(d_closest) ~ session*n_categories, 
  random = ~ 1 | participant_id, data = tbl_cr
)
m_rs <- lme(
  sqrt(d_closest) ~ session*n_categories, 
  random = ~ 1 + session | participant_id, data = tbl_cr
)
anova(m_ri, m_rs)
anova(m_rs)

# stats on mean effects show that there is hardly an effect (n = 119)

# the following plots suggest a different option
# i.e., deltas could represent a mixture between visuo-spatial responses
# and categorical respones
# what should be analyzed is whether the proportion of categorical respones
# differs between groups


plot_mean_deltas(tbl_cr)
tbl_cr_moves <- after_vs_before(tbl_cr)
l_outliers <- extract_movement_outliers(tbl_cr_moves, 1, "Not Transformed")
tbl_outliers <- l_outliers$tbl_outliers
tbl_labels <- l_outliers$tbl_labels
pl_outliers_prior <- plot_movement_outliers(tbl_outliers, tbl_labels)

move_model_mixture <- stan_move_mixture()
move_model_mixture <- cmdstan_model(move_model_mixture)

# make sure tbl_cr_d1 and tbl_cr_moves are ordered in exactly the same way
tbl_cr_d1 <- tbl_cr %>% filter(session == 1)
tbl_cr_d1 <- tbl_cr_d1 %>% arrange(participant_id, trial_id)
tbl_cr_moves <- tbl_cr_moves %>% arrange(participant_id, trial_id)

assert_that(
  sum(
    fct_inorder(factor(tbl_cr_d1$participant_id)) %>% as.numeric() == 
      fct_inorder(factor(tbl_cr_moves$participant_id)) %>% as.numeric()
  ) == nrow(tbl_cr_d1)
)

tbl_participants_lookup <- tibble(
  participant_id = fct_inorder(factor(unique(tbl_cr_moves$participant_id))),
  participant_id_num = as.numeric(fct_inorder(factor(unique(tbl_cr_moves$participant_id))))
)


l_data_move <- list(
  n_data = nrow(tbl_cr_moves),
  n_subj = length(unique(tbl_cr_moves$participant_id)),
  #d1 = tbl_cr_d1$d_closest,
  d_moved = tbl_cr_moves$d_move_abs,
  subj = fct_inorder(factor(tbl_cr_moves$participant_id)) %>% as.numeric()
)

fit_move_mixture <- move_model_mixture$sample(
  data = l_data_move, iter_sampling = 5000, iter_warmup = 2000,
  chains = 4, parallel_chains = 4,
  save_warmup = FALSE
)
file_loc_move_mixture <- str_c("experiments/2022-07-category-learning-II/data/cr-move-mixture-fixed-model.RDS")
fit_move_mixture$save_object(file = file_loc_move_mixture, compress = "gzip")

pars_interest <- "theta"
pars_interest <- c("sigma_subject", "theta", "mg_mn", "mg_sd")
tbl_draws <- fit_move_mixture$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_move_mixture$summary(variables = pars_interest)

params_bf <- c("Intercept", "Timepoint", "Group", "Timepoint x Group")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))


# inspect distribution of movements of participants with high posterior
# proportion of responses from gamma

filter_theta2 <- str_detect(tbl_summary$variable, "theta\\[[0-9]+,2\\]")
tbl_mix <- tbl_summary[filter_theta2, ] %>% arrange(desc(mean))
tbl_mix$participant_id_num <- as.numeric(str_match(tbl_mix$variable, "theta\\[([0-9]+)")[,2])
tbl_mix <- tbl_mix %>% left_join(tbl_participants_lookup, by = "participant_id_num")
p_ids_to_plot <- tbl_mix %>% head(20) %>% select(participant_id)

tbl_cr_moves_posterior <- tbl_cr_moves %>% filter(participant_id %in% p_ids_to_plot$participant_id)


l_outliers_posterior <- extract_movement_outliers(tbl_cr_moves_posterior, 0, "Not Transformed")
pl_outliers_posteriors <- plot_movement_outliers(l_outliers_posterior$tbl_outliers, l_outliers_posterior$tbl_labels)

grid.draw(arrangeGrob(pl_outliers_prior, pl_outliers_posteriors))