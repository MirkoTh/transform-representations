rm(list = ls())
# Import Packages ---------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(docstring)
library(rutils)
library(cmdstanr)
library(kde1d)
conflicted::conflict_prefer("select", "dplyr", "MASS")
conflicted::conflict_prefer("filter", "dplyr", "stats")
conflicted::conflict_prefer("lag", "dplyr", "stats")
conflicted::conflict_prefer("count", "dplyr", "plyr")
conflicted::conflict_prefer("arrange", "dplyr", "plyr")
conflicted::conflict_prefer("summarize", "dplyr", "plyr")
conflicted::conflict_prefer("mutate", "dplyr", "plyr")
conflicted::conflict_prefer("desc", "dplyr", "plyr")
conflicted::conflict_prefer("rename", "dplyr", "plyr")




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

tbl_cr <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri-psychological-representation.rds")
tbl_cat <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri-psychological-representation.rds")
tbl_sim <- read_rds("experiments/2022-07-category-learning-II/data/tbl_sim-treps-long-ri-psychological-representation.rds")
tbl_moves_agg <- read_csv("experiments/2022-07-category-learning-II/data/movements-catacc-gt.csv")

# sqrt tf on d_closest
tbl_cr$d_closest_sqrt <- sqrt(tbl_cr$d_closest)
tbl_cr_moves <- after_vs_before(tbl_cr)
tbl_cr_moves$participant_id_num <- as.numeric(fct_inorder(factor(tbl_cr_moves$participant_id)))

tbl_participants_lookup <- tbl_cr_moves %>% group_by(participant_id, participant_id_num, n_categories) %>%
  count() %>% ungroup() %>% select(-n)


is_fit <- FALSE


# Category Learning -------------------------------------------------------


tbl_cat_binned <- tbl_cat %>% 
  group_by(participant_id, trial_id_binned) %>%
  summarize(n_trials = n(), n_correct = sum(accuracy))

tbl_cat_binned %>% group_by(trial_id_binned) %>% summarize(sum(n_correct)/sum(n_trials))

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
file_loc <- str_c("experiments/2022-07-category-learning-II/data/cat-model-posterior.RDS")
pars_interest <- c("mu_tf")

if (is_fit) {
  fit_cat <- mod_cat$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  tbl_draws <- fit_cat$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc)
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
}

tbl_summary <- fit_cat$summary()#variables = pars_interest)
tbl_summary %>% arrange(desc(rhat))

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


pl_hdi_cat <- plot_map_hdi_bf(tbl_thx, bfs, "Category Learning")






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

file_loc <- str_c("experiments/2022-07-category-learning-II/data/sim-model-posterior.RDS")
pars_interest <- c("mu_tf")

if (is_fit) {
  fit_sim <- mod_sim$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  tbl_draws <- fit_sim$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc)
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
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


pl_hdi_seq <- plot_map_hdi_bf(tbl_thx, bfs, "Seq. Comparison")


# Movements Towards Centers -----------------------------------------------



plot_distances_to_centers(tbl_cr)

pl_groupmeans <- plot_groupmeans_against_session(
  tbl_cr %>% mutate(n_categories = factor(n_categories, labels = c("Similarity Judgment", "Category Learning")))
) + theme(legend.position = "bottom") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(.02, .02)))
save_my_tiff(pl_groupmeans, "experiments/2022-07-category-learning-II/data/means-sqrt-tf.tiff", 5, 4)

# analysis of the means suggests that there may be an interaction effect
# note, cis are within-participant cis


# Mean Effects ------------------------------------------------------------


# random slopes on session seem reasonable
# results from frequentist comparison of ri and rs show that rs are necessary

cr_model_rs <- stan_cr_rs()
mod_cr_rs <- cmdstan_model(cr_model_rs)

cr_model_ri <- stan_cr_ri()
mod_cr_ri <- cmdstan_model(cr_model_ri)

mm_cr <- model.matrix(
  d_closest ~ as.factor(session) + as.factor(n_categories), data = tbl_cr
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

pars_interest <- c("b", "mu")
file_loc_rs <- str_c("experiments/2022-07-category-learning-II/data/center-rs-model-posterior.RDS")
file_loc_loo_rs <- str_c("experiments/2022-07-category-learning-II/data/center-rs-loo.RDS")
file_loc_ri <- str_c("experiments/2022-07-category-learning-II/data/center-ri-model-posterior.RDS")
file_loc_loo_ri <- str_c("experiments/2022-07-category-learning-II/data/center-ri-loo.RDS")

if (is_fit) {
  # random slopes
  fit_cr_rs <- mod_cr_rs$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  tbl_draws <- fit_cr_rs$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc_rs)
  
  loo_rs <- fit_cr_rs$loo(variables = "log_lik_pred")
  saveRDS(loo_rs, file = file_loc_loo_rs)
  
  # only random intercept
  fit_cr_ri <- mod_cr_ri$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3
  )
  tbl_draws <- fit_cr_ri$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc_ri)
  
  loo_ri <- fit_cr_ri$loo(variables = "log_lik_pred")
  saveRDS(loo_ri, file = file_loc_loo_ri)
} else if (!is_fit) {
  tbl_draws_rs <- readRDS(file_loc_rs)
  tbl_draws_ri <- readRDS(file_loc_ri)
  loo_rs <- readRDS(file_loc_loo_rs)
  loo_ri <- readRDS(file_loc_loo_ri)
}




loo::loo_model_weights(list(loo_ri, loo_rs), method = "stacking")
tbl_draws <- tbl_draws_rs

tbl_summary <- fit_cr_rs$summary(variables = pars_interest)
tbl_summary %>% arrange(desc(rhat))

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


pl_hdi_move <- plot_map_hdi_bf(tbl_thx, bfs, "Move: Cat. Center")


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




# Normal-Gamma Mixture Modeling -------------------------------------------


# the following plots suggest a different option
# i.e., deltas could represent a mixture between visuo-spatial responses
# and categorical respones
# what should be analyzed is whether the proportion of categorical respones
# differs between groups

l_out <- plot_mean_deltas(tbl_cr)
l_out[[2]]
tbl_cr_move_agg <- l_out[[1]]
l_outliers <- extract_movement_outliers(tbl_cr_moves, 2, "Not Transformed")
tbl_outliers <- l_outliers$tbl_outliers
tbl_labels <- l_outliers$tbl_labels
pl_outliers_prior <- plot_movement_outliers(
  tbl_outliers, 
  tbl_labels, 
  "Outliers above/below 2 sds of the Mean"
)
pl_outliers_prior


# make sure tbl_cr_d1 and tbl_cr_moves are ordered in exactly the same way
tbl_cr_d1 <- tbl_cr %>% filter(session == 1)
tbl_cr_d1 <- tbl_cr_d1 %>% arrange(participant_id, trial_id)
tbl_cr_moves <- tbl_cr_moves %>% arrange(participant_id, trial_id)

assertthat::assert_that(
  sum(
    fct_inorder(factor(tbl_cr_d1$participant_id)) %>% as.numeric() == 
      fct_inorder(factor(tbl_cr_moves$participant_id)) %>% as.numeric()
  ) == nrow(tbl_cr_d1)
)


# story line
# 1. analysis of means shows that there is a stronger tendency in the experimental group to be attracted by category means than in the control group
# 2. stats of this analysis show that the evidence is rather in favor of the Null hypothesis that there is an effect
# 3. closer look at by-participant distributions of movements suggests a different picture: some responses are attracted by the centers, but most are not
# 4. this is implemented in the mixture model (, which fits better than the model predicting a mean shift using a Gaussian distribution)
# 5. comparing the group-level parameters of probability responding categorically does not show any group effect
# 6. explanation of this general trend could be that participants break down the continuous response scale into a categorical one;
# a speed up in rts is at least consistent with this idea


plot_group_rts_against_session(tbl_cr) + theme(legend.position = "bottom") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(.02, .02)))


l_data_mixture_groups <- list(
  n_data = nrow(tbl_cr_moves),
  n_subj = length(unique(tbl_cr_moves$participant_id)),
  n_groups = length(unique(tbl_cr_moves$n_categories)),
  d_moved = tbl_cr_moves$d_move_abs,
  subj = tbl_cr_moves$participant_id_num,
  group = as.numeric(tbl_participants_lookup$n_categories)
)


move_model_mixture_group <- stan_move_mixture_groups()
move_model_mixture_group <- cmdstan_model(move_model_mixture_group)

file_loc_mixture_groups <- str_c(
  "experiments/2022-07-category-learning-II/data/cr-move-mixture-fixed-groups-posterior.RDS"
)
file_loc_loo_mixture_group <- str_c(
  "experiments/2022-07-category-learning-II/data/mixture-group-loo.RDS")

pars_interest <- "theta"
pars_interest <- c("sigma_subject", "theta", "mu_theta", "mg_mn", "mg_sd", "posterior_prediction")

if (is_fit) {
  fit_move_mixture <- move_model_mixture_group$sample(
    data = l_data_mixture_groups, iter_sampling = 2000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  tbl_draws <- fit_move_mixture$draws(variables = pars_interest, format = "df")
  tbl_draws$theta_meandiff <- tbl_draws$`mu_theta[1]` - tbl_draws$`mu_theta[2]`
  inv_logit <- function(x) exp(x)/(exp(x) + 1)
  tbl_draws$theta_cat_prob <- inv_logit(tbl_draws$`mu_theta[2]`)
  tbl_draws$theta_sim_prob <- inv_logit(tbl_draws$`mu_theta[1]`)
  tbl_draws$theta_meandiff_prob <- inv_logit(tbl_draws$`mu_theta[1]`) - inv_logit(tbl_draws$`mu_theta[2]`)
  tbl_summary <- fit_move_mixture$summary(variables = pars_interest)
  tbl_summary %>% arrange(desc(rhat))
  tbl_summary %>% arrange(rhat)
  saveRDS(tbl_draws, file_loc_mixture_groups)
  
  
  loo_mixture_group <- fit_move_mixture$loo(variables = "log_lik_pred")
  saveRDS(loo_mixture_group, file = file_loc_loo_mixture_group)
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc_mixture_groups)
  loo_mixture_group <- readRDS(file_loc_loo_mixture_group)
}



# in probability space
pl_prop_gamma <- tbl_draws %>% dplyr::select(c(theta_cat_prob, theta_sim_prob)) %>%
  mutate(theta_diff = theta_cat_prob - theta_sim_prob) %>%
  pivot_longer(c(theta_cat_prob, theta_sim_prob, theta_diff)) %>%
  mutate(
    name = fct_inorder(factor(name)),
    name = factor(
      name, 
      labels = c("4 Categories", "Similarity", "Difference"))
  ) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "white", fill = "#440154", aes(y = ..density..)) +
  facet_wrap(~ name) +
  theme_bw() +
  theme(
    strip.background =element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom",
    text = element_text(size = 16)
  ) +
  labs(x = "Proportion Categorical", y = "Posterior Density") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0))
save_my_pdf_and_tiff(pl_prop_gamma, "experiments/2022-07-category-learning-II/data/figures/prop-gamma-e2", 8, 3)
save_my_pdf_and_tiff(pl_prop_gamma, "figures/prop-gamma-e2", 8, 3)
save_my_pdf_and_tiff(pl_prop_gamma, "figures/figures-ms/prop-gamma-e2", 8, 3)



params_bf <- "Group Difference Theta"
tbl_posterior <- tbl_draws %>% 
  dplyr::select(theta_meandiff, .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("theta")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = "Group Difference Theta")
l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
l_pl[[1]]

tbl_mix <- tbl_summary %>% filter(str_starts(variable, "theta")) %>% arrange(desc(mean))
tbl_mix$participant_id_num <- as.numeric(str_match(tbl_mix$variable, "theta\\[([0-9]+)")[,2])
tbl_mix <- tbl_mix %>% left_join(tbl_participants_lookup, by = "participant_id_num")


# p_ids_to_plot <- tbl_mix %>% head(20) %>% select(participant_id)
# p_ids_to_plot <- tbl_mix %>% head(10) %>% rbind(tbl_mix %>% tail(10)) %>% select(participant_id)
# map_prop_gamma <- tbl_mix %>% head(10) %>% rbind(tbl_mix %>% tail(10)) %>%
#   select(mean, participant_id, n_categories) %>%
#   mutate(participant_id = str_c(substr(participant_id, 1, 6), ", ", n_categories))
n_ps <- 3
# p_ids_to_plot <- sample(tbl_mix$participant_id, n_ps)
p_ids_to_plot <- c( "7903d5946cfbb088dad0bdc08e7f3c5b", "1e649ff8b7bbc5f1eeaf928d44353401", "cb9741361e5b34be202b686d9a2ec3a1")

map_prop_gamma <- tbl_mix %>% filter(participant_id %in% p_ids_to_plot) %>%
  select(mean, participant_id, n_categories) %>%
  mutate(
    participant_id = str_c(substr(participant_id, 1, 6), ", ", n_categories),
  )

tbl_cr_moves_posterior <- tbl_cr_moves %>% filter(participant_id %in% p_ids_to_plot) #$participant_id
tbl_cr_moves_posterior$n_categories <- fct_relevel(tbl_cr_moves_posterior$n_categories, "Similarity", after = 1)

l_outliers_posterior <- extract_movement_outliers(tbl_cr_moves_posterior, 0, "Not Transformed")
pl_outliers_posteriors <- plot_movement_outliers(
  l_outliers_posterior$tbl_outliers, 
  l_outliers_posterior$tbl_labels %>% left_join(map_prop_gamma, by = c("participant_id")), 
  "Three Individual Participants", 
  nrcols = 5, as_outlier = FALSE
) + theme(
  strip.background =element_rect(fill="white"), 
  strip.text = element_text(colour = 'black'), 
  legend.position = "bottom",
  text = element_text(size = 16)
)
pl_outliers_posteriors

save_my_pdf_and_tiff(pl_outliers_posteriors, "experiments/2022-07-category-learning-II/data/figures/random-3-mixture", 6, 3.5)

l_combined <- combine_data_with_posterior_outliers(tbl_mix, tbl_cr_moves, tbl_draws, 94)
tbl_empirical <- l_combined$tbl_empirical
tbl_post_preds <- l_combined$tbl_post_preds
levels(tbl_empirical$n_categories) <- c("Similarity", "4 Categories")
tbl_empirical$n_categories <- fct_relevel(tbl_empirical$n_categories, "Similarity", after = 1)
pl_pp_mixture <- plot_predictions_with_data_mixture(tbl_empirical, tbl_post_preds, facet_by = "group") +
  ggtitle("Normal-Gamma Mixture") +
  theme(
    strip.background = element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom",
    text = element_text(size = 16)
  )



# Normal Model with Different Group Means ---------------------------------



move_model_shift_normal <- stan_move_shift_normal()
move_model_shift_normal <- cmdstan_model(move_model_shift_normal)
file_loc_move_shift_normal <- str_c(
  "experiments/2022-07-category-learning-II/data/cr-move-shift-normal-model-posterior.RDS"
)
file_loc_loo_move_shift_normal <- str_c(
  "experiments/2022-07-category-learning-II/data/move-shift-normal-loo.RDS")

pars_interest <- c("sigma_subject", "mu", "posterior_prediction") # 

if (is_fit) {
  fit_move_shift_normal <- move_model_shift_normal$sample(
    data = l_data_mixture_groups, iter_sampling = 2000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  loo_move_shift_normal <- fit_move_shift_normal$loo(variables = "log_lik_pred")
  saveRDS(loo_move_shift_normal, file = file_loc_loo_move_shift_normal)
  tbl_draws <- fit_move_shift_normal$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_move_shift_normal$summary(variables = pars_interest)
  saveRDS(tbl_draws, file_loc_move_shift_normal)
} else if (!is_fit) {
  loo_move_shift_normal <- readRDS(file_loc_loo_move_shift_normal)
  tbl_draws <- readRDS(file_loc_move_shift_normal)
  
}

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

plot_map_hdi_bf(tbl_thx, bfs, "MAP, 95% HDIs, & BFs")

tbl_cr_moves$n_categories <- fct_relevel(tbl_cr_moves$n_categories, "Similarity", after = 1)
l_combined <- combine_data_with_posterior_outliers(tbl_mix, tbl_cr_moves, tbl_draws, 94)
tbl_empirical <- l_combined$tbl_empirical
tbl_post_preds <- l_combined$tbl_post_preds
pl_pp_shift <- plot_predictions_with_data_mixture(tbl_empirical, tbl_post_preds, facet_by = "group") +
  ggtitle("Shift Normal") +
  theme(
    strip.background =element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom",
    text = element_text(size = 16)
  )

# only predictions
save_my_pdf_and_tiff(
  arrangeGrob(
    pl_pp_shift + theme(legend.position = "none") + scale_fill_viridis_d() + 
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = expansion(add = c(0, .002))),
    pl_pp_mixture + theme(legend.position = "none") + scale_fill_viridis_d() + 
      scale_x_continuous(expand = c(0, 0)) + 
      scale_y_continuous(expand = expansion(add = c(0, .002))),
    nrow = 1, ncol = 2), 
  "experiments/2022-07-category-learning-II/data/posterior-predictions-comparison",
  7.5, 3.5
)

# predictions and a few outliers
pl_ppred <- arrangeGrob(
  pl_pp_shift + theme(legend.position = "none") + scale_fill_viridis_d() + 
    scale_x_continuous(expand = c(.1, .1)) + 
    scale_y_continuous(expand = expansion(add = c(0, .002))),
  pl_pp_mixture + theme(legend.position = "none") + scale_fill_viridis_d() + 
    scale_x_continuous(expand = c(.1, .1)) + 
    scale_y_continuous(expand = expansion(add = c(0, .002))),
  nrow = 1, ncol = 2)
pl_poutliers <- pl_outliers_posteriors + coord_cartesian(xlim = c(-60, 110)) + 
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(add = c(0, .002))) +
  theme(
    strip.background =element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom"
  )

pl_all <- arrangeGrob(
  # pl_psychonomics_means + ggtitle("Empirical Means") + 
  #   theme(plot.title = element_text(size = 14, face = "plain")), 
  pl_ppred, 
  pl_poutliers, 
  nrow = 1
)
save_my_pdf_and_tiff(
  pl_all, 
  "experiments/2022-07-category-learning-II/data/figures/posterior-predictions-and-outliers-e2",
  20, 3.75
)
save_my_pdf_and_tiff(
  pl_all, 
  "figures/posterior-predictions-and-outliers-e2",
  20, 3.75
)



l_combined <- combine_data_with_posterior_outliers(tbl_mix, tbl_cr_moves, tbl_draws, 94)
tbl_empirical <- l_combined$tbl_empirical
tbl_post_preds <- l_combined$tbl_post_preds

plot_predictions_with_data_mixture(tbl_empirical, tbl_post_preds)

plot_predictions_with_data_mixture(tbl_empirical, tbl_post_preds, facet_by = "group") +
  ggtitle("Normal-Gamma Mixture")


# Ex-Gaussian -------------------------------------------------------------

l_data_exgaussian <- list(
  n_data = nrow(tbl_cr_moves),
  n_subj = length(unique(tbl_cr_moves$participant_id)),
  n_groups = length(unique(tbl_cr_moves$n_categories)),
  d_moved = tbl_cr_moves$d_move_abs,
  #d_moved = d_moved,
  subj = tbl_cr_moves$participant_id_num,
  group = as.numeric(tbl_participants_lookup$n_categories)
)


move_model_exgaussian <- stan_move_exgaussian()
move_model_exgaussian <- cmdstan_model(move_model_exgaussian)


file_loc_exgaussian <- str_c(
  "experiments/2022-07-category-learning-II/data/exgaussian-posterior.RDS"
)
file_loc_loo_exgaussian <- str_c(
  "experiments/2022-07-category-learning-II/data/exgaussian-loo.RDS"
)
pars_interest <- c("mu_tau", "sigma_tau", "sigma_subject", "tau_subject")

if (is_fit) {
  fit_move_exgaussian <- move_model_exgaussian$sample(
    data = l_data_exgaussian, iter_sampling = 2000, iter_warmup = 1000,
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE #, init = init_fun
  )
  
  tbl_draws <- fit_move_exgaussian$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_move_exgaussian$summary(variables = pars_interest)
  tbl_summary %>% arrange(desc(rhat))
  tbl_summary %>% arrange(rhat)
  saveRDS(tbl_draws, file_loc_exgaussian)
  
  loo_exgaussian <- fit_move_exgaussian$loo(variables = "log_lik_pred")
  saveRDS(loo_exgaussian, file_loc_loo_exgaussian)
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc_exgaussian)
  loo_exgaussian <- readRDS(file_loc_loo_exgaussian)
}


params_bf <- c("mu_tau_diff", "mu_tau[1]", "mu_tau[2]") #, "sigma_subject")

tbl_posterior <- tbl_draws %>% 
  # select(c("sigma", "tau", ".chain")) %>%
  mutate(
    `mu_tau[1]` = 1/`mu_tau[1]`,
    `mu_tau[2]` = 1/`mu_tau[2]`,
    mu_tau_diff = `mu_tau[1]` - `mu_tau[2]`
  ) %>%
  dplyr::select(starts_with(params_bf), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(params_bf), names_to = "parameter", values_to = "value") %>%
  # pivot_longer(c("sigma", "tau"), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter))

#ggplot(tbl_posterior, aes(value)) + geom_histogram() + facet_wrap(~ parameter, scales = "free_x")

kdes <- estimate_kd(tbl_posterior, params_bf)
par_lims <- limit_axes(kdes)
bfs <- kdes %>% map_dbl(~(dnorm(0, 0, 1))/dkde1d(0, .))
tbl_thx <- par_lims

# plot the posteriors and the bfs
map(as.list(params_bf[c(1, 2, 3)]), plot_posterior, tbl_posterior, tbl_thx, bfs)

tbl_thx$parameter <- factor(tbl_thx$parameter, labels = c("mu(Sim.J.)", "mu(Cat.L.)", "mu Diff."))
names(bfs) <- c("mu(Sim.J.)", "mu(Cat.L.)", "mu Diff.")

pl_hdi_exg <- plot_map_hdi_bf(tbl_thx, bfs, "ExGaussian")





loo::loo_model_weights(
  list(loo_move_shift_normal, loo_exgaussian), #, loo_mixture_group
  method = "stacking"
)

tbl_posterior_tau <- tbl_draws %>% 
  dplyr::select(starts_with("tau_subject"), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with("tau_subject"), names_to = "parameter", values_to = "value") %>%
  mutate(
    parameter = factor(parameter),
    participant_id_num = as.numeric(str_extract(parameter, "[0-9]+"))
  ) %>% left_join(
    tbl_participants_lookup %>% select(-participant_id), by = "participant_id_num"
  ) %>% group_by(participant_id_num, n_categories) %>%
  summarize(mn_shift = mean(1/value)) %>% ungroup()

tbl_empirical_posterior <- tbl_cr_move_agg %>% 
  filter(name == "Not Transformed") %>%
  left_join(tbl_participants_lookup %>% select(-n_categories), by = "participant_id") %>%
  left_join(tbl_posterior_tau %>% select(-n_categories), by = "participant_id_num")

pl_bivar_1 <- ggplot(tbl_empirical_posterior %>% filter(n_categories == "Similarity Judgment"), aes(value, mn_shift)) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "tomato3", linetype = "dotdash", linewidth = .75) +
  geom_hline(yintercept = 0, color = "tomato3", linetype = "dotdash", linewidth = .75) +
  geom_point(shape = 1) +
  #facet_wrap(~ n_categories) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  coord_cartesian(xlim = c(-.5, 1.75)) +
  labs(x = "Mean Move (Empirical)", y = "MAP Tau (Posterior)", title = "Sequential Comparison") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  )

pl_bivar_2 <- ggplot(tbl_empirical_posterior %>% filter(n_categories == "4 Categories"), aes(value, mn_shift)) +
  theme_bw() +
  geom_vline(xintercept = 0, color = "tomato3", linetype = "dotdash", linewidth = .75) +
  geom_hline(yintercept = 0, color = "tomato3", linetype = "dotdash", linewidth = .75) +
  geom_point(shape = 1) +
  #facet_wrap(~ n_categories) +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  coord_cartesian(xlim = c(-.5, 1.75)) +
  labs(x = "Mean Move (Empirical)", y = "MAP Tau (Posterior)", title = "Category Learning") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  )

ggM1 <- ggExtra::ggMarginal(pl_bivar_1, type = "histogram", bins = 20, fill = "#3399FF")
ggM2 <- ggExtra::ggMarginal(pl_bivar_2, type = "histogram", bins = 20, fill = "#3399FF")

pl_gg_both <- arrangeGrob(ggM1, ggM2, ncol = 1)
save_my_pdf_and_tiff(
  pl_gg_both, "figures/figures-ms/exGaussian-bivariates", 6, 11
)
save_my_pdf_and_tiff(pl_hdi_exg, "figures/figures-ms/exGaussian-bf-hdi", 5, 4)

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

move_model <- stan_move_by_success_square()
mod_move <- cmdstan_model(move_model)

# use final accuracy
mm_move_finalacc <- model.matrix(
  movement ~ mean_accuracy, data = tbl_moves_agg
) %>% as_tibble()
mm_move_finalacc[, 2] <- scale(mm_move_finalacc[, 2], scale = FALSE)[, 1]

l_data <- list(
  n_data = nrow(tbl_moves_agg),
  response = tbl_moves_agg$movement,
  x = as.matrix(mm_move_finalacc)
)

fit_move_finalacc <- mod_move$sample(
  data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
)

pars_interest <- c("mu")
tbl_draws <- fit_move_finalacc$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_move_finalacc$summary(variables = pars_interest)


params_bf <- c("Intercept", "Final Accuracy")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))


library(kde1d)
kdes <- estimate_kd(tbl_posterior, params_bf)
par_lims <- limit_axes(kdes)
bfs <- kdes %>% map_dbl(~(dnorm(0, 0, 1))/dkde1d(0, .))
tbl_thx <- par_lims


# plot the posteriors and the bfs
map(as.list(params_bf[c(1, 2)]), plot_posterior, tbl_posterior, tbl_thx, bfs)


pl_hdi_finacc <- plot_map_hdi_bf(tbl_thx, bfs, "ID: Final Accuracy")


# use delta in categorization accuracy
mm_move_deltaacc <- model.matrix(
  movement ~ mean_delta_accuracy, data = tbl_moves_agg
) %>% as_tibble()
mm_move_deltaacc[, 2] <- scale(mm_move_finalacc[, 2], scale = FALSE)[, 1]

l_data <- list(
  n_data = nrow(tbl_moves_agg),
  response = tbl_moves_agg$movement,
  x = as.matrix(mm_move_deltaacc)
)

fit_move_deltaacc <- mod_move$sample(
  data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
)


pars_interest <- c("mu")
tbl_draws <- fit_move_deltaacc$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_move_deltaacc$summary(variables = pars_interest)


params_bf <- c("Intercept", "Delta Accuracy")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu")), .chain) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with(c("mu")), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = params_bf))

kdes <- estimate_kd(tbl_posterior, params_bf)
par_lims <- limit_axes(kdes)
bfs <- kdes %>% map_dbl(~(dnorm(0, 0, 1))/dkde1d(0, .))
tbl_thx <- par_lims

# plot the posteriors and the bfs
map(as.list(params_bf[c(1, 2)]), plot_posterior, tbl_posterior, tbl_thx, bfs)


pl_hdi_deltaacc <- plot_map_hdi_bf(tbl_thx, bfs, "ID: Delta Accuracy")



# Boundary Aversion -------------------------------------------------------

plot_groupmeans_against_session(
  tbl_cr %>% 
    mutate(
      n_categories = factor(
        n_categories, labels = c("Similarity J.", "Category L.")
      ),
      d_closest_sqrt = sqrt(d_boundary)),
  yttl = "Distance to Border"
) + theme(legend.position = "bottom")


# same models as for centers, just plug-in different DV

aversion_model_rs <- stan_cr_rs()
mod_aversion_rs <- cmdstan_model(aversion_model_rs)

aversion_model_ri <- stan_cr_ri()
mod_aversion_ri <- cmdstan_model(aversion_model_ri)

mm_cr <- model.matrix(
  d_boundary ~ as.factor(session) + as.factor(n_categories), data = tbl_cr
) %>% as_tibble()
colnames(mm_cr) <- c("ic", "session", "ncat")
mm_cr$session <- mm_cr$session - .5
mm_cr$ncat <- mm_cr$ncat - .5
mm_cr$ia <- mm_cr$session * mm_cr$ncat

l_data <- list(
  n_data = nrow(tbl_cr),
  n_subj = length(unique(tbl_cr$participant_id)),
  # d_boundary goes under d_closest because same model as above
  d_closest = scale(sqrt(tbl_cr$d_boundary), scale = FALSE)[, 1],
  subj = as.numeric(factor(
    tbl_cr$participant_id, 
    labels = 1:length(unique(tbl_cr$participant_id))
  )),
  x = as.matrix(mm_cr)
)


file_loc_rs <- str_c("experiments/2022-07-category-learning-II/data/aversion-rs-model-posterior.RDS")
file_loc_loo_rs <- str_c("experiments/2022-07-category-learning-II/data/aversion-rs-loo.RDS")
file_loc_ri <- str_c("experiments/2022-07-category-learning-II/data/aversion-ri-model-posterior.RDS")
file_loc_loo_ri <- str_c("experiments/2022-07-category-learning-II/data/aversion-ri-loo.RDS")
pars_interest <- c("b", "mu")

if (is_fit) {
  # random slopes
  fit_aversion_rs <- mod_aversion_rs$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000, # 2000, 1000
    chains = 3, parallel_chains = 3,
    save_warmup = FALSE
  )
  tbl_draws_rs <- fit_aversion_rs$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws_rs, file_loc_rs)
  
  tbl_summary_rs <- fit_aversion_rs$summary(variables = pars_interest)
  
  loo_rs_aversion <- fit_aversion_rs$loo(variables = "log_lik_pred")
  saveRDS(loo_rs_aversion, file = file_loc_loo_rs)
  
  # only random intercept
  fit_aversion_ri <- mod_aversion_ri$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000, # 2000, 1000
    chains = 3, parallel_chains = 3
  )
  tbl_draws_ri <- fit_aversion_rs$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws_ri, file_loc_ri)
  
  tbl_summary_ri <- fit_aversion_rs$summary(variables = pars_interest)
  
  loo_ri_aversion <- fit_aversion_ri$loo(variables = "log_lik_pred")
  saveRDS(loo_ri_aversion, file = file_loc_loo_ri)
  
} else if (!is_fit) {
  tbl_draws_rs <- read_rds(file_loc_rs)
  tbl_draws_ri <- read_rds(file_loc_ri)
  loo_rs_aversion <- read_rds(file_loc_loo_rs)
  loo_ri_aversion <- read_rds(file_loc_loo_ri)
  
}


loo::loo_model_weights(list(loo_ri_aversion, loo_rs_aversion), method = "stacking")

# rs again wins, so focus on that model

params_bf <- c("Intercept", "Timepoint", "Group", "Timepoint x Group")

tbl_posterior <- tbl_draws_rs %>% 
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

pl_hdi_aversion <- plot_map_hdi_bf(tbl_thx, bfs, "Move: Boundary Aversion")



pl_hdi_all <- arrangeGrob(
  pl_hdi_cat, pl_hdi_seq,
  pl_hdi_move, pl_hdi_aversion,
  pl_hdi_exg,
  pl_hdi_finacc, pl_hdi_deltaacc,
  ncol = 2
)
save_my_pdf_and_tiff(pl_hdi_all, "figures/figures-ms/all-hdi-e2", 15, 16)
