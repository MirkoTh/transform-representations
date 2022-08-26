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

# plot mean effects
tbl_cr %>% group_by(n_categories, session) %>%
  summarize(
    d_closest_avg_sqrt = mean(sqrt(d_closest)),
    d_closest_avg_abs = mean(d_closest)
    ) %>%
  mutate(session = factor(session, labels = c("Before Category Learning", "After Category Learning"))) %>%
  pivot_longer(c(d_closest_avg_sqrt, d_closest_avg_abs)) %>%
  ggplot(aes(session, value, group = n_categories)) +
  geom_line(aes(color = n_categories)) +
  geom_point(size = 3, color = "white") +
  geom_point(aes(color = n_categories)) +
  facet_wrap(~ name, scales = "free") +
  scale_color_brewer(name = "Nr. Categories", palette = "Set1") +
  theme_bw() +
  labs(
    x = "Timepoint",
    y = "Distance to Closest Center"
  )

# plot distributions of deltas
tbl_cr %>% group_by(participant_id, session, n_categories) %>%
  summarize(
    d_closest_mn_sqrt = mean(sqrt(d_closest)),
    d_closest_mn_abs = mean(d_closest)
  ) %>%
  group_by(participant_id) %>%
  mutate(
    d_closest_before_sqrt = lag(d_closest_mn_sqrt),
    d_move_sqrt = d_closest_before_sqrt - d_closest_mn_sqrt,
    d_closest_before_abs = lag(d_closest_mn_abs),
    d_move_abs = d_closest_before_abs - d_closest_mn_abs
  ) %>%
  ungroup() %>%
  mutate(
    n_categories = factor(n_categories, labels = c("Similarity Judgment", "4 Categories"))
  ) %>%
  dplyr::filter(!is.na(d_closest_before_abs)) %>%
  pivot_longer(c(d_move_sqrt, d_move_abs)) %>% 
  ggplot(aes(value)) +
  geom_histogram(bins = 60, fill = "#66CCFF", color = "white") + # "dodgerblue"
  geom_vline(xintercept = 0, color = "darkred", size = 1, linetype = "dashed") +
  facet_wrap(name ~ n_categories, scales = "free_x") +
  theme_bw() +
  labs(x = "Movement Towards Center", y = "Nr. Participants")

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
  chains = 3, parallel_chains = 3,
  save_warmup = FALSE
)
# file_loc_rs <- str_c("experiments/2022-07-category-learning-II/data/cr-rs-model.RDS")
# fit_cr_rs$save_object(file = file_loc_rs, compress = "gzip")

# fit_cr_rs <- readRDS(file_loc_rs)
# file_loc_loo_rs <- str_c("experiments/2022-07-category-learning-II/data/cr-rs-loo.RDS")
# loo_rs <- fit_cr_rs$loo(variables = "log_lik_pred")
# saveRDS(loo_rs, file = file_loc_loo_rs)
# loo_rs <- readRDS(file_loc_loo_rs)

# only random intercept
fit_cr_ri <- mod_cr_ri$sample(
  data = l_data, iter_sampling = 10000, iter_warmup = 2000,
  chains = 3, parallel_chains = 3
)
# file_loc_ri <- str_c("experiments/2022-07-category-learning-II/data/cr-ri-model.RDS")
# fit_cr_ri$save_object(file = file_loc_ri)

# fit_cr_ri <- readRDS(file_loc_ri)
# file_loc_loo_ri <- str_c("experiments/2022-07-category-learning-II/data/cr-ri-loo.RDS")
# loo_ri <- fit_cr_ri$loo(variables = "log_lik_pred")
# saveRDS(loo_ri, file = file_loc_loo_ri)
# loo_ri <- readRDS(file_loc_loo_ri)

# loo::loo_model_weights(list(loo_ri, loo_rs), method = "stacking")


pars_interest <- c("mu_tf")
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
map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)

# without random slopes on session, only a random intercept, BF for ia is decisive
install.packages("BayesFactor")
library(BayesFactor)
r = 1/2;
df_bf <- as.data.frame(tbl_cr)
df_bf$n_categories <- as.factor(df_bf$n_categories)

bfall <- anovaBF(d_closest ~ n_categories*session + participant_id, data = df_bf, whichRandom="participant_id",
                 rscaleFixed=r, whichModel="all", iterations=50000, progress=TRUE)
which(bfall@bayesFactor$bf==max(bfall@bayesFactor$bf))

winner <- lmBF(d_closest ~ session + n_categories:session + participant_id, data = df_bf, whichRandom="participant_id",
               rscaleFixed=r, iterations=50000, progress=TRUE)
min_ia <- lmBF(d_closest ~ session + participant_id, data = df_bf, whichRandom="participant_id",
               rscaleFixed=r, iterations=50000, progress=TRUE)
winner/min_ia

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
