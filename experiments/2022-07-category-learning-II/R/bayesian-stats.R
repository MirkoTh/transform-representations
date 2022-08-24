# Import Packages ---------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(docstring)
library(rutils)
library(cmdstanr)
library(brms)


# Import Home-Grown Modules -----------------------------------------------

files <- c(
  "R/utils.R",
  "R/plotting.R",
  "R/analysis-utils.R",
  "R/analysis-plotting.R",
  "R/summarySEwithin.R",
  "R/summarySE.R",
  "R/normDataWithin.R"
)
walk(files, source)


# Load Data ---------------------------------------------------------------

tbl_cr <- read_rds("experiments/2022-07-category-learning-II/data/2022-08-24-tbl_cr-treps-long-ri.rds")
tbl_cat <- read_rds("experiments/2022-07-category-learning-II/data/2022-08-24-tbl_cat-treps-long-ri.rds")
tbl_sim <- read_rds("experiments/2022-07-category-learning-II/data/2022-08-24-tbl_sim-treps-long-ri.rds")


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




