rm(list = ls())

library(tidyverse)
library(grid)
library(gridExtra)
library(furrr)

home_grown <- c("R/utils.R")
walk(home_grown, source)


tbl_secondary <- read_csv(
  file = "experiments/2022-07-category-learning-II/data/secondary-task.csv"
) %>% rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id) %>%
  filter(n_categories == 4)

tbl_start <- tbl_secondary %>% filter(trial_id < 100)
l_start <- tbl_start %>% split(.$participant_id)
tbl_end <- tbl_secondary %>% filter(trial_id >= 300)
l_end <- tbl_end %>% split(.$participant_id)


plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))

l_start_results <- future_map(
  l_start, safely(fit_gcm_one_participant), 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)

l_end_results <- future_map(
  l_end, safely(fit_gcm_one_participant), 
  .progress = TRUE, .options = furrr_options(seed = TRUE)
)


tbl_params_start <- post_process_gcm_fits(l_start_results) %>% as_tibble() %>% mutate(t = "Start")
rownames(tbl_params_start) <- NULL
tbl_params_end <- post_process_gcm_fits(l_end_results) %>% as_tibble() %>% mutate(t = "End")
rownames(tbl_params_end) <- NULL

tbl_params_both <- rbind(tbl_params_start, tbl_params_end)

ggplot(tbl_params_both, aes(w, group = t)) +
  geom_density(aes(color = t))




