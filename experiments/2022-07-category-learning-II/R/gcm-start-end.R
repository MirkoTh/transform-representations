rm(list = ls())

library(tidyverse)
library(grid)
library(gridExtra)
library(furrr)

home_grown <- c("R/utils.R", "R\analysis-plotting.R")
walk(home_grown, source)


tbl_secondary <- readRDS(
  file = "experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri.rds"
) %>% rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id)

tbl_start <- tbl_secondary %>% filter(trial_id < 100) #100
l_start <- tbl_start %>% split(.$participant_id)
tbl_end <- tbl_secondary %>% filter(trial_id >= 300) #300
l_end <- tbl_end %>% split(.$participant_id)

is_fit <- TRUE


if (is_fit) {
  plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
  
  # initial trials
  l_start_results <- future_map(
    l_start, safely(fit_gcm_one_participant), 
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
  saveRDS(l_start_results, file = "data/gcm-start.rds")
  
  # final trials
  l_end_results <- future_map(
    l_end, safely(fit_gcm_one_participant), 
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
  saveRDS(l_end_results, file = "data/gcm-end.rds")
  
} else {
  l_start_results <- readRDS("data/gcm-start.rds")
  l_end_results <- readRDS("data/gcm-end.rds")
}




tbl_params_start <- post_process_gcm_fits(l_start_results) %>% as_tibble() %>% mutate(t = "Start")
rownames(tbl_params_start) <- NULL
tbl_params_end <- post_process_gcm_fits(l_end_results) %>% as_tibble() %>% mutate(t = "End")
rownames(tbl_params_end) <- NULL

tbl_params_both <- rbind(tbl_params_start, tbl_params_end)

pl_dist_w <- ggplot(tbl_params_both, aes(w, group = t)) +
  geom_freqpoly(aes(color = t), binwidth = .1)+ 
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "w(Head Spikiness)", y = "Prop. Participants") +
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 16)
  ) +
  scale_color_viridis_d(name = "Time Point")
save_my_pdf_and_tiff(pl_dist_w, "figures/w-before-after-e2", 4.5, 3.5)



