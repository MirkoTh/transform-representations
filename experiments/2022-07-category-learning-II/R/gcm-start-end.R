rm(list = ls())

library(tidyverse)
library(grid)
library(gridExtra)
library(furrr)
library(rutils)

home_grown <- c("R/utils.R", "R/analysis-plotting.R")
walk(home_grown, source)


tbl_secondary <- readRDS(
  file = "experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri.rds"
) %>% rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id)

tbl_start <- tbl_secondary %>% filter(trial_id < 100) #100
l_start <- tbl_start %>% split(.$participant_id)
tbl_end <- tbl_secondary %>% filter(trial_id >= 300) #300
l_end <- tbl_end %>% split(.$participant_id)

l_all <- tbl_secondary %>% filter(trial_id >= 100) %>% split(.$participant_id)

is_fit <- FALSE#TRUE

tbl_1p <- tbl_start %>% filter(participant_id == "02ac2073803c199426b7637306d28880")
tbl_pt <- tbl_secondary %>% group_by(category) %>% summarize(x1_pt = mean(x1), x2_pt = mean(x2))



# Initial Vs. Final Data --------------------------------------------------


## GCM ---------------------------------------------------------------------


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
  
  plan("sequential")
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
  labs(x = "w(Head Spikiness)", y = "Nr. Participants") +
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 16)
  ) +
  scale_color_viridis_d(name = "Time Point")
save_my_pdf_and_tiff(pl_dist_w, "figures/w-before-after-e2", 4.5, 3.5)




# All Data ----------------------------------------------------------------

# use all data from E2 - E4
tbl_secondary_e2 <- tbl_secondary
tbl_secondary_e3 <- readRDS(
  file = "experiments/2022-09-category-learning-similarity/data/tbl_cat-treps.rds"
) %>% rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id)
tbl_secondary_e4 <- readRDS(
  file = "experiments/2023-01-category-learning-catsim/data/tbl_cat-trepsno-outliers.rds"
) %>% rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id) %>%
  relocate(participant_id, .before = n_categories)


l_all_e2 <- tbl_secondary_e2 %>% filter(trial_id >= 100) %>% split(.$participant_id)
l_all_e3 <- tbl_secondary_e3 %>% filter(trial_id >= 100) %>% split(.$participant_id)
l_all_e4 <- tbl_secondary_e4 %>% filter(trial_id >= 100) %>% split(.$participant_id)


## GCM --------------------------------------------------------------------


l_all_e234 <- list(l_all_e2, l_all_e3, l_all_e4)

if (is_fit) {
  
  for (i in 1:length(l_all_e234)) {
    
    l_all <- l_all_e234[[i]]
    filter_cat <- map_lgl(l_all, ~ nrow(.x) > 0)
    l_all <- l_all[filter_cat]
    plan(multisession, workers = min(future::availableCores() - 1, length(l_start)))
    
    l_results_gcm <- future_map(
      l_all, safely(fit_gcm_one_participant), 
      .progress = TRUE, .options = furrr_options(seed = TRUE)
    )
    saveRDS(l_results_gcm, file = str_c("data/", c("e2", "e3", "e4")[i], "-gcm-300-trials.rds"))
    plan("sequential")
  }
  
} else {
  l_results_gcm_e2 <- readRDS("data/e2-gcm-300-trials.rds")
  l_results_gcm_e3 <- readRDS("data/e3-gcm-300-trials.rds")
  l_results_gcm_e4 <- readRDS("data/e4-gcm-300-trials.rds")
  
}



## Multiplicative Prototype Model -----------------------------------------


if (is_fit) {
  for (i in 1:length(l_all_e234)) {
    l_all <- l_all_e234[[i]]
    filter_cat <- map_lgl(l_all, ~ nrow(.x) > 0)
    l_all <- l_all[filter_cat]
    
    plan(multisession, workers = min(future::availableCores() - 1, length(l_start)))
    
    l_results_pt <- future_map(
      l_all, safely(fit_prototype_one_participant), 
      tbl_pt = tbl_pt,
      .progress = TRUE, .options = furrr_options(seed = TRUE)
    )
    saveRDS(l_results_pt, file = str_c("data/", c("e2", "e3", "e4")[i], "-prototype-300-trials.rds"))
    plan("sequential")
  }
  
} else {
  l_results_pt_e2 <- readRDS("data/e2-prototype-300-trials.rds")
  l_results_pt_e3 <- readRDS("data/e3-prototype-300-trials.rds")
  l_results_pt_e4 <- readRDS("data/e4-prototype-300-trials.rds")
  
}



## rule-based model -------------------------------------------------------


tbl_rules <- tibble(
  category = 1:4,
  lo = list(c(-100, -100), c(-100, 50), c(50, -100), c(50, 50)),
  hi = list(c(50, 50), c(50, 200), c(200, 50), c(200, 200))
)

if (is_fit) {
  for (i in 1:length(l_all_e234)) {
    l_all <- l_all_e234[[i]]
    filter_cat <- map_lgl(l_all, ~ nrow(.x) > 0)
    l_all <- l_all[filter_cat]
    
    plan(multisession, workers = min(future::availableCores() - 1, length(l_start)))
    
    l_results_rb <- future_map(
      l_all, safely(fit_rb_one_participant), 
      tbl_rules = tbl_rules,
      .progress = TRUE, .options = furrr_options(seed = TRUE)
    )
    saveRDS(l_results_rb, file = str_c("data/", c("e2", "e3", "e4")[i], "-rulebased-300-trials.rds"))
    plan("sequential")
  }
  
} else {
  l_results_rb_e2 <- readRDS("data/e2-rulebased-300-trials.rds")
  l_results_rb_e3 <- readRDS("data/e3-rulebased-300-trials.rds")
  l_results_rb_e4 <- readRDS("data/e4-rulebased-300-trials.rds")
  
}


# Compare Models ----------------------------------------------------------


# save tbls with params
# save tbl with ll, aic, and bic


compare_models <- function(l_results_gcm, l_results_pt, l_results_rb, ntrials) {
  #' @description compare gcm, pt, and rb using aic and bic
  #' @return list with comparison metrics, plot of metrics, and
  #' list with parameters for each model
  #' 
  filter_cat <- map_lgl(l_results_gcm, ~ !(is.null(.x$result)))
  l_results_gcm <- l_results_gcm[filter_cat]
  
  
  tbl_params_gcm <- post_process_gcm_fits(l_results_gcm) %>% as_tibble()
  tbl_params_rb <- extract_from_results(l_results_rb, "params", c("sd_x1", "sd_x2"))
  tbl_params_pt <- extract_from_results(l_results_pt, "params", c("c", "w", "g"))
  
  
  tbl_ll_gcm <- extract_from_results(l_results_gcm, "neg2ll", "neg2ll")
  tbl_ll_pt <- extract_from_results(l_results_pt, "neg2ll", "neg2ll")
  tbl_ll_rb <- extract_from_results(l_results_rb, "neg2ll", "neg2ll")
  
  tbl_ll <- aic_and_bic(tbl_ll_gcm, tbl_ll_pt, tbl_ll_rb, ntrials)
  
  pl_hm_bic <- plot_grouped_and_ranked_models(
    tbl_ll, c(bic_gcm, bic_pt, bic_rb), winner_bic, "Winner BIC"
  )
  pl_hm_aic <- plot_grouped_and_ranked_models(
    tbl_ll, c(aic_gcm, aic_pt, aic_rb), winner_aic, "Winner AIC"
  )
  
  pl_comp <- arrangeGrob(pl_hm_bic, pl_hm_aic, nrow = 1)
  
  return(list(
    tbl_ll = tbl_ll, 
    pl_comp = pl_comp, 
    l_tbl_params = list(
      tbl_params_gcm = tbl_params_gcm,
      tbl_params_pt = tbl_params_pt,
      tbl_params_rb = tbl_params_rb
    )
  ))
}


l_comp_e2 <- compare_models(l_results_gcm_e2, l_results_pt_e2, l_results_rb_e2, ntrials = nrow(l_all_e2[[1]]))
l_comp_e3 <- compare_models(l_results_gcm_e3, l_results_pt_e3, l_results_rb_e3, ntrials = nrow(l_all_e3[[1]]))
l_comp_e4 <- compare_models(l_results_gcm_e4, l_results_pt_e4, l_results_rb_e4, ntrials = nrow(l_all_e4[[1]]))



tbl_params_rb %>%
  mutate(id = 1:nrow(tbl_params_rb)) %>%
  pivot_longer(-id) %>%
  mutate(name = factor(name, labels = c("Head", "Belly"))) %>%
  ggplot(aes(value)) +
  geom_density(aes(color = name), linewidth = 1) +
  theme_bw() +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Standard Deviation", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Dimension")


tbl_secondary$is_move_horizontal <- 0
tbl_secondary$is_move_vertical <- 0

tbl_secondary$is_move_horizontal[tbl_secondary$category == 1 & tbl_secondary$response == 3] <- 1
tbl_secondary$is_move_horizontal[tbl_secondary$category == 3 & tbl_secondary$response == 1] <- 1
tbl_secondary$is_move_horizontal[tbl_secondary$category == 2 & tbl_secondary$response == 4] <- 1
tbl_secondary$is_move_horizontal[tbl_secondary$category == 4 & tbl_secondary$response == 2] <- 1

tbl_secondary$is_move_vertical[tbl_secondary$category == 1 & tbl_secondary$response == 2] <- 1
tbl_secondary$is_move_vertical[tbl_secondary$category == 2 & tbl_secondary$response == 1] <- 1
tbl_secondary$is_move_vertical[tbl_secondary$category == 3 & tbl_secondary$response == 4] <- 1
tbl_secondary$is_move_vertical[tbl_secondary$category == 4 & tbl_secondary$response == 3] <- 1

tbl_rule_swaps <- tbl_secondary %>%
  filter(between(trial_id, 0, 100) | between(trial_id, 300, 400)) %>%
  mutate(expt_progress = trial_id <= 100) %>%
  group_by(participant_id, expt_progress) %>%
  summarize(
    mn_horizontal = mean(is_move_horizontal),
    mn_vertical = mean(is_move_vertical)
  ) %>%
  grouped_agg(c(expt_progress), c(mn_horizontal, mn_vertical))


tbl_rule_swaps$expt_progress <- factor(tbl_rule_swaps$expt_progress, labels = c("End", "Start"))
tbl_rule_swaps$expt_progress <- fct_relevel(tbl_rule_swaps$expt_progress, "End", after = 2)
tbl_rule_swaps %>%
  pivot_longer(c(mean_mn_horizontal, mean_mn_vertical)) %>%
  mutate(name = factor(name, labels = c("Head", "Belly"))) %>%
  ggplot(aes(expt_progress, value, group = name)) +
  geom_line(aes(color = name)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = name)) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Progress in Experiment", y = "Prop. Linear Confusions") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Dimension")
