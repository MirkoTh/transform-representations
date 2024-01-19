rm(list = ls())

library(tidyverse)
library(grid)
library(gridExtra)
library(furrr)

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

l_all <- tbl_secondary %>% split(.$participant_id)

is_fit <- TRUE

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



## GCM --------------------------------------------------------------------


if (is_fit) {
  plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
  
  # initial trials
  l_results_gcm <- future_map(
    l_all, safely(fit_gcm_one_participant), 
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
  saveRDS(l_results_gcm, file = "data/gcm.rds")
  plan("sequential")
} else {
  l_results_gcm <- readRDS("data/gcm.rds")
}


## Multiplicative Prototype Model -----------------------------------------


if (is_fit) {
  plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
  
  l_results_pt <- future_map(
    l_all, safely(fit_prototype_one_participant), 
    tbl_pt = tbl_pt,
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
  saveRDS(l_results_pt, file = "data/prototype.rds")
  plan("sequential")
  
} else {
  l_results_pt <- readRDS("data/prototype.rds")
}



## rule-based model -------------------------------------------------------


tbl_rules <- tibble(
  category = 1:4,
  lo = list(c(-100, -100), c(-100, 50), c(50, -100), c(50, 50)),
  hi = list(c(50, 50), c(50, 200), c(200, 50), c(200, 200))
)

if (is_fit) {
  plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
  
  # initial trials
  l_results_rb <- future_map(
    l_all, safely(fit_rb_one_participant), 
    tbl_rules = tbl_rules,
    .progress = TRUE, .options = furrr_options(seed = TRUE)
  )
  saveRDS(l_results_rb, file = "data/rulbased.rds")
  plan("sequential")
  
} else {
  l_results_rb <- readRDS("data/rulebased.rds")
}


# Compare Models ----------------------------------------------------------


tbl_params_gcm <- post_process_gcm_fits(l_results_gcm) %>% as_tibble()
tbl_params_rb <- extract_from_results(l_results_rb, "params", c("sd_x1", "sd_x2"))
tbl_params_pt <- extract_from_results(l_results_pt, "params", c("c", "w", "g"))


tbl_ll_gcm <- extract_from_results(l_results_gcm, "neg2ll", "neg2ll")
tbl_ll_pt <- extract_from_results(l_results_pt, "neg2ll", "neg2ll")
tbl_ll_rb <- extract_from_results(l_results_rb, "neg2ll", "neg2ll")

tbl_ll <- aic_and_bic(tbl_ll_gcm, tbl_ll_pt, tbl_ll_rb)

pl_hm_bic <- plot_grouped_and_ranked_models(
  tbl_ll, c(bic_gcm, bic_pt, bic_rb), winner_bic, "Winner BIC"
)
pl_hm_aic <- plot_grouped_and_ranked_models(
  tbl_ll, c(aic_gcm, aic_pt, aic_rb), winner_aic, "Winner AIC"
)

grid.draw(arrangeGrob(pl_hm_bic, pl_hm_aic, nrow = 1))
