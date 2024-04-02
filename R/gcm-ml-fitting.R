# Load Packages and Data --------------------------------------------------
rm(list = ls())



library(tidyverse)
library(grid)
library(gridExtra)
library(furrr)
library(rutils)
library(naivebayes)
library(mvtnorm)

home_grown <- c("R/utils.R", "R/analysis-plotting.R")
walk(home_grown, source)


tbl_secondary <- readRDS(
  file = "experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri-physical-properties.rds"
) %>% rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id)

tbl_start <- tbl_secondary %>% filter(trial_id < 100) #100
l_start <- tbl_start %>% split(.$participant_id)
tbl_end <- tbl_secondary %>% filter(trial_id >= 300) #300
l_end <- tbl_end %>% split(.$participant_id)

l_all <- tbl_secondary %>% filter(trial_id >= 100) %>% split(.$participant_id)

is_fit <- FALSE#TRUE#
is_fit_init_end <- FALSE
is_psychological <- TRUE#FALSE#
suffix <- c("", "-psych")[is_psychological + 1]


n_it <- 2

tbl_1p <- tbl_start %>% filter(participant_id == "02ac2073803c199426b7637306d28880")
tbl_pt <- tbl_secondary %>% group_by(category) %>% summarize(x1_pt = mean(x1), x2_pt = mean(x2))



# Initial Vs. Final Data --------------------------------------------------


## GCM ---------------------------------------------------------------------


if (is_fit_init_end) {
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


if (is_fit_init_end) {
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
  
}


# All Data E2 - E4 --------------------------------------------------------


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

# add psych representations
tbl_psych <- readRDS("data/psych-representations.rds")
tbl_secondary <- tbl_secondary %>% 
  left_join(tbl_psych, by = c("x1" = "x1_obj", "x2" = "x2_obj"))
tbl_secondary_e2 <- tbl_secondary_e2 %>% 
  left_join(tbl_psych, by = c("x1" = "x1_obj", "x2" = "x2_obj"))
tbl_secondary_e3 <- tbl_secondary_e3 %>% 
  left_join(tbl_psych, by = c("x1" = "x1_obj", "x2" = "x2_obj"))
tbl_secondary_e4 <- tbl_secondary_e4 %>% 
  left_join(tbl_psych, by = c("x1" = "x1_obj", "x2" = "x2_obj"))

if (is_psychological) {
  tbl_pt <- tbl_secondary %>% 
    group_by(category) %>% 
    summarize(x1_pt = mean(x1_psych), x2_pt = mean(x2_psych))
} else {
  tbl_pt <- tbl_secondary %>% 
    group_by(category) %>% 
    summarize(x1_pt = mean(x1), x2_pt = mean(x2))
}


l_all_e2 <- tbl_secondary_e2 %>% filter(trial_id >= 100) %>% split(.$participant_id)
l_all_e3 <- tbl_secondary_e3 %>% filter(trial_id >= 100) %>% split(.$participant_id)
l_all_e4 <- tbl_secondary_e4 %>% filter(trial_id >= 100) %>% split(.$participant_id)
l_all_e234 <- list(l_all_e2, l_all_e3, l_all_e4)


## GCM --------------------------------------------------------------------


l_results_gcm_e2 <- list()
l_results_gcm_e3 <- list()
l_results_gcm_e4 <- list()


for (it in 1:n_it) {
  
  if (is_fit) {
    
    for (i in 1:length(l_all_e234)) {
      
      l_all <- l_all_e234[[i]]
      filter_cat <- map_lgl(l_all, ~ nrow(.x) > 0)
      l_all <- l_all[filter_cat]
      if (is_psychological) {
        l_all <- map(l_all, ~ .x %>% select(-c(x1, x2)) %>% rename(x1 = x1_psych, x2 = x2_psych))
      }
      plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
      
      l_results_gcm <- future_map(
        l_all, safely(fit_gcm_one_participant), 
        .progress = TRUE, .options = furrr_options(seed = FALSE)
      )
      fl_name <- str_c(
        "data/", c("e2", "e3", "e4")[i], 
        "-gcm-300-trials-it-", it, suffix, ".rds"
      )
      saveRDS(l_results_gcm, file = fl_name)
      plan("sequential")
    }
    
  } else {
    fl_name <- str_c(
      "data/", c("e2", "e3", "e4"), 
      "-gcm-300-trials-it-", it, suffix, ".rds"
    )
    l_results_gcm_e2[[it]] <- readRDS(fl_name[1])
    l_results_gcm_e3[[it]] <- readRDS(fl_name[2])
    l_results_gcm_e4[[it]] <- readRDS(fl_name[3])
    
  }
}

# post-process gcm
# take best fits across iterations

extract_best_fit <- function(l_in) {
  #' @description extract fitting iteration with lowest ll
  #' @return shrunk list only containing best iteration per participant
  #' 
  tbl_ll_it <- reduce(map(l_in, ~ map_dbl(map(.x, "result"), "neg2ll")), cbind) %>%
    as.data.frame()
  colnames(tbl_ll_it) <- 1:ncol(tbl_ll_it)
  tbl_ll_it <- as_tibble(tbl_ll_it)
  tbl_ll_it$minimum <- pmap_dbl(tbl_ll_it[, 1:n_it], min)
  tbl_ll_it$maximum <- pmap_dbl(tbl_ll_it[, 1:n_it], max)
  tbl_ll_it$max_diff <- tbl_ll_it$maximum - tbl_ll_it$minimum
  lowest_ll_gcm_ids <- apply(tbl_ll_it[, 1:n_it], 1, which.min)
  id_names <- names(l_in[[1]])
  
  return(list(
    lowest = map2(lowest_ll_gcm_ids, id_names, ~ l_in[[..1]][[..2]]),
    tbl_ll = tbl_ll_it
  ))
}


l_results_gcm_e2 <- extract_best_fit(l_results_gcm_e2)[[1]]
l_results_gcm_e3 <- extract_best_fit(l_results_gcm_e3)[[1]]
l_results_gcm_e4 <- extract_best_fit(l_results_gcm_e4)[[1]]


## Multiplicative Prototype Model -----------------------------------------


l_results_pt_e2 <- list()
l_results_pt_e3 <- list()
l_results_pt_e4 <- list()

for (it in 1:n_it) {
  if (is_fit) {
    for (i in 1:length(l_all_e234)) {
      l_all <- l_all_e234[[i]]
      filter_cat <- map_lgl(l_all, ~ nrow(.x) > 0)
      l_all <- l_all[filter_cat]
      if (is_psychological) {
        l_all <- map(l_all, ~ .x %>% select(-c(x1, x2)) %>% rename(x1 = x1_psych, x2 = x2_psych))
      }
      
      plan(multisession, workers = min(future::availableCores(), length(l_start)))
      l_results_pt <- future_map(
        l_all, safely(fit_prototype_one_participant), 
        tbl_pt = tbl_pt,
        .progress = TRUE, .options = furrr_options(seed = TRUE)
      )
      fl_name <- str_c(
        "data/", c("e2", "e3", "e4")[i], 
        "-prototype-300-trials-it-", it, suffix, ".rds"
      )
      saveRDS(l_results_pt, file = fl_name)
      plan("sequential")
    }
    
  } else {
    fl_name <- str_c(
      "data/", c("e2", "e3", "e4"), 
      "-prototype-300-trials-it-", it, suffix, ".rds"
    )
    l_results_pt_e2[[it]] <- readRDS(fl_name[1])
    l_results_pt_e3[[it]] <- readRDS(fl_name[2])
    l_results_pt_e4[[it]] <- readRDS(fl_name[3])
    
  }
}

# post-process pt
# take best fits across iterations
l_results_pt_e2 <- extract_best_fit(l_results_pt_e2)[[1]]
l_results_pt_e3 <- extract_best_fit(l_results_pt_e3)[[1]]
l_results_pt_e4 <- extract_best_fit(l_results_pt_e4)[[1]]


## rule-based model -------------------------------------------------------

if (is_psychological) {
  # psych
  tbl_rules <- tibble(
    category = 1:4,
    lo = list(c(-100, -100), c(-100, 3.6487456), c(3.9377975, -100), c(3.9377975, 3.6487456)),
    hi = list(c(3.9377975, 3.6487456), c(3.9377975, 200), c(200, 3.6487456), c(200, 200))
  )
} else {
  # obj
  tbl_rules <- tibble(
    category = 1:4,
    lo = list(c(-100, -100), c(-100, 50), c(50, -100), c(50, 50)),
    hi = list(c(50, 50), c(50, 200), c(200, 50), c(200, 200))
  )
}


l_results_rb_e2 <- list()
l_results_rb_e3 <- list()
l_results_rb_e4 <- list()

for (it in 1:n_it) {
  
  if (is_fit) {
    for (i in 1:length(l_all_e234)) {
      l_all <- l_all_e234[[i]]
      filter_cat <- map_lgl(l_all, ~ nrow(.x) > 0)
      l_all <- l_all[filter_cat]
      if (is_psychological) {
        l_all <- map(l_all, ~ .x %>% select(-c(x1, x2)) %>% rename(x1 = x1_psych, x2 = x2_psych))
      }
      
      plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
      l_results_rb <- future_map(
        l_all, safely(fit_rb_one_participant), 
        tbl_rules = tbl_rules,
        .progress = TRUE, .options = furrr_options(seed = TRUE)
      )
      fl_name <- str_c(
        "data/", c("e2", "e3", "e4")[i], 
        "-rulebased-300-trials-it-", it, suffix, ".rds"
      )
      saveRDS(l_results_rb, file = fl_name)
      plan("sequential")
    }
    
  } else {
    fl_name <- str_c(
      "data/", c("e2", "e3", "e4"), 
      "-rulebased-300-trials-it-", it, suffix, ".rds"
    )
    l_results_rb_e2[[it]] <- readRDS(fl_name[1])
    l_results_rb_e3[[it]] <- readRDS(fl_name[2])
    l_results_rb_e4[[it]] <- readRDS(fl_name[3])
    
  }
}


# post-process rb
# take best fits across iterations

l_results_rb_e2 <- extract_best_fit(l_results_rb_e2)[[1]]
l_results_rb_e3 <- extract_best_fit(l_results_rb_e3)[[1]]
l_results_rb_e4 <- extract_best_fit(l_results_rb_e4)[[1]]


## Compare Models ----------------------------------------------------------


l_comp_e2 <- compare_models(l_results_gcm_e2, l_results_pt_e2, l_results_rb_e2, "2", ntrials = nrow(l_all_e2[[1]]), lg_pos = "none")
l_comp_e3 <- compare_models(l_results_gcm_e3, l_results_pt_e3, l_results_rb_e3, "3", ntrials = nrow(l_all_e3[[2]]), lg_pos = "none")
l_comp_e4 <- compare_models(l_results_gcm_e4, l_results_pt_e4, l_results_rb_e4, "4", ntrials = nrow(l_all_e4[[1]]), lg_pos = "bottom")

table_ic <- l_comp_e2$tbl_ll %>% select(starts_with("winner")) %>% table() +
  l_comp_e3$tbl_ll %>% select(starts_with("winner")) %>% table() +
  l_comp_e4$tbl_ll %>% select(starts_with("winner")) %>% table() %>% cbind(c(0, 0, 0))

props_agreed <- table_ic / sum(diag(table_ic))

## Save Fitted Parameters --------------------------------------------------

tbl_gcm_params <- read_out_params_3e(
  l_comp_e2$l_tbl_params$tbl_params_gcm,
  l_comp_e3$l_tbl_params$tbl_params_gcm,
  l_comp_e4$l_tbl_params$tbl_params_gcm
)

tbl_pt_params <- read_out_params_3e(
  l_comp_e2$l_tbl_params$tbl_params_pt,
  l_comp_e3$l_tbl_params$tbl_params_pt,
  l_comp_e4$l_tbl_params$tbl_params_pt
)


tbl_rb_params <- read_out_params_3e(
  l_comp_e2$l_tbl_params$tbl_params_rb,
  l_comp_e3$l_tbl_params$tbl_params_rb,
  l_comp_e4$l_tbl_params$tbl_params_rb
)


hist_gcm_params <- tbl_gcm_params %>%
  rename("Bias 1" = bias1, "Bias 2" = bias2, "Bias 3" = bias3, "Bias 4" = bias4) %>%
  pivot_longer(c(c, w, starts_with("bias"))) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "skyblue2") +
  facet_grid(E ~ name, scales = "free_x") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value", y = "Nr. Participants", title = "Parameters GCM") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .3)
  )

hist_pt_params <- tbl_pt_params %>%
  pivot_longer(c(c, w, g)) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "skyblue2") +
  facet_grid(E ~ name, scales = "free_x") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value", y = "Nr. Participants", title = "Parameters PT") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .3)
  )

hist_rb_params <- tbl_rb_params %>%
  rename(
    "sd(Head)" = sd_x1,
    "sd(Belly)" = sd_x2
  ) %>%
  pivot_longer(c("sd(Head)", "sd(Belly)")) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "skyblue2") +
  facet_grid(E ~ name) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0), breaks = seq(0, 20, by = 3)) +
  labs(x = "Parameter Value", y = "Nr. Participants", title = "Parameters RB") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .3)
  )

save_my_pdf_and_tiff(hist_gcm_params, str_c("figures/gcm-model-params-e234", suffix), 12, 7)
save_my_pdf_and_tiff(hist_pt_params, str_c("figures/pt-model-params-e234", suffix), 6, 7)
save_my_pdf_and_tiff(hist_rb_params, str_c("figures/rb-model-params-e234", suffix), 4, 7)


# params look relatively similar across Es
gcm_avg_params <- colMeans(tbl_gcm_params %>% select(-c(E, id)))
pt_avg_params <- colMeans(tbl_pt_params %>% select(-c(E, id)))
rb_avg_params <- colMeans(tbl_rb_params %>% select(-c(E, id)))

tbl_avg_params <- tibble(
  p = names(gcm_avg_params), val = gcm_avg_params, model = "GCM"
) %>% rbind(
  tibble(
    p = names(pt_avg_params), val = pt_avg_params, model = "PT"
  )
) %>% rbind(
  tibble(
    p = names(rb_avg_params), val = rb_avg_params, model = "RB"
  )
) %>% mutate(
  cat_structure = "square",
  representation = c("object-properties", "psychological-representation")[is_psychological + 1]
)



## Analyze Rule-Based Parameters -------------------------------------------


tbl_rb_params %>%
  pivot_longer(-c(E, id)) %>%
  mutate(name = factor(name, labels = c("Head", "Belly"))) %>%
  ggplot(aes(value)) +
  geom_density(aes(color = name), linewidth = 1) +
  theme_bw() +
  facet_wrap(~ E) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Standard Deviation", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Dimension")


tbl_secondary <- rbind(
  tbl_secondary_e2 %>% mutate(E = 2),
  tbl_secondary_e3 %>% mutate(E = 3),
  tbl_secondary_e4 %>% mutate(E = 4)
)

tbl_secondary <- horizontal_and_vertical_moves(tbl_secondary)


tbl_rule_swaps <- tbl_secondary %>%
  filter(between(trial_id, 0, 100) | between(trial_id, 300, 400)) %>%
  mutate(expt_progress = trial_id <= 100) %>%
  group_by(participant_id, E, expt_progress) %>%
  summarize(
    mn_horizontal = mean(is_move_horizontal),
    mn_vertical = mean(is_move_vertical)
  ) %>%
  pivot_longer(c(mn_horizontal, mn_vertical)) %>% ungroup()

tbl_rule_swaps_agg <- summary_se_within(
  tbl_rule_swaps, 
  "value", 
  betweenvars = c("E"),
  withinvars = c("expt_progress", "name")
)


tbl_rule_swaps_agg$expt_progress <- factor(tbl_rule_swaps_agg$expt_progress, labels = c("End", "Start"))
tbl_rule_swaps_agg$expt_progress <- fct_relevel(tbl_rule_swaps_agg$expt_progress, "End", after = 2)
pd <- position_dodge(width = .15)
tbl_rule_swaps_agg %>%
  mutate(name = factor(name, labels = c("Head", "Belly"))) %>%
  ggplot(aes(expt_progress, value, group = name)) +
  geom_errorbar(aes(ymax = value + ci, ymin = value - ci, color = name), width = .15, position = pd) +
  geom_line(aes(color = name), position = pd) +
  geom_point(color = "white", size = 3, position = pd) +
  geom_point(aes(color = name), position = pd) +
  theme_bw() +
  facet_wrap(~ E) +
  scale_x_discrete(expand = c(0.05, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Progress in Experiment", y = "Prop. Linear Confusions") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = .5)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Dimension")



# All Data E1 -------------------------------------------------------------

tbl_secondary_e1 <- readRDS("experiments/2022-02-category-learning/data/tbl_cat_sim-physical-properties.rds") %>%
  rename(x1 = x1_true, x2 = x2_true, category = cat_true) %>%
  arrange(participant_id, trial_id)

tbl_secondary_e1 <- tbl_secondary_e1 %>% 
  left_join(tbl_psych, by = c("x1" = "x1_obj", "x2" = "x2_obj"))

l_all_e1 <- tbl_secondary_e1 %>% filter(trial_id >= 340) %>% split(.$participant_id)


filter_cat <- map_lgl(l_all_e1, ~ .x$n_categories[1] == 2)
l_all_e1 <- l_all_e1[filter_cat]


## GCM --------------------------------------------------------------------


l_results_gcm_e1 <- list()

if (is_psychological) {
  l_all_e1 <- map(
    l_all_e1, ~ .x %>% 
      select(-c(x1, x2)) %>% 
      rename(x1 = x1_psych, x2 = x2_psych)
  )
}

for (it in 1:n_it) {
  if (is_fit) {
    
    plan(multisession, workers = min(future::availableCores(), length(l_start)))
    
    l_results_gcm <- future_map(
      l_all_e1, safely(fit_gcm_one_participant), 
      n_cat = 2,
      .progress = TRUE, .options = furrr_options(seed = FALSE)
    )
    
    fl_name <- str_c(
      "data/e1-gcm-300-trials-it-", it, suffix, ".rds"
    )
    saveRDS(l_results_gcm, file = fl_name)
    
    plan("sequential")
  } else {
    l_results_gcm_e1[[it]] <- readRDS(str_c("data/e1-gcm-300-trials-it-", it, suffix, ".rds"))
  }
}

l_results_gcm_e1 <- extract_best_fit(l_results_gcm_e1)[[1]]
n2ll_gcm_e1 <- map_dbl(map(l_results_gcm_e1, "result"), "neg2ll")
tbl_gcm_e1_params <- map(map(l_results_gcm_e1, "result"), "params") %>%
  reduce(rbind) %>% as.data.frame() %>% as_tibble()
colnames(tbl_gcm_e1_params) <- c("c", "w", "bias")



par_mn_gcm_e1 <- colMeans(tbl_gcm_e1_params)
tbl_par_gcm_e1 <- tibble(
  p = names(par_mn_gcm_e1), 
  val = par_mn_gcm_e1, 
  model = "GCM",
  cat_structure = "ellipses")
tbl_avg_params <- rbind(
  tbl_avg_params, 
  tbl_par_gcm_e1 %>% 
    mutate(representation = c("object-properties", "psychological-representation")[is_psychological + 1])
)
saveRDS(tbl_avg_params, file = str_c("data/avg-params-all-catlearn-models-", c("physical-properties", "psychological-representation")[is_psychological + 1], ".rds"))


hist_gcm_params_e1 <- tbl_gcm_e1_params %>%
  rename("Bias" = bias) %>%
  pivot_longer(c(c, w, Bias)) %>%
  ggplot(aes(value)) +
  geom_histogram(color = "black", fill = "skyblue2") +
  facet_wrap( ~ name, scales = "free_x") +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Parameter Value", y = "Nr. Participants", title = "Parameters GCM") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 90, vjust = .3)
  )
save_my_pdf_and_tiff(
  hist_gcm_params_e1, str_c(
    "figures/gcm-model-params-e1-", 
    c("object-properties", "psychological-representation")[is_psychological + 1]),
  6, 7
)


## Naive Bayes Prototype Model --------------------------------------------


get_n2ll_nb <- function(m, tbl_df) {
  #' @description negative 2 * log likelihood of naive bayes prototype model
  #' given model m and data (x1 and x2) in tbl_df to be predicted
  
  nb_preds <- predict(m, tbl_df[, c("x1", "x2")], "prob")
  tbl_nb_preds <- as.data.frame(nb_preds) %>% as_tibble()
  tbl_nb_preds$response <- tbl_df$response
  p_corr <- pmap_dbl(tbl_nb_preds, ~ c(..1, ..2)[..3])
  -2*sum(log(p_corr))
}

if (is_fit) {
  plan(multisession, workers = min(future::availableCores() - 2, length(l_start)))
  l_results_nb <- future_map(
    l_all_e1, ~ naive_bayes(y = as.character(.x$response), x = .x[, c("x1", "x2")]), 
    .progress = TRUE, .options = furrr_options(seed = FALSE)
  )
  n2ll_nb <- future_map2_dbl(
    l_results_nb, l_all_e1, get_n2ll_nb,
    .progress = TRUE
  )
  saveRDS(n2ll_nb, file = str_c("data/e1-pt-nb-300-trials", suffix, ".rds"))
  plan("sequential")
} else if (!is_fit) {
  n2ll_nb <- readRDS(file = str_c("data/e1-pt-nb-300-trials", suffix, ".rds"))
}

e1_abic_plots <- function(n2ll_gcm_e1, n2ll_nb, ntrials_e1) {
  tbl_ll_e1 <- tibble(
    id = 1:length(l_all_e1),
    n2ll_gcm = n2ll_gcm_e1,
    n2ll_nb = n2ll_nb,
    bic_gcm = n2ll_gcm + 3*log(ntrials_e1),
    bic_pt = n2ll_nb + 4*log(ntrials_e1),
    aic_gcm = n2ll_gcm + 6,
    aic_pt = n2ll_nb + 8
  )
  tbl_ll_e1$winner_bic <- c("GCM", "PT")[
    pmap_dbl(
      tbl_ll_e1[, c("bic_gcm", "bic_pt")], ~ which.min(c(..1, ..2))
    ) %>% as.factor()
  ]
  tbl_ll_e1$winner_aic <- c("GCM", "PT")[
    pmap_dbl(
      tbl_ll_e1[, c("aic_gcm", "aic_pt")], ~ which.min(c(..1, ..2))
    ) %>% as.factor()
  ]
  
  pl_hm_bic <- plot_grouped_and_ranked_models(
    tbl_ll_e1, c(bic_gcm, bic_pt), winner_bic, "E1: Winner BIC", "none"
  )
  pl_hm_aic <- plot_grouped_and_ranked_models(
    tbl_ll_e1, c(aic_gcm, aic_pt), winner_aic, "E1: Winner AIC", "none"
  )
  pl_comp <- arrangeGrob(pl_hm_bic, pl_hm_aic, nrow = 1)
  
  return(list(tbl_ll_e1 = tbl_ll_e1, pl_comp = pl_comp))
}

ntrials_e1 <- nrow(l_all_e1[[1]])
l_comp_e1 <- e1_abic_plots(n2ll_gcm_e1, n2ll_nb, ntrials_e1)
table_ic_1 <- l_comp_e1$tbl_ll_e1 %>% select(starts_with("winner")) %>% table() 
table_ic_1/sum(diag(table_ic_1))

plot_m_comp <- arrangeGrob(
  l_comp_e1$pl_comp,
  l_comp_e2$pl_comp,
  l_comp_e3$pl_comp,
  l_comp_e4$pl_comp,
  nrow = 4, ncol = 1, heights = c(1, 1, 1, 1.2)
)

path_m_comp <- str_c("figures/plot-model-comparison-e1-e4", suffix, ".rds")
save_my_pdf_and_tiff(
  plot_m_comp, 
  path_m_comp,
  7.5, 11
)

# average fit

tbl_l_all_e1 <- reduce(l_all_e1, rbind)  %>%
  mutate(
    category = as.character(category),
    response = as.character(response)
  )
fml <- formula("response ~ x1 + x2")
m_nb <- naive_bayes(
  fml, data = tbl_l_all_e1 %>%
    mutate(
      category = as.character(category),
      response = as.character(response)
    )
)
saveRDS(m_nb, file = str_c("data/e1-nb-pt-avg-fit", suffix, ".rds"))



# Compare psychological representations to physical properties ------------


