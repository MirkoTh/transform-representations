# TODOs
## - some participants seem to have restarted the experiment
## - plot change in categorization accuracy against movement towards center


# Import Packages ---------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggExtra)
library(docstring)
library(rutils)
library(catlearn)


# Import Home-Grown Modules -----------------------------------------------
files <- c(
  "R/utils.R",
  "experiments/2022-02-category-learning/R/analysis-utils.R",
  "experiments/2022-02-category-learning/R/analysis-plotting.R",
  "experiments/2022-02-category-learning/R/summarySEwithin.R",
  "experiments/2022-02-category-learning/R/summarySE.R",
  "experiments/2022-02-category-learning/R/normDataWithin.R"
)
walk(files, source)


# Load Data and Preprocess Data -------------------------------------------

path_data <- c(
  "experiments/2022-02-category-learning/data/2022-03-30-pilot-1/",
  "experiments/2022-02-category-learning/data/2022-04-20-pilot-2/",
  "experiments/2022-02-category-learning/data/2022-04-21-experiment/"
)
l_tbls_data <- map(path_data[2:3], load_data)
l_tbl_data <- list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))


# add deviation from response to stimulus
l_deviations <- add_deviations(l_tbl_data)
l_tbl_data[[1]] <- l_deviations$tbl_cr

l_cases <- preprocess_data(l_tbl_data)
tbl_cr <- l_cases$l_guessing$keep$tbl_cr
tbl_cat_sim <- l_cases$l_guessing$keep$tbl_cat_sim

# Categorization ----------------------------------------------------------

tbl_cat_sim <- add_binned_trial_id(tbl_cat_sim, 20, 40)
tbl_cat <- tbl_cat_sim %>% filter(n_categories %in% c(2, 3), as.numeric(as.character(trial_id_binned)) <= 15)


tbl_cat_overview <- tbl_cat %>% 
  grouped_agg(c(n_categories, participant_id), c(accuracy, rt)) %>%
  arrange(mean_rt)
tbl_chance2 <- tbl_cat_overview %>% group_by(n_categories) %>%
  summarize(dummy = mean(mean_accuracy)) %>%
  mutate(p_chance = 1/as.numeric(str_extract(n_categories, "[2-3]$")))

# categorization accuracy overview
histograms_accuracies_rts(tbl_cat_overview)
l_pl <- plot_categorization_accuracy_against_blocks(tbl_cat)
# overall trajectory
l_pl[[1]]
# by-participant trajectories
l_pl[[2]]

l_movement <- movement_towards_category_center(tbl_cat_sim, tbl_cr, "d_closest")
tbl_movement <- l_movement[[1]]
# plot movement towards category center against task2 accuracy
l_movement[[2]]

tbl_cat_grid <- aggregate_category_responses_by_x1x2(tbl_cat, 241)

plot_categorization_heatmaps(tbl_cat_grid, 2)
plot_categorization_heatmaps(tbl_cat_grid, 3)

# model fits
## prototype model

participant_ids_2_cat <- unique(tbl_cat$participant_id[tbl_cat$n_categories == 2]) %>% as.character()
l_nb <- map(participant_ids_2_cat, fit_predict_nb, tbl = tbl_cat %>% filter(n_categories == 2))
tbl_preds_nb <- reduce(map(l_nb, 2), rbind) %>% 
  mutate(participant_id = fct_inorder(substr(participant_id, 1, 6), ordered = TRUE))
l_m_nb_pds <- map(l_nb, 1) %>% map("tables")
names(l_m_nb_pds) <- levels(tbl_preds_nb$participant_id)

tbl_cr <- add_distance_to_representation_center(tbl_cr, l_m_nb_pds)
l_movement <- movement_towards_category_center(tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[1])
tbl_movement <- l_movement[[1]]
l_movement[[2]]

# can we also include variability in the representations
# i.e. people with preciser category representations
# should experience stronger pull/push towards/away the center

plot_categorization_heatmaps(tbl_cat_grid, 2, "Mode") + 
  geom_contour(data = tbl_preds_nb, aes(x1, x2, z = density, alpha = density), color = "black") +
  geom_point(data = tibble(x=50, y=50), aes(x, y), color = "#FF3333", size = 3.5)



## exemplar model

tbl_gcm <- tbl_tmp[, c("x1_true", "x2_true", "response")] %>%
  mutate(cat1 = 0, cat2 = 0) %>%
  rename(category = response) %>%
  mutate(category = fct_inorder(as.factor(category)))

tbl_gcm$cat1[tbl_gcm$category == 1] <- 1
tbl_gcm$cat2[tbl_gcm$category == 2] <- 1

l_info <- list(
  feature_names = c("x1_true", "x2_true"),
  categories = levels(tbl_gcm$category),
  n_categories = 2,
  sens = 1,
  wgh = .5
)

fit_gcm <- optim(
  f = ll_gcm, c(8, .52), lower = c(0, 0), upper = c(10, 1),
  l_info = l_info, tbl_stim = tbl_gcm, method = "L-BFGS-B"
)

# Similarity Judgments ----------------------------------------------------

tbl_sim <- tbl_cat_sim %>%
  filter(n_categories == 1) %>%
  mutate(
    x1_prev_true = lag(x1_true, 1),
    x2_prev_true = lag(x2_true, 1),
    distance_euclidian = sqrt((x1_true - x1_prev_true)^2 + (x2_true - x2_prev_true)^2)
  ) %>% filter(trial_id != 0) %>% replace_na(list(distance_euclidian = 0))
n_bins_distance <- 10
bins_distance <- c(seq(-1, max(tbl_sim$distance_euclidian), n_bins_distance - 1), Inf)
tbl_sim$distance_binned <- cut(tbl_sim$distance_euclidian, bins_distance, labels = FALSE)
tbl_sim$distance_binned %>% unique()

tbl_sim_agg <- tbl_sim %>% 
  rutils::grouped_agg(c(distance_binned), c(response, rt))
tbl_sim_ci <- summarySEwithin(
  tbl_sim, "response", "n_categories", "distance_binned", "participant_id", TRUE
) %>% as_tibble()
tbl_sim_ci$distance_binned <- as.numeric(as.character(tbl_sim_ci$distance_binned))


tbl_sim %>% group_by(participant_id, n_categories, distance_binned) %>%
  summarize(response_mn = mean(response)) %>%
  ggplot(aes(distance_binned, response_mn, group = participant_id)) +
  geom_line(aes(color = participant_id))

ggplot() +
  geom_errorbar(data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))), aes(
    distance_binned, 
    ymin = response - ci, 
    ymax = response + ci
  )) + geom_point(size = 3, color = "white") +
  geom_point(
    data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))), 
    aes(distance_binned, response)) +
  geom_smooth(
    data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))), 
    aes(distance_binned, response), color = "purple"
  ) + 
  theme_bw() +
  coord_cartesian(ylim = c(1, 4)) +
  labs(
    x = "Distance Binned",
    y = "Average Similarity (Range: 1 - 4)"
  )


# Continuous Reproduction ----------------------------------------------------------
# 2d marginals
pl_marginal_before <- plot_marginals_one_session(1, tbl_cr)
pl_marginal_after <- plot_marginals_one_session(2, tbl_cr)

# heat map of errors over 2d space

pl_heamaps <- plot_2d_binned_heatmaps(l_deviations$tbl_checker, l_deviations$tbl_checker_avg)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)


plot_distance_to_category_center(tbl_cr)



