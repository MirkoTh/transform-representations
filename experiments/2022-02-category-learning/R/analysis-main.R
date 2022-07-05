# Some Notes

# for simulation f_stretch <- 1, f_shift <- 0
# for empirical data f_stretch <- 9 f_shift <- 1
# as in empirical data x1 and x2 ranged from 1-100 and the ellipses are scaled 

# Import Packages ---------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggExtra)
library(docstring)
library(rutils)
library(catlearn)
library(cmdstanr)
library(modelr)


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
l_tbl_data <-
  list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))


# add deviation from response to stimulus
l_deviations <- add_deviations(l_tbl_data)
l_tbl_data[[1]] <- l_deviations$tbl_cr

l_cases <- preprocess_data(l_tbl_data)
tbl_cr <- l_cases$l_guessing$keep$tbl_cr
tbl_cat_sim <- l_cases$l_guessing$keep$tbl_cat_sim

# exclusions
excl_incomplete <-
  dplyr::union(
    unique(l_cases$l_incomplete$drop[[1]]$participant_id),
    unique(l_cases$l_incomplete$drop[[2]]$participant_id)
  )
excl_outlier <-
  dplyr::union(
    unique(l_cases$l_outliers$drop[[1]]$participant_id),
    unique(l_cases$l_outliers$drop[[2]]$participant_id)
  )
excl_guessing <-
  dplyr::union(
    unique(l_cases$l_guessing$drop[[1]]$participant_id),
    unique(l_cases$l_guessing$drop[[2]]$participant_id)
  )
# inclusions
cat(str_c("final N analyzed: ", length(unique(
  tbl_cr$participant_id
)), "\n"))
same_n <-
  length(unique(tbl_cr$participant_id)) == length(unique(tbl_cat_sim$participant_id))
cat(str_c("same n participants in cat and cr data sets: ", same_n, "\n"))


# Categorization ----------------------------------------------------------

tbl_cat_sim <- add_binned_trial_id(tbl_cat_sim, 20, 40)
tbl_cat <-
  tbl_cat_sim %>% filter(n_categories %in% c(2, 3), as.numeric(as.character(trial_id_binned)) <= 15)


tbl_cat_overview <- tbl_cat %>%
  grouped_agg(c(n_categories, participant_id), c(accuracy, rt)) %>%
  arrange(mean_rt)
tbl_chance2 <- tbl_cat_overview %>% group_by(n_categories) %>%
  summarize(dummy = mean(mean_accuracy)) %>%
  mutate(p_chance = 1 / as.numeric(str_extract(n_categories, "[2-3]$")))

# categorization accuracy overview
histograms_accuracies_rts(tbl_cat_overview)
l_pl <- plot_categorization_accuracy_against_blocks(tbl_cat)
# overall trajectory
l_pl[[1]]
# by-participant trajectories
l_pl[[2]]



tbl_cat_agg <-
  l_pl[[3]] %>% group_by(participant_id, trial_id_binned) %>%
  summarize(accuracy = mean(accuracy_mn_participant)) %>% ungroup() %>%
  mutate(
    trial_id_binned = as.numeric(as.character(trial_id_binned)),
    trial_id_binned = trial_id_binned - mean(trial_id_binned)
  )
by_participant_coefs(tbl_cat_agg, "trial_id_binned", "accuracy", "LM Cat. Accuracy")

# stat analysis

tbl_cat_agg <-
  tbl_cat %>% group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(accuracy_mn = mean(accuracy)) %>% ungroup() %>%
  mutate(
    trial_id_binned = as.numeric(as.character(trial_id_binned)),
    trial_id_binned = scale(trial_id_binned)[, 1]
  ) %>% group_by(cat_true, trial_id_binned) %>%
  mutate(participant_id_num = row_number(participant_id))

library(nlme)
m_rs <-
  lme(
    accuracy_mn ~ cat_true * trial_id_binned,
    random = ~ 1 + cat_true + trial_id_binned |
      participant_id,
    data = tbl_cat_agg
  )
summary(m_rs)
anova(m_rs)
tbl_cat_agg$preds <- predict(m_rs, tbl_cat_agg)

l_movement <-
  movement_towards_category_center(tbl_cat_sim, tbl_cr, "d_closest")
tbl_movement <- l_movement[[1]]
# plot movement towards category center against task2 accuracy
l_movement[[2]]

marrangeGrob(list(l_pl[[1]], l_movement[[2]]$hist_delta_last),
             nrow = 1,
             ncol = 2)


tbl_cat_grid <- aggregate_category_responses_by_x1x2(tbl_cat, 241)
sample_ids <- tbl_cat_grid %>% group_by(participant_id) %>%
  summarize(mean_accuracy = max(mean_accuracy)) %>%
  arrange(desc(mean_accuracy))
select_ids <- round(seq(1, nrow(sample_ids), length.out = 4))
sample_ids <-
  as.character(sample_ids[select_ids, "participant_id"] %>% as_vector() %>% unname())
plot_categorization_heatmaps(tbl_cat_grid %>% filter(participant_id %in% sample_ids), 2)

ggplot(tbl_movement,
       aes(mean_delta_accuracy, mean_accuracy, group = n_categories)) +
  geom_point() +
  geom_smooth(method = "lm") +
  facet_wrap( ~ n_categories)
# model fits
## prototype model

participant_ids_2_cat <-
  unique(tbl_cat$participant_id[tbl_cat$n_categories == 2]) %>% as.character()
l_nb <-
  map(participant_ids_2_cat,
      fit_predict_nb,
      tbl = tbl_cat %>% filter(n_categories == 2))
tbl_preds_nb <- reduce(map(l_nb, 2), rbind) %>%
  mutate(participant_id = fct_inorder(substr(participant_id, 1, 6), ordered = TRUE))
l_m_nb_pds <- map(l_nb, 1) %>% map("tables")
names(l_m_nb_pds) <- levels(tbl_preds_nb$participant_id)

tbl_cr <- add_distance_to_representation_center(tbl_cr, l_m_nb_pds)
l_movement <-
  movement_towards_category_center(tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[1])
tbl_movement <- l_movement[[1]]
l_movement[[2]]

# can we also include variability in the representations
# i.e. people with preciser category representations
# should experience stronger pull/push towards/away the center

plot_categorization_heatmaps(tbl_cat_grid %>% filter(participant_id %in% sample_ids),
                             2,
                             "Mode") +
  geom_contour(
    data = tbl_preds_nb %>% filter(participant_id %in% substr(sample_ids, 1, 6)),
    aes(x1, x2, z = density, alpha = density),
    color = "black"
  ) + geom_point(data = tibble(x = 50, y = 50),
                 aes(x, y),
                 color = "#FF3333",
                 size = 3.5)



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
  f = ll_gcm,
  c(8, .52),
  lower = c(0, 0),
  upper = c(10, 1),
  l_info = l_info,
  tbl_stim = tbl_gcm,
  method = "L-BFGS-B"
)

# Similarity Judgments ----------------------------------------------------

tbl_sim <- tbl_cat_sim %>%
  filter(n_categories == 1) %>%
  mutate(
    x1_prev_true = lag(x1_true, 1),
    x2_prev_true = lag(x2_true, 1),
    distance_euclidian = sqrt((x1_true - x1_prev_true) ^ 2 + (x2_true - x2_prev_true) ^
                                2)
  ) %>% filter(trial_id != 0) %>% replace_na(list(distance_euclidian = 0))
n_bins_distance <- 9
bins_distance <-
  c(seq(-1, max(tbl_sim$distance_euclidian), length.out = n_bins_distance), Inf)
tbl_sim$distance_binned <-
  cut(tbl_sim$distance_euclidian, bins_distance, labels = FALSE)
tbl_sim$distance_binned %>% unique()


tbl_sim_agg <- tbl_sim %>%
  rutils::grouped_agg(c(distance_binned), c(response, rt))
tbl_sim_ci <- summarySEwithin(tbl_sim,
                              "response",
                              "n_categories",
                              "distance_binned",
                              "participant_id",
                              TRUE) %>% as_tibble()
tbl_sim_ci$distance_binned <-
  as.numeric(as.character(tbl_sim_ci$distance_binned))

sample_ids <-
  unique(tbl_sim$participant_id)[seq(1, length(unique(tbl_sim$participant_id)), length.out = 4)]
tbl_sim %>% group_by(participant_id, n_categories, distance_binned) %>%
  filter(participant_id %in% sample_ids) %>%
  summarize(response_mn = mean(response)) %>%
  ggplot(aes(distance_binned, response_mn, group = participant_id)) +
  geom_line(aes(color = participant_id))

ggplot() +
  geom_smooth(
    data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
    aes(distance_binned, response),
    color = "purple",
    method = "lm"
  ) + geom_errorbar(
    data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
    aes(
      distance_binned,
      ymin = response - ci,
      ymax = response + ci,
      width = .2
    )
  ) +  geom_point(size = 3, color = "white") +
  geom_point(data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
             aes(distance_binned, response)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  coord_cartesian(ylim = c(1, 4)) +
  labs(x = "Euclidean Distance",
       y = "Average Similarity (Range: 1 - 4)")

tbl_sim_agg_subj <- tbl_sim %>%
  mutate(distance_binned = distance_binned - mean(distance_binned)) %>%
  rutils::grouped_agg(c(participant_id, distance_binned), c(response, rt))

m_rs_sim <-
  lme(
    mean_response ~ distance_binned,
    random = ~ 1 + distance_binned |
      participant_id,
    data = tbl_sim_agg_subj
  )
summary(m_rs_sim)
anova(m_rs_sim)
tbl_sim_agg_subj$preds <- predict(m_rs_sim, tbl_sim_agg_subj)

by_participant_coefs(tbl_sim_agg_subj, "distance_binned", "mean_response", "LM Sim. Ratings")



# Continuous Reproduction ----------------------------------------------------------
# 2d marginals
pl_marginal_before <- plot_marginals_one_session(1, tbl_cr)
pl_marginal_after <- plot_marginals_one_session(2, tbl_cr)

# heat map of errors over 2d space

pl_heamaps <-
  plot_2d_binned_heatmaps(l_deviations$tbl_checker, l_deviations$tbl_checker_avg)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)


tbl_cr$n_categories <-
  factor(tbl_cr$n_categories,
         labels = c("Control Group", "Experimental Group"))
pl_empirical <- plot_distance_to_category_center(tbl_cr)
plot_distance_from_decision_boundary(tbl_cr, 10)




marrangeGrob(list(pl_avg_move, pl_empirical),
             nrow = 1,
             ncol = 2)

tbl_cr_agg <-
  grouped_agg(tbl_cr,
              c(participant_id, session, n_categories),
              eucl_deviation)

ggplot(tbl_cr_agg, aes(mean_eucl_deviation, group = session)) +
  geom_density(aes(color = session))

summarySEwithin(tbl_cr_agg, "mean_eucl_deviation", withinvars = "session") %>%
  mutate(session = factor(
    session,
    labels = c("Before\nCategory Learning", "After\nCategory Learning")
  )) %>%
  ggplot(aes(session, mean_eucl_deviation)) +
  geom_col(aes(fill = session), alpha = .5, show.legend = FALSE) +
  geom_point(aes(color = session), show.legend = FALSE) +
  geom_errorbar(
    aes(
      ymin = mean_eucl_deviation - 1.96 * se,
      ymax = mean_eucl_deviation + 1.96 * se,
      color = session
    ),
    width = .25,
    show.legend = FALSE
  ) +
  scale_fill_brewer(name = "", palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "",
       y = "Mean Euclidean Deviation")

tbl_cr_agg %>%
  left_join(tbl_movement[, c("participant_id", "mean_accuracy", "mean_delta_accuracy")],
            by = "participant_id") %>% select(
              participant_id,
              session,
              n_categories,
              mean_eucl_deviation,
              mean_accuracy,
              mean_delta_accuracy
            ) %>% filter(n_categories == 2) %>%
  pivot_longer(c(mean_accuracy, mean_delta_accuracy)) %>%
  mutate(name = factor(
    name,
    labels = c(
      "Final Categorization Accuracy",
      "Delta Categorization Accuracy"
    )
  )) %>%
  ggplot(aes(mean_eucl_deviation, value, group = name)) +
  facet_wrap( ~ name) +
  geom_point(aes(color = name), show.legend = FALSE) +
  geom_smooth(method = "lm", aes(color = name), show.legend = FALSE) +
  scale_color_brewer(palette = "Set1") +
  theme_bw() +
  labs(x = "Euclidean Deviation Reproduction",
       y = "(Delta) Categorization Accuracy")

# lmes
## movement to center
m_rs_cr <-
  lme(
    d_closest ~ session * category,
    random = ~ 1 + session + category + session:category |
      participant_id,
    data = tbl_cr %>% filter(n_categories == "Experimental Group")
  )
summary(m_rs_cr)
anova(m_rs_cr)
tbl_cr$preds <- predict(m_rs_sim, m_rs_cr)

m_rs_cr_control <-
  lme(
    d_closest ~ session,
    random = ~ 1 + session | participant_id,
    data = tbl_cr %>% filter(n_categories == "Control Group")
  )
summary(m_rs_cr_control)





# Behavioral Representational Similarity Analysis -------------------------


p_id <- unique(tbl_cr$participant_id)
l_rsa_delta <- map(p_id, delta_representational_distance, tbl_cr = tbl_cr)
ggplot(l_rsa_delta[[8]], aes(l, r)) +
  geom_raster(aes(fill = d_euclidean_delta)) +
  theme_bw() +
  scale_fill_viridis_c(name = "Euclidean Distance Delta") +
  scale_x_continuous(breaks = seq(0, 100, by = 10)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10)) +
  labs(x = "Stimulus ID 1", y = "Stimulus ID 2")

ggplot(l_rsa[[23]], aes(d_euclidean_true, d_euclidean_response)) +
  geom_point() +
  geom_abline(intercept = 0, slope = 1)











