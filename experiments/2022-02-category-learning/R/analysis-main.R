rm(list = ls())
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
library(ids)


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


sim_center <- "ellipses"


# Load Data and Preprocess Data -------------------------------------------

path_data <- c(
  "experiments/2022-02-category-learning/data/2022-03-30-treps1-pilot-1/",
  "experiments/2022-02-category-learning/data/2022-04-20-treps1-pilot-2/",
  "experiments/2022-02-category-learning/data/2022-04-21-treps1-experiment/"
)

# hash prolific ids and load data
# only hashed ids are uploaded on osf
# returned_timeout <- timeout_and_returns_e1()
# walk(path_data[2:3], hash_ids_e1_e2, participants_returned = timeout_and_returns_e1(), expt = 1)

l_tbls_data <- map(path_data[2:3], load_data_e1)
l_tbl_data <-
  list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))

# these are the files uploaded on osf
write_csv(l_tbl_data[[1]], "experiments/2022-02-category-learning/data/continuous-reproduction.csv")
write_csv(l_tbl_data[[2]], "experiments/2022-02-category-learning/data/secondary-task.csv")

# add deviation from response to stimulus
l_deviations <- add_deviations(l_tbl_data, sim_center = sim_center)
l_tbl_data[[1]] <- l_deviations$tbl_cr

l_cases <- preprocess_data(l_tbl_data, 192, 600, n_sds = 1)
tbl_cr <- l_cases$l_guessing$keep$tbl_cr
tbl_cat_sim <- l_cases$l_guessing$keep$tbl_cat_sim

l_deviations <- map(l_deviations, ~ filter(.x, substr(participant_id, 1, 6) %in% substr(unique(tbl_cat_sim$participant_id), 1, 6)))



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


same_n <-
  length(unique(tbl_cr$participant_id)) == length(unique(tbl_cat_sim$participant_id))
cat(str_c("same n participants in cat and cr data sets: ", same_n, "\n"))

tbl_cat_sim <- add_binned_trial_id(tbl_cat_sim, 20, 40)


rbind(
  l_cases$l_outliers$drop$tbl_cr, l_cases$l_guessing$drop$tbl_cr, l_cases$l_incomplete$drop$tbl_cr
) %>% rbind(tbl_cr) %>% group_by(participant_id) %>% count(Sex) %>% ungroup() %>% count(Sex)
cat(str_c("dropouts: ", length(unique(l_cases$l_incomplete$drop$tbl_cr$participant_id))))
cat(str_c("outliers in cr: ", length(unique(l_cases$l_outliers$drop$tbl_cr$participant_id))))

repeats <- tbl_cr %>% count(participant_id) %>% arrange(desc(n)) %>% filter(n >= 235)
tbl_cr <- tbl_cr %>% filter(!(participant_id %in% repeats$participant_id))
tbl_cat_sim <- tbl_cat_sim %>% filter(!(participant_id %in% repeats$participant_id))


# inclusions
cat(str_c("final N analyzed: ", length(unique(tbl_cr$participant_id)), "\n"))

# exclusions
cat(str_c("N excluded: ", length(excl_incomplete) + length(excl_outlier) + length(excl_guessing) + nrow(repeats)))


# ns per group
tbl_cr %>% group_by(participant_id, n_categories) %>% count() %>% 
  group_by(n_categories) %>% count()

saveRDS(tbl_cr, "experiments/2022-02-category-learning/data/tbl_cr.rds")
saveRDS(tbl_cat_sim, "experiments/2022-02-category-learning/data/tbl_cat_sim.rds")


# Categorization ----------------------------------------------------------

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
pl_cat_learn_psychonomics <- l_pl[[1]] + scale_color_viridis_d(name = "Category") +
  theme(legend.position = "bottom") + scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), breaks = seq(.5, .8, by = .1)) +
  coord_cartesian(ylim = c(.5, .85))
# by-participant trajectories
#l_pl[[2]]



tbl_cat_agg <-
  l_pl[[3]] %>% group_by(participant_id, trial_id_binned, n_categories) %>%
  summarize(accuracy = mean(accuracy_mn_participant)) %>% ungroup() %>%
  mutate(
    trial_id_binned = as.numeric(as.character(trial_id_binned)),
    trial_id_binned = trial_id_binned - mean(trial_id_binned)
  )
by_participant_coefs(tbl_cat_agg, "trial_id_binned", "accuracy", "LM Cat. Accuracy") +
  coord_cartesian(ylim = c(-.2, .2))

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


tbl_cat_grid <- aggregate_category_responses_by_x1x2(tbl_cat, 241)
sample_ids <- tbl_cat_grid %>% group_by(participant_id) %>%
  summarize(mean_accuracy = max(mean_accuracy)) %>%
  arrange(desc(mean_accuracy))
select_ids <- round(seq(1, nrow(sample_ids), length.out = 4))
sample_ids <-
  as.character(sample_ids[select_ids, "participant_id"] %>% as_vector() %>% unname())
plot_categorization_heatmaps(tbl_cat_grid %>% filter(participant_id %in% sample_ids), 2)

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

gt_or_reps <- "gt" # ground-truth center or representational center?
if (gt_or_reps == "gt") {
  l_movement <- movement_towards_category_center(
    tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[1], sim_center
  )
} else if (gt_or_reps == "reps") {
  l_movement <- movement_towards_category_center(
    tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[2], sim_center
  )
}

tbl_movement <- l_movement[[1]]
# plot movement towards category center against task2 accuracy

pls_moves_catlearn <- arrangeGrob(
  l_movement[[2]]$hist_movements,
  l_movement[[2]]$pl_delta,
  l_movement[[2]]$pl_last,
  nrow = 1
)

grid.draw(pls_moves_catlearn)

save_my_pdf_and_tiff(
  pls_moves_catlearn,
  "experiments/2022-02-category-learning/data/figures/moves-compilation.pdf",
  13, 4.5
)
write_csv(tbl_movement, str_c("experiments/2022-02-category-learning/data/movements-catacc-", gt_or_reps, ".csv"))

ggplot(tbl_movement,
       aes(mean_delta_accuracy, mean_accuracy, group = n_categories)) +
  geom_point() +
  geom_smooth(method = "lm", alpha = .2) +
  facet_wrap( ~ n_categories) +
  labs(x = "Delta Categorization Accuracy", y = "Final Categorization Accuracy") +
  theme_bw() +
  theme(
    strip.background =element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom"
  )  +
  scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))


# can we also include variability in the representations
# i.e. people with preciser category representations
# should experience stronger pull/push towards/away the center

plot_categorization_heatmaps(
  tbl_cat_grid %>% filter(participant_id %in% sample_ids), 2, "Mode"
) + geom_contour(
  data = tbl_preds_nb %>% filter(participant_id %in% substr(sample_ids, 1, 6)),
  aes(x1, x2, z = density, alpha = density),
  color = "black"
) + geom_point(
  data = tibble(x = 50, y = 50), aes(x, y), color = "#FF3333", size = 3.5)



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

saveRDS(tbl_sim, "experiments/2022-02-category-learning/data/tbl_sim.rds")


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

sample_ids_sim <-
  unique(tbl_sim$participant_id)[seq(1, length(unique(tbl_sim$participant_id)), length.out = 4)]
tbl_sim %>% group_by(participant_id, n_categories, distance_binned) %>%
  filter(participant_id %in% sample_ids_sim) %>%
  summarize(response_mn = mean(response)) %>%
  ggplot(aes(distance_binned, response_mn, group = participant_id)) +
  geom_line(aes(color = participant_id))


l_pl_sim <- plot_similarity_against_distance(
  tbl_sim, tbl_sim_ci, sample_ids_sim, sim_edges = c(1.5, 3)
)
l_pl_sim[[3]] <- l_pl_sim[[3]] + scale_x_continuous(expand = c(0, 0)) + 
  scale_y_continuous(expand = c(0, 0))

pl_sim_psychonomics <- ggplot() +
  geom_line(
    data = tbl_sim_ci %>% 
      filter(!(distance_binned %in% c(1, 13))) %>%
      mutate(n_categories = factor(n_categories, labels = "Similarity")),
    aes(distance_binned, response, group = n_categories,
        color = n_categories)) +
  # geom_smooth(
  #   data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
  #   aes(distance_binned, response),
  #   color = "#440154",
  #   method = "lm"
  # ) +
  geom_errorbar(
    data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
    aes(
      distance_binned,
      ymin = response - ci,
      ymax = response + ci,
      width = .2
    )
  ) +  geom_point(
    data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
    size = 2, color = "white", aes(distance_binned, response)
  ) +
  geom_point(data = tbl_sim_ci %>% filter(!(distance_binned %in% c(1, 13))),
             aes(distance_binned, response)) +
  theme_bw() +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  coord_cartesian(ylim = c(1, 4)) +
  labs(x = "Euclidean Distance",
       y = "Average Similarity (Range: 1 - 4)") +
  scale_color_viridis_d(name = "Group") +
  theme(legend.position = "bottom")

tbl_sim_agg_subj <- tbl_sim %>%
  mutate(distance_binned = distance_binned - mean(distance_binned)) %>%
  rutils::grouped_agg(c(participant_id, n_categories, distance_binned), c(response, rt))

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

doubles <- tbl_cr %>% count(participant_id) %>% filter(n > 200)
missings <- tbl_cr %>% count(participant_id) %>% filter(n < 200)

# only use first trial for a session when participants re-started the experiment
tbl_first_attempt <- tbl_cr %>% filter(participant_id %in% doubles$participant_id) %>% 
  group_by(participant_id, session, stim_id) %>%
  mutate(rwn = row_number(stim_id)) %>%
  filter(rwn == 1)
tbl_cr <- tbl_cr %>% filter(!(participant_id %in% doubles$participant_id))
tbl_cr <- tbl_cr %>% filter(!(participant_id %in% missings$participant_id))
tbl_cr <- rbind(tbl_cr, tbl_first_attempt %>% select(-rwn))

# 2d marginals
pl_marginal_before <- plot_marginals_one_session(1, tbl_cr)
pl_marginal_after <- plot_marginals_one_session(2, tbl_cr)

# heat map of errors over 2d space

pl_heamaps <-
  plot_2d_binned_heatmaps(l_deviations$tbl_checker, l_deviations$tbl_checker_avg)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)


tbl_cr$n_categories <- factor(tbl_cr$n_categories, labels = c("Similarity", "2 Categories"))
l_empirical <- plot_distance_to_category_center(tbl_cr, sim_center = sim_center)
pl_d_by_category <- l_empirical$pl + facet_wrap(~ factor(category, labels = c("Bukil", "Venak"))) +
  theme(
    strip.background =element_rect(fill="white"), 
    strip.text = element_text(colour = 'black'), 
    legend.position = "bottom"
  )  +
  scale_fill_viridis_d(name = "Group") +
  scale_color_viridis_d() +
  scale_x_discrete(expand = c(0, 0)) + 
  scale_y_continuous(expand = expansion(add = c(0, .1)))

plot_distance_from_decision_boundary(tbl_cr, 10, sim_center = "ellipse")


# for psychonomics only plot category 2 stimuli to reduce complexity 
# with different designs a bit

pl_d_psychonomics <- plot_distance_psychonomics(
  l_empirical$tbl_cr_agg %>% 
    mutate(
      n_categories = fct_relevel(n_categories, "Similarity", after = 1)
    ) %>% filter(category == 2)
) + scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(0, .2)))
save_my_tiff(
  pl_d_psychonomics, 
  "experiments/2022-02-category-learning/data/figures/distances-centers-psychonomics.tiff", 
  5, 4.2
)

# 
# marrangeGrob(list(pl_avg_move, pl_empirical),
#              nrow = 1,
#              ncol = 2)
# 
tbl_cr_agg <-
  grouped_agg(tbl_cr,
              c(participant_id, session, n_categories),
              eucl_deviation)

ggplot(
  tbl_cr_agg %>% mutate(session = factor(
    session, levels = c(1, 2), labels = c("Before\nCategory Learning", "After\nCategory Learning"))
  ), aes(mean_eucl_deviation, group = session)) +
  geom_density(aes(color = session)) +
  facet_wrap(~ n_categories) +
  theme_bw() +
  scale_color_brewer(palette = "Set1", name = "") +
  labs(
    x = "Mean Euclidean Deviation",
    y = "Probability Density"
  )

pdg <- position_dodge(width = .15)
summarySEwithin(tbl_cr_agg, "mean_eucl_deviation", betweenvars = "n_categories", withinvars = "session") %>%
  mutate(session = factor(
    session,
    labels = c("Before\nCategory Learning", "After\nCategory Learning")
  )) %>%
  ggplot(aes(session, mean_eucl_deviation, group = n_categories)) +
  geom_point(aes(color = n_categories), position = pdg) +
  geom_line(aes(color = n_categories), show.legend = FALSE, position = pdg) +
  geom_errorbar(
    aes(
      ymin = mean_eucl_deviation - 1.96 * se,
      ymax = mean_eucl_deviation + 1.96 * se,
      color = n_categories
    ),
    width = .15, show.legend = FALSE, position = pdg
  ) +
  scale_fill_brewer(name = "", palette = "Set1") +
  scale_color_brewer(name = "Group", palette = "Set1") +
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
            ) %>% filter(n_categories == "2 Categories") %>%
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
    data = tbl_cr %>% filter(n_categories == "2 Categories")
  )
summary(m_rs_cr)
anova(m_rs_cr)
tbl_cr$preds <- predict(m_rs_sim, m_rs_cr)

m_rs_cr_control <-
  lme(
    d_closest ~ session,
    random = ~ 1 + session | participant_id,
    data = tbl_cr %>% filter(n_categories == "Similarity")
  )
summary(m_rs_cr_control)


# Behavioral Representational Similarity Analysis -------------------------


# calculate delta of pairwise distances for empirical data by participant
l_rsa_all <- pairwise_distances(tbl_cr)
plot_true_ds_vs_response_ds(l_rsa_all[["tbl_rsa"]])

f_name <- "data/2023-02-08-grid-search-vary-constrain-space.rds"
tbl_both <- load_predictions(f_name, sim_center = sim_center, is_simulation = TRUE)
tbl_rsa_delta_prediction <- delta_representational_distance("prediction", tbl_both)
pl_pred <- plot_distance_matrix(tbl_rsa_delta_prediction) +
  labs(x = "Stimulus ID 1", y = "Stimulus ID 2", title = "Model Matrix") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0))
pl_exp <- l_rsa_all$pl_m_avg_experimental +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0))
pl_control <- l_rsa_all$pl_m_avg_control +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0))

# color scale: delta after - delta before
pls_rsa <- arrangeGrob(pl_pred, pl_exp, pl_control, nrow = 1)
save_my_pdf_and_tiff(pls_rsa, "experiments/2022-02-category-learning/data/figures/rsa-avg-plots", 12, 4)

# correlation between model matrix and delta in responses
tbl_rsa_delta_prediction_lower <- tbl_rsa_delta_prediction %>% 
  filter(l >= r)
tbl_rsa_delta_prediction_lower %>% select(l, r, d_euclidean_delta) %>%
  left_join(
    l_rsa_all$tbl_rsa %>% select(l, r, n_categories, d_euclidean_delta),
    by = c("l", "r"), suffix = c("_pred", "_empirical")
  ) %>% group_by(n_categories) %>%
  summarise(
    corr = cor(d_euclidean_delta_pred, d_euclidean_delta_empirical),
    p_corr = cor.test(d_euclidean_delta_pred, d_euclidean_delta_empirical)$p.value
  )


# save some plots ---------------------------------------------------------


pl <- arrangeGrob(pl_cat_learn_psychonomics, l_pl_sim[[3]], pl_d_by_category, ncol = 3)
save_my_pdf_and_tiff(
  pl, 
  "experiments/2022-02-category-learning/data/figures/three-tasks-agg-overview", 
  13, 3.75
)
