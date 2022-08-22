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
library(plotly)


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


# Load Data and Preprocess Data -------------------------------------------

path_data <- c(
  "experiments/2022-07-category-learning-II/data/2022-08-16-treps2-experiment/",
  "experiments/2022-07-category-learning-II/data/2022-08-17-treps2-experiment/",
  "experiments/2022-07-category-learning-II/data/2022-08-18-treps2-experiment/",
  "experiments/2022-07-category-learning-II/data/2022-08-19-treps2-experiment/",
  "experiments/2022-07-category-learning-II/data/2022-08-19-treps2-experiment-II/",
  "experiments/2022-07-category-learning-II/data/2022-08-19-treps2-experiment-III/"  
)

# flag defining whether distance to category center in similarity condition
# is computed using ellipse center (i.e., middle of feature space) or
# category centers of four square categories

sim_center <- "square"

# Load Data ---------------------------------------------------------------

returned_timeout <- timeout_and_returns()

l_tbls_data <- map(path_data, load_data, participants_returned = returned_timeout)
l_tbl_data <-
  list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))


# add deviation from response to stimulus
l_deviations_all <- add_deviations(l_tbl_data, sim_center = sim_center)
l_tbl_data[[1]] <- l_deviations_all$tbl_cr


# Set Exclusion Criteria Appropriately ------------------------------------


l_cases <- preprocess_data(l_tbl_data, 200, 400)
tbl_cr <- l_cases$l_guessing$keep$tbl_cr
tbl_cat_sim <- rbind(l_cases$l_guessing$keep$tbl_cat_sim, l_cases$l_outliers$drop$tbl_cat_sim)

# exclusions
excl_incomplete <-
  dplyr::union(
    unique(l_cases$l_incomplete$drop[["tbl_cr"]]$participant_id),
    unique(l_cases$l_incomplete$drop[["tbl_cat"]]$participant_id)
  )
excl_outlier <-
  dplyr::union(
    unique(l_cases$l_outliers$drop[["tbl_cr"]]$participant_id),
    unique(l_cases$l_outliers$drop[["tbl_cat"]]$participant_id)
  )
excl_guessing <-
  dplyr::union(
    unique(l_cases$l_guessing$drop[["tbl_cr"]]$participant_id),
    unique(l_cases$l_guessing$drop[["tbl_cat"]]$participant_id)
  )

# inclusions
cat(str_c("final N analyzed: ", length(unique(tbl_cr$participant_id)), "\n"))

# exclusions
cat(str_c("N excluded: ", length(excl_incomplete) + length(excl_outlier) + length(excl_guessing)))

same_n <-
  length(unique(tbl_cr$participant_id)) == length(unique(tbl_cat_sim$participant_id))
cat(str_c("same n participants in cat and cr data sets: ", same_n, "\n"))
# ns per group
tbl_cr %>% group_by(participant_id, n_categories) %>% count() %>% 
  group_by(n_categories) %>% count()


l_tbl_data <-
  list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))
l_deviations_incl <- add_deviations(l_tbl_data, sim_center = sim_center, subset_ids = unique(tbl_cat_sim$participant_id))

tbl_cr_incomplete <- l_cases$l_incomplete$drop[["tbl_cr"]]
tbl_cr_incomplete %>% group_by(participant_id) %>% count() %>% arrange(desc(n))
tbl_cat_sim %>% group_by(participant_id) %>% count() %>% arrange(desc(n))


# Categorization ----------------------------------------------------------

# occasionally, the same trial was saved twice
tbl_cat_sim <- tbl_cat_sim %>%
  group_by(participant_id, trial_id) %>%
  mutate(rwn = row_number(participant_id)) %>%
  filter(rwn == 1) %>% select(-rwn)


tbl_cat_sim <- add_binned_trial_id(tbl_cat_sim, 20, 0)
tbl_cat <-
  tbl_cat_sim %>% filter(n_categories %in% c(2, 4), as.numeric(as.character(trial_id_binned)) <= 15)


tbl_cat_overview <- tbl_cat %>%
  grouped_agg(c(n_categories, participant_id), c(accuracy, rt)) %>%
  arrange(mean_rt)
tbl_chance2 <- tbl_cat_overview %>% group_by(n_categories) %>%
  summarize(dummy = mean(mean_accuracy)) %>%
  mutate(p_chance = 1 / as.numeric(str_extract(n_categories, "[2-4]$")))

# categorization accuracy overview
tbl_dropouts <- tbl_cat_overview %>% filter(mean_accuracy <= .7) %>% select(participant_id)

l_histogram <- histograms_accuracies_rts(tbl_cat_overview)
l_histogram[[1]] + coord_cartesian(xlim = c(.4, 1))
l_histogram[[2]] + coord_cartesian(xlim = c(750, 2000))


l_pl <- plot_categorization_accuracy_against_blocks(
  tbl_cat,# %>% filter(!(participant_id %in% tbl_dropouts$participant_id)), 
  show_errorbars = TRUE
)
# overall trajectory
l_pl[[1]]
# by-participant trajectories
l_pl[[2]]


tbl_cat_agg <-
  l_pl[[3]] %>% group_by(participant_id, n_categories, trial_id_binned) %>%
  summarize(accuracy = mean(accuracy_mn_participant)) %>% group_by(participant_id) %>%
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

l_movement <-
  movement_towards_category_center(tbl_cat_sim, tbl_cr, "d_closest", sim_center)
tbl_movement <- l_movement[[1]]

# plot movement towards category center against task2 accuracy
l_movement[[2]]

marrangeGrob(list(l_pl[[1]], l_movement[[2]]$hist_delta_last),
             nrow = 1,
             ncol = 2)

tbl_cat_grid <- aggregate_category_responses_by_x1x2(tbl_cat, 201)
sample_ids <- tbl_cat_grid %>% group_by(participant_id) %>%
  summarize(mean_accuracy = max(mean_accuracy)) %>%
  arrange(desc(mean_accuracy))
select_ids <- round(seq(1, nrow(sample_ids), length.out = 4))
sample_ids <-
  as.character(sample_ids[select_ids, "participant_id"] %>% as_vector() %>% unname())
plot_categorization_heatmaps(tbl_cat_grid %>% filter(participant_id %in% sample_ids), c(2, 4))
mean_against_delta_cat_accuracy(tbl_movement)

# prototype analyses

participant_ids_4_cat <-
  unique(tbl_cat$participant_id[tbl_cat$n_categories == 4]) %>% as.character()
l_nb <- map(
  participant_ids_4_cat, fit_predict_nb,
  tbl = tbl_cat %>% filter(n_categories == 4)
)
names(l_nb) <- participant_ids_4_cat

tbl_square <- tbl_cr %>% filter(as.numeric(n_categories) > 1)
tbl_square$participant_id <- factor(tbl_square$participant_id)
l_tbl_square <- split(tbl_square, tbl_square$participant_id)
tbl_d2_rep_center <- map2(
  l_tbl_square, l_nb, 
  d2_rep_center_square
) %>% reduce(rbind)
tbl_cr <- tbl_cr %>% 
  left_join(tbl_d2_rep_center, by = c("participant_id", "session", "trial_id"))

representational_precision <- function(nb_participant) {
  tbl_params <- as_tibble(data.frame(t(matrix(unlist(
    nb_participant[[1]][["tables"]]
  ), nrow = 4, ncol = 4))))
  names(tbl_params) <- c("mean_x1", "sd_x1", "mean_x2", "sd_x2")
  mean(colMeans(tbl_params %>% select(starts_with("sd"))))
}
v_precision_representation <- map_dbl(l_nb, representational_precision)
tbl_precision_representation <- tibble(v_precision_representation)
tbl_precision_representation$participant_id <- participant_ids_4_cat
tbl_precision <- tbl_cr %>% filter(n_categories == "4") %>%
  grouped_agg(participant_id, c(d_closest, d_rep_center)) %>%
  left_join(tbl_precision_representation, by = "participant_id") %>%
  select(-c(n, nunique_d_closest, nunique_d_rep_center))
tbl_precision %>% pivot_longer(
  cols = c(mean_d_closest, mean_d_rep_center)
) %>% mutate(
  name = factor(
    name, 
    labels = c("True Center", "Representational Center")
  )) %>%
ggplot(aes(v_precision_representation, value, group = name)) +
  geom_point(aes(color = name)) +
  scale_color_brewer(palette = "Set1", name = "Distance to") +
  theme_bw() +
  labs(x = "Representational Precision", y = "Distance to Closest Prototype")
# 
tbl_preds_nb <- reduce(map(l_nb, 2), rbind) %>%
  mutate(participant_id = fct_inorder(substr(participant_id, 1, 6), ordered = TRUE))

l_movement <-
  movement_towards_category_center(
    tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[2], sim_center
  )
tbl_movement <- l_movement[[1]]
l_movement[[2]][[2]]

plot_categorization_heatmaps(
  tbl_cat_grid %>% filter(participant_id %in% sample_ids),
  4, "Mode"
) + geom_contour(
  data = tbl_preds_nb %>% 
    filter(participant_id %in% substr(sample_ids, 1, 6)),
  aes(x1, x2, group = category, z = density),
  color = "black"
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
  #filter(participant_id %in% sample_ids) %>%
  summarize(response_mn = mean(response), n = n()) %>%
  ggplot(aes(distance_binned, response_mn, group = as.numeric(participant_id))) +
  geom_line(aes(color = as.numeric(participant_id))) +
  geom_point(aes(color = as.numeric(participant_id), size = n)) +
  theme_bw() +
  scale_color_viridis_c(name = "Participant ID") +
  scale_size_continuous(name = "Nr. Similarity Judgments") +
  labs(x = "Euclidean Distance (Binned)",
       y = "Average Similarity (Range: 1 - 4)")

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
  labs(x = "Euclidean Distance (Binned)",
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
# 
# doubles <- tbl_cr %>% count(participant_id) %>% filter(n > 200)
# missings <- tbl_cr %>% count(participant_id) %>% filter(n < 200)
# 
# # only use first trial for a session when participants re-started the experiment
# tbl_first_attempt <- tbl_cr %>% filter(participant_id %in% doubles$participant_id) %>% 
#   group_by(participant_id, session, stim_id) %>%
#   mutate(rwn = row_number(stim_id)) %>%
#   filter(rwn == 1)
# tbl_cr <- tbl_cr %>% filter(!(participant_id %in% doubles$participant_id))
# tbl_cr <- tbl_cr %>% filter(!(participant_id %in% missings$participant_id))
# tbl_cr <- rbind(tbl_cr, tbl_first_attempt %>% select(-rwn))


tbl_cr %>% group_by(participant_id) %>% summarize(move = sum(move_sum), n_trials = n()) %>%
  mutate(move_avg = move/n_trials) %>% arrange(move_avg)

# 2d marginals
pl_marginal_before <- plot_marginals_one_session(1, tbl_cr)
pl_marginal_after <- plot_marginals_one_session(2, tbl_cr)

# heat map of errors over 2d space

pl_heamaps <-
  plot_2d_binned_heatmaps(l_deviations_incl$tbl_checker, l_deviations_incl$tbl_checker_avg)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)


tbl_cr$n_categories <- fct_inseq(tbl_cr$n_categories)
levels(tbl_cr$n_categories) <- c("Similarity", "4 Categories")
pl_empirical <- plot_distance_to_category_center(tbl_cr, sim_center = sim_center)
pl_empirical + labs(title = str_c("Distance in Similarity Condition = ", sim_center))

plot_distance_from_decision_boundary(tbl_cr, 10)




marrangeGrob(list(pl_avg_move, pl_empirical),
             nrow = 1,
             ncol = 2)

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
            ) %>% filter(n_categories == "4 Categories") %>%
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

# calculate delta of pairwise distances for empirical data by participant
l_rsa_all <- pairwise_distances(tbl_cr)
plot_true_ds_vs_response_ds(l_rsa_all[["tbl_rsa"]])

f_name <- "data/2022-06-13-grid-search-vary-constrain-space.rds"
tbl_both <- load_predictions(f_name)
tbl_rsa_delta_prediction <- delta_representational_distance("prediction", tbl_both)
plot_distance_matrix(tbl_rsa_delta_prediction) +
  labs(x = "Stimulus ID 1", y = "Stimulus ID 2", title = "Model Matrix")

# correlation between model matrix and delta in responses
tbl_rsa_delta_prediction_lower <- tbl_rsa_delta_prediction %>% 
  filter(l >= r)
tbl_rsa_delta_prediction_lower %>% select(l, r, d_euclidean_delta) %>%
  left_join(
    tbl_rsa %>% select(l, r, n_categories, d_euclidean_delta),
    by = c("l", "r"), suffix = c("_pred", "_empirical")
  ) %>% group_by(n_categories) %>%
  summarise(corr = cor(d_euclidean_delta_pred, d_euclidean_delta_empirical))



