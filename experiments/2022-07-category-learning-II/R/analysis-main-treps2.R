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
  "experiments/2022-07-category-learning-II/data/2022-08-19-treps2-experiment-III/",
  "experiments/2022-07-category-learning-II/data/2022-09-02-treps2-experiment/"
)

# flag defining whether distance to category center in similarity condition
# is computed using ellipse center (i.e., middle of feature space) or
# category centers of four square categories

sim_center <- "square"

# Load Data ---------------------------------------------------------------

returned_timeout <- timeout_and_returns_e2()

l_tbls_data <- map(path_data, load_data, participants_returned = returned_timeout)
l_tbl_data <-
  list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))


# add several distance measures: response to stimulus, response to true
# category center, & response to closest true decision boundary

l_deviations_all <- add_deviations(
  l_tbl_data, sim_center = sim_center, slider_start_postition = "random"
)
l_tbl_data[[1]] <- l_deviations_all$tbl_cr


# Set Exclusion Criteria Appropriately ------------------------------------


l_cases <- preprocess_data(l_tbl_data, 200, 400)
tbl_cr <- l_cases$l_guessing$keep$tbl_cr
tbl_cat_sim <- l_cases$l_guessing$keep$tbl_cat_sim

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

l_cat_sim <- separate_cat_and_sim(tbl_cat_sim)
tbl_cat_sim <- l_cat_sim[["tbl_cat_sim"]]
tbl_cat <- l_cat_sim[["tbl_cat"]]
tbl_sim <- l_cat_sim[["tbl_sim"]]

# saveRDS(tbl_cr, file = str_c("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri.rds"))
# saveRDS(tbl_cat, file = str_c("experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri.rds"))
# saveRDS(tbl_sim, file = str_c("experiments/2022-07-category-learning-II/data/tbl_sim-treps-long-ri.rds"))


# Categorization ----------------------------------------------------------


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
l_pl[[1]] + labs(x = "Block of 20 Trials", y = "Categorization Accuracy", caption = "")
# by-participant trajectories
#l_pl[[2]]

# by-participant intercepts vs. slopes from individual lms
tbl_cat_agg <-
  l_pl[[3]] %>% group_by(participant_id, n_categories, trial_id_binned) %>%
  summarize(accuracy = mean(accuracy_mn_participant)) %>% group_by(participant_id) %>%
  mutate(
    trial_id_binned = as.numeric(as.character(trial_id_binned)),
    trial_id_binned = trial_id_binned - mean(trial_id_binned)
  )
by_participant_coefs(
  tbl_cat_agg, "trial_id_binned", "accuracy", "LM Cat. Accuracy"
)

# stat analysis

tbl_cat_agg <-
  tbl_cat %>% group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(accuracy_mn = mean(accuracy)) %>% ungroup() %>%
  mutate(
    trial_id_binned = as.numeric(as.character(trial_id_binned)),
    trial_id_binned = scale(trial_id_binned)[, 1]
  ) %>% group_by(cat_true, trial_id_binned) %>%
  mutate(participant_id_num = row_number(participant_id))

# gt for ground truth as compared to representations
l_movement_gt <-
  movement_towards_category_center(tbl_cat_sim, tbl_cr, "d_closest", sim_center)
tbl_movement_gt <- l_movement_gt[[1]]

# plot movement towards category center against task2 accuracy
marrangeGrob(list(
  l_movement_gt[[2]][[1]],
  l_movement_gt[[2]][[2]],
  l_movement_gt[[2]][[3]]
), nrow = 1, ncol = 3)

marrangeGrob(
  list(l_pl[[1]], l_movement_gt[[2]]$hist_delta_last), nrow = 1, ncol = 2
)

# exclude initial trials from following analyses
n_start_exclude <- 200
tbl_cat_grid <- aggregate_category_responses_by_x1x2(tbl_cat, n_start_exclude)
sample_ids <- tbl_cat_grid %>% group_by(participant_id) %>%
  summarize(mean_accuracy = max(mean_accuracy)) %>%
  arrange(desc(mean_accuracy))
select_ids <- round(seq(1, nrow(sample_ids), length.out = 4))
sample_ids <-
  as.character(sample_ids[select_ids, "participant_id"] %>% as_vector() %>% unname())
plot_categorization_heatmaps(tbl_cat_grid %>% filter(participant_id %in% sample_ids), c(2, 4))
mean_against_delta_cat_accuracy(tbl_movement_gt)


# prototype analyses ------------------------------------------------------

# fit individual nb models
participant_ids_4_cat <-
  unique(tbl_cat$participant_id[tbl_cat$n_categories == 4]) %>% as.character()
l_nb <- by_participant_nb(tbl_cat %>% filter(trial_id >= n_start_exclude), participant_ids_4_cat)

# distance to representational centers and precision of representations
tbl_square <- tbl_cr %>% filter(as.numeric(n_categories) == 4)
tbl_square$participant_id <- factor(tbl_square$participant_id)
l_tbl_square <- split(tbl_square, tbl_square$participant_id)
tbl_d2_rep_center <- map2(l_tbl_square, l_nb, d2_rep_center_square) %>% reduce(rbind)
tbl_cr <- tbl_cr %>% 
  left_join(tbl_d2_rep_center, by = c("participant_id", "session", "trial_id"))
l_movement_representation <- movement_towards_category_center(
  tbl_cat_sim, tbl_cr, "d_rep_center", sim_center
)
tbl_movement_representation <- l_movement_representation[[1]]
marrangeGrob(list(
  l_movement_representation[[2]][[1]],
  l_movement_representation[[2]][[2]]
), nrow = 1, ncol = 2)


tbl_precision <- combine_precision_and_movements(l_nb, participant_ids_4_cat)
plot_movement_against_precision(tbl_precision)
plot_heatmaps_with_representations(l_nb, sample_ids)



# Similarity Judgments ----------------------------------------------------

tbl_sim_agg <- tbl_sim %>%
  rutils::grouped_agg(c(distance_binned), c(response, rt))
tbl_sim_ci <- summarySEwithin(
  tbl_sim, "response", "n_categories", "distance_binned", "participant_id", TRUE
) %>% as_tibble()

tbl_sim_ci$distance_binned <-
  as.numeric(as.character(tbl_sim_ci$distance_binned))

# some sample participants to plot similarity ratings
sample_ids_sim <-
  unique(tbl_sim$participant_id)[seq(1, length(unique(tbl_sim$participant_id)), length.out = 4)]
l_pl_sim <- plot_similarity_against_distance(tbl_sim, tbl_sim_ci, sample_ids_sim)
grid.arrange(l_pl_sim[[1]], l_pl_sim[[2]], nrow = 1, ncol = 2)


tbl_sim_agg_subj <- tbl_sim %>%
  mutate(distance_binned = distance_binned - mean(distance_binned)) %>%
  rutils::grouped_agg(c(participant_id, distance_binned), c(response, rt))

m_rs_sim <-
  nlme::lme(
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
pl_heamaps <- plot_2d_binned_heatmaps(
  l_deviations_incl$tbl_checker, l_deviations_incl$tbl_checker_avg
)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)


tbl_cr$n_categories <- fct_inseq(tbl_cr$n_categories)
levels(tbl_cr$n_categories) <- c("Similarity", "4 Categories")
tbl_cr$n_categories <- fct_relevel(tbl_cr$n_categories, "Similarity", after = 1)
l_empirical <- plot_distance_to_category_center(tbl_cr, sim_center = sim_center)

pl_psychonomics_means <- l_empirical$pl +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank()) +
  labs(title = str_c("Distance in Similarity Condition = ", sim_center)) +
  theme(
    legend.position = "bottom",
    plot.title = element_blank()) +
  scale_fill_viridis_d(name = "Session") +
  scale_color_viridis_d()

save_my_tiff(
  pl_psychonomics_means, 
  "experiments/2022-07-category-learning-II/data/figures/movements-means-psychonomics.tiff",
  5, 4
)

plot_distance_from_decision_boundary(tbl_cr, 10)

# 
# marrangeGrob(list(pl_avg_move, pl_empirical),
#              nrow = 1,
#              ncol = 2)

tbl_cr_agg <-
  grouped_agg(tbl_cr,
              c(participant_id, session, n_categories),
              eucl_deviation)
l_pl_euclidean <- plot_deviations_from_stimulus(tbl_cr_agg)
grid.arrange(l_pl_euclidean$pl_density, l_pl_euclidean$pl_agg, nrow = 1, ncol = 2)


# lmes
## movement to center
m_rs_cr <-
  nlme::lme(
    scale(sqrt(d_closest), scale = FALSE) ~ session * n_categories,
    random = ~ 1 + session | participant_id,
    data = tbl_cr
  )
summary(m_rs_cr)
anova(m_rs_cr)
tbl_cr$preds <- predict(m_rs_sim, m_rs_cr)

m_rs_cr_control <-
  nlme::lme(
    sqrt(d_closest) ~ session,
    random = ~ 1 + session | participant_id,
    data = tbl_cr %>% filter(n_categories == "Control Group")
  )
summary(m_rs_cr_control)

# Behavioral Representational Similarity Analysis -------------------------

# calculate delta of pairwise distances for empirical data by participant
l_rsa_all <- pairwise_distances(tbl_cr)
# repulsion and attraction taking place at the same time
# could maybe related to spicer et al. (2022) psych science

plot_true_ds_vs_response_ds(l_rsa_all[["tbl_rsa"]])

f_name <- "data/2022-06-13-grid-search-vary-constrain-space.rds"
f_name <- "data/2022-08-24-grid-search-vary-constrain-space.rds"
tbl_both <- load_predictions(f_name, sim_center = "square", is_simulation = TRUE)
tbl_rsa_delta_prediction <- delta_representational_distance("prediction", tbl_both)
plot_distance_matrix(tbl_rsa_delta_prediction) +
  labs(x = "Stimulus ID 1", y = "Stimulus ID 2", title = "Model Matrix")

# correlation between model matrix and delta in responses
tbl_rsa_delta_prediction_lower <- tbl_rsa_delta_prediction %>% 
  filter(l >= r)
tbl_rsa_delta_prediction_lower %>% dplyr::select(l, r, d_euclidean_delta) %>%
  left_join(
    l_rsa_all[["tbl_rsa"]] %>% dplyr::select(l, r, n_categories, d_euclidean_delta),
    by = c("l", "r"), suffix = c("_pred", "_empirical")
  ) %>% group_by(n_categories) %>%
  summarise(corr = cor(d_euclidean_delta_pred, d_euclidean_delta_empirical))



