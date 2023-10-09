rm(list = ls())
# Some Notes

# for simulation f_stretch <- 1, f_shift <- 0
# for empirical data f_stretch <- 9 f_shift <- 1
# as in empirical data x1 and x2 ranged from 1-100 and the ellipses are scaled 

# Import Packages ---------------------------------------------------------

library(ids)
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






# hash prolific ids and load data
# only hashed ids are uploaded on osf
# returned_timeout <- timeout_and_returns_e2()
# walk(path_data, hash_ids_e1_e2, participants_returned = returned_timeout, expt = 2)

# l_tbls_data <- map(path_data, load_data_e1)
saveRDS(l_tbls_data, file = "experiments/2022-07-category-learning-II/data/l_tbls_data.rds")
l_tbls_data <- readRDS("experiments/2022-07-category-learning-II/data/l_tbls_data.rds")
# l_tbl_data <-
#   list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))
# 
# # these are the files uploaded on osf
# write_csv(l_tbl_data[[1]], "experiments/2022-07-category-learning-II/data/continuous-reproduction.csv")
# write_csv(l_tbl_data[[2]], "experiments/2022-07-category-learning-II/data/secondary-task.csv")

l_tbl_data <- list()
l_tbl_data[[1]] <- read_csv("experiments/2022-07-category-learning-II/data/continuous-reproduction.csv")
l_tbl_data[[2]] <- read_csv("experiments/2022-07-category-learning-II/data/secondary-task.csv")

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

rbind(
  l_cases$l_outliers$drop$tbl_cr, l_cases$l_guessing$drop$tbl_cr, l_cases$l_incomplete$drop$tbl_cr
) %>% rbind(tbl_cr) %>% group_by(participant_id) %>% count(Sex) %>% ungroup() %>% count(Sex)
cat(str_c("dropouts: ", length(unique(l_cases$l_incomplete$drop$tbl_cr$participant_id))))
cat(str_c("outliers in cr: ", length(unique(l_cases$l_outliers$drop$tbl_cr$participant_id))))

repeats <- tbl_cr %>% count(participant_id) %>% arrange(desc(n)) %>% arrange(desc(n))
# none of remaining participants did restart experiment

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

saveRDS(tbl_cr, file = str_c("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri.rds"))
saveRDS(tbl_cat, file = str_c("experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri.rds"))
saveRDS(tbl_sim, file = str_c("experiments/2022-07-category-learning-II/data/tbl_sim-treps-long-ri.rds"))


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

tbl_cat$cat_true <- as.numeric(tbl_cat$cat_true)
l_pl <- plot_categorization_accuracy_against_blocks(
  tbl_cat,# %>% filter(!(participant_id %in% tbl_dropouts$participant_id)), 
  show_errorbars = TRUE
)
pl_cat_learn_pretty <- l_pl[[1]] + 
  scale_color_manual(values = c("#A4D3EE", "#CD4F39", "#CE7E72", "#C3A9AF"), name = "Category") +
  theme(legend.position = "bottom") + scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(.01, .01)))
# overall trajectory
pl_cat_agg <- l_pl[[1]] + labs(x = "Block of 20 Trials", y = "Categorization Accuracy") + # , caption = ""
  scale_color_viridis_d(name = "Category")
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


# prototype analyses ------------------------------------------------------

# fit individual nb models
participant_ids_4_cat <-
  unique(tbl_cat$participant_id[tbl_cat$n_categories == 4]) %>% as.character()
l_nb <- by_participant_nb(tbl_cat %>% filter(trial_id >= n_start_exclude), participant_ids_4_cat)

# distance to representational centers and precision of representations
levels(tbl_cr$n_categories) <- c(4, 1)
tbl_square <- tbl_cr %>% filter(n_categories == 4)
tbl_square$participant_id <- factor(tbl_square$participant_id)
l_tbl_square <- split(tbl_square, tbl_square$participant_id)
tbl_d2_rep_center <- map2(l_tbl_square, l_nb, d2_rep_center_square) %>% reduce(rbind)
tbl_cr <- tbl_cr %>% 
  left_join(tbl_d2_rep_center, by = c("participant_id", "session", "trial_id"))


gt_or_reps <- "gt"
if (gt_or_reps == "gt") {
  l_movement <- movement_towards_category_center(
    tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[1], sim_center
  )
  tbl_movement_gt <- l_movement[[1]]
} else if (gt_or_reps == "reps") {
  l_movement <- movement_towards_category_center(
    tbl_cat_sim, tbl_cr, c("d_closest", "d_rep_center")[2], sim_center
  )
}

tbl_movement <- l_movement[[1]]
mean_against_delta_cat_accuracy(tbl_movement)

marrangeGrob(list(
  l_movement[[2]][[1]],
  l_movement[[2]][[2]]
), nrow = 1, ncol = 2)

write_csv(tbl_movement, str_c("experiments/2022-07-category-learning-II/data/movements-catacc-", gt_or_reps, ".csv"))

# plot movement towards category center against task2 accuracy
marrangeGrob(list(
  l_movement[[2]][[1]],
  l_movement[[2]][[2]],
  l_movement[[2]][[3]]
), nrow = 1, ncol = 3)

marrangeGrob(
  list(l_pl[[1]], l_movement[[2]]$hist_delta_last), nrow = 1, ncol = 2
)

pls_moves_catlearn <- arrangeGrob(
  l_movement[[2]]$hist_movements,
  l_movement[[2]]$pl_delta,
  l_movement[[2]]$pl_last,
  nrow = 1
)

save_my_pdf_and_tiff(
  pls_moves_catlearn,
  "experiments/2022-07-category-learning-II/data/figures/moves-compilation",
  13, 4.5
)

save_my_pdf_and_tiff(
  pls_moves_catlearn,
  "figures/moves-compilation-e2",
  13, 4.5
)



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
l_pl_sim <- plot_similarity_against_distance(tbl_sim, tbl_sim_ci, sample_ids_sim, sim_edges = c(1, 3.5))
l_pl_sim[[3]] <- l_pl_sim[[3]] + scale_color_manual(values = "#FDE725FF", name = "Group")
save_my_pdf_and_tiff(
  pls_moves_catlearn,
  "experiments/2022-07-category-learning-II/data/figures/moves-compilation",
  13, 4.5
)
grid.arrange(l_pl_sim[[1]], l_pl_sim[[2]], nrow = 1, ncol = 2)

grid.arrange(pl_cat_agg, l_pl_sim[[2]], nrow = 1)


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

n_consider <- 100
tbl_cr <- tbl_cr %>% filter(trial_id < 100 | session == 1)



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
  # theme(axis.title.x=element_blank(),
  #       axis.text.x=element_blank(),
  #       axis.ticks.x=element_blank()) +
  labs(title = str_c("Distance in Similarity Condition = ", sim_center)) +
  theme(
    legend.position = "bottom") +
  scale_fill_viridis_d(name = "Session") +
  scale_color_viridis_d() +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(0, .2)))

save_my_tiff(
  pl_psychonomics_means, 
  "experiments/2022-07-category-learning-II/data/figures/movements-means-psychonomics.tiff",
  5, 4
)

plot_distance_from_decision_boundary(tbl_cr, 10)


tbl_cr_stim <- grouped_agg(
  tbl_cr, c(session, n_categories, x1_true, x2_true), c(x1_response, x2_response)
) %>% ungroup()
ggplot(data = tbl_cr_stim) +
  geom_point(aes(mean_x1_response, mean_x2_response), color = "lightblue") +
  geom_point(aes(x1_true, x2_true), color = "black") +
  geom_segment(aes(
    x = x1_true, xend = mean_x1_response, y = x2_true, yend = mean_x2_response
  ),
  arrow = grid::arrow(length = unit(0.02, "npc")), color = "grey80") +
  facet_grid(n_categories ~ session) + 
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(x = expression(x[1]), y = expression(x[2])) + 
  theme(strip.background = element_rect(fill = "white")) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")


tbl_cr_move <- tbl_cr_stim %>% filter(session == 1) %>%
  left_join(
    tbl_cr_stim %>% 
      filter(session == 2) %>% 
      dplyr::select(n_categories, x1_true, x2_true, mean_x1_response, mean_x2_response),
    by = c("n_categories", "x1_true", "x2_true"), suffix = c("_bef", "_aft")
  ) %>% mutate(
    x1_move = mean_x1_response_aft - mean_x1_response_bef,
    x2_move = mean_x2_response_aft - mean_x2_response_bef
  )


pl_2d_moves <- ggplot(data = tbl_cr_move) +
  geom_point(aes(x1_true, x2_true), color = "black") +
  geom_segment(aes(
    x = mean_x1_response_bef, xend = mean_x1_response_bef + x1_move, 
    y = mean_x2_response_bef, yend = mean_x2_response_bef + x2_move
  ),
  arrow = grid::arrow(length = unit(0.02, "npc")), color = "grey60") +
  geom_segment(aes(
    x = x1_true, xend = mean_x1_response_bef, 
    y = x2_true, yend = mean_x2_response_bef
  ), color = "grey90", linetype = "dotdash"
  ) + facet_wrap(~ n_categories) + 
  theme_bw() +
  scale_x_continuous(expand = c(0.02, 0.02)) +
  scale_y_continuous(expand = c(0.02, 0.02)) +
  labs(x = expression(x[1]), y = expression(x[2])) + 
  theme(strip.background = element_rect(fill = "white")) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")
save_my_pdf_and_tiff(
  pl_2d_moves,
  "figures/2d-moves-e2",
  8, 4.25
)

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

plot_true_ds_vs_response_ds(l_rsa_all[["tbl_rsa"]])

f_name <- "data/2023-01-27-grid-search-vary-constrain-space.rds"
tbl_both <- load_predictions(f_name, sim_center = "square", is_simulation = TRUE)
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
save_my_pdf_and_tiff(pls_rsa, "experiments/2022-07-category-learning-II/data/figures/rsa-avg-plots", 12, 4)

# correlation between model matrix and delta in responses
tbl_rsa_delta_prediction_lower <- tbl_rsa_delta_prediction %>% 
  filter(l >= r)
tbl_rsa_delta_prediction_lower %>% dplyr::select(l, r, d_euclidean_delta) %>%
  left_join(
    l_rsa_all[["tbl_rsa"]] %>% dplyr::select(l, r, n_categories, d_euclidean_delta),
    by = c("l", "r"), suffix = c("_pred", "_empirical")
  ) %>% group_by(n_categories) %>%
  summarise(
    corr = cor(d_euclidean_delta_pred, d_euclidean_delta_empirical),
    p_corr = cor.test(d_euclidean_delta_pred, d_euclidean_delta_empirical)$p.value
  )



# Save some plots ---------------------------------------------------------


pl <- arrangeGrob(
  pl_cat_learn_pretty, l_pl_sim[[3]], 
  pl_psychonomics_means + theme(plot.title = element_blank()),
  ncol = 3
)
save_my_pdf_and_tiff(
  pl, 
  "experiments/2022-07-category-learning-II/data/figures/three-tasks-agg-overview", 
  13, 3.75
)
save_my_pdf_and_tiff(
  pl, 
  "figures/three-tasks-agg-overview-e2", 
  13, 3.75
)
