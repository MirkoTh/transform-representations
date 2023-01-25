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
  #"experiments/2023-01-category-learning-catsim/data/2023-01-15-treps4-pilot-1/",
  "experiments/2023-01-category-learning-catsim/data/2023-01-17-treps4-experiment/"
)

# flag defining whether distance to category center in similarity condition
# is computed using ellipse center (i.e., middle of feature space) or
# category centers of four square categories

sim_center <- "square"

# Load Data ---------------------------------------------------------------

returned_timeout <- timeout_and_returns_e4()

# load_data_e3 function works for e4 as well
l_tbls_data <- map(path_data, load_data_e3, participants_returned = returned_timeout)
l_tbl_data <-
  list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))


# Set Exclusion Criteria Appropriately ------------------------------------
n_resp_simult <- 200
n_resp_cat_min <- 397 # 3 trials were not stored for one participant; details in participant-report.Rmd
n_resp_cat <- 400
l_cases <- preprocess_data_e3(l_tbl_data, n_resp_simult, n_resp_cat_min)

# do not exclude these participants; category membership judgments can be highly subjective
# l_cases$l_outliers$drop$tbl_simult %>% 
#   mutate(d_euclidean_cut = as.numeric(cut(d_euclidean, 8, labels = FALSE))) %>%
#   grouped_agg(c(participant_id, d_euclidean_cut), response) %>%
#   ggplot(aes(d_euclidean_cut, mean_response)) + 
#   geom_point(aes(size = n)) + 
#   geom_smooth(method = "lm", color = "#440154", alpha = .1) +
#   facet_wrap(~ participant_id) +
#   theme_bw() + labs(x = "Distance Binned", y = "Avg. Rating") +
#   coord_cartesian(ylim = c(0, 8))
# 
# l_cases$l_outliers$drop$tbl_simult$comparison_pool_binary <- l_cases$l_outliers$drop$tbl_simult$comparison_pool
# l_cases$l_outliers$drop$tbl_simult$comparison_pool_binary[l_cases$l_outliers$drop$tbl_simult$comparison_pool %in% c("side", "cross")] <- "different"
# tbl_cor_outliers <- cor_ratings_distances_time_pool(l_cases$l_outliers$drop$tbl_simult)
# tbl_cor_outliers <- tbl_cor_outliers[!is.na(tbl_cor_outliers$cor), ]
# ggplot(tbl_cor_outliers, aes(cor, group = participant_id)) +
#   geom_histogram(aes(fill = participant_id)) +
#   facet_grid(session ~ comparison_pool_binary)
# 


l_cases$l_outliers$keep$tbl_simult <- rbind(l_cases$l_outliers$keep$tbl_simult, l_cases$l_outliers$drop$tbl_simult)
l_cases$l_outliers$keep$tbl_cat_sim <- rbind(l_cases$l_outliers$keep$tbl_cat_sim, l_cases$l_outliers$drop$tbl_cat_sim)

tbl_simult <- l_cases$l_outliers$keep$tbl_simult
tbl_cat_sim <- l_cases$l_outliers$keep$tbl_cat_sim

drops <- map(l_cases, participants_ntrials, stage = "drop")
drops$l_incomplete %>% print(n = 100)
drops$l_guessing %>% print(n = 100)


keeps <- map(l_cases, participants_ntrials, stage = "keep")$l_outliers
keeps %>% arrange(desc(n_seq))
keeps %>% arrange(desc(n_simult))

# some participants seem to  have restarted the experiment
# exclude them from the data sets by hand
repeats <- keeps %>% filter(n_seq > n_resp_cat | n_simult > n_resp_simult) %>% select(participant_id) %>% as_vector() %>% unname()
#repeats <- c("5e8783b0fde5153fbd9dca43", "611cf79541223a8ee170a30f", "6134d182408816f4c1284496")
tbl_simult <- tbl_simult %>% filter(!(participant_id %in% repeats))
tbl_cat_sim <- tbl_cat_sim %>% filter(!(participant_id %in% repeats))

# exclusions
excl_incomplete <-
  dplyr::union(
    unique(l_cases$l_incomplete$drop[["tbl_simult"]]$participant_id),
    unique(l_cases$l_incomplete$drop[["tbl_cat"]]$participant_id)
  )
excl_guessing <-
  dplyr::union(
    unique(l_cases$l_guessing$drop[["tbl_simult"]]$participant_id),
    unique(l_cases$l_guessing$drop[["tbl_cat"]]$participant_id)
  )

# inclusions
cat(str_c("final N analyzed: ", length(unique(tbl_simult$participant_id)), "\n"))

# exclusions
cat(str_c("N excluded: ", length(excl_incomplete) + length(excl_guessing)))

# repeats
cat(str_c("N repeats/restarts: ", length(repeats)))

same_n <-
  length(unique(tbl_simult$participant_id)) == length(unique(tbl_cat_sim$participant_id))
cat(str_c("same n participants in cat and cr data sets: ", same_n, "\n"))
# ns per group
tbl_simult %>% group_by(participant_id, n_categories) %>% count() %>% 
  group_by(n_categories) %>% count()

l_cat_sim <- separate_cat_and_sim(tbl_cat_sim)
tbl_cat_sim <- l_cat_sim[["tbl_cat_sim"]]
tbl_cat <- l_cat_sim[["tbl_cat"]]
tbl_seq <- l_cat_sim[["tbl_sim"]]


tbl_simult <- fix_data_types_simult(tbl_simult)
tbl_simult$d_euclidean_cut <- cut(tbl_simult$d_euclidean, 8)

tbl_simult$session <- as.numeric(tbl_simult$session)

tbl_cor_outliers <- cor_ratings_distances_time_pool(tbl_simult)
tbl_cor_outliers <- tbl_cor_outliers[!is.na(tbl_cor_outliers$cor), ]
tbl_cor_outliers$participant_id <- factor(tbl_cor_outliers$participant_id)

ggplotly(
  ggplot(tbl_cor_outliers, aes(cor, group = as.numeric(participant_id))) +
  geom_histogram(aes(fill = as.numeric(participant_id))) +
  facet_grid(session ~ comparison_pool_binary)
)

ggplot(tbl_cor_outliers, aes(session, cor, group = as.numeric(participant_id))) +
  geom_line(aes(color = as.numeric(participant_id))) +
  geom_point(size = 4, color = "white") +
  geom_point(aes(color = as.numeric(participant_id))) +
  facet_wrap(~ comparison_pool_binary) +
  theme_bw()

tbl_changers <- tbl_cor_outliers %>% 
  pivot_wider(
    id_cols = c(participant_id, comparison_pool_binary), 
    names_from = session, values_from = cor
    ) %>%
  filter(sign(`1`) != sign(`2`)) %>%
  pivot_longer(cols = c(`1`, `2`)) %>%
  rename(session = name, cor = value)

ggplotly(
  ggplot(tbl_changers, aes(session, cor, group = as.factor(participant_id))) +
  geom_line(aes(color = as.factor(participant_id))) +
  geom_point(aes(color = as.factor(participant_id))) +
  facet_wrap(~ comparison_pool_binary) +
  theme_bw() +
  scale_color_viridis_d(name = "ID")
)

tbl_simult$session <- factor(tbl_simult$session)
levels(tbl_simult$session) <- c("Before Training", "After Training")




# look only at initial trials after training
n_consider <- 100
tbl_simult <- tbl_simult %>% filter(trial_id < n_consider | session == "Before Training")

# create data set with movements
tbl_simult_move <- delta_simultaneous(tbl_simult)

# save data sets for statistical analyses
saveRDS(tbl_simult, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_simult-treps.rds"))
saveRDS(tbl_simult_move, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_simult_move-treps.rds"))
saveRDS(tbl_cat, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_cat-treps.rds"))
saveRDS(tbl_seq, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_seq-treps.rds"))


# folder for plots
if (!dir.exists("experiments/2023-01-category-learning-catsim/data/figures")){
  dir.create("experiments/2023-01-category-learning-catsim/data/figures")
}

tbl_simult_agg <- summarySEwithin(
  tbl_simult, 
  "response", 
  c("n_categories"), 
  c("session", "comparison_pool_binary"),
  "participant_id"
)

move_agg <- summarySEwithin(
  tbl_simult_move, "move_response", 
  c("n_categories"), c("comparison_pool_binary", "d_euclidean_cut"), 
  idvar = "participant_id"
)


dg <- position_dodge(.2)
pl_lines_simult <- ggplot(tbl_simult_agg, aes(comparison_pool_binary, response, group = session)) +
  geom_errorbar(width = .2, position = dg, 
                aes(ymin = response - ci, ymax = response + ci, color = session)
  ) +
  geom_line(aes(color = session), position = dg) +
  geom_point(color = "white", size = 3, position = dg) +
  geom_point(aes(color = session), position = dg) +
  theme(axis.text = element_text(angle = 90)) +
  facet_wrap(~ n_categories) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90),
    legend.position = "bottom"
  ) +
  scale_color_viridis_d(name = "Time Point") +
  labs(
    x = "Comparison",
    y = "Similarity (Scale from 1-8)"
  )
save_my_pdf(
  pl_lines_simult, 
  "experiments/2023-01-category-learning-catsim/data/figures/simult-avg-comparison.pdf",
  6, 4
  )

# tbl_simult_move %>%
#   group_by(participant_id, n_categories, comparison_pool_binary) %>%
#   summarize(move_mn = mean(move_response)) %>%
#   ggplot(aes(move_mn, group = n_categories)) +
#   geom_freqpoly(aes(color = n_categories), binwidth = 1)

pl_move_mass <- ggplot(tbl_simult_move, aes(move_response, group = comparison_pool_binary)) +
  geom_freqpoly(aes(color = comparison_pool_binary, y = ..density..), binwidth = 1, size = .75) +
  geom_vline(xintercept = 0, linetype = "dotdash", size = 1, color = "grey") +
  geom_point(stat="bin", aes(y=..density..), color = "white", size = 3, binwidth=1) +
  geom_point(stat="bin", aes(y=..density.., color = comparison_pool_binary), binwidth=1)  +
  facet_wrap( ~ n_categories) +
  scale_x_continuous(breaks = seq(-6, 6, by = 2)) +
  scale_color_viridis_d(name = "Category") +
  theme_bw() +
  labs(x = "Move After - Before", y = "Probability Mass")

save_my_pdf(
  pl_move_mass, 
  "experiments/2023-01-category-learning-catsim/data/figures/simult-move-mass.pdf",
  6.5, 3.5
)


pl_rating_mass1 <- ggplot(tbl_simult, aes(response, group = comparison_pool)) +
  geom_freqpoly(aes(color = comparison_pool, y = ..density..), binwidth = 1, size = 1) +
  facet_grid(session ~ n_categories) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  scale_color_viridis_d(name = "Category Comparison") +
  theme_bw() +
  labs(x = "Similarity Rating (Scale 1-8)", y = "Probability Mass")

pl_rating_mass2 <- ggplot(tbl_simult, aes(response, group = session)) +
  geom_freqpoly(aes(color = session, y = ..density..), binwidth = 1, size = 1) +
  facet_grid(comparison_pool ~ n_categories) +
  scale_x_continuous(breaks = seq(1, 8, by = 1)) +
  scale_color_viridis_d(name = "Category Comparison") +
  theme_bw() +
  labs(x = "Similarity Rating (Scale 1-8)", y = "Probability Mass")

save_my_pdf(
  pl_rating_mass2, 
  "experiments/2023-01-category-learning-catsim/data/figures/simult-ratings-mass.pdf",
  8.5, 7.5
)

l_ribbons <- ribbon_plot(tbl_simult_move)
l_ribbons[[1]]

save_my_pdf(
  l_ribbons[[1]], 
  "experiments/2023-01-category-learning-catsim/data/figures/move-by-distance.pdf",
  7, 4.5
)


grouped_agg(tbl_simult_move, c(participant_id, n_categories, comparison_pool_binary), move_response) %>%
  ggplot(aes(mean_move_response, group = n_categories)) +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotdash") +
  geom_histogram(aes(fill = n_categories), color = "white") +
  facet_grid(comparison_pool_binary ~ n_categories) +
  theme_bw() +
  scale_fill_viridis_d(name = "Condition") +
  labs(x = "Mean Move", y = "Nr. Participants")



tbl_simult_agg <- summarySEwithin(
  tbl_simult_move, "move_response", "n_categories", 
  "comparison_pool_binary", "participant_id"
)
tbl_simult_agg$n_categories <- fct_relevel(tbl_simult_agg$n_categories, "4 Categories", "Similarity")
dg <- position_dodge(width = .2)
pl_groupmeans <- ggplot(tbl_simult_agg, aes(comparison_pool_binary, move_response, group = n_categories)) +
  geom_hline(yintercept = 0, color = "grey", size = 1, linetype = "dotdash") +
  geom_errorbar(aes(ymin = move_response - ci, ymax = move_response + ci, color = n_categories), width = .2, position = dg) +
  geom_line(aes(color = n_categories), position = dg) +
  geom_point(color = "white", size = 4, position = dg) +
  geom_point(aes(color = n_categories), position = dg) +
  scale_color_viridis_d(name = "Group") +
  theme_bw() +
  theme(legend.position = "bottom") +
  labs(
    x = "Category Comparison",
    y = "Rating After - Before"
  )
save_my_pdf(pl_groupmeans, "experiments/2023-01-category-learning-catsim/data/figures/mean-moves.pdf", 6, 4)


# looks like participants find it difficult to differentiate between categories when stimuli come close from boundaries

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
pl_cat_learn_pretty <- l_pl[[1]] + theme(legend.position = "bottom")
save_my_pdf(
  pl_cat_learn, 
  "experiments/2023-01-category-learning-catsim/data/figures/category-learning.pdf",
  5.5, 3.5
)


# by-participant trajectories
l_pl[[2]]

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

# exclude initial trials from following analyses
n_trials_split <- 200
tbl_cat_grid_start <- aggregate_category_responses_by_x1x2(tbl_cat, n_trials_split, is_start = TRUE)
tbl_cat_grid_end <- aggregate_category_responses_by_x1x2(tbl_cat, n_trials_split, is_start = FALSE)

sample_ids <- tbl_cat_grid_end %>% group_by(participant_id) %>%
  summarize(mean_accuracy = max(mean_accuracy)) %>%
  arrange(desc(mean_accuracy))
select_ids <- round(seq(1, nrow(sample_ids), length.out = 4))
sample_ids <-
  as.character(sample_ids[select_ids, "participant_id"] %>% as_vector() %>% unname())

hm_start <- plot_categorization_heatmaps(tbl_cat_grid_start %>% filter(participant_id %in% sample_ids), c(2, 4)) +
  ggtitle("Start of Training")
hm_end <- plot_categorization_heatmaps(tbl_cat_grid_end %>% filter(participant_id %in% sample_ids), c(2, 4)) +
  ggtitle("End of Training")


# prototype analyses ------------------------------------------------------

# fit individual nb models
participant_ids_4_cat <-
  unique(tbl_cat$participant_id[tbl_cat$n_categories == 4]) %>% as.character()
l_nb_end <- by_participant_nb(tbl_cat %>% filter(trial_id >= n_trials_split), participant_ids_4_cat)
hm_pt_end <- plot_heatmaps_with_representations(l_nb_end, sample_ids, tbl_cat_grid_end) +
  ggtitle("First Half of Training")
l_nb_start <- by_participant_nb(tbl_cat %>% filter(trial_id < n_trials_split), participant_ids_4_cat)
hm_pt_start <- plot_heatmaps_with_representations(l_nb_start, sample_ids, tbl_cat_grid_start) +
  ggtitle("Second Half of Training")


save_my_pdf(
  arrangeGrob(hm_pt_start, hm_pt_end), 
  "experiments/2023-01-category-learning-catsim/data/figures/prototypes-improvement.pdf",
  12, 7
)

# Similarity Judgments ----------------------------------------------------

tbl_seq_agg <- tbl_seq %>%
  rutils::grouped_agg(c(distance_binned), c(response, rt))
tbl_seq_ci <- summarySEwithin(
  tbl_seq, "response", "n_categories", "distance_binned", "participant_id", TRUE
) %>% as_tibble()
tbl_seq_ci$distance_binned <-
  as.numeric(as.character(tbl_seq_ci$distance_binned))

# some sample participants to plot similarity ratings
sample_ids_seq <-
  unique(tbl_seq$participant_id)[seq(1, length(unique(tbl_seq$participant_id)), length.out = 4)]
l_pl_sim <- plot_similarity_against_distance(tbl_seq, tbl_seq_ci, sample_ids_seq, sim_edges = c(1.5, 6))
grid.arrange(l_pl_sim[[1]], l_pl_sim[[2]], nrow = 1, ncol = 2)
pl_sequential_agg <- l_pl_sim[[3]] + theme(legend.position = "bottom")
save_my_pdf(
  pl_sequential_agg, 
  "experiments/2023-01-category-learning-catsim/data/figures/sequential-comparison.pdf",
  4, 3.5
)

tbl_seq_agg_subj <- tbl_seq %>%
  mutate(distance_binned = distance_binned - mean(distance_binned)) %>%
  rutils::grouped_agg(c(participant_id, distance_binned, n_categories), c(response, rt))

m_rs_sim <-
  nlme::lme(
    mean_response ~ distance_binned,
    random = ~ 1 + distance_binned |
      participant_id,
    data = tbl_seq_agg_subj
  )
summary(m_rs_sim)
anova(m_rs_sim)
tbl_seq_agg_subj$preds <- predict(m_rs_sim, tbl_seq_agg_subj)

by_participant_coefs(tbl_seq_agg_subj, "distance_binned", "mean_response", "LM Sim. Ratings")


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





# save aggregate plots
pl <- arrangeGrob(pl_groupmeans + theme(plot.title = element_blank()), pl_cat_learn_pretty, pl_sequential_agg, ncol = 3)
save_my_tiff(
  pl, 
  "experiments/2023-01-category-learning-catsim/data/figures/three-tasks-agg-overview.tiff", 
  13, 3.75
)
save_my_pdf(
  pl, 
  "experiments/2023-01-category-learning-catsim/data/figures/three-tasks-agg-overview.pdf", 
  13, 3.75
)
