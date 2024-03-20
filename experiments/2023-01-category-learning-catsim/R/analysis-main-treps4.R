rm(list = ls())


with_outliers <- FALSE
pl_suffix <- ifelse(with_outliers, "-outliers", "no-outliers")

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
library(ids)
library(naivebayes)
library(mvtnorm)


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

# path_data <- c(
#   #"experiments/2023-01-category-learning-catsim/data/2023-01-15-treps4-pilot-1/",
#   "experiments/2023-01-category-learning-catsim/data/2023-01-17-treps4-experiment/"
# )

# flag defining whether distance to category center in similarity condition
# is computed using ellipse center (i.e., middle of feature space) or
# category centers of four square categories

sim_center <- "square"

# Load Data ---------------------------------------------------------------

# returned_timeout <- timeout_and_returns_e4()
# 
# # hash prolific ids and load data
# # only hashed ids are uploaded on osf
# walk(path_data, hash_ids_e3_e4, participants_returned = returned_timeout, expt = 4)
# 
# l_tbls_data <- map(path_data, load_data_e3)
# l_tbl_data <-
#   list(reduce(map(l_tbls_data, 1), rbind), reduce(map(l_tbls_data, 2), rbind))
# # 
# # # these are the files uploaded on osf
# write_csv(l_tbl_data[[1]], "experiments/2023-01-category-learning-catsim/data/simult-comparison.csv")
# write_csv(l_tbl_data[[2]], "experiments/2023-01-category-learning-catsim/data/secondary-task.csv")

l_tbl_data <- list()
l_tbl_data[[1]] <- read_csv("experiments/2023-01-category-learning-catsim/data/simult-comparison.csv")
l_tbl_data[[2]] <- read_csv("experiments/2023-01-category-learning-catsim/data/secondary-task.csv")


l_info <- list(
  use_exptl_stimuli = TRUE, 
  informed_by_data = FALSE, 
  representation = c("psychological-representation", "physical-properties")[1]
)

# tf to psych space?
if (l_info$representation == "psychological-representation") {
  l_tbl_data <- tf_to_psychological_e34(l_tbl_data)
}

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


if (with_outliers) {
  l_cases$l_outliers$keep$tbl_simult <- rbind(l_cases$l_outliers$keep$tbl_simult, l_cases$l_outliers$drop$tbl_simult)
  l_cases$l_outliers$keep$tbl_cat_sim <- rbind(l_cases$l_outliers$keep$tbl_cat_sim, l_cases$l_outliers$drop$tbl_cat_sim)
  
}

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
rbind(
  l_cases$l_outliers$drop$tbl_simult, l_cases$l_guessing$drop$tbl_simult, l_cases$l_incomplete$drop$tbl_simult
) %>% rbind(tbl_simult) %>% group_by(participant_id) %>% count(Sex) %>% ungroup() %>% count(Sex)
cat(str_c("dropouts: ", length(unique(l_cases$l_incomplete$drop$tbl_simult$participant_id))))
cat(str_c("outliers in cr: ", length(unique(l_cases$l_outliers$drop$tbl_simult$participant_id))))

repeats <- keeps %>% filter(n_seq > n_resp_cat | n_simult > n_resp_simult) %>% select(participant_id) %>% as_vector() %>% unname()
tbl_simult <- tbl_simult %>% filter(!(participant_id %in% repeats))
tbl_cat_sim <- tbl_cat_sim %>% filter(!(participant_id %in% repeats))
cat(str_c("restarts: ", length(repeats)))



# ns per group
tbl_simult %>% group_by(participant_id, n_categories) %>% count() %>% 
  group_by(n_categories) %>% count()

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
saveRDS(tbl_simult, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_simult-treps", pl_suffix, ".rds"))
saveRDS(tbl_simult_move, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_simult_move-treps", pl_suffix, ".rds"))
saveRDS(tbl_cat, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_cat-treps", pl_suffix, ".rds"))
saveRDS(tbl_seq, file = str_c("experiments/2023-01-category-learning-catsim/data/tbl_seq-treps", pl_suffix, ".rds"))


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

levels(tbl_simult_agg$n_categories) <- c("Similarity", "4 Categories")


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
    legend.position = "bottom",
    text = element_text(size = 16)
  ) +
  scale_color_viridis_d(name = "Time Point") +
  labs(
    x = "Comparison",
    y = "Similarity (Scale from 1-8)"
  )

save_my_pdf_and_tiff(
  pl_lines_simult, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/simult-avg-comparison", pl_suffix),
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

save_my_pdf_and_tiff(
  pl_move_mass, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/simult-move-mass", pl_suffix), 
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

save_my_pdf_and_tiff(
  pl_rating_mass2, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/simult-ratings-mass", pl_suffix),  
  8.5, 7.5
)

l_ribbons <- ribbon_plot(tbl_simult_move)
l_ribbons[[1]]

save_my_pdf_and_tiff(
  l_ribbons[[1]],
  str_c("experiments/2023-01-category-learning-catsim/data/figures/move-by-distance", pl_suffix),
  7, 4.5
)


grouped_agg(tbl_simult_move, c(participant_id, n_categories, comparison_pool_binary), move_response) %>%
  ggplot(aes(mean_move_response, group = n_categories)) +
  geom_vline(xintercept = 0, color = "grey", linetype = "dotdash") +
  geom_histogram(aes(fill = n_categories), color = "white") +
  facet_grid(comparison_pool_binary ~ n_categories) +
  theme_bw() +
  scale_fill_viridis_c(name = "Condition") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(.025, .025))) +
  labs(x = "Mean Move", y = "Nr. Participants") +
  theme(strip.background =element_rect(fill="white"))+
  theme(strip.text = element_text(colour = 'black'))



tbl_simult_agg <- summarySEwithin(
  tbl_simult_move, "move_response", "n_categories", 
  "comparison_pool_binary", "participant_id"
)
levels(tbl_simult_agg$n_categories) <- c("Similarity", "4 Categories")
tbl_simult_agg$n_categories <- fct_relevel(tbl_simult_agg$n_categories, "4 Categories", "Similarity")
dg <- position_dodge(width = .2)
pl_groupmeans <- ggplot(tbl_simult_agg, aes(comparison_pool_binary, move_response, group = n_categories)) +
  geom_hline(yintercept = 0, color = "grey", size = 1, linetype = "dotdash") +
  geom_errorbar(aes(ymin = move_response - ci, ymax = move_response + ci, group = n_categories), color = "grey30", width = .2, position = dg) +
  geom_line(aes(color = n_categories), position = dg) +
  geom_point(color = "white", size = 4, position = dg) +
  geom_point(aes(color = n_categories), position = dg) +
  scale_color_viridis_d(name = "Group") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(.025, .025))) +
  theme_bw() +
  theme(legend.position = "bottom", text = element_text(size = 16)) +
  labs(
    x = "Category Comparison",
    y = "Rating After - Before"
  )
save_my_pdf_and_tiff(
  pl_groupmeans, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/mean-moves", pl_suffix),  
  6, 4
)


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
pl_cat_learn_pretty <- l_pl[[1]] + theme(legend.position = "bottom", text = element_text(size = 16)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(0, .02))) +
  scale_color_manual(values = c("#A4D3EE", "#CD4F39", "#CE7E72", "#C3A9AF"), name = "Category")

save_my_pdf_and_tiff(
  pl_cat_learn_pretty, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/category-learning", pl_suffix),  
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


save_my_pdf_and_tiff(
  arrangeGrob(hm_pt_start, hm_pt_end),
  str_c("experiments/2023-01-category-learning-catsim/data/figures/prototypes-improvement", pl_suffix),
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
l_pl_sim <- plot_similarity_against_distance(tbl_seq, tbl_seq_ci, sample_ids_seq, sim_edges = c(1.5, 6.25))
grid.arrange(l_pl_sim[[1]], l_pl_sim[[2]], nrow = 1, ncol = 2)
pl_sequential_agg <- l_pl_sim[[3]] + theme(legend.position = "bottom", text = element_text(size = 16)) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = expansion(add = c(0, .02))) + 
  scale_color_manual(values = "#FDE725FF", name = "Group")

save_my_pdf_and_tiff(
  pl_sequential_agg, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/sequential-comparison", pl_suffix),
  4, 3.5
)

tbl_seq_agg_subj <- tbl_seq %>%
  mutate(distance_binned = distance_binned - mean(distance_binned)) %>%
  rutils::grouped_agg(c(participant_id, distance_binned, n_categories), c(response, rt))



# Behavioral Representational Similarity Analysis -------------------------

# tbl_simult_move already includes pairwise distances, in similarity metric, though

tbl_simult_move_agg <- tbl_simult_move %>%
  group_by(n_categories, stim_id_lo, stim_id_hi) %>%
  summarize(move_response = mean(move_response)) %>%
  ungroup()

plot_symmetric_moves <- function(tbl_df, ttl) {
  ggplot(tbl_df) +
    geom_tile(aes(stim_id_lo, stim_id_hi, fill = move_response)) +
    geom_tile(aes(stim_id_hi, stim_id_lo, fill = move_response)) +
    theme_bw() +
    scale_fill_viridis_c(name = "Euclidean Distance Delta") + #, limits = c(0, 75)) +
    scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
    scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
    labs(x = "Stimulus ID 1", y = "Stimulus ID 2") +
    theme(legend.position = "omit") +
    labs(title = ttl)
}

tbl_simult_move_agg$n_categories <- factor(tbl_simult_move_agg$n_categories, labels = c("Similarity", "4 Categories"))

pl_control <- plot_symmetric_moves(tbl_simult_move_agg %>% filter(n_categories == "Similarity"), "Sequential Comparison")
pl_experimental <- plot_symmetric_moves(tbl_simult_move_agg %>% filter(n_categories == "4 Categories"), "Category Learning")

f_name <- "data/2024-03-18-grid-search-vary-constrain-space.rds"
tbl_both <- load_predictions(f_name, sim_center = "square", is_simulation = TRUE)
tbl_rsa_delta_prediction <- delta_representational_distance("prediction", tbl_both)

# constrain to those comparisons that were actually made by participants
# and make matrix symmetrical
tbl_constrain <- tbl_simult_move %>% count(stim_id_lo, stim_id_hi) %>% select(-n)
tbl_mm <- tbl_rsa_delta_prediction %>% 
  inner_join(tbl_constrain, by = c("l" = "stim_id_lo", "r" = "stim_id_hi")) %>%
  rbind(
    tbl_rsa_delta_prediction %>% 
      inner_join(tbl_constrain, by = c("r" = "stim_id_lo", "l" = "stim_id_hi")) 
  )

pl_pred <- plot_distance_matrix(tbl_mm) +
  labs(x = "Stimulus ID 1", y = "Stimulus ID 2", title = "Model Matrix") +
  scale_x_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 100, by = 10), expand = c(0, 0))

# correlation between model matrix and delta in responses
# tbl_rsa_delta_prediction_lower <- tbl_mm %>% 
#   filter(l >= r)
tbl_simult_move_agg %>% 
  rename(l = stim_id_lo, r = stim_id_hi, d_euclidean_delta = move_response) %>%
  dplyr::select(n_categories, l, r, d_euclidean_delta) %>%
  left_join(
    tbl_mm  %>% 
      filter(l <= r) %>% dplyr::select(l, r, d_euclidean_delta),
    by = c("l", "r"), suffix = c("_pred", "_empirical")
  ) %>% group_by(n_categories) %>%
  summarise(
    corr = cor(d_euclidean_delta_pred, d_euclidean_delta_empirical),
    p_corr = cor.test(d_euclidean_delta_pred, d_euclidean_delta_empirical)$p.value
  )
pls_rsa <- arrangeGrob(
  pl_pred + theme(text = element_text(size = 16)), 
  pl_experimental + theme(text = element_text(size = 16)), 
  pl_control + theme(text = element_text(size = 16)), 
  nrow = 1)
save_my_pdf_and_tiff(
  pls_rsa,
  str_c("experiments/2023-01-category-learning-catsim/data/figures/rsa-avg-plots-e4", pl_suffix),  
  12, 4
)
save_my_pdf_and_tiff(
  pls_rsa,
  str_c("figures/rsa-avg-plots-e4"),  
  12, 4
)


# save aggregate plots ----------------------------------------------------


pl <- arrangeGrob(
  pl_cat_learn_pretty + guides(color = guide_legend(nrow=2,byrow=TRUE)),
  pl_sequential_agg + guides(color = guide_legend(nrow=2,byrow=TRUE)),
  pl_groupmeans + theme(plot.title = element_blank()) + guides(color = guide_legend(nrow=2,byrow=TRUE)),
  ncol = 3
)
save_my_pdf_and_tiff(
  pl, 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/three-tasks-agg-overview", pl_suffix),  
  13, 3.75
)
save_my_pdf_and_tiff(
  pl, 
  str_c("figures/three-tasks-agg-overview-e4", pl_suffix),  
  13, 3.75
)
save_my_pdf_and_tiff(
  pl, 
  str_c("figures/figures-ms/three-tasks-agg-overview-e4", pl_suffix),  
  13, 3.75
)
