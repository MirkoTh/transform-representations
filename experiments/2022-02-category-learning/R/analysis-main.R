# Import Packages ---------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggExtra)
library(docstring)
library(rutils)



# Import Home-Grown Modules -----------------------------------------------
files <- c(
  "R/utils.R",
  "experiments/2022-02-category-learning/R/analysis-utils.R",
  "experiments/2022-02-category-learning/R/summarySEwithin.R",
  "experiments/2022-02-category-learning/R/summarySE.R",
  "experiments/2022-02-category-learning/R/normDataWithin.R"
)
walk(files, source)

path_data <- "experiments/2022-02-category-learning/data/2022-03-30-pilot-1/"
l_tbl_data <- load_data(path_data)
l_tbl_data <- exclude_incomplete_datasets(l_tbl_data)

tbl_cr <- l_tbl_data[[1]] %>% filter(session %in% c(1, 2))
tbl_cat_sim <- l_tbl_data[[2]]
# add deviation variables
tbl_cr$x1_deviation <- tbl_cr$x1_true - tbl_cr$x1_response
tbl_cr$x2_deviation <- tbl_cr$x2_true - tbl_cr$x2_response
tbl_cr$eucl_deviation <- sqrt(tbl_cr$x1_deviation^2 + tbl_cr$x2_deviation^2)
tbl_cr <- add_distance_to_nearest_center(tbl_cr)


# Categorization ----------------------------------------------------------


tbl_cat_sim <- tbl_cat_sim %>%
  filter(trial_id >= 40) %>%
  group_by(participant_id, cat_true) %>%
  arrange(n_categories, participant_id, trial_id) %>%
  mutate(
    trial_id_by_condition = row_number(trial_id)
  ) %>% ungroup() %>% mutate(
    trial_id_binned = as.factor(ceiling((trial_id_by_condition) / 20)),
    n_categories = factor(n_categories, labels = c(
      "Nr. Categories = 1", "Nr. Categories = 2", "Nr. Categories = 3")
      )
  )
tbl_cat <- tbl_cat_sim %>% filter(n_categories %in% c(2, 3))

tbl_cat_overview <- tbl_cat %>% grouped_agg(c(n_categories, participant_id), accuracy) %>%
  arrange(mean_accuracy)
tbl_chance2 <- tbl_cat_overview %>% group_by(n_categories) %>%
  summarize(dummy = mean(mean_accuracy)) %>%
  mutate(p_chance = 1/as.numeric(str_extract(n_categories, "[2-3]$")))


dg <- position_dodge(width = .8)

ggplot() + 
  geom_histogram(
    data = tbl_cat_overview, aes(mean_accuracy, group = participant_id), fill = "black", color = "white"
  ) +
  geom_segment(
    data = tbl_chance2, aes(
      x = p_chance, xend = p_chance, y = 0, yend = 3, group = n_categories
    ), linetype = "dotdash", color = "red") +
  facet_wrap(~ n_categories) +
  theme_dark() +
  labs(
    x = "Overall Accuracy",
    y = "Participant Counts"
  )


tbl_cat_agg <- tbl_cat %>% group_by(participant_id, n_categories, cat_true, trial_id_binned) %>%
  summarize(
    accuracy_mn_participant = mean(accuracy)
  ) %>%  ungroup()

tbl_cat_agg_ci <- summarySEwithin(
  tbl_cat_agg, "accuracy_mn_participant", c("n_categories"), 
  c("cat_true", "trial_id_binned"), "participant_id"
) %>% as_tibble()
tbl_cat_agg_ci$trial_id_binned <- as.numeric(as.character(tbl_cat_agg_ci$trial_id_binned))

tbl_chance <- chance_performance_cat(tbl_cat)
tbl_chance$block <- as.numeric(as.character(tbl_chance$block))

ggplot() + 
  geom_errorbar(data = tbl_cat_agg_ci, aes(
    trial_id_binned, ymin = accuracy_mn_participant - ci, 
    ymax = accuracy_mn_participant + ci, color = cat_true
  ), width = .25, position = dg) +
  geom_line(data = tbl_cat_agg_ci, aes(
    trial_id_binned, accuracy_mn_participant, group = cat_true, color = cat_true
  ), position = dg) +
  geom_point(data = tbl_cat_agg_ci, aes(
    trial_id_binned, accuracy_mn_participant, group = cat_true
  ), position = dg, color = "white", size = 3) +
  geom_point(data = tbl_cat_agg_ci, aes(
    trial_id_binned, accuracy_mn_participant, group = cat_true, color = cat_true
  ), position = dg) +
  geom_line(
    data = tbl_chance, aes(block, prop_chance, group = 1), 
    linetype = "dotdash", size = .5) +
  facet_wrap(~ n_categories) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(name = "Category", palette = "Set1") +
  scale_x_continuous(breaks = seq(2, 10, by = 2)) +
  labs(
    x = "Block of 20 Trials",
    y = "Categorization Accuracy"
  ) + theme_bw()




tbl_cat_last <- tbl_cat_sim %>% 
  mutate(category = cat_true) %>%
  group_by(participant_id) %>%
  filter(trial_id_binned == max(as.numeric(as.character(trial_id_binned)))) %>%
  grouped_agg(c(participant_id, n_categories, category), accuracy)
# similarity condition gets a dummy accuracy of .5
tbl_cat_last$mean_accuracy[tbl_cat_last$n_categories == "Nr. Categories = 1"] <- .5
tbl_movement <- grouped_agg(tbl_cr, c(participant_id, n_categories, session, category), d_closest) %>%
  select(participant_id, n_categories, session, category, mean_d_closest) %>%
  left_join(tbl_cat_last[, c("participant_id", "category", "mean_accuracy")], by = c("participant_id", "category")) %>%
  ungroup() %>% arrange(participant_id, category, session) %>%
  group_by(participant_id, category) %>%
  mutate(
    mean_d_closest_before = lag(mean_d_closest),
    movement = mean_d_closest_before - mean_d_closest,
    category = fct_relabel(category, ~ ifelse(.x == 1, "Background Category", str_c("Category = ", .x))),
    n_categories = fct_relabel(n_categories, ~ ifelse(.x == 1, "Control (Similarity)", str_c("Nr. Categories = ", .x)))
  ) %>%
  filter(!is.na(mean_d_closest_before))

ggplot(tbl_movement, aes(mean_accuracy, movement, group = category)) + 
  geom_point(aes(color = category)) +
  geom_smooth(method = "lm", aes(color = category)) +
  facet_grid(n_categories ~ category) +
  scale_color_brewer(palette = "Set1", name = "Category") +
  theme_bw() +
  labs(
    x = "Categorization Accuracy",
    y = "Movement (Euclidian Distance)"
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
pl_heamaps <- plot_2d_binned_heatmaps(tbl_cr, 4)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)



tbl_cr_agg <- tbl_cr %>% group_by(n_categories, participant_id, session, category) %>%
  summarize(dmin_mn_participant = mean(d_closest)) %>%
  group_by(n_categories, session, category) %>%
  summarize(dmin_mn = mean(dmin_mn_participant),
            dmin_se = sd(dmin_mn_participant)/sqrt(length(unique(tbl_cr$participant_id)))) %>%
  ungroup() %>%
  mutate(session = factor(session, labels = c("Before Cat. Learning", "After Cat. Learning")))

ggplot() +
  geom_col(data = tbl_cr_agg, aes(
    category, dmin_mn, group = session, fill = session
  ), position = dg, alpha = .5) +
  geom_point(data = tbl_cr_agg, aes(
    category, dmin_mn, color = session
  ), position = dg, show.legend = FALSE) +
  geom_errorbar(data = tbl_cr_agg, aes(
    category, ymin = dmin_mn - 1.96 * dmin_se, 
    ymax = dmin_mn + 1.96 * dmin_se, color = session
  ), position = dg, width = .25, show.legend = FALSE) +
  facet_wrap(~ n_categories) +
  theme_bw() +
  scale_fill_brewer(name = "Session", palette = "Set1") +
  scale_color_brewer(palette = "Set1") +
  labs(
    x = "Category",
    y = "Distance to Closest Category Center"
  )



