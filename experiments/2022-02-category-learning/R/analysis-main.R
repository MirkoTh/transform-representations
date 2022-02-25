# Import Packages ---------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(grid)
library(gridExtra)
library(ggExtra)
library(docstring)


# Import Home-Grown Modules -----------------------------------------------
files <- c(
  "R/utils.R",
  "experiments/2022-02-category-learning/R/analysis-utils.R"
)
walk(files, source)

l_tbl_data <- load_data()
tbl_cr <- l_tbl_data[[1]] %>% filter(session %in% c(1, 3))
tbl_cat <- l_tbl_data[[2]]
tbl_conditions <- tbl_cat %>% group_by(participant_id) %>% summarise(n_categories = max(cat_true)) %>% ungroup()
tbl_cr <- tbl_cr %>% left_join(tbl_conditions, by = c("participant_id"))
# add deviation variables
tbl_cr$x1_deviation <- tbl_cr$x1_true - tbl_cr$x1_response
tbl_cr$x2_deviation <- tbl_cr$x2_true - tbl_cr$x2_response


# Overview Plots ----------------------------------------------------------
# 2d marginals
pl_marginal_before <- plot_marginals_one_session(1, tbl_cr)
pl_marginal_after <- plot_marginals_one_session(3, tbl_cr)

# heat map of errors over 2d space
pl_heamaps <- plot_2d_binned_heatmaps(tbl_cr, 4)

# 1d marginal histograms & freq polys of deviations x1 and x2 before vs. after
pl_1d_marginals <- plot_1d_marginals(tbl_cr)

tbl_cr <- add_distance_to_nearest_center(tbl_cr)

tbl_cr_agg <- tbl_cr %>% group_by(n_categories, participant_id, session, category) %>%
  summarize(dmin_mn = mean(d_closest),
            dmin_se = sd(d_closest)/sqrt(length(unique(tbl_cr$participant_id)))) %>%
  ungroup() %>%
  mutate(session = factor(session, labels = c("Before Cat. Learning", "After Cat. Learning")))

dg <- position_dodge(width = .8)
ggplot() +
  geom_col(data = tbl_cr_agg, aes(category, dmin_mn, group = session, fill = session), position = dg, alpha = .5, show.legend = FALSE) +
  geom_point(data = tbl_cr_agg, aes(category, dmin_mn, color = session), position = dg) +
  geom_errorbar(data = tbl_cr_agg, aes(category, ymin = dmin_mn - 1.96 * dmin_se, ymax = dmin_mn + 1.96 * dmin_se, color = session), position = dg, width = .25) +
  theme_bw() +
  scale_color_brewer(name = "Session", palette = "Set1") +
  labs(
    x = "Category",
    y = "Distance to Closest Category Center"
  )


tbl_cat <- tbl_cat %>%
  mutate(trial_id_binned = as.factor(ceiling((trial_id + 1) / 20)))
tbl_cat_agg <- tbl_cat %>% group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(
    accuracy_avg = mean(accuracy),
    accuracy_se = sd(accuracy) / length(unique(tbl_cat$participant_id))
    )
ggplot() + 
  geom_point(data = tbl_cat_agg, aes(trial_id_binned, accuracy_avg, group = cat_true, color = cat_true), position = dg) +
  geom_line(data = tbl_cat_agg, aes(trial_id_binned, accuracy_avg, group = cat_true, color = cat_true), position = dg) +
  #geom_errorbar(data = tbl_cat_agg, aes(trial_id_binned, ymin = accuracy_avg - 1.96 * accuracy_se, ymax = accuracy_avg + 1.96 * accuracy_se, color = cat_true), width = .25, position = dg) +
  coord_cartesian(ylim = c(0, 1)) +
  scale_color_brewer(name = "Category", palette = "Set1") +
  labs(
    x = "Block",
    y = "Accuracy"
  ) + theme_bw()



