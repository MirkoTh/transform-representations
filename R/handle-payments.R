
# Load Packages and Home-Grown Modules ------------------------------------

rm(list = ls())
library(tidyverse)
library(jsonlite)

# files <- c("R/utils.R", "R/plotting.R")
# walk(files, source)
# 
# 
# # Simulation Parameters ---------------------------------------------------
# 
# n_stimuli <- 144L
# nruns <- 10000
# 
# # constant
# l_info_prep <- list(
#   n_stimuli = n_stimuli,
#   nruns = nruns
# )
# 
# # variable
# l_vary <- crossing(
#   n_stimuli = 144L, nruns = 1000,
#   n_categories = c(2L), cat_type = c("prototype"),
#   prior_sd = c(.75), sampling = c("improvement"),
#   constrain_space = c(TRUE), category_shape = c("ellipses"),
#   is_reward = c(FALSE)
# ) %>% as.list()
# 
# 
# l_stimuli <- map(l_info, make_stimuli)
# 
# tbl <- l_stimuli[[1]][[1]] %>% mutate(
#   x1_expt = (x1 + 1) * 8 - 2,
#   x2_expt = (x2 + 1) * 8 - 2
# )
# tbl <- rbind(tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl,
#              tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl, tbl)
# 
# 
# tbl$x1_response <- round(runif(nrow(tbl), 0, 100))
# tbl$x2_response <- round(runif(nrow(tbl), 0, 100))
# tbl <- tbl %>% mutate(
#   deviation = sqrt((x1_expt - x1_response)^2 + (x2_expt - x2_response)^2)
# )
# tbl %>% summarise(mean(deviation))

# guessing leads to an expected average of 51 units deviation

# read individual performance
l_tbl_data <- load_data()
cr <- l_tbl_data[[1]] %>% filter(session %in% c(1, 2))
cat <- l_tbl_data[[2]]
cr <- cr %>%
  mutate(
    x1_response = as.numeric(x1_response),
    x2_response = as.numeric(x2_response),
    deviation = sqrt((x1_true - x1_response)^2 + (x2_true - x2_response)^2))
coef_bonus <- min(51, cr %>% summarize(deviation_avg = mean(deviation)) %>% as_vector())
max_possible_above_chance <- 46 # anything better than that is almost impossible
above_chance <- 51 - coef_bonus
prop_bonus <- above_chance/max_possible_above_chance
prop_bonus * 2.35

cat %>% filter(trial_id > nrow(cat) / 2) %>%
  summarize(mn_acc = mean(accuracy)) * 2.35











