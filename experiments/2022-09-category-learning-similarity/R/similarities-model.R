rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(furrr)
library(parallel)
library(future)
library(rutils)


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

tbl_simult <- readRDS("experiments/2022-09-category-learning-similarity/data/tbl_simult-treps.rds")

tbl_simult <- tbl_simult %>%
  mutate(
    d_cityblock = abs(x1_true_l - x1_true_r) + abs(x2_true_l - x2_true_r),
    d_x1 = abs(x1_true_l - x1_true_r),
    d_x2 = abs(x2_true_l - x2_true_r),
    response_scaled = response/max(response)
  )


tbl_p1 <- tbl_simult %>% filter(participant_id == "55c333b0fdf99b1dbd2131bd")
tbl_data <- tbl_p1

similarity_cityblock(c(.5, .5), tbl_p1)
similarity_euclidean(c(.5, .5), tbl_p1)


# Fit Both Metrics --------------------------------------------------------



tbl_design <- tbl_simult %>%
  group_by(participant_id, n_categories, comparison_pool_binary, session) %>%
  count() %>% select(-n) %>%
  rename(p_id = participant_id, tp = session, pool = comparison_pool_binary)


n_workers_available <- parallel::detectCores()
plan(multisession, workers = n_workers_available - 2)

fit_safely <- safely(fit_one_subset)

# fit cityblock
l_results_cityblock <- furrr::future_pmap(
  tbl_design[, c("p_id", "tp", "pool")], 
  fit_safely, 
  tbl_data = tbl_simult, 
  metric = "cityblock", 
  .progress = TRUE
)
if(!is.null(map(l_results_cityblock, "error") %>% unlist())) {
  stop("error for one or more participants")
}

# fit euclidean
l_results_euclidean <- furrr::future_pmap(
  tbl_design[, c("p_id", "tp", "pool")], 
  fit_safely, 
  tbl_data = tbl_simult, 
  metric = "euclidean", 
  .progress = TRUE
)
if(!is.null(map(l_results_euclidean, "error") %>% unlist())) {
  stop("error for one or more participants")
}


compare_rsqs(l_results_cityblock, l_results_euclidean)


l_cityblock <- summarize_model_results(l_results_cityblock, tbl_design)
l_euclidean <- summarize_model_results(l_results_euclidean, tbl_design)

l_euclidean$hist_w
l_cityblock$hist_w

l_euclidean$pl_c
l_cityblock$pl_c


fit_one_subset("5aff33bae55f90000139f664", "Before Training", "Same", tbl_simult, "euclidean")
fit_one_subset("5aff33bae55f90000139f664", "Before Training", "Different", tbl_simult, "euclidean")



# Exemplary Predictions ---------------------------------------------------

p_id <- "5d3587a13e11900001093ae8"
l_results <- l_cityblock

tmp <- tbl_simult %>% group_by(participant_id) %>% count()
random_id <- as_vector(tmp[ceiling(runif(1, 1, nrow(tmp))), "participant_id"])
l_preds_city <- plot_simult_comp_preds(random_id, l_cityblock, tbl_simult)
l_preds_eucl <- plot_simult_comp_preds(random_id, l_euclidean, tbl_simult)

grid.draw(arrangeGrob(
  l_preds_city$pl_pred_data +
    ggtitle("City Block"), 
  l_preds_eucl$pl_pred_data +
    ggtitle("Euclidean")
  ))
