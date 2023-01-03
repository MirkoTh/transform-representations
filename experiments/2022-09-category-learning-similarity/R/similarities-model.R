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

similarity_euclidean <- function(x, tbl_data) {
  c <- x[1]
  w1 <- x[2]
  w2 <- 1 - w1
  sims <- exp(-c^2*(w1*tbl_data$d_x1^2 + w2*tbl_data$d_x2^2))
  rsq <- (sims - tbl_data$response_scaled)^2
  return(sum(rsq))
}

pred_euclidean <- function(x, tbl_data) {
  c <- x[1]
  w1 <- x[2]
  w2 <- 1 - w1
  return(exp(-c^2*(w1*tbl_data$d_x1^2 + w2*tbl_data$d_x2^2)))
}

similarity_cityblock <- function(x, tbl_data) {
  c <- x[1]
  w1 <- x[2]
  w2 <- 1 - w1
  sims <- exp(-c*(w1*tbl_data$d_x1 + w2*tbl_data$d_x2))
  rsq <- (sims - tbl_data$response_scaled)^2
  return(sum(rsq))
}

pred_cityblock <- function(x, tbl_data) {
  c <- x[1]
  w1 <- x[2]
  w2 <- 1 - w1
  return(exp(-c*(w1*tbl_data$d_x1 + w2*tbl_data$d_x2)))
}

tbl_p1 <- tbl_simult %>% filter(participant_id == "55c333b0fdf99b1dbd2131bd")
tbl_data <- tbl_p1

similarity_cityblock(c(.5, .5), tbl_p1)
similarity_euclidean(c(.5, .5), tbl_p1)


# Fit Both Metrics --------------------------------------------------------


fit_one_session <- function(p_id, tp, pool, tbl_data, metric) {
  tbl_fit <- tbl_data %>% 
    filter(
      participant_id == p_id & session == tp & comparison_pool_binary == pool
    )
  if (metric == "cityblock") f = similarity_cityblock
  if (metric == "euclidean") f = similarity_euclidean
  results <- optim(
    c(.5, .5), 
    f, 
    tbl_data = tbl_fit, 
    lower = c(.0, .01), upper = c(10, .99),
    method = "L-BFGS-B"
  )
  return(results)
}

tbl_design <- tbl_simult %>%
  group_by(participant_id, n_categories, comparison_pool_binary, session) %>%
  count() %>% select(-n) %>%
  rename(p_id = participant_id, tp = session, pool = comparison_pool_binary)


n_workers_available <- parallel::detectCores()
plan(multisession, workers = n_workers_available - 2)

fit_safely <- safely(fit_one_session)

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


summarize_model_results <- function(l) {
  tmp <- map(map(l, "result"), "par") %>% reduce(rbind)
  tmp <- data.frame(tmp)
  tbl_design <- cbind(
    tbl_design, tmp
  )
  colnames(tbl_design) <- c("participant_id", "task", "comparison_pool", "session", "c", "w1")
  
  hist_w <- ggplot(tbl_design, aes(w1)) +
    geom_histogram(binwidth = .025, fill = "#66CCFF", color = "white") +
    geom_vline(xintercept = .5, color = "grey30", linetype = "dotdash", size = .75) +
    #scale_y_continuous(breaks = seq(0, 200, by = 25)) +
    theme_bw() +
    labs(x = expr(w[1]), y = "Nr. Participants")
  
  tbl_results_agg <- summarySEwithin(
    tbl_design, "c", 
    withinvars = c("session", "comparison_pool"), 
    betweenvars = "task", 
    idvar = "participant_id"
  )
  pd <- position_dodge(width = .3)
  pl_c <- tbl_results_agg %>% 
    ggplot(aes(session, c, group = comparison_pool)) +
    geom_point(aes(color = comparison_pool), position = pd) +
    geom_errorbar(
      aes(ymin = c - ci, ymax = c + ci, color = comparison_pool),
      width = .2, position = pd
    ) + 
    scale_color_viridis_d(name = "Category\nComparison") +
    facet_wrap(~ task) +
    theme_bw() +
    labs(x = "Timepoint")
  
  l_out <- list(
    tbl_results = tbl_design,
    tbl_results_agg = tbl_results_agg,
    hist_w = hist_w,
    pl_c = pl_c
  )
  
  return(l_out)
  
}


l_cityblock <- summarize_model_results(l_results_cityblock)
l_euclidean <- summarize_model_results(l_results_euclidean)

l_euclidean$hist_w
l_cityblock$hist_w

l_euclidean$pl_c
l_cityblock$pl_c

compare_rsqs <- function(l_results_cityblock, l_results_euclidean) {
  tbl_rsq <- map(map(l_results_cityblock, "result"), "value") %>% unlist() %>%
    cbind(
      map(map(l_results_euclidean, "result"), "value") %>% unlist()
    ) %>% data.frame()
  tbl_rsq <- tibble(tbl_rsq)
  colnames(tbl_rsq) <- c("cityblock", "euclidean")
  tbl_rsq_long <- tbl_rsq %>% pivot_longer(c(cityblock, euclidean))
  tbl_labels <- tbl_rsq_long %>% 
    grouped_agg(name, value)
  
  hist_rsq <- ggplot(
    tbl_rsq_long, aes(value, group = name)) + 
    geom_histogram(aes(fill = name), color = "white", binwidth = 5) +
    geom_label(
      data = tbl_labels, 
      aes(x = 30, y = 225, label = str_c("Mean = ", round(mean_value, 1)))
    ) +
    scale_fill_viridis_d(guide = "none") +
    facet_wrap(~ name) +
    theme_bw() +
    labs(x = "R Squared", y = "Nr. Data Points")
  
  l_out <- list(
    tbl_rsq = tbl_rsq,
    tbl_rsq_agg = tbl_labels,
    hist_rsq = hist_rsq
  )
  
  return(l_out)
}


# Exemplary Predictions ---------------------------------------------------


l_cityblock

p_id <- "5d3587a13e11900001093ae8"
l_results <- l_cityblock



plot_simult_comp_preds <- function(p_id, l_results, tbl_simult) {
  tbl_participant <- tbl_simult %>% filter(participant_id == p_id)
  tbl_results <- l_results$tbl_results %>% filter(participant_id == p_id)
  l_participant_split <- split(
    tbl_participant, 
    interaction(tbl_participant$comparison_pool_binary, tbl_participant$session)
  )
  l_results_split <- split(
    tbl_results, 
    interaction(tbl_results$comparison_pool, tbl_results$session)
  )
  for (p in tbl_results$comparison_pool) {
    for (s in  tbl_results$session) {
      filt <- which(names(l_results_split) == str_c(p, ".", s))
      l_participant_split[[filt]]$preds <- pred_cityblock(
        as_vector(l_results_split[[filt]][, c("c", "w1")]), 
        l_participant_split[[filt]]
      )
    }
  }
  tbl_participant <- reduce(l_participant_split, rbind)
  pl_pred_data <- ggplot(tbl_participant, aes(preds, response_scaled)) +
    geom_point(shape = 1) +
    facet_grid(comparison_pool_binary ~ session) +
    geom_abline(slope = 1) +
    theme_bw() +
    labs(x = "Prediction", y = "Response Scaled")
  
  l_out <- list(tbl_participant = tbl_participant, pl_pred_data = pl_pred_data)
  
  return(l_out)
  
}



tmp <- tbl_simult %>% group_by(participant_id) %>% count()
random_id <- as_vector(tmp[ceiling(runif(1, 1, nrow(tmp))), "participant_id"])
l_preds_city <- plot_simult_comp_preds(random_id, l_cityblock, tbl_simult)
l_preds_eucl <- plot_simult_comp_preds(random_id, l_euclidean, tbl_simult)

l_preds_city$pl_pred_data
l_preds_eucl$pl_pred_data






