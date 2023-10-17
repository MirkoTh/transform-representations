library(tidyverse)
library(grid)
library(gridExtra)

upper_and_lower_bounds <- function(par, lo, hi) {
  log(((par - lo) / (hi - lo)) / (1 - (par - lo) / (hi - lo)))
}

upper_and_lower_bounds_revert <- function(par, lo, hi) {
  lo + ((hi - lo) / (1 + exp(-par)))
}

upper_and_lower_constrain_bias <- function(bias) {
  bias[4] <- 1 - (bias[1] + bias[2] + bias[3])
  bias_out <- c()
  bias_out[1] <- upper_and_lower_bounds(bias[1], 0, 1)
  bias_out[2] <- upper_and_lower_bounds(bias[2], 0, 1 - bias[1])
  bias_out[3] <- upper_and_lower_bounds(bias[3], 0, 1 - (bias[1] + bias[2]))
  bias_out[4] <- upper_and_lower_bounds(bias[4], 0, 1 - (bias[1] + bias[2] + bias[3]))
  return(bias_out)
}


upper_and_lower_unconstrain_bias <- function(bias_constrained) {
  bias_remap <- c()
  bias_remap[1] <- upper_and_lower_bounds_revert(bias_constrained[1], 0, 1)
  bias_remap[2] <- upper_and_lower_bounds_revert(bias_constrained[2], 0, (1 - bias_remap[1]))
  bias_remap[3] <- upper_and_lower_bounds_revert(bias_constrained[3], 0, (1 - (bias_remap[1] + bias_remap[2])))
  bias_remap[4] <- upper_and_lower_bounds_revert(bias_constrained[4], 0, (1 - (bias_remap[1] + bias_remap[2] + bias_remap[3])))
  return(bias_remap)
}


gcm_likelihood_no_forgetting <- function(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi) {
  #' @description -2 * negative log likelihood of transfer set given training data
  #' and a gcm without a forgetting parameter (i.e., forgetting set to 0)
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_x training data
  #' @param n_feat number of features
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  tbl_probs <- category_probs(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi)
  ll <- log(tbl_probs$prob_correct)
  neg2llsum <- -2 * sum(ll)
  return(neg2llsum)
}

category_probs <- function(x, tbl_transfer, tbl_x, n_feat, d_measure, lo, hi) {
  #' @description calculate category probabilities for every stimulus in the transfer set
  #' @param x parameters
  #' @param tbl_transfer transfer/test data
  #' @param tbl_x training data
  #' @param n_feat number of features
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @param lo vector with lower bounds of parameters
  #' @param hi vector with upper bounds of parameters
  #' @return negative 2 * summed log likelihood
  #' 
  params_untf <- pmap(list(x[1:2], lo, hi), upper_and_lower_bounds_revert)
  bias_untf <- upper_and_lower_unconstrain_bias(x$bias)
  params_untf$bias <- bias_untf
  params_untf$w[2] <- 1 - params_untf$w[1] 
  
  l_transfer_x <- split(tbl_transfer[, c("x1", "x2")], 1:nrow(tbl_transfer))
  l_category_probs <- map(
    l_transfer_x, gcm_base, 
    tbl_x = tbl_x, 
    n_feat = n_feat, 
    c = params_untf[["c"]], 
    w = params_untf[["w"]], 
    bias = params_untf[["bias"]], 
    delta = ifelse(is.null(params_untf[["delta"]]), 0, params_untf[["delta"]]),
    d_measure = d_measure
  )
  tbl_probs <- as.data.frame(reduce(l_category_probs, rbind)) %>% mutate(response = tbl_transfer$response)
  tbl_probs$prob_correct <- pmap_dbl(
    tbl_probs[, c("1", "2", "3", "4", "response")],
    ~ c(..1, ..2, ..3, ..4)[as.numeric(as.character(..5))]
  )
  return(tbl_probs)
}

gcm_base <- function(x_new, tbl_x, n_feat, c, w, bias, delta, d_measure = 1){
  #' compute class probabilities with the GCM model
  #' 
  #' @description summed similarity computation with gcm;
  #' using sensitivity, attentional weighting, and response bias;
  #' @param x_new the x coordinates of the new item
  #' @param tbl_x the tbl with all memory exemplars,
  #' including a column "category" denoting the category of that item
  #' @param n_feat number of feature dimensions
  #' @param c sensitivity
  #' @param w attentional weighting
  #' @param bias response bias / category prior
  #' @param delta forgetting rate (if delta == 0, no forgetting)
  #' @param d_measure distance measure, 1 for city-block, 2 for euclidean
  #' @return a vector with the class probabilities for the new item
  l_x_cat <- split(tbl_x, tbl_x$category)
  sims_cat <- map(l_x_cat, f_similarity_cat, w, c, delta, x_new, d_measure)
  sims_cat_sum <- map_dbl(sims_cat, sum)
  sims_cat_sum_biased <- sims_cat_sum * bias
  map_dbl(sims_cat_sum_biased, ~ .x/sum(sims_cat_sum_biased))
}


f_similarity <- function(x1, x2, w, c, x_new, d_measure) {
  #' @description helper function calculating similarity for one item
  #' given a currently presented item to be classified
  d <- (
    w[1]*abs((x_new$x1 - x1))^d_measure + w[2]*abs((x_new$x2 - x2))^d_measure
  )^(1/d_measure)
  exp(-d*c)
}


f_similarity_cat <- function(x, w, c, delta, x_new, d_measure) {
  #' @description helper function calculating similarities for all items
  #' within a given category
  x$lag <- abs(x$trial_id - max(x$trial_id))
  x$prop_decay <- exp(- (delta * x$lag))
  sims <- pmap_dbl(x[, c("x1", "x2")], f_similarity, w, c, x_new, d_measure)
  return(sims*x$prop_decay)
}



tbl_secondary <- read_csv(
  file = "experiments/2022-07-category-learning-II/data/secondary-task.csv"
)

tbl_one_participant <- tbl_secondary %>% 
  filter(participant_id == "359aac2b4cc28c9eeffd5dfeec5b029c") %>%
  rename(x1 = x1_true, x2 = x2_true, category = cat_true)

tbl_start <- tbl_one_participant %>% filter(trial_id < 10)


params <- list(c = 1, w = .5)
lo <- c(0, .0001)
hi <- c(10, .9999)
params <- pmap(list(params, lo, hi), upper_and_lower_bounds)
bias <- rep(.25, 3)
bias_constrained <- upper_and_lower_constrain_bias(bias)
params$bias <- bias_constrained
params_init_tf <- x <- params

n_feat <- 2
d_measure <- 2



results_start <- optim(
  params_init_tf,
  gcm_likelihood_no_forgetting,
  tbl_transfer = tbl_start,
  tbl_x = tbl_start, 
  n_feat = n_feat,
  d_measure = d_measure,
  lo = lo,
  hi = hi
)
gcm_likelihood_no_forgetting(params_init_tf, tbl_start, tbl_start, n_feat, d_measure, lo, hi)


tbl_probs %>% 
  mutate(
    trial_id = seq_along(response),
    trial_cut = cut(trial_id, 2)
    ) %>% group_by(trial_cut) %>%
  summarize(mean(prob_correct))
  




bias_unconstrained <- upper_and_lower_unconstrain_bias(bias_constrained)
