upper_and_lower_bounds <- function(par, lo, hi) {
  log(((par - lo) / (hi - lo)) / (1 - (par - lo) / (hi - lo)))
}

upper_and_lower_bounds_revert <- function(par, lo, hi) {
  lo + ((hi - lo) / (1 + exp(-par)))
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
  x <- pmap(list(x, lo, hi), upper_and_lower_bounds_revert)
  l_transfer_x <- split(tbl_transfer[, c("x1", "x2")], 1:nrow(tbl_transfer))
  l_category_probs <- map(l_transfer_x, gcm_base, tbl_x = tbl_x, n_feat = n_feat, c = x[["c"]], w = x[["w"]], bias = x[["bias"]], delta = ifelse(is.null(x[["delta"]]), 0, x[["delta"]]), d_measure = d_measure)
  tbl_probs <- as.data.frame(reduce(l_category_probs, rbind)) %>% mutate(response = tbl_transfer$response)
  tbl_probs$prob_correct <- pmap_dbl(
    tbl_probs[, c("0", "1", "response")],
    ~ c(..1, ..2)[as.numeric(as.character(..3)) + 1]
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
  w <- c(w, 1 - w)
  bias <- c(bias, 1 - bias)
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


optim(
  params_init_tf,
  gcm_likelihood_no_forgetting,
  tbl_transfer = tbl_generate,
  tbl_x = l_tbl_train_strat[[i]] %>% mutate(trial_id = sample(1:nrow(.), nrow(.), replace = FALSE)), 
  n_feat = n_feat,
  d_measure = d_measure,
  lo = lo[1:3],
  hi = hi[1:3]
)


tbl_secondary <- read_csv(
  file = "experiments/2022-07-category-learning-II/data/secondary-task.csv"
  )

params <- c(c = 1, w = .5, bias = .5, delta = .5)

lo <- c(0, .01, .0001, 1e-10)
hi <- c(10, .99, .9999, .999999999)
params <- pmap(list(params[1:3], lo[1:3], hi[1:3]), upper_and_lower_bounds)
params_init <- params