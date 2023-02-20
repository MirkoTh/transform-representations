rm(list = ls())


# Load Packages -----------------------------------------------------------

library(tidyverse)
library(furrr)
library(parallel)
library(future)
library(rutils)
library(docstring)
library(cmdstanr)

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


tbl_p1 <- tbl_simult %>% filter(participant_id == as_vector(tbl_simult$participant_id[1]))
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


levels(tbl_design$tp) <- c("Before\nTraining", "After\nTraining")
l_cityblock <- summarize_model_results(l_results_cityblock, tbl_design)
# l_euclidean <- summarize_model_results(l_results_euclidean, tbl_design)

# l_euclidean$hist_w
l_cityblock$hist_w

# l_euclidean$pl_c
l_cityblock$pl_c

save_my_pdf(
  l_cityblock$pl_c, 
  "experiments/2022-09-category-learning-similarity/data/figures/c-parameters.pdf",
  8, 3.25
)
save_my_tiff(
  l_cityblock$pl_c, 
  "experiments/2022-09-category-learning-similarity/data/figures/c-parameters.tiff",
  8, 3.25
)

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


# run Bayesian model

sim_model_city <- stan_sim_city_plain2()
mod_sim_city <- cmdstan_model(sim_model_city)

mm <- model.matrix(
  rt ~ n_categories + comparison_pool_binary + session, data = tbl_simult
) %>% as_tibble()
mm[, 2:4] <- mm[, 2:4] - .5
mm[, 5] <- mm[, 2] * mm[, 3] * mm[, 4]

l_data <- list(
  n_data = nrow(tbl_simult),
  n_subj = length(unique(tbl_simult$participant_id)),
  subj = as.numeric(factor(
    tbl_simult$participant_id, 
    labels = 1:length(unique(tbl_simult$participant_id))
  )),
  distance1 = abs(tbl_simult$x1_true_l - tbl_simult$x1_true_r)/max(abs(tbl_simult$x1_true_l - tbl_simult$x1_true_r)), #
  distance2 = abs(tbl_simult$x2_true_l - tbl_simult$x2_true_r)/max(abs(tbl_simult$x2_true_l - tbl_simult$x2_true_r)), #
  response = tbl_simult$response_scaled,
  x = mm[, 1:5]
)

init_fun <- function() list(mu = 3, w_group = .5)

fit_sim_city <- mod_sim_city$sample(
  data = l_data, iter_sampling = 1000, iter_warmup = 500, chains = 3, parallel_chains = 3#, init = init_fun
)

# analyze posterior samples
pars_interest <- c("mu", "w_group", "sigma")
tbl_draws <- fit_sim_city$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_sim_city$summary(variables = pars_interest)
saveRDS(tbl_draws, "experiments/2022-09-category-learning-similarity/data/similarity-model-stan.RDS")


lbls <- c("Intercept", "Group", "Category Comparison", "Time Point", "3-way IA", "w_group", "sigma")
#lbls <- c("Intercept", "w_group", "sigma")

tbl_posterior <- tbl_draws %>% 
  as_tibble() %>%
  pivot_longer(starts_with(pars_interest), names_to = "parameter", values_to = "value") %>%
  rename(chain = .chain) %>%
  mutate(
    parameter = factor(parameter),
    parameter = fct_inorder(parameter)
  )
levels(tbl_posterior$parameter) <- lbls

params_bf <- levels(tbl_posterior$parameter)
l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]
bfs <- bfs[names(bfs) %in% params_bf]
tbl_thx <- tbl_thx %>% filter(parameter %in% params_bf)

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
#grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 1, ncol = 3)

grid.arrange(
  l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], l_pl[[5]], l_pl[[6]], l_pl[[7]], nrow = 2, ncol = 4
)
