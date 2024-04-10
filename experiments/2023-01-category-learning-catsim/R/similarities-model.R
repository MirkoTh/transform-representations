rm(list = ls())

with_outliers <- FALSE
pl_suffix <- ifelse(with_outliers, "-outliers", "no-outliers")
is_fit <- TRUE

# Load Packages -----------------------------------------------------------

library(tidyverse)
library(furrr)
library(parallel)
library(future)
library(rutils)
library(grid)
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
  "R/normDataWithin.R",
  "R/stan-models.R"
)
walk(files, source)

tbl_simult <- readRDS(str_c("experiments/2023-01-category-learning-catsim/data/tbl_simult-treps", pl_suffix, ".rds"))
tbl_simult$n_categories <- factor(tbl_simult$n_categories)
levels(tbl_simult$n_categories) <- c("Similarity", "4 Categories")

tbl_simult <- tbl_simult %>%
  mutate(
    d_cityblock = abs(x1_true_l - x1_true_r) + abs(x2_true_l - x2_true_r),
    d_x1 = abs(x1_true_l - x1_true_r),
    d_x2 = abs(x2_true_l - x2_true_r),
    response_scaled = response/max(response)
  )


tbl_p1 <- tbl_simult %>% filter(participant_id == sample(unique(tbl_simult$participant_id), 1))
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

# l_euclidean$hist_c
l_cityblock$hist_c

# l_euclidean$pl_c
l_cityblock$pl_c


fit_one_subset("5aff33bae55f90000139f664", "Before Training", "Same", tbl_simult, "euclidean")
fit_one_subset("5aff33bae55f90000139f664", "Before Training", "Different", tbl_simult, "euclidean")

save_my_pdf_and_tiff(
  l_cityblock$pl_c + theme(text = element_text(size = 16)), 
  str_c("experiments/2023-01-category-learning-catsim/data/figures/c-parameters", pl_suffix),
  8, 3.25
)
save_my_pdf_and_tiff(
  l_cityblock$pl_c + theme(text = element_text(size = 16)), 
  str_c("figures/c-parameters", pl_suffix),
  8, 3.25
)

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


v_outliers <- l_cityblock$tbl_results %>% ungroup() %>%
  filter(c > 1 | w1 < .02) %>%
  arrange(participant_id) %>%
  select(participant_id) %>%
  as_vector() %>% unique() %>%
  as.character()
tbl_simult %>% filter(participant_id %in% v_outliers) %>%
  grouped_agg(c(participant_id, d_euclidean_cut), response) %>%
  ggplot(aes(d_euclidean_cut, mean_response), group = participant_id) +
  geom_point(aes(size = n)) +
  facet_wrap(~ participant_id)



# run Bayesian model ------------------------------------------------------

tbl_simult %>%
  grouped_agg(c(n_categories, session, comparison_pool_binary), response) %>%
  ggplot(aes(session, mean_response, group = comparison_pool_binary)) +
  geom_point(aes(color = comparison_pool_binary)) +
  geom_line(aes(color = comparison_pool_binary)) +
  facet_wrap(~ n_categories)



sim_model_city <- stan_sim_city()
mod_sim_city <- cmdstan_model(sim_model_city)

mm <- model.matrix(
  rt ~ as.factor(n_categories) + as.factor(comparison_pool_binary) + as.factor(session),
  data = tbl_simult
) %>% as_tibble()
mm[, 2:4] <- mm[, 2:4] - .5

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
  x = mm[, 1:4]
)

pars_interest <- c("mu", "w_group", "sigma")
file_loc_sim <- "experiments/2023-01-category-learning-catsim/data/similarity-model-stan-posterior.RDS"

if (is_fit) {
  fit_sim_city <- mod_sim_city$sample(
    data = l_data, iter_sampling = 10000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  
  # analyze posterior samples
  tbl_draws <- fit_sim_city$draws(variables = pars_interest, format = "df")
  saveRDS(tbl_draws, file_loc_sim)
  tbl_summary <- fit_sim_city$summary(variables = pars_interest)
  tbl_summary %>% arrange(desc(rhat))
  
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc_sim)
  
}


lbls <- c(
  "Intercept", 
  "Group", "Category Comparison", "Time Point", 
  "Gr x CC", "Cr x TP", "CC x TP",
  "Gr x CC x TP")# , "w_group", "sigma"

#lbls <- c("Intercept", "Group", "Category Comparison", "Time Point", "3-way IA", "w_group", "sigma")
#lbls <- c("Intercept", "w_group", "sigma")

tbl_posterior <- tbl_draws %>% 
  as_tibble() %>%
  pivot_longer(starts_with(pars_interest[!(pars_interest %in% c("w_group", "sigma"))]), names_to = "parameter", values_to = "value") %>%
  rename(chain = .chain) %>%
  mutate(
    parameter = factor(parameter),
    parameter = fct_inorder(parameter),
    parameter = factor(parameter, ordered = TRUE)
  )
levels(tbl_posterior$parameter) <- lbls

params_bf <- levels(tbl_posterior$parameter)
l <- sd_bfs(tbl_posterior, params_bf, .5)
bfs <- l[[1]]
tbl_thx <- l[[2]]
bfs <- bfs[names(bfs) %in% params_bf]
tbl_thx <- tbl_thx %>% filter(parameter %in% params_bf)
tbl_thx$parameter <- fct_inorder(factor(tbl_thx$parameter))

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
#grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 1, ncol = 3)

gridExtra::grid.arrange(
  l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]], l_pl[[5]], l_pl[[6]], l_pl[[7]], nrow = 2, ncol = 4
)

pl_c <- plot_map_hdi_bf(tbl_thx, bfs, "c Model E4") + theme(title = element_blank())
save_my_pdf_and_tiff(pl_c, "figures/figures-ms/c-hdi-e4", 6.5, 4)


tbl_design <- unique(mm)
colnames(tbl_design) <- c("Intercept", "Group", "Comparison", "Session")

multiply_my_posterior <- function(Intercept, Group, Comparison, Session) {
  Intercept * tbl_draws$`mu[1]` +
    Group * tbl_draws$`mu[2]` +
    Comparison * tbl_draws$`mu[3]` +
    Session * tbl_draws$`mu[4]` +
    Group * Comparison * tbl_draws$`mu[5]` +
    Group * Session * tbl_draws$`mu[6]` +
    Comparison * Session * tbl_draws$`mu[7]` +
    Group * Comparison * Session * tbl_draws$`mu[8]`
}

tmp_draws <- as.data.frame(reduce(pmap(tbl_design, multiply_my_posterior), cbind))
v_map <- tmp_draws %>%
  colMeans()
v_hdi <- apply(
  tmp_draws, 2, function(x) {
    v_sorted <- sort(x)
    tbl_sorted <- tibble(val = v_sorted, rwn = 1:length(v_sorted)) %>%
      mutate(prop = rwn / length(v_sorted)) %>%
      filter(prop >= .05 & prop <= .95)
    return(c(tbl_sorted$val[1], tbl_sorted$val[nrow(tbl_sorted)]))
  }
)


tbl_design$map <- v_map
tbl_design$hdi_lo <- v_hdi[1, ]
tbl_design$hdi_hi <- v_hdi[2, ]
tbl_design$Group <- factor(tbl_design$Group, labels = c("Seq. Comparison", "Cat. Learning"))
tbl_design$Comparison <- factor(tbl_design$Comparison, labels = c("Same", "Different"))
tbl_design$Session <- factor(tbl_design$Session, labels = c("Before", "After"))


pd <- position_dodge(width = .2)
pl_c <- ggplot(tbl_design, aes(Session, map, group = Comparison), position = pd) +
  geom_errorbar(aes(ymin = hdi_lo, ymax = hdi_hi, color = Comparison), width = .2, position = pd) +
  geom_line(aes(color = Comparison), position = pd) +
  geom_point(size = 3, color = "white", position = pd) +
  geom_point(aes(color = Comparison), position = pd) +
  facet_wrap(~ Group) +
  theme_bw() +
  scale_x_discrete(expand = c(0.15, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Time Point", y = "c") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Category\nComparison")

save_my_pdf_and_tiff(pl_c, "figures/figures-ms/c-parameters-e4", 8, 4)
