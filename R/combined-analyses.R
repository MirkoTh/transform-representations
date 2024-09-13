rm(list = ls())
# Import Packages ---------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(docstring)
library(rutils)
library(cmdstanr)
library(ggExtra)
library(bayesplot)

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



suffix <- c("psychological-representation", "object-properties")[2]
is_fit <- TRUE



# Load and Visualize Data -------------------------------------------------


# load tbls from two expts and combine
tbl_cr1 <- read_rds(str_c("experiments/2022-02-category-learning/data/tbl_cr-", suffix, ".rds")) %>% mutate(experiment = "E1: Ellipse")
tbl_cr2 <- read_rds(str_c("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri-", suffix, ".rds")) %>% mutate(experiment = "E2: Squares")
cols_required <- c("experiment", "participant_id", "n_categories", "session", "x1_deviation", "x2_deviation", "d_boundary_stim")
tbl_combined <- tbl_cr1[, cols_required] %>% mutate(n_categories = as.numeric(n_categories)) %>% 
  rbind(tbl_cr2[, cols_required]) %>% mutate(n_categories = as.numeric(n_categories))
tbl_combined$n_categories <- as.numeric(as.character(tbl_combined$n_categories))
tbl_combined$n_categories[tbl_combined$n_categories > 1] <- 2
tbl_combined$n_categories <- factor(tbl_combined$n_categories, labels = c("seqcomp", "cat"))
tbl_combined$session <- factor(tbl_combined$session)
# plot 2d distributions before and after training (collapsed across groups)
# hardcoded x and y limits, which leads to the exclusion of some data points in the plot

levels(tbl_combined$session) <- c("Before", "After")
levels(tbl_combined$n_categories) <- c("Sequential Comparison", "Category Learning")


l_precision <- plot_2d_distributions(tbl_combined, represent = suffix, save = TRUE)
grid.draw(l_precision[[1]])
grid.draw(l_precision[[2]])



# EDA on Marginal given Distance from Boundary ----------------------------


if (suffix == "psychological-representation") {
  cut_vals <- c(0, .5, 2, 5)
} else if (suffix == "object-properties") {
  cut_vals <- c(0, 6, 26, 100)
}

tbl_combined_agg <- tbl_combined %>%
  mutate(d_boundary_stim_cut = cut(d_boundary_stim, cut_vals, labels = FALSE)) %>% #10
  group_by(experiment, participant_id, n_categories, d_boundary_stim_cut, session) %>%
  summarize(
    mn_x1 = mean(x1_deviation),
    mn_x2 = mean(x2_deviation),
    sd_x1 = sd(x1_deviation),
    sd_x2 = sd(x2_deviation),
    n = n()
  ) %>% ungroup() %>%
  pivot_wider(
    id_cols = c(experiment, participant_id, n_categories, d_boundary_stim_cut), 
    names_from = session,
    values_from = c(sd_x1, sd_x2)
  ) %>% mutate(
    sd_x1_change = sd_x1_After - sd_x1_Before,
    sd_x2_change = sd_x2_After - sd_x2_Before
  )

tbl_plt <- tbl_combined_agg %>% 
  pivot_longer(ends_with("_change"), names_to = "sd", values_to = "sd_val")

tbl_plt_agg <- summary_se_within(
  tbl_plt %>% filter(sd == "sd_x1_change"),
  measurevar = "sd_val", 
  betweenvars = c("experiment", "n_categories"), 
  withinvars = c("d_boundary_stim_cut")
) %>% mutate(var = "Spikiness of Head") %>%
  rbind(
    summary_se_within(
      tbl_plt %>% filter(sd == "sd_x2_change"),
      measurevar = "sd_val", 
      betweenvars = c("experiment", "n_categories"), 
      withinvars = c("d_boundary_stim_cut")
    ) %>% mutate(var = "Fill of Belly")
  )

dg <- position_dodge(width = .2)
pl_d_bd <- ggplot(tbl_plt_agg, aes(d_boundary_stim_cut, sd_val, group = n_categories)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotdash", linewidth = 1) +
  geom_errorbar(aes(ymin = sd_val - ci, ymax = sd_val + ci, color = n_categories), position = dg, width = .2) +
  geom_line(aes(color = n_categories), position = dg) +
  geom_point(color = "white", size = 3, position = dg) +
  geom_point(aes(color = n_categories), position = dg) +
  facet_grid(experiment ~ var) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(
    x = "Dist. To Closest Boundary (Binned)", 
    y = "SD (After) - SD (Before)",
    title = "Changes in Precision"
  ) + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom"
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")

save_my_pdf_and_tiff(
  arrangeGrob(l_precision[[2]], pl_d_bd, nrow = 1),
  str_c("figures/figures-ms/precision-reconstruction-", suffix),
  14.5, 7
)

save_my_pdf_and_tiff(
  arrangeGrob(l_precision[[2]], pl_d_bd, nrow = 1),
  str_c("figures/precision-reconstruction-", suffix),
  14.5, 7
)


# Combined Model ----------------------------------------------------------


if (suffix == "object-properties") {
  combined_model <- stan_cr_2d_nested_db_obj()
  
} else if (suffix == "psychological-representation") {
  combined_model <- stan_cr_2d_nested_db_psych()
}

mod_2d <- cmdstan_model(combined_model)
tbl_combined$d_boundary_stim_z <- scale(tbl_combined$d_boundary_stim)[, 1]
# 
# some_participants <- sample(unique(tbl_combined$participant_id), 10)
# tbl_combined <- tbl_combined %>% filter(participant_id %in% some_participants)

mm <- model.matrix(
  x1_deviation ~ 
    as.factor(n_categories) + as.factor(session) + 
    as.factor(experiment) + d_boundary_stim_z,
  data = tbl_combined
)
mm[, 2:4] <- mm[, 2:4] - .5
mm <- as_tibble(mm)

l_data <- list(
  n_data = nrow(tbl_combined),
  n_subj = length(unique(tbl_combined$participant_id)),
  y = as.matrix(tbl_combined[, c("x1_deviation", "x2_deviation")]),
  subj = as.numeric(factor(
    tbl_combined$participant_id, 
    labels = 1:length(unique(tbl_combined$participant_id))
  )),
  x = as.matrix(mm[, 2:5])
)

# to consider: initial precision per subject should not be < 0
# increase b0 by some large value to make sure

if (suffix == "psychological-representation") {
  init_fun <- function() list(
    muGr = rnorm(2, .2, .01), 
    muTime = rnorm(2, .2, .01), 
    muGrTime = rnorm(2, .2, .01), 
    mu0 = rnorm(2, .2, .01), 
    muBd = rnorm(2, 0, .01),
    muExp = rnorm(2, 0, .01),
    muGrBd = rnorm(2, 0, .01),
    muTimeBd = rnorm(2, 0, .01),
    muGrBdTime = rnorm(2, 0, .01),
    b0 = matrix(rnorm(2*180, 3, .01), nrow = 180), 
    sdsubj = c(.1, .1)
  )
} else if (suffix == "object-properties") {
  init_fun <- function() list(
    mu0 = rnorm(2, 100, .01), 
    muGroup = rnorm(2, -5, .01), 
    muTime = rnorm(2, -5, .01), 
    muBd = rnorm(2, 0, .01),
    muExp = rnorm(2, 0, .01),
    muGrTime = rnorm(2, -5, .01),
    muGrBd = rnorm(2, 0, .01),
    muTimeBd = rnorm(2, 0, .01),
    muGrBdTime = rnorm(2, 0, .01),
    b0 = matrix(rnorm(2*180, 100, .01), nrow = 180), 
    sdsubj = c(.1, .1)
  )
}


file_loc <- str_c("data/combined-analysis-boundary-posterior-", suffix, ".rds")
pars_interest <- c("b0", "mu0", "muGr", "muBd", "muExp", "muTime", "muGrBd", "muGrTime", "muTimeBd", "muGrBdTime")

if (is_fit) {
  fit_2d <- mod_2d$sample(
    
    #data = l_data, iter_sampling = 50, iter_warmup = 100, chains = 3, parallel_chains = 3, init = init_fun
    data = l_data, iter_sampling = 1000, iter_warmup = 500, chains = 3, parallel_chains = 3, init = init_fun
  )
  tbl_draws <- fit_2d$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_2d$summary(variables = pars_interest)
  #tbl_summary <- tbl_summary %>% mutate(dim = str_match(variable, ",([1-2])\\]$")[,2])
  saveRDS(tbl_draws, file_loc)
  
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
}


p <- mcmc_trace(tbl_draws,  pars = c("muExp[1]", "b0[122,1]"), n_warmup = 500,
                facet_args = list(nrow = 2, labeller = label_parsed))

p + facet_text(size = 15)
# lbls <- c("SD (Head)", "SD (Belly)")

tbl_posterior <- tbl_draws %>% 
  as_tibble() %>%
  rename(chain = .chain) %>%
  pivot_longer(contains(pars_interest), names_to = "parameter", values_to = "value") %>%
  mutate(
    parameter = factor(parameter),
    parameter = fct_inorder(parameter)
  )

lbls <- c(
  "Intercept (Head)", "Intercept (Belly)", "Group (Head)", "Group (Belly)", "Group x Boundary (Head)", "Group x Boundary (Belly)", 
  "Group x Time (Head)", "Group x Time (Belly)", "Group x Boundary x Time (Head)", "Group x Boundary x Time (Belly)",
  "Boundary (Head)", "Boundary (Belly)", "Exp. (Head)", "Exp. (Belly)",
  "Time (Head)", "Time (Belly)", 
  "Time x Boundary (Head)", "Time x Boundary (Belly)"
)

levels(tbl_posterior$parameter) <- lbls

# params_bf <- c("SD (Head)", "SD (Belly)", "Corr(1)")
params_bf <- unique(tbl_posterior$parameter)
l <- sd_bfs(tbl_posterior, params_bf, .5, limits = c(.025, .975))
bfs <- l[[1]]
tbl_thx <- l[[2]]
bfs <- bfs[names(bfs) %in% params_bf]
tbl_thx <- tbl_thx %>% filter(parameter %in% params_bf)

# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
#grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 1, ncol = 3)

grid.arrange(
  l_pl[[1]], l_pl[[2]], l_pl[[3]], l_pl[[4]],
  l_pl[[5]], l_pl[[6]], l_pl[[7]], l_pl[[8]],
  l_pl[[9]], l_pl[[10]], l_pl[[11]], l_pl[[12]],
  l_pl[[13]], l_pl[[14]], l_pl[[15]], l_pl[[16]],
  l_pl[[17]], l_pl[[18]],
  nrow = 6, ncol = 3)

tbl_segments <- tbl_thx %>% 
  filter(!str_detect(parameter, "Intercept")) %>%
  pivot_wider(id_cols = c(parameter), names_from = variable, values_from = value) %>%
  mutate(
    mn = (thxlo_x + thxhi_x) / 2,
    parameter = factor(parameter)
  )

tbl_bf <- tibble(parameter = names(bfs), bf = bfs) %>% filter(!str_detect(parameter, "Intercept"))
tbl_bf$bf <- format(round(tbl_bf$bf, 2), big.mark = "'", big.interval = 3L)
tbl_bf$bf[str_detect(tbl_bf$bf, "Inf")] <- "Decisive"
tbl_bf <- tbl_bf %>% left_join(tbl_segments, by = "parameter")
tbl_bf$bf <- str_trim(tbl_bf$bf, "left")


levels(tbl_segments$parameter)
tbl_segments$parameter <- fct_relevel(tbl_segments$parameter, "Time (Head)", after = 0)
tbl_segments$parameter <- fct_relevel(tbl_segments$parameter, "Time (Belly)", after = 0)
tbl_segments$parameter <- fct_relevel(tbl_segments$parameter, "Group x Boundary x Time (Belly)", after = 15)
tbl_segments$parameter <- fct_relevel(tbl_segments$parameter, "Group x Boundary x Time (Head)", after = 15)


pl_post_combined <- ggplot(tbl_segments) +
  geom_vline(xintercept = 0, color = "tomato3", linetype = "dotdash", linewidth = 1) +
  geom_segment(
    aes(x = thxlo_x, xend = thxhi_x, y = fct_rev(parameter), yend = fct_rev(parameter)), 
    linewidth = 1.25, lineend = "round"
  ) +
  geom_point(aes(mn, parameter), size = 3, color = "skyblue2") +
  geom_text(data = tbl_bf, aes(mn, parameter, label = str_c("BF = ", bf)), vjust = -.75) +
  theme_bw() +
  scale_x_continuous(expand = c(0.03, 0)) +
  scale_y_discrete(expand = c(0.1, 0)) +
  labs(x = "Posterior Value", y = "Parameter", title = "MAP, 95% HDIs, & BFs") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  )

save_my_pdf_and_tiff(
  pl_post_combined, "figures/figures-ms/bf-hdi-combined", 9, 7
)



# plot individual precision estimates
tbl_prec_ind <- tbl_draws %>% 
  as_tibble() %>%
  rename(chain = .chain) %>%
  select(c("chain", starts_with("b0"))) %>%
  pivot_longer(-chain, names_to = "parameter", values_to = "value") %>%
  mutate(
    id = str_match(parameter, "\\[([0-9]*),")[,2],
    dimension = str_match(parameter, "([1-2])\\]$")[,2],
    parameter = factor(parameter),
    parameter = fct_inorder(parameter)
  ) %>%
  group_by(id, dimension) %>%
  summarize(prec_map = mean(value)) %>%
  ungroup()
tbl_prec_ind$dimension <- factor(tbl_prec_ind$dimension, labels = c("Spikiness of Head", "Fill of Belly"))

tbl_prec_agg <- tbl_prec_ind %>% filter(dimension == "Spikiness of Head") %>% summarize(mn = mean(prec_map), sd = sd(prec_map))

hist_ind_prec <- ggplot(tbl_prec_ind, aes(prec_map)) +
  geom_histogram(aes(fill = dimension), color = "white") +
  facet_wrap(~ dimension) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0), breaks = seq(0, 30, by = 5)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Precision", y = "Nr. Participants") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_fill_brewer(palette = "Set2", name = "Feature Dimension") +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE))



pl_ind_densities <- ggplot() +
  geom_function(fun = dnorm, aes(color = "Mean"), linewidth = 1, args = list(mean = 50, sd = tbl_prec_agg$mn)) +
  geom_function(fun = dnorm, aes(color = "Mean + 1 SD"), linewidth = 1, args = list(mean = 50, sd = tbl_prec_agg$mn + tbl_prec_agg$sd)) +
  geom_function(fun = dnorm, aes(color = "Mean - 1 SD"), linewidth = 1, args = list(mean = 50, sd = tbl_prec_agg$mn - tbl_prec_agg$sd)) +
  geom_function(fun = dnorm, aes(color = "Mean + 2 SD"), linewidth = 1, args = list(mean = 50, sd = tbl_prec_agg$mn + 2*tbl_prec_agg$sd)) +
  geom_function(fun = dnorm, aes(color = "Mean - 2 SD"), linewidth = 1, args = list(mean = 50, sd = tbl_prec_agg$mn - 2*tbl_prec_agg$sd)) +
  xlim(0, 125) +
  theme_bw() +
  labs(x = "Perceived Feature Value", y = "Probability Density") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) +
  scale_color_manual(
    values = c("#93BA97", "#B3B08A", "#CFA67D", "#E69B6F", "#FC8D62"), 
    name = "",
    breaks = c("Mean - 2 SD", "Mean - 1 SD", "Mean", "Mean + 1 SD", "Mean + 2 SD")
    ) +
  guides(color = guide_legend(nrow = 2, byrow = TRUE))

pl_ind_prec_combined <- arrangeGrob(hist_ind_prec, pl_ind_densities, nrow = 1)

save_my_pdf(pl_ind_prec_combined, "~/science/post-doc/applications/2024-ambizione/analyses/individual-precision-rep-change.pdf", 12, 5)

# Category Difficulty Analysis E2 - E4 ------------------------------------


is_fit <- TRUE

tbl_cat_e2 <- readRDS("experiments/2022-07-category-learning-II/data/tbl_cat-treps-long-ri-psychological-representation.rds")
tbl_cat_e3 <- readRDS("experiments/2022-09-category-learning-similarity/data/tbl_cat-treps-psychological-representation.rds")
tbl_cat_e4 <- readRDS("experiments/2023-01-category-learning-catsim/data/tbl_cat-treps-no-outliers-psychological-representation.rds")

cols_required_cat <- c("participant_id", "n_categories", "cat_true", "trial_id", "accuracy", "trial_id_binned")

tbl_cat_sq <- tbl_cat_e2[, cols_required_cat] %>% mutate(experiment = 2) %>% rbind(
  tbl_cat_e3[, cols_required_cat] %>% mutate(experiment = 3)
) %>% rbind(
  tbl_cat_e4[, cols_required_cat] %>% mutate(experiment = 4)
  
)

tbl_cat_agg <- tbl_cat_sq %>% group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(prop_correct = mean(accuracy)) %>% ungroup()
tbl_cat_agg$cat_true <- factor(tbl_cat_agg$cat_true, labels = c("Bukil", "Venak", "Monus", "Ladiv"))
summary_se_within(tbl_cat_agg, "prop_correct", withinvars = c("cat_true", "trial_id_binned")) %>%
  ggplot(aes(trial_id_binned, prop_correct, group = cat_true)) +
  geom_errorbar(aes(ymin = prop_correct - ci, ymax = prop_correct + ci), width = .2, position = dg) +
  geom_line(aes(color = cat_true), position = dg) +
  geom_point(size = 3, color = "white", position = dg) +
  geom_point(aes(color = cat_true), position = dg) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Trial (Binned)", y = "Prop. Correct") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("#A4D3EE", "#CD4F39", "#CE7E72", "#C3A9AF"), name = "")


# set up a model, which uses binned trial, three simple coded variables as category as predictors
# set bukil category as reference category and test if easier to learn

tbl_cat_counts <- tbl_cat_sq %>%
  group_by(participant_id, cat_true, trial_id_binned) %>%
  summarize(n_trials = n(), n_correct = sum(accuracy)) %>%
  ungroup()
tbl_cat_counts$cat_true <- factor(tbl_cat_counts$cat_true)
tbl_cat_counts$trial_id_binned <- as.numeric(as.character(tbl_cat_counts$trial_id_binned))
tbl_cat_counts$trial_id_binned_c <- scale(tbl_cat_counts$trial_id_binned, scale = FALSE)[, 1]
# 
# some_participants <- sample(unique(tbl_combined$participant_id), 10)
# tbl_combined <- tbl_combined %>% filter(participant_id %in% some_participants)


mm <- model.matrix(n_correct ~ trial_id_binned + cat_true, data = tbl_cat_counts)
mm[, 3:5] <- mm[, 3:5] - .25

l_data <- list(
  n_data = nrow(tbl_cat_counts),
  n_subj = length(unique(tbl_cat_counts$participant_id)),
  n_trials = tbl_cat_counts$n_trials,
  n_correct = tbl_cat_counts$n_correct,
  subj = as.numeric(factor(
    tbl_cat_counts$participant_id, 
    labels = 1:length(unique(tbl_cat_counts$participant_id))
  )),
  x = mm
)

cat_difficulty_model <- stan_cat_difficulty()
mod_cat_difficulty <- cmdstan_model(cat_difficulty_model)

file_loc <- "data/cat-model-difficulty-e234-posterior.rds"
pars_interest <- c("mu_tf")

if (is_fit) {
  fit_difficulty <- mod_cat_difficulty$sample(
    data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3
  )
  
  tbl_draws <- fit_difficulty$draws(variables = pars_interest, format = "df")
  tbl_summary <- fit_difficulty$summary(variables = pars_interest)
  tbl_summary %>% arrange(desc(rhat))
  saveRDS(tbl_draws, file_loc)
  
} else if (!is_fit) {
  tbl_draws <- readRDS(file_loc)
}

par_lbls <- c("Intercept", "Trial (Binned)", "LR - LL", "UL - LL", "UR - LL", "LR - UL", "LR - UR", "UL - UR")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu_tf")), .chain) %>%
  mutate(
    `LR - UL` = `mu_tf[3]` - `mu_tf[4]`,
    `LR - UR` = `mu_tf[3]` - `mu_tf[5]`,
    `UL - UR` = `mu_tf[4]` - `mu_tf[5]`,
  ) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with("mu_tf") | matches(" - "), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = fct_inorder(factor(parameter)))
levels(tbl_posterior$parameter) <- par_lbls
tbl_posterior$parameter <- factor(tbl_posterior$parameter, ordered = TRUE)

l <- sd_bfs(tbl_posterior, par_lbls, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]
tbl_thx$parameter <- factor(tbl_thx$parameter)
tbl_thx$parameter <- fct_inorder(tbl_thx$parameter)

# plot the posteriors and the bfs
l_post <- map(as.list(par_lbls), plot_posterior, tbl_posterior, tbl_thx, bfs)

grid.draw(arrangeGrob(l_post[[1]], l_post[[2]], l_post[[3]], l_post[[4]], l_post[[5]], l_post[[6]], l_post[[7]], l_post[[8]], nrow = 2))

pl_hdi_difficulty <- plot_map_hdi_bf(tbl_thx, bfs, "Category Difficulty")



# Attraction to the Global Average ----------------------------------------

tbl_cr1_psych <- read_rds("experiments/2022-02-category-learning/data/tbl_cr-psychological-representation.rds") %>% mutate(experiment = "Exp. 1")
tbl_cr2_psych <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri-psychological-representation.rds") %>% mutate(experiment = "Exp. 2")
cols_req_psych <- c("participant_id", "experiment", "n_categories", "category", "session", "x1_true", "x2_true", "x1_response", "x2_response", "d_boundary", "d_closest")

tbl_psych <- tbl_cr1_psych[, cols_req_psych] %>% rbind(tbl_cr2_psych[, cols_req_psych])

# vals taken from psychophysics script
tbl_psych$x1_avg <- 3.9377975
tbl_psych$x2_avg <- 3.6487456

tbl_psych <- tbl_psych %>%
  mutate(
    d_avg_stim = sqrt((x1_true - x1_avg)^2 + (x2_true - x2_avg)^2),
    d_avg_resp = sqrt((x1_response - x1_avg)^2 + (x2_response - x2_avg)^2),
    d_move_avg = d_avg_stim - d_avg_resp
  )
hist(tbl_psych$d_move_avg)
summary(tbl_psych$d_move_avg)


tbl_cr_stim <- grouped_agg(
  tbl_psych, c(experiment, session, n_categories, x1_true, x2_true, category), c(x1_response, x2_response)
) %>% ungroup()


tbl_cr_stim$x1_avg <- 3.9377975
tbl_cr_stim$x2_avg <- 3.6487456

tbl_cr_stim <- tbl_cr_stim %>%
  mutate(
    d_avg_stim = sqrt((x1_true - x1_avg)^2 + (x2_true - x2_avg)^2),
    d_avg_resp = sqrt((mean_x1_response - x1_avg)^2 + (mean_x2_response - x2_avg)^2),
    d_move_avg = d_avg_stim - d_avg_resp
  )
tbl_cr_stim$n_categories <- factor(tbl_cr_stim$n_categories, labels = c("Seq. Comparison", "Cat. Learning", "Cat. Learning"))
tbl_cr_stim$session <- factor(tbl_cr_stim$session, labels = c("Before", "After"))

# tendencies towards global average
pl_global <- grouped_agg(tbl_cr_stim, c(experiment, session, n_categories), d_move_avg) %>%
  ggplot(aes(as.factor(session), mean_d_move_avg, group = as.factor(n_categories))) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotdash", linewidth = 1) +
  geom_line(aes(color = as.factor(n_categories))) +
  geom_errorbar(
    aes(ymin = mean_d_move_avg - se_d_move_avg, 
        ymax = mean_d_move_avg + se_d_move_avg, 
        color = as.factor(n_categories)),
    width = .2
  ) +
  geom_point(size = 3, color = "white") +
  geom_point(aes(color = n_categories)) +
  facet_wrap(~ experiment) +
  theme_bw() +
  scale_x_discrete(expand = c(0.2, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Session", y = "Move To Global Average") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4", "black"), name = "")


# delta in tendencies towards global average
tbl_move_avg <- tbl_cr_stim %>% filter(session == "Before") %>%
  select(experiment, n_categories, x1_true, x2_true, d_move_avg) %>%
  left_join(
    tbl_cr_stim %>% filter(session == "After") %>% select(experiment, n_categories, x1_true, x2_true, d_move_avg),
    by = c("experiment", "n_categories", "x1_true", "x2_true"), suffix = c("_before", "_after")
  ) %>% mutate(
    delta_d_move_avg = d_move_avg_after - d_move_avg_before
  )
pl_delta_global <- summary_se(tbl_move_avg, "delta_d_move_avg", c("experiment", "n_categories")) %>%
  mutate(a = "Delta Move To Global Average") %>%
  ggplot(aes(n_categories, delta_d_move_avg, group = experiment)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotdash", size = 1) +
  geom_line(aes(color = experiment)) +
  geom_errorbar(aes(ymin = delta_d_move_avg - ci, ymax = delta_d_move_avg + ci, color = experiment), width = .2) +
  geom_point(size = 3, color = "white") +
  geom_point(aes(color = experiment)) +
  facet_wrap(~ a) +
  theme_bw() +
  scale_x_discrete(expand = c(0.5, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Group", y = "Delta (After) - Delta (Before)") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Experiment")

pl_global2 <- arrangeGrob(pl_global, pl_delta_global, nrow = 1, widths = c(1.4, 1))
save_my_pdf_and_tiff(pl_global2, "figures/figures-ms/movement-global-average", 12, 5)
save_my_pdf_and_tiff(pl_global2, "figures/movement-global-average", 12, 5)

library(BayesFactor)
tbl_move_avg$stim_id <- interaction(tbl_move_avg$x1_true, tbl_move_avg$x2_true)
tbl_move_avg$experiment <- as.factor(tbl_move_avg$experiment)
r_anova <- anovaBF(delta_d_move_avg ~ experiment*n_categories, whichRandom = "stim_id", data = tbl_move_avg, whichModels = "top")
# n_categories
1/0.0007095356
# experiment
1/6.163355e-14
# ia
1/1.252886


# aggregate over stimuli before calculating distances (center + boundary)
tbl_cr_stim <- grouped_agg(
  tbl_cr1_psych, 
  c(session, n_categories, x1_true, x2_true, category), c(x1_response, x2_response)
) %>%
  group_by(n_categories, session) %>% 
  arrange(n_categories, session, x1_true, x2_true) %>%
  mutate(
    stim_id = row_number(x1_true)
  ) %>% ungroup() %>%
  rename(x1_response = mean_x1_response, x2_response = mean_x2_response)

tmp <- tbl_cr_stim %>% select(-category)
tmp <-  add_distance_to_nearest_center(tmp, l_centers, FALSE, "ellipses")
tbl_cr_stim$d_closest <- tmp$d_closest

grouped_agg(tbl_cr_stim, c(session, n_categories), d_closest) %>%
  ggplot(aes(session, mean_d_closest, group = as.factor(n_categories))) +
  geom_line(aes(color = as.factor(n_categories))) +
  geom_errorbar(aes(ymin = mean_d_closest - se_d_closest, ymax = mean_d_closest + se_d_closest, color = as.factor(n_categories)))


tbl_cr_stim <- grouped_agg(
  tbl_cr2_psych, c(session, n_categories, x1_true, x2_true, category), c(x1_response, x2_response)
) %>% ungroup() %>%
  rename(x1_response = mean_x1_response, x2_response = mean_x2_response)
l_info = list(representation = "psychological-representation", use_exptl_stimuli = TRUE)
l_centers <- category_centers(1, 0, l_info)
tbl_cr_stim$d_boundary <- add_distance_to_boundary(tbl_cr_stim, l_centers, "square", l_info)

grouped_agg(tbl_cr_stim, c(session, n_categories), d_boundary) %>%
  ggplot(aes(session, mean_d_boundary, group = as.factor(n_categories))) +
  geom_line(aes(color = as.factor(n_categories))) +
  geom_errorbar(aes(ymin = mean_d_boundary - se_d_boundary, ymax = mean_d_boundary + se_d_boundary, color = as.factor(n_categories)))


tbl_cr_stim <- grouped_agg(
  tbl_psych, c(experiment, session, n_categories, x1_true, x2_true, category), d_closest
) %>% ungroup()

grouped_agg(tbl_cr_stim, c(experiment, session, n_categories), mean_d_closest) %>%
  ggplot(aes(session, mean_mean_d_closest, group = as.factor(n_categories))) +
  geom_line(aes(color = as.factor(n_categories))) +
  geom_errorbar(aes(ymin = mean_mean_d_closest - se_mean_d_closest, ymax = mean_mean_d_closest + se_mean_d_closest, color = as.factor(n_categories))) +
  facet_wrap(~ experiment)
