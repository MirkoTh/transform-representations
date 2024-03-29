rm(list = ls())
# Import Packages ---------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(docstring)
library(rutils)
library(cmdstanr)
library(ggExtra)


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


# Load and Visualize Data -------------------------------------------------

# load tbls from two expts and combine
tbl_cr1 <- read_rds("experiments/2022-02-category-learning/data/tbl_cr-psychological-representation.rds") %>% mutate(experiment = "E1: Ellipse")
tbl_cr2 <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri-psychological-representation.rds") %>% mutate(experiment = "E2: Squares")
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


l_precision <- plot_2d_distributions(tbl_combined, save = TRUE)
grid.draw(l_precision[[1]])
grid.draw(l_precision[[2]])



# EDA on Marginal given Distance from Boundary ----------------------------

tbl_combined_agg <- tbl_combined %>%
  mutate(d_boundary_stim_cut = cut(d_boundary_stim, c(0, .5, 2, 5), labels = FALSE)) %>%
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
  labs(x = "Dist. To Closest Boundary (Binned)", y = "SD (After) - SD (Before)") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom"
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")

save_my_pdf_and_tiff(
  arrangeGrob(l_precision[[2]], pl_d_bd, nrow = 1),
  "figures/figures-ms/precision-reconstruction",
  13.5, 6
)

save_my_pdf_and_tiff(
  arrangeGrob(l_precision[[2]], pl_d_bd, nrow = 1),
  "figures/precision-reconstruction",
  13.5, 6
)


# Combined Model ----------------------------------------------------------



combined_model <- stan_cr_2d_nested_db()
mod_2d <- cmdstan_model(combined_model)
tbl_combined$d_boundary_stim_z <- scale(tbl_combined$d_boundary_stim)[, 1]
# 
# some_participants <- sample(unique(tbl_combined$participant_id), 10)
# tbl_combined <- tbl_combined %>% filter(participant_id %in% some_participants)

mm <- model.matrix(x1_deviation ~ as.factor(n_categories) + as.factor(session) + d_boundary_stim_z, data = tbl_combined)
mm[, 2:3] <- mm[, 2:3] - .5

l_data <- list(
  n_data = nrow(tbl_combined),
  n_subj = length(unique(tbl_combined$participant_id)),
  y = as.matrix(tbl_combined[, c("x1_deviation", "x2_deviation")]),
  subj = as.numeric(factor(
    tbl_combined$participant_id, 
    labels = 1:length(unique(tbl_combined$participant_id))
  )),
  x = mm[, 2:4]
)
init_fun <- function() list(
  muGroup = rnorm(2, 0, .01), 
  muTime = rnorm(2, 0, .01), 
  muIA = rnorm(2, 0, .01), 
  mu0 = rnorm(2, 0, .01), 
  muBoundary = rnorm(2, 0, .01),
  b0 = matrix(rnorm(2*180, 0, .01), nrow = 180), 
  sdsubj = c(.1, .1)
)

init_fun <- function() list(
  muGroup = rnorm(2, .2, .01), 
  muTime = rnorm(2, .2, .01), 
  muIA = rnorm(2, .2, .01), 
  mu0 = rnorm(2, .2, .01), 
  muBoundary = rnorm(2, 0, .01),
  b0 = matrix(rnorm(2*180, .2, .01), nrow = 180), 
  sdsubj = c(.1, .1)
)
fit_2d <- mod_2d$sample(
  data = l_data, iter_sampling = 5000, iter_warmup = 1000, chains = 3, parallel_chains = 3, init = init_fun
)

# file_loc <- str_c("data/cr-e1-e2-combined-2d-regression-on-prec.RDS")
# fit_2d$save_object(file = file_loc)
pars_interest <- c("mu0", "muGroup", "muTime", "muIA", "muBoundary")
tbl_draws <- fit_2d$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_2d$summary(variables = pars_interest)
#tbl_summary <- tbl_summary %>% mutate(dim = str_match(variable, ",([1-2])\\]$")[,2])
saveRDS(tbl_draws, "data/cr-e1-e2-combined-2d-regression-on-prec-mcmc-samples.RDS")


lbls <- c("Intercept (Head)", "Intercept (Belly)", "Group (Head)", "Group (Belly)", "Time (Head)", "Time(Belly)", "IA (Head)", "IA (Belly)", "Boundary (Head)", "Boundary (Belly)")
# lbls <- c("SD (Head)", "SD (Belly)")

tbl_posterior <- tbl_draws %>% 
  as_tibble() %>%
  rename(chain = .chain) %>%
  pivot_longer(contains(pars_interest), names_to = "parameter", values_to = "value") %>%
  mutate(
    parameter = factor(parameter),
    parameter = fct_inorder(parameter)
  )
levels(tbl_posterior$parameter) <- lbls

# params_bf <- c("SD (Head)", "SD (Belly)", "Corr(1)")
params_bf <- unique(tbl_posterior$parameter)
params_bf <- params_bf %>% filter(r)
l <- sd_bfs(tbl_posterior, params_bf, .5)
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
  l_pl[[9]], l_pl[[10]],
  nrow = 3, ncol = 4)





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

par_lbls <- c("Intercept", "Trial (Binned)", "C1 vs. C2", "C1 vs. C3", "C1 vs. C4", "C2 vs. C3", "C2 vs. C4", "C3 vs. C4")

tbl_posterior <- tbl_draws %>% 
  dplyr::select(starts_with(c("mu_tf")), .chain) %>%
  mutate(
    `C2 vs. C3` = `mu_tf[3]` - `mu_tf[4]`,
    `C2 vs. C4` = `mu_tf[3]` - `mu_tf[5]`,
    `C3 vs. C4` = `mu_tf[4]` - `mu_tf[5]`,
  ) %>%
  rename(chain = .chain) %>%
  pivot_longer(starts_with("mu_tf") | matches("vs"), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = fct_inorder(factor(parameter)))
levels(tbl_posterior$parameter) <- par_lbls

l <- sd_bfs(tbl_posterior, par_lbls, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]

# plot the posteriors and the bfs
l_post <- map(as.list(par_lbls), plot_posterior, tbl_posterior, tbl_thx, bfs)

grid.draw(arrangeGrob(l_post[[1]], l_post[[2]], l_post[[3]], l_post[[4]], l_post[[5]], l_post[[6]], l_post[[7]], l_post[[8]], nrow = 2))

