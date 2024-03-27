rm(list = ls())
# Import Packages ---------------------------------------------------------

library(tidyverse)
library(grid)
library(gridExtra)
library(docstring)
library(rutils)
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
  mutate(d_boundary_stim_cut = cut(d_boundary_stim, c(0, .5, 2, 5))) %>%
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
  ) %>% mutate(var = "Head Spikiness") %>%
  rbind(
    summary_se_within(
      tbl_plt %>% filter(sd == "sd_x2_change"),
      measurevar = "sd_val", 
      betweenvars = c("experiment", "n_categories"), 
      withinvars = c("d_boundary_stim_cut")
    ) %>% mutate(var = "Belly Fill")
  )

dg <- position_dodge(width = .2)
ggplot(tbl_plt_agg, aes(d_boundary_stim_cut, sd_val, group = n_categories)) +
  geom_hline(yintercept = 0, color = "grey", linetype = "dotdash", linewidth = 1) +
  geom_errorbar(aes(ymin = sd_val - ci, ymax = sd_val + ci, color = n_categories), position = dg, width = .2) +
  geom_line(aes(color = n_categories), position = dg) +
  geom_point(color = "white", size = 3, position = dg) +
  geom_point(aes(color = n_categories), position = dg) +
  facet_grid(experiment ~ var) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Distance To Closest Boundary", y = "SD (After) - SD (Before)") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    axis.text.x = element_text(angle = 45, vjust = .5),
    legend.position = "bottom"
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "")




# Combined Model ----------------------------------------------------------



combined_model <- stan_cr_2d_nested()
mod_2d <- cmdstan_model(combined_model)

# 
# some_participants <- sample(unique(tbl_combined$participant_id), 10)
# tbl_combined <- tbl_combined %>% filter(participant_id %in% some_participants)

mm <- model.matrix(x1_deviation ~ n_categories + session, data = tbl_combined)
mm[, 2:3] <- mm[, 2:3] - .5

l_data <- list(
  n_data = nrow(tbl_combined),
  n_subj = length(unique(tbl_combined$participant_id)),
  y = as.matrix(tbl_combined[, c("x1_deviation", "x2_deviation")]),
  subj = as.numeric(factor(
    tbl_combined$participant_id, 
    labels = 1:length(unique(tbl_combined$participant_id))
  )),
  x = mm[, 2:3]
)
init_fun <- function() list(
  muGroup = rnorm(2, .2, .01), 
  muTime = rnorm(2, .2, .01), 
  muIA = rnorm(2, .2, .01), 
  mu0 = rnorm(2, .2, .01), 
  b0 = matrix(rnorm(2*178, .2, .01), nrow = 178), 
  sdsubj = c(.1, .1)
)
fit_2d <- mod_2d$sample(
  data = l_data, iter_sampling = 50, iter_warmup = 20, chains = 1, init = init_fun, parallel_chains = 1
)

# file_loc <- str_c("data/cr-e1-e2-combined-2d-regression-on-prec.RDS")
# fit_2d$save_object(file = file_loc)
pars_interest <- c("mu0", "muGroup", "muTime", "muIA")
tbl_draws <- fit_2d$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_2d$summary(variables = pars_interest)
#tbl_summary <- tbl_summary %>% mutate(dim = str_match(variable, ",([1-2])\\]$")[,2])
saveRDS(tbl_draws, "data/cr-e1-e2-combined-2d-regression-on-prec-mcmc-samples.RDS")


lbls <- c("Intercept (Head)", "Intercept (Belly)", "Group (Head)", "Group (Belly)", "Time (Head)", "Time(Belly)", "IA (Head)", "IA (Belly)")
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
  nrow = 2, ncol = 4)















