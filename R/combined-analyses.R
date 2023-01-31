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
tbl_cr1 <- read_rds("experiments/2022-02-category-learning/data/tbl_cr.rds")
tbl_cr2 <- read_rds("experiments/2022-07-category-learning-II/data/tbl_cr-treps-long-ri.rds")
cols_required <- c("participant_id", "n_categories", "session", "x1_deviation", "x2_deviation")
tbl_combined <- tbl_cr1[, cols_required] %>% rbind(tbl_cr2[, cols_required])
tbl_combined$n_categories <- as.numeric(as.character(tbl_combined$n_categories))
tbl_combined$n_categories[tbl_combined$n_categories > 1] <- 2
tbl_combined$n_categories <- factor(tbl_combined$n_categories, labels = c("seqcomp", "cat"))
# plot 2d distributions before and after training (collapsed across groups)
pl_precision <- plot_2d_distributions(tbl_combined, save = TRUE)
grid.draw(pl_precision)

combined_model <- stan_cr_2d()
mod_2d <- cmdstan_model(combined_model)

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

fit_2d <- mod_2d$sample(
  data = l_data, iter_sampling = 500, iter_warmup = 200, chains = 1
)

file_loc <- str_c("data/cr-e1-e2-combined-2d.RDS")
fit_2d$save_object(file = file_loc)
pars_interest <- c("L_std", "Sigma")
tbl_draws <- fit_2d$draws(variables = pars_interest, format = "df")
tbl_summary <- fit_2d$summary()#variables = pars_interest)


lbls <- c("SD (Head)", "SD (Belly)", "Selfcorr(Head)", "Corr(1)", "Corr(2)", "Selfcorr(Belly)")
tbl_posterior <- tbl_draws %>% 
  as_tibble() %>%
  rename(chain = .chain) %>%
  pivot_longer(contains(pars_interest), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter, labels = lbls))

params_bf <- c("SD (Head)", "SD (Belly)", "Corr(1)")
l <- sd_bfs(tbl_posterior, params_bf, sqrt(2)/4)
bfs <- l[[1]]
tbl_thx <- l[[2]]
# plot the posteriors and the bfs
l_pl <- map(as.list(params_bf), plot_posterior, tbl_posterior, tbl_thx, bfs)
grid.arrange(l_pl[[1]], l_pl[[2]], l_pl[[3]], nrow = 1, ncol = 3)

l_pl[[1]] + coord_cartesian(xlim = c(15.5, 16.5))
l_pl[[2]] + coord_cartesian(xlim = c(14.25, 15))

















