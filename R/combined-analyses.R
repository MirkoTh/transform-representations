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


# plot 2d distributions before and after training (collapsed across groups)
pl_precision <- plot_2d_distributions(tbl_combined, save = TRUE)
grid.draw(pl_precision)

