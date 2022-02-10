# Load Packages and Home-Grown Modules ------------------------------------

rm(list = ls())
library(tidyverse)
library(jsonlite)

files <- c("R/utils.R", "R/plotting.R")
walk(files, source)


# Simulation Parameters ---------------------------------------------------

n_stimuli <- 144L
nruns <- 10000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(2L, 4L), cat_type = c("prototype"),
  prior_sd = c(.75), sampling = c("improvement"),
  constrain_space = c(TRUE), category_shape = c("ellipses") 
)
l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6
    )
  )
)


l_stimuli <- map(l_info, make_stimuli)

l_stimuli[[1]][[1]] %>% mutate(x1 = x1 + 1, x2 = x2 + 1) %>%
  select(x1, x2, category) %>% as.list() %>% toJSON()

save_category_coords <- function(x){
  max_category <- max(as.numeric(as.character(x[[1]][[1]]$category)))
  x[[1]] <- x[[1]] %>% 
    mutate(
      category = as.numeric(as.character(category)),
      x1 = x1 + 1, x2 = x2 + 1
      )
  x[[1]][, c("x1", "x2", "category")] %>% 
    as.list() %>%
    toJSON() %>%
    write_json(str_c("experiments/2022-02-category-learning/category-mapping-", max_category, ".json"))
}

map(l_stimuli, save_category_coords)
