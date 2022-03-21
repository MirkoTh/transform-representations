# Load Packages and Home-Grown Modules ------------------------------------

rm(list = ls())
library(tidyverse)
library(jsonlite)

files <- c("R/utils.R", "R/plotting.R")
walk(files, source)


# Simulation Parameters ---------------------------------------------------

n_stimuli <- 100L
nruns <- 10000

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli,
  nruns = nruns
)

# variable
tbl_vary <- crossing(
  n_categories = c(2L, 3L), cat_type = c("prototype"),
  prior_sd = c(.75), sampling = c("improvement"),
  constrain_space = c(TRUE), category_shape = c("ellipses"),
  is_reward = c(FALSE)
)
l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6, is_reward = ..7
    )
  )
)


l_stimuli <- map(l_info, make_stimuli)

l_stimuli[[2]][[1]] %>% mutate(x1 = x1 + 1, x2 = x2 + 1) %>%
  select(x1, x2, category) %>% as.list() %>% toJSON()

plot_conditions <- function(tbl) {
  ggplot() +
    geom_point(data = tbl[[1]], aes(x1, x2, group = category, color = category)) + 
    geom_point(data = tbl[[2]]$tbl_ellipses, aes(x_rotated, y_rotated, group = category), color = "grey", size = .5) +
    scale_color_brewer(name = "Category", palette = "Set1") +
    theme_bw() +
    labs(
      x = "Head Spikiness",
      y = "Fill of Belly"
    )
}
x11()
plot_conditions(l_stimuli[[2]])
x11()
plot_conditions(l_stimuli[[1]])

save_category_coords <- function(x){
  max_category <- max(as.numeric(as.character(x[[1]]$category)))
  x[[1]] <- x[[1]] %>% 
    mutate(
      category = as.numeric(as.character(category)),
      stim_id = stim_id - 1
    )
  x[[1]][, c("stim_id", "x1", "x2", "category")] %>% 
    as.list() %>%
    jsonlite::toJSON(dataframe = "values") %>%
    write_json(str_c("experiments/2022-02-category-learning/category-mapping-", max_category, ".json"))
  for (i in 2:max_category) {
    x[[1]][, c("stim_id", "category")] %>% 
      filter(category == i) %>%
      select(-category) %>%
      as.list() %>%
      jsonlite::toJSON(dataframe = "values") %>%
      write_json(str_c("experiments/2022-02-category-learning/category-exemplars-", max_category, "-", i, ".json"))
      }
}

walk(l_stimuli, save_category_coords)
