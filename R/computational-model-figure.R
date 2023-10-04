rm(list = ls())


library(tidyverse)
library(mvtnorm)
library(ggstar)
library(ggforce)
library(grid)
library(gridExtra)

files <- c(
  "R/utils.R", "R/plotting.R", 
  "R/analysis-utils.R",
  "R/analysis-plotting.R"
)
walk(files, source)

n_stimuli <- 100L
# constant
l_info_prep <- list(
  n_stimuli = n_stimuli
)

thx <- c(0, sqrt(n_stimuli) - 1)
x1 <- seq(thx[1], thx[2], by = 1)
x2 <- seq(thx[1], thx[2], by = 1)
features <- crossing(x1, x2)
tbl <- tibble(stim_id = seq(1, nrow(features)), features)
tbl <- create_categories(tbl, sqrt(9)) %>% select(-c(x1_cat, x2_cat))

# variable
tbl_vary <- crossing(
  n_categories = c(4L), cat_type = c("prototype"), # "rule", 
  prior_sd = c(.75), sampling = c("improvement"),
  constrain_space = c(TRUE), category_shape = c("square"),
  is_reward = FALSE
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
tbl_info <- tibble(do.call(rbind.data.frame, l_info)) %>%
  mutate(condition_id = seq(1:length(l_info))) %>%
  relocate(condition_id, .before = n_stimuli)


l_stimuli <- make_stimuli(l_info[[1]])
l_stimuli[[1]]$category <- factor(l_stimuli[[1]]$category, labels = seq(1, 4, by = 1))
levels(l_stimuli[[1]]$category) <- c("Bukil", "Venak", "Monus", "Ladiv")


grid_density <- function(x1_mn, x2_mn, stim_id) {
  x1 <- seq(x1_mn - .45, x1_mn + .45, by = .1)
  x2 <- seq(x2_mn - .45, x2_mn + .45, by = .1)
  grid2d <- crossing(x1, x2)
  grid2d$density <- dmvnorm(
    grid2d, c(x1_mn, x2_mn), matrix(c(1, 0, 0, 1), nrow = 2, byrow = TRUE)
  )
  grid2d$stim_id <- stim_id
  return(grid2d)
}

l_tbl_prior <- pmap(tbl[, c("x1", "x2", "stim_id")], grid_density)
tbl_prior <- reduce(l_tbl_prior, rbind)


xy_breaks <- seq(min(tbl$x1), max(tbl$x1), by = 2)

tbl_star <- tibble(
  x1 = c(4.55, 5.45),
  x2 = c(7.1, 6.9),
  lbl = as.factor(c(1, 2))
)

plt_prior_stim <- ggplot(tbl_prior, aes(x1, x2)) +
  geom_raster(aes(fill = density), interpolate = TRUE, show.legend = FALSE) +
  geom_contour(aes(z = density), color = "white", binwidth = .005) +
  geom_star(data = tbl_star, aes(x1, x2, color = lbl), size = 4, starstroke = 3, show.legend = FALSE) +
  ggrepel::geom_label_repel(data = tbl_star, aes(x1, x2, label = lbl), color = "black") +
  scale_fill_gradient(low = "black", high = "white") +
  scale_color_manual(values = c("skyblue2", "tomato3"), name = "Sampled Stimulus") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 8, by = 2), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-1, 10), ylim = c(-1, 10)) +
  labs(
    title = "Perceptual Representation",
    x = "Spikiness of Head",
    y = "Fill of Belly"
  ) + theme(strip.background = element_rect(fill = "white"))

plt_categories <- ggplot() +
  geom_point(data = l_stimuli[[1]], aes(x1, x2), color = "grey", alpha = .25, size = 4, show.legend = FALSE) +
  geom_point(data = tibble(x1 = 5, x2 = 7), aes(x1, x2), color = "grey60", size = 4, show.legend = FALSE) +
  geom_hline(yintercept = 4.5) +
  geom_vline(xintercept = 4.5) +
  geom_star(data = tbl_star, aes(x1, x2, fill = lbl), color = "black", size = 4, show.legend = FALSE) +
  ggrepel::geom_label_repel(data = tbl_star, aes(x1, x2, label = lbl), color = "black", show.legend = FALSE) +
  theme_bw() +
  scale_color_viridis_d(name = "Category") +
  scale_fill_manual(values = c("skyblue2", "tomato3"), name = "Sampled Stimulus") +
  theme_bw() +
  scale_x_continuous(breaks = seq(0, 8, by = 2), expand = c(0, 0)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2), expand = c(0, 0)) +
  coord_cartesian(xlim = c(-1, 10), ylim = c(-1, 10)) +
  labs(
    x = "Spikiness of Head",
    y = "Fill of Belly",
    title = "Category Structure"
  ) +
  theme(strip.background = element_rect(fill = "white"))

tbl_acceptance <- tibble(
  x1 = c(5, 4.55, 5.45), 
  x2 = c(7, 7.1, 6.9), 
  stim_sample = fct_inorder(factor(c("Baseline", 1, 2))),
  "Posterior Probability" = c(0.767, .529, .904),
  "Metropolis-Hastings" = pmin(1, c(.767/.767, .529/.767, .904/.767))
  ) %>% pivot_longer(cols = c("Posterior Probability", "Metropolis-Hastings"))
tbl_acceptance$name <- fct_inorder(factor(tbl_acceptance$name))

plt_acceptance <- ggplot(tbl_acceptance, aes(stim_sample, value, group = stim_sample)) +
  geom_col(aes(fill = stim_sample), show.legend = FALSE) +
  facet_wrap(~ name) + 
  theme_bw() +
  scale_fill_manual(values = c("grey60", "skyblue2", "tomato3"), name = "Sampled Stimulus") +
  scale_x_discrete(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = "Stimulus Sample", y = "Acceptance Probability", title = "Acceptance Algorithms") +
  theme(strip.background = element_rect(fill = "white"))
  



grid.draw(arrangeGrob(plt_prior_stim, plt_categories, plt_acceptance, nrow = 1))
  