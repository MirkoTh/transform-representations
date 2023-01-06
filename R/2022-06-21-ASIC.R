library(tidyverse)
library(mvtnorm)
library(ggstar)

files <- c(
  "R/utils.R", "R/plotting.R", 
  "R/analysis-utils.R",
  "R/analysis-plotting.R"
)
walk(files, source)


# Priors ------------------------------------------------------------------


n_stimuli <- 100L

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli
)

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

tbl_star <- tibble(
  x1 = c(4.55, 5.45),
  x2 = c(7.1, 6.9)
)

plt_categories <- ggplot() +
  geom_point(data = l_stimuli[[1]], aes(x1, x2, color = category), size = 4) +
  #geom_point(data = l_stimuli[[2]]$tbl_ellipses, aes(x_rotated, y_rotated), alpha = .2) +
  geom_hline(yintercept = 4.5) +
  geom_vline(xintercept = 4.5) +
  #geom_star(data = tbl_star, aes(x1, x2), color = "red", fill = "red", size = 4) +
  theme_bw() +
  scale_color_viridis_d(name = "Category") +
  scale_x_continuous(breaks = seq(0, 9, 2)) +
  scale_y_continuous(breaks = seq(0, 9, 2)) +
  # theme(
  #   legend.position = c(.82, .165), 
  #   panel.border = element_rect(colour = "black", fill=NA),
  #   legend.box.background = element_rect(colour = "black"),
  #   legend.background = element_blank()
  # ) +
  labs(
    x = "Spikiness of Head",
    y = "Fill of Belly",
    title = "Category Structure"
  )


# Category Structure ------------------------------------------------------


thx <- c(0, sqrt(n_stimuli) - 1)
x1 <- seq(thx[1], thx[2], by = 1)
x2 <- seq(thx[1], thx[2], by = 1)
features <- crossing(x1, x2)
tbl <- tibble(stim_id = seq(1, nrow(features)), features)

tbl <- create_categories(tbl, sqrt(9)) %>% select(-c(x1_cat, x2_cat))


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

plt_prior_stim <- ggplot(tbl_prior, aes(x1, x2)) +
  geom_raster(aes(fill = density), interpolate = TRUE) +
  geom_contour(aes(z = density), color = "white", binwidth = .005) +
  # geom_segment(aes(x = -.5, xend = 4.75, y = 7.25, yend = 7.25), color = "darkorchid2", size = 1.5) +
  # geom_segment(aes(x = -.5, xend = 5.25, y = 6.75, yend = 6.75), color = "darkorchid2", size = 1.5) +
  # geom_segment(aes(x = 4.75, xend = 4.75, y = -.5, yend = 7.25), color = "darkorchid2", size = 1.5) +
  # geom_segment(aes(x = 5.25, xend = 5.25, y = -.5, yend = 6.75), color = "darkorchid2", size = 1.5) +
  geom_star(data = tbl_star, aes(x1, x2), color = "red", fill = "red", size = 4) +
  theme_bw() +
  labs(
    title = "Stimulus Priors",
    x = "Spikiness of Head",
    y = "Fill of Belly"
  ) +
  theme(legend.position = "omit") +
  scale_fill_gradient(low = "black", high = "white") +
  scale_x_continuous(breaks = xy_breaks) +
  scale_y_continuous(breaks = xy_breaks)



png(filename = "figures/2-ellipses-setup.png", width = 8, height = 4, units = "in", res = 200)
plot_arrangement(list(plt_prior_stim, plt_categories))
dev.off()



