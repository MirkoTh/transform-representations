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


# Priors ------------------------------------------------------------------


n_stimuli <- 100L

# constant
l_info_prep <- list(
  n_stimuli = n_stimuli
)

# variable
tbl_vary <- tibble(
  n_categories = c(2L, 2L, 4L, 4L), 
  cat_type = rep("prototype", 4),
  prior_sd = rep(.75, 4), sampling = rep("improvement", 4),
  constrain_space = rep(TRUE, 4), 
  category_shape = rep(c("ellipses", "square"), each = 2),
  is_reward = rep(FALSE, 4),
  use_exptl_stimuli = rep(c(TRUE, TRUE), 2),
  representation = rep(c("object-properties", "psychological-representation"), 2)
)
l_info <- pmap(
  tbl_vary, ~ append(
    l_info_prep, 
    list(
      n_categories = ..1, cat_type = ..2, prior_sd = ..3,
      sampling = ..4, constrain_space = ..5,
      category_shape = ..6, is_reward = ..7,
      use_exptl_stimuli = ..8,
      representation = ..9
    )
  )
)
tbl_info <- tibble(do.call(rbind.data.frame, l_info)) %>%
  mutate(condition_id = seq(1:length(l_info))) %>%
  relocate(condition_id, .before = n_stimuli)


l_stimuli <- map(l_info, make_stimuli)

category_names <- c("Bukil", "Venak", "Monus", "Ladiv")
for (i in 1:length(l_stimuli)) {
  l_stimuli[[i]][[1]]$category <- factor(
    l_stimuli[[i]][[1]]$category, labels = category_names[1:l_info[[i]]$n_categories]
  )
}

plt_ellipse_obj <- my_ellipse_f(l_stimuli[[1]])
plt_ellipse_psych <- my_ellipse_f(l_stimuli[[2]])

plt_square_obj <- my_square_f(l_stimuli[[3]])
plt_square_psych <- my_square_f(l_stimuli[[4]])

plt_setups <- arrangeGrob(plt_ellipse_obj, plt_square_obj, plt_ellipse_psych, plt_square_psych, nrow = 2)

save_my_pdf_and_tiff(plt_setups, "figures/category-learning-setup", 8.5*.9, 10*.9)
save_my_pdf_and_tiff(plt_setups, "figures/figures-ms/category-learning-setup", 8.5*.9, 10*.9)

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



