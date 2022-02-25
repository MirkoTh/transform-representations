
# Import Packages ---------------------------------------------------------

library(jsonlite)
library(tidyverse)
library(ggExtra)
library(docstring)


# Import Home-Grown Modules -----------------------------------------------
files <- c("experiments/2022-02-category-learning/R/analysis-utils.R")
walk(files, source)

l_tbl_data <- load_data()
tbl_cr <- l_tbl_data[[1]]
tbl_cat <- l_tbl_data[[2]]

# add deviation variables
tbl_cr$x1_deviation <- tbl_cr$x1_true - tbl_cr$x1_response
tbl_cr$x2_deviation <- tbl_cr$x2_true - tbl_cr$x2_response
lims <- tbl_cr %>% 
  summarise(min_x = min(x1_true), max_x = max(x2_true)) %>%
  mutate(min_x = min_x - 1, max_x = max_x + 1) %>%
  as_vector()
cutpoints <- seq(lims[1], lims[2], length.out = 6)


tbl_cr_agg <- tbl_cr %>%
  filter(session %in% c(1, 3)) %>%
  mutate(
    x1_true_binned = cut(x1_true, cutpoints, labels = FALSE),
    x2_true_binned = cut(x2_true, cutpoints, labels = FALSE)
  ) %>% group_by(participant_id, session, x1_true_binned, x2_true_binned) %>%
  summarise(avg_deviation_x1x2 = mean(sqrt(x1_deviation^2 + x2_deviation^2))) %>%
  ungroup()

ggplot(tbl_cr_agg, aes(x1_true_binned, x2_true_binned)) +
  geom_tile(aes(fill = avg_deviation_x1x2)) +
  scale_fill_gradient(name = "Avg. Deviation", low = "#009966", high = "#FF6666") +
  labs(
    x = "Spikiness of Head (Binned)",
    y = "Fill of Belly (Binned)"
  ) + facet_wrap(~ session) + theme_bw()



plot_marginals_one_session <- function(idx_session, tbl) {
  idx_session <- 1
  idx_color <- ifelse(idx_session == 1, 1, 2)
  col <- c("#3399FF", "#990099")[idx_color]
  pl <- ggplot(tbl %>% filter(session == idx_session), aes(x1_deviation, x2_deviation, group = session)) +
    geom_point(color = col, shape = 1, size = 2) +
    theme_bw() +
    theme(plot.title = element_text(size=10)) +
    scale_color_brewer(palette = "Set1") +
    labs(
      x = bquote(x[1]),
      y = bquote(x[2])
    ) + coord_cartesian(xlim = c(-50, 50), ylim = c(-50, 50))
  
  pl_marginals <- ggMarginal(pl, groupColor = TRUE, fill = col, type = "densigram")
  return(pl_marginals)
}


