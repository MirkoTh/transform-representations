library(tidyverse)
library(MASS)


# idea
# explanation of assimilation and contrast as scaling issues

m_cov <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2, byrow = FALSE)

x1_true <- seq(1, 10, by = 1)
x2_true <- x1_true
tbl_gt <- crossing(x1_true, x2_true)
tbl_gt$cat <- 4
tbl_gt$cat[tbl_gt$x1_true < 5.5 & tbl_gt$x2_true < 5.5] <- 1
tbl_gt$cat[tbl_gt$x1_true < 5.5 & tbl_gt$x2_true >= 5.5] <- 2
tbl_gt$cat[tbl_gt$x1_true >= 5.5 & tbl_gt$x2_true >= 5.5] <- 3


my_mvsample <- function(x1_true, x2_true, cat, m_cov) {
  tbl_prototypes <- tibble(
    category = 1:4,
    x1_pt = c(2.5, 2.5, 7.5, 7.5),
    x2_pt = c(2.5, 7.5, 2.5, 7.5)
  )
  sd_push_center <- 1 - sqrt((x1_true - 5)^2 + (x2_true - 5) ^ 2) / sqrt(50)
  s <- as_tibble(mvrnorm(100, c(x1_true, x2_true), m_cov), .name_repair = "universal")
  colnames(s) <- c("x1_response", "x2_response")
  tbl_pt <- tbl_prototypes[tbl_prototypes$category == cat, c("x1_pt", "x2_pt")]
  tbl_pull <- cbind(tbl_pt, s) %>% 
    mutate(
      x1_pull = x1_pt - x1_response, 
      x2_pull = x2_pt - x2_response,
      m = 1 / sqrt(x1_pull^2 + x2_pull^2),
      x1_pull = x1_pull * m * sd_push_center * 5,
      x2_pull = x2_pull * m * sd_push_center * 5
      )
  mu <- tbl_pull[, c("x1_pull", "x2_pull")]
  s_pull <- as.data.frame(t(apply(mu, 1, function(x) mvrnorm(n = 1, mu = x, Sigma = m_cov))))
  s <- s + s_pull

  tibble(x1_true, x2_true, s)
}

tbl_samples <- pmap(
  tbl_gt[, c("x1_true", "x2_true", "cat")], my_mvsample, m_cov = m_cov
  ) %>% reduce(rbind) %>% 
  filter(x1_response > 0 & x2_response > 0) %>%
  filter(x1_response < 10 & x2_response < 10) %>%
  group_by(x1_true, x2_true) %>%
  summarize(x1_response = mean(x1_response), x2_response = mean(x2_response)) %>%
  ungroup() %>%
  mutate(
    participant_id = "4321",
    stim_id = seq_along(x1_true),
    session = 1
  )

tbl_distances <- representational_distances("4321", 1, tbl_samples)
tbl_distances_agg <- tbl_distances %>% group_by(d_euclidean_true) %>%
  summarize(d_euclidean_response = mean(d_euclidean_response))
ggplot(tbl_distances_agg, aes(d_euclidean_true, d_euclidean_response)) +
  geom_point(shape = 1) +
  geom_smooth(method = "lm") +
  geom_abline(intercept = 0, slope = 1) +
  theme_bw() +
  scale_color_brewer(name = "", palette = "Set1") +
  labs(
    x = "Euclidean Distance True",
    y = "Euclidean Distance Response",
    title = "Model Predictions"
  )
