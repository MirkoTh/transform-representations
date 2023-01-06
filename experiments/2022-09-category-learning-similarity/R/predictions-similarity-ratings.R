
tbl_bef <- tibble(
  timepoint = "Before",
  id = 1:32,
  category = rep(c(1, 2), each = 16),
  x1_true = c(rep(1:4, 4), rep(1:4, 4)),
  x2_true = c(rep(1:4, each = 4), rep(5:8, each = 4)),
)

tbl_aft <- tibble(
  timepoint = "After",
  id = 1:32,
  category = rep(c(1, 2), each = 16),
  x1_true = c(rep(seq(1.75, 3.25, by = .5), 4), rep(seq(1.75, 3.25, by = .5), 4)),
  x2_true = c(rep(seq(1.75, 3.25, by = .5), each = 4), rep(seq(5.75, 7.25, by = .5), each = 4))
)
tbl_both <- rbind(tbl_bef, tbl_aft)
ggplot(tbl_aft, aes(x1_true, x2_true, group = category)) +
  geom_point(aes(color = category)) +
  coord_cartesian(xlim = c(1, 8), ylim = c(1, 8))


outer <- function(x11_true, x21_true, tbl_df) {
  pmap(tbl_df[, c("x1_true", "x2_true")], inner, x11_true, x21_true)
}

inner <- function(x1_true, x2_true, x11_true, x21_true) {
  sqrt((x11_true - x1_true)^2 + (x21_true - x2_true)^2)
}

sum_of_distances <- function(tbl_1, tbl_2, cat1, cat2) {
  pmap(
    tbl_1 %>% filter(category %in% cat1) %>% select(x1_true, x2_true) %>%
      rename(x11_true = x1_true, x21_true = x2_true),
    outer,
    tbl_df = tbl_2 %>% filter(category %in% cat2)
  ) %>% unlist() %>% sum()
}

wrap_sum_of_distances <- function(tp, cat1, cat2, tbl_df) {
  tbl_select <- tbl_df  %>% filter(timepoint == tp)
  sum_of_distances(tbl_select, tbl_select,  cat1, cat2)
}
tbl_ds <- tibble(
  comparison = rep(c("Within", "Between"), each = 2),
  timepoint = rep(c("Before", "After"), 2),
  distance = c(
    sum_of_distances(tbl_bef, tbl_bef, 1, 1),
    sum_of_distances(tbl_bef, tbl_aft, 1, 1),
    sum_of_distances(tbl_bef, tbl_bef, 1, 2),
    sum_of_distances(tbl_aft, tbl_aft, 1, 2)
  )
)
ggplot(tbl_ds, aes(timepoint, distance, group = comparison)) +
  geom_line(aes(color = comparison)) +
  geom_point(color = "white", size = 3) +
  geom_point(aes(color = comparison)) +
  theme_bw() +
  scale_fill_viridis_d() +
  coord_cartesian(ylim = c(0, 1500))




tbl_comparisons <- tibble(
  tp = factor(rep(c("Before Training", "After Training"), 8), levels = c("Before Training", "After Training"),ordered = TRUE),
  cat1 = rep(1:4, each = 4),
  cat2 = c(
    1, 1, list(c(2, 3, 4)), list(c(2, 3, 4)),
    2, 2, list(c(1, 3, 4)), list(c(1, 3, 4)),
    3, 3, list(c(1, 2, 4)), list(c(1, 2, 4)),
    4, 4, list(c(1, 2, 3)), list(c(1, 2, 3))
  ),
  comparison = rep(rep(c("Within", "Between"), each = 2), 4)
)

tbl_comparisons$distances_sum <- pmap_dbl(
  tbl_comparisons %>% select(-comparison), 
  wrap_sum_of_distances, 
  tbl_df = my_posterior %>% mutate(timepoint = as.character(timepoint))
)


tbl_comparisons %>% 
  group_by(comparison, tp) %>% 
  summarize(ds_sum = sum(distances_sum)) %>%
  group_by(comparison) %>%
  mutate(ds_prop = ds_sum / lag(ds_sum, 1))
