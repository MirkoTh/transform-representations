library(tidyverse)

# compute pairwise distances between all 100 stimuli for E3 aka similarity

x1 <- (seq(0, 9, by = 1) + 1) * 9 + 1
x2 <- x1

tbl_x <- crossing(x1, x2)
tbl_x$category <- 1
tbl_x$category[tbl_x$x2 > 50] <- 2
tbl_x$category[tbl_x$x1 > 50] <- 3
tbl_x$category[tbl_x$x2 > 50 & tbl_x$x1 > 50] <- 4
tbl_x$stim_id <- seq(1, nrow(tbl_x), by = 1)
tbl_x$x1x2 <- str_c("[", tbl_x$x1, ",", tbl_x$x2, "]")

#tbl_x$category <- fct_inseq(factor(tbl_x$category))
ggplot(tbl_x, aes(x1, x2, color = as.factor(category))) +
  geom_point()

tbl_x2 <- tbl_x %>% 
  rename(x12 = x1, x22 = x2, category2 = category, stim_id2 = stim_id, x1x22 = x1x2)
tbl_pairwise <- crossing(tbl_x, tbl_x2)

tbl_pairwise <- tbl_pairwise %>%
  group_by(stim_id, stim_id2) %>%
  mutate(
    d_euclidean = sqrt((x1 - x12)^2 + (x2 - x22)^2),
    category_min = min(category, category2),
    category_max = max(category, category2),
    pool_comparison = interaction(category_min, category_max),
    stim_min = min(stim_id, stim_id2),
    stim_max = max(stim_id, stim_id2),
    stim_comparison = interaction(stim_min, stim_max)
    ) %>% group_by(stim_comparison) %>%
  mutate(rwn = row_number(stim_id)) %>%
  ungroup()


# drop comparisons with same stimulus
tbl_pairwise <- tbl_pairwise[tbl_pairwise$stim_id != tbl_pairwise$stim_id2, ]
# drop duplicate comparisons (i.e., upper triangle)
tbl_pairwise <- tbl_pairwise[tbl_pairwise$rwn == 1, ]
tbl_pairwise <- tbl_pairwise %>%
  mutate(
    d_euclidean_cut = cut(d_euclidean, c(0, 30, 50, 70, 90, 200)),
    d_euclidean_cut_int = cut(d_euclidean, c(0, 30, 50, 70, 90, 200), labels = FALSE),
    d_euclidean_round = round(d_euclidean, 1)
    )


ggplot(tbl_pairwise, aes(d_euclidean, group = d_euclidean_cut)) +
  geom_histogram(binwidth = 5, color = "white", aes(fill = d_euclidean_cut)) +
  facet_wrap(~ pool_comparison, ncol = 5) +
  scale_fill_brewer(palette = "Set1", name = "Distance Binned") +
  theme_bw() +
  labs(
    x = "Euclidean Distance", y = "Nr. Comparisons"
  )

# we cannot sample randomly within the comparison pools because then
# short distances between stimuli would be sampled too often when
# comparison stimuli from the same comparison pool
# that would likely result in a floor effect

# suggestion:
# same comparison pool
# 50% from bin2 & 50% from bin1
# different comparison pool
# 50% from bin2 & 20% from bin1 & 20% from bin3 & 10% from bin4

# bin5 is excluded completely due to ceiling effect


tbl_pairwise$stim_ids <- str_c("[", tbl_pairwise$stim_id, ",", tbl_pairwise$stim_id2, "]")
l_stim_pools <- split(tbl_pairwise[, c("stim_ids")], tbl_pairwise$pool_comparison)
l_distance_pools <- split(tbl_pairwise[, c("d_euclidean_round")], tbl_pairwise$pool_comparison)

l_f_names_stim <- str_c("experiments/2022-09-category-learning-similarity/stimuli/stimids-category-pool-", names(l_stim_pools), ".txt")
l_f_names_ds <- str_c("experiments/2022-09-category-learning-similarity/stimuli/distances-category-pool-", names(l_stim_pools), ".txt")

walk2(l_stim_pools, l_f_names_stim, ~ write_file(str_c(..1 %>% as_vector(), collapse = ", "), ..2))
walk2(l_distance_pools, l_f_names_ds, ~ write_file(str_c(..1 %>% as_vector(), collapse = ", "), ..2))
write_file(
  str_c(tbl_x[, c("x1x2")] %>% as_vector(), collapse = ", "), 
  file = "experiments/2022-09-category-learning-similarity/stimuli/stim_ids.txt"
  )

# calculate proportions of distances for each of the bins

tbl_counts <- tbl_pairwise %>% 
  group_by(pool_comparison, d_euclidean_cut) %>%
  count() %>%
  group_by(pool_comparison) %>%
  mutate(
    prop_pool = round(n / sum(n), 2)
  ) %>% ungroup() %>%
  select(-n) %>%
  pivot_wider(
    names_from = d_euclidean_cut, values_from = prop_pool, values_fill = 0
    )



