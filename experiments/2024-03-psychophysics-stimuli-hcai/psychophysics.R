rm(list = ls())

library(tidyverse)
library(MLDS)
library(ids)
library(rutils)


pth_home_grown <- c("R/analysis-utils.R", "R/analysis-plotting.R")
walk(pth_home_grown, source)

path_data <- c("experiments/2024-03-psychophysics-stimuli-hcai/data/")
# select the time range for the data to be loaded
time_period <- c(
  make_datetime(2024, 3, 13, 8, tz = "CET"), 
  make_datetime(2024, 3, 13, 12, tz = "CET")
)
add_gender <- FALSE
participants_returned <- c()
random_hashes <- FALSE
load_and_hash_triplets(
  path_data,
  participants_returned,
  add_gender,
  time_period,
  random_hashes = FALSE
)
tbl_triplets <- readRDS("experiments/2024-03-psychophysics-stimuli-hcai/data/tbl_similarity.rds")
tbl_lookup <- read_csv("experiments/2024-03-psychophysics-stimuli-hcai/data/participant-lookup.csv")

tbl_triplets <- tbl_triplets %>% group_by(is_practice, trial_id) %>%
  mutate(rwn = row_number(participant_id)) %>% ungroup()

l_tbl_triplets <- split(tbl_triplets, tbl_triplets$participant_id)


mlds_two_dims <- function(my_tbl, p_id) {
  perceptual_head <- mlds(
    my_tbl %>%
      filter(dimension == "head") %>% 
      select(response, id_left, id_target, id_right) %>% 
      rename(resp = response, S1 = id_left, S2 = id_target, S3 = id_right) %>%
      mutate(S1 = S1 + 1, S2 = S2 + 1, S3 = S3 + 1, resp = abs(resp - 1))
  )
  perceptual_belly <- mlds(
    my_tbl %>%
      filter(dimension == "belly") %>% 
      select(response, id_left, id_target, id_right) %>% 
      rename(resp = response, S1 = id_left, S2 = id_target, S3 = id_right) %>%
      mutate(S1 = S1 + 1, S2 = S2 + 1, S3 = S3 + 1, resp = abs(resp - 1))
  )
  tbl_perceptual <- tibble(
    participant_id = p_id,
    stimulus = c(perceptual_head$stimulus, perceptual_belly$stimulus),
    v_perceptual = c(perceptual_head$pscale, perceptual_belly$pscale),
    dimension = rep(c("Head", "Belly"), each = length(perceptual_head$stimulus))
  )
  
  return(tbl_perceptual)
  
}

l_mlds <- map2(l_tbl_triplets, 1:length(l_tbl_triplets), mlds_two_dims)

tbl_mlds <- reduce(l_mlds, rbind)




ggplot(tbl_mlds, aes(stimulus, v_perceptual, group = dimension)) +
  geom_abline() +
  geom_point(aes(color= dimension)) +
  geom_smooth(method = "lm", aes(color = dimension)) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Stimulus Value", y = "Perceptual Value") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22)
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "") +
  facet_wrap(~ participant_id)







