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
  make_datetime(2024, 3, 14, 20, tz = "CET")
)
add_gender <- FALSE
participants_returned <- c(
  "613867f34e206e4f573bc6ef", "5fb14a073d81805bc65f6a4f", 
  "64f3ad25c93a6f6d60e6acd8", "615f2d67574095fe86bfe619",
  "5df1a3387caa1e0c69dca179"
)
# do not exclude the following id
returned_but_finished <- c("584823aed2be990001174e56")
random_hashes <- FALSE
load_and_hash_triplets(
  path_data,
  participants_returned,
  add_gender,
  time_period,
  random_hashes = random_hashes
)
tbl_triplets <- readRDS("experiments/2024-03-psychophysics-stimuli-hcai/data/tbl_similarity.rds") %>%
  mutate(participant_id = as.numeric(as.character(participant_id)))
tbl_lookup <- read_csv("experiments/2024-03-psychophysics-stimuli-hcai/data/participant-lookup.csv")

tbl_triplets <- tbl_triplets %>% group_by(is_practice, trial_id) %>%
  mutate(rwn = row_number(participant_id)) %>% ungroup()


tbl_triplets <- tbl_triplets %>%
  mutate(
    diff_l = abs(id_target - id_left),
    diff_r = abs(id_target - id_right),
    diff_diff = diff_l - diff_r
  )

tbl_triplets$dimension <- factor(tbl_triplets$dimension, labels = c("Belly", "Head"))

tbl_exclude <- tbl_triplets %>%
  left_join(tbl_lookup, by = c("participant_id" = "participant_id_randomized"), suffix = c("_hashed", "_prolific")) %>%
  group_by(participant_id, participant_id_prolific, name) %>%
  summarize(
    mean_response = mean(response),
    n = n()
  ) %>% arrange(mean_response) %>%
  ungroup() %>%
  mutate(exclude = n < .8*374) %>% #round()) %>%
  filter(n > 5)

tbl_triplets %>% 
  left_join(
    tbl_exclude, by = "participant_id"
  ) %>% filter(!exclude) %>%
  mutate(
    diff_diff_cut = cut(diff_diff, c(-20, seq(-9, 9, by = 3), 20), labels = FALSE)
  ) %>%
  group_by(participant_id, dimension, diff_diff_cut) %>%
  summarize(mn_response = mean(response), n = n()) %>%
  ggplot(aes(diff_diff_cut, mn_response, group = participant_id)) +
  geom_line(aes(color = participant_id)) +
  facet_wrap(~ dimension) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Difference of Differences", y = "Prop. Choices") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22)
  ) + 
  scale_color_viridis_c(name = "Participant ID")

tbl_triplets_include <- tbl_triplets %>% 
  left_join(tbl_exclude, by = "participant_id") %>% filter(!exclude) %>%
  mutate(participant_id = factor(participant_id))

l_tbl_triplets <- split(tbl_triplets_include, tbl_triplets_include$participant_id)

l_glms <- map(l_tbl_triplets, ~ glm(response ~ diff_diff, data = .x %>% filter(dimension == "Head")))
coefs_head_sorted <- sort(map_dbl(map(l_glms, "coefficients"), 2))

mlds_two_dims <- function(my_tbl, p_id) {
  perceptual_head <- mlds(
    my_tbl %>%
      filter(dimension == "Head") %>% 
      select(response, id_left, id_target, id_right) %>% 
      rename(resp = response, S1 = id_left, S2 = id_target, S3 = id_right) %>%
      mutate(S1 = S1 + 1, S2 = S2 + 1, S3 = S3 + 1, resp = abs(resp - 1))
  )
  perceptual_belly <- mlds(
    my_tbl %>%
      filter(dimension == "Belly") %>% 
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

l_mlds <- map2(l_tbl_triplets, names(l_tbl_triplets), mlds_two_dims)

tbl_mlds <- reduce(l_mlds, rbind) %>%
  mutate(participant_id = factor(participant_id, levels = names(coefs_head_sorted)))



l_lms_head <- map(l_mlds, ~ summary(lm(v_perceptual ~ stimulus, data = .x %>% filter(dimension == "Head"))))
tbl_lm_head <- reduce(map(l_lms_head, ~ .x$coefficients[2, c(1, 4)]), rbind) %>%
  as.data.frame() %>% as_tibble() %>%
  mutate(participant_id = names(l_lms_head)) %>%
  arrange(Estimate) %>%
  relocate(participant_id, .before = Estimate)
colnames(tbl_lm_head) <- c("participant_id", "b_head", "p_val_head")
tbl_lm_head$exclude_head <- tbl_lm_head$p_val_head > .05 | tbl_lm_head$b_head <= 0



l_lms_belly <- map(l_mlds, ~ summary(lm(v_perceptual ~ stimulus, data = .x %>% filter(dimension == "Belly"))))
tbl_lm_belly <- reduce(map(l_lms_belly, ~ .x$coefficients[2, c(1, 4)]), rbind) %>%
  as.data.frame() %>% as_tibble() %>%
  mutate(participant_id = names(l_lms_belly)) %>%
  arrange(Estimate) %>%
  relocate(participant_id, .before = Estimate)
colnames(tbl_lm_belly) <- c("participant_id", "b_belly", "p_val_belly")
tbl_lm_belly$exclude_belly <- tbl_lm_belly$p_val_belly > .05 | tbl_lm_belly$b_belly <= 0

tbl_perceptual <- tbl_lm_head %>% left_join(tbl_lm_belly, by = "participant_id") %>%
  mutate(exclude = exclude_head | exclude_belly)

tbl_perceptual %>%
  filter(!exclude) %>%
  summarize(B_head = mean(b_head), B_belly = mean(b_belly))

tbl_perceptual_indiv <- tbl_mlds %>% 
  left_join(tbl_perceptual, by = "participant_id") %>%
  filter(!exclude)
tbl_perceptual_indiv$participant_id <- factor(
  tbl_perceptual_indiv$participant_id,
  labels = c(
    1:length(unique(tbl_perceptual_indiv$participant_id))
  ))
tbl_perceptual_indiv$participant_id <- as.character(tbl_perceptual_indiv$participant_id)

tbl_perceptual_avg <- summary_se_within(
  tbl_perceptual_indiv, 
  "v_perceptual", 
  withinvars = c("dimension", "stimulus")
) %>% mutate(participant_id = "Mean")

pl_perceptual_indiv <- ggplot(
  tbl_perceptual_indiv %>% 
    select(stimulus, v_perceptual, participant_id, dimension) %>%
    rbind(
      tbl_perceptual_avg %>%
        select(stimulus, v_perceptual, participant_id, dimension)
    ), 
  aes(as.numeric(stimulus), v_perceptual, group = dimension)) +
  geom_line(aes(color = dimension)) +
  geom_point(color = "white", size = 2) +
  geom_point(aes(color = dimension), shape = 1, size = 1) +
  facet_wrap(~ participant_id, ncol = 6) +
  theme_bw() +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Stimulus", y = "Perceptual Value") + 
  theme(
    strip.background = element_rect(fill = "white"),
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_manual(
    values = c("skyblue2", "tomato4"), name = "Dimension"
  )

save_my_pdf_and_tiff(
  pl_perceptual_indiv, 
  "experiments/2024-03-psychophysics-stimuli-hcai/figures/stimulus-vs-perception",
  12, 17
  )


# average representation
pd <- position_dodge(width = .4)
ggplot(tbl_perceptual_avg, aes(stimulus, v_perceptual, group = dimension)) +
  geom_line(aes(color = dimension), position = pd) +
  geom_errorbar(aes(ymin = v_perceptual - ci, ymax = v_perceptual + ci, color = dimension), width = .4, position = pd) +
  geom_point(color = "white", size = 3, position = pd) +
  geom_point(aes(color = dimension), position = pd) +
  theme_bw() +
  scale_x_discrete(expand = c(0.01, 0)) +
  scale_y_continuous(expand = c(0.01, 0)) +
  labs(x = "Stimulus", y = "Perceptual Value") + 
  theme(
    strip.background = element_rect(fill = "white"), 
    text = element_text(size = 22),
    legend.position = "bottom"
  ) + 
  scale_color_manual(values = c("skyblue2", "tomato4"), name = "Dimension")

# by-participant representation
ggplot(tbl_mlds, aes(stimulus, v_perceptual, group = dimension)) +
  geom_abline(color = "forestgreen", linetype = "dotdash", linewidth = 1) +
  geom_hline(yintercept = 0, color = "red", linetype = "dotdash", linewidth = 1) +
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




# map object space to subject space ---------------------------------------


tbl_presented <- read_csv(
  file = "data/values-objective-stimuli-presented.csv"
) %>% rename(x1 = x1_true, x2 = x2_true)
x1_unique <- sort(unique(tbl_presented$x1))
x2_unique <- sort(unique(tbl_presented$x2))

steps_pilot <- seq(11, 89, by = 6)
tbl_perceptual_avg <- tbl_perceptual_avg %>% arrange(stimulus)
x1_pilot <- tbl_perceptual_avg$v_perceptual[tbl_perceptual_avg$dimension == "Head"]
x2_pilot <- tbl_perceptual_avg$v_perceptual[tbl_perceptual_avg$dimension == "Belly"]

# linearly interpolate values used in E1 - E4
x1_psych_e1234 <- signal::interp1(
  x = steps_pilot, y = x1_pilot, 
  xi = x1_unique, method=c('linear'), extrap=T
)

x2_psych_e1234 <- signal::interp1(
  x = steps_pilot, y = x2_pilot, 
  xi = x2_unique, method=c('linear'), extrap=T
)
x1_critical <- signal::interp1(
  x = steps_pilot, y = x1_pilot,
  xi = c(0, 50, 100), method = "linear", extrap = TRUE
)
x2_critical <- signal::interp1(
  x = steps_pilot, y = x2_pilot,
  xi = c(0, 50, 100), method = "linear", extrap = TRUE
)

# here, x1 refers the head spikiness
# and x2 refers to the belly fill

tbl_psych <- crossing(
  x1_psych = x1_psych_e1234,
  x2_psych = x2_psych_e1234
)
tbl_psych <- tbl_psych %>%
  mutate(
    x1_obj = rep(x1_unique, each = length(x1_psych_e1234)),
    x2_obj = rep(x2_unique, length(x2_psych_e1234))
  )
saveRDS(tbl_psych, file = "data/psych-representations.rds")

