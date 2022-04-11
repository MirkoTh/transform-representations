library(tidyverse)

file_path_paid <- "experiments/2022-02-category-learning/data/2022-03-30-pilot-1/bonus-paid.csv"
file_path_pending <- "experiments/2022-02-category-learning/data/2022-03-30-pilot-1/bonus-pending.csv"
file_path_new <- "experiments/2022-02-category-learning/data/2022-03-30-pilot-1/bonus.json"

tbl_bonus_paid <- read.csv(file = file_path_paid) %>% as_tibble()

js_bonus <- read_file(file_path_new)
js_bonus <-str_c("[", str_replace_all(js_bonus, "\\}", "\\},"), "]")
js_bonus <- str_replace(js_bonus, ",\n]", "]")
tbl_bonus_all <- jsonlite::fromJSON(js_bonus) %>% as_tibble()

rejections <- c("6167e763015c13b2b087b9df", "603f6e643234e512fc197ae1")


tbl_bonus_not_paid <- tbl_bonus_all %>% 
  left_join(tbl_bonus_paid, by = "participant_id", suffix = c("_all", "_paid")) %>%
  filter(is.na(bonus_total_paid)) %>%
  select(-bonus_total_paid) %>%
  rename(bonus_total = bonus_total_all)

tbl_bonus_not_paid <- tbl_bonus_not_paid %>% filter(!(participant_id %in% rejections))

tbl_bonus_not_paid %>% select(participant_id, bonus_total) %>% write.table(file = file_path_paid, quote = FALSE, sep = ", ", col.names = FALSE, row.names = FALSE, append = TRUE)
tbl_bonus_not_paid %>% select(participant_id, bonus_total) %>% write.table(file = file_path_pending, quote = FALSE, sep = ", ", row.names = FALSE)
