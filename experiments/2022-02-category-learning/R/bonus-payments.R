library(tidyverse)

file_path_paid <- "experiments/2022-02-category-learning/data/2022-04-21-experiment/bonus-paid.csv"
file_path_pending <- "experiments/2022-02-category-learning/data/2022-04-21-experiment/bonus-pending.csv"
file_path_new <- "experiments/2022-02-category-learning/data/2022-04-21-experiment/bonus.json"

tbl_bonus_paid <- read.csv(file = file_path_paid) %>% as_tibble()

if (!exists("tbl_bonus_paid")) {
  tbl_bonus_paid <- tibble(participant_id = "0", bonus_total_paid = 0)
}

js_bonus <- read_file(file_path_new)
js_bonus <-str_c("[", str_replace_all(js_bonus, "\\}", "\\},"), "]")
js_bonus <- str_replace(js_bonus, ",\n]", "]")
tbl_bonus_all <- jsonlite::fromJSON(js_bonus) %>% as_tibble()

returned_timeout <- c(
  "61274c5d48457b27bb106961",
  "60c5a05a8015a0a73d03a99d",
  "5fc8aaa41a1f842422484db2",
  "616823f498b414afa4fb4c71",
  "5ff5a2e6a89e1b02557a29bf",
  "615164a489588e22d2bd33df",
  "5fac647436421623d15dfcf5",
  "616f94cd9cd003d37c9db38c",
  "6139f08d5ad17f719138935f",
  "6166eb40f3fdb397f7333849",
  "5ee35bd68326e102273173ee",
  "5f8c0f0018ada633f6d834e9",
  "616e7879455c454872403556",
  "61315b1ab3c739cc59d4840b",
  "61029c7b9333b958acfc91a3",
  "5e9ec2b859d135000a358a13",
  "5df50a49a98a7a3924e4137e"
)



tbl_bonus_not_paid <- tbl_bonus_all %>% 
  left_join(tbl_bonus_paid, by = "participant_id", suffix = c("_all", "_paid")) %>%
  filter(is.na(bonus_total_paid)) %>%
  select(-bonus_total_paid) 
# %>%  rename(bonus_total = bonus_total_all)

tbl_bonus_not_paid <- tbl_bonus_not_paid %>% filter(!(participant_id %in% returned_timeout))

tbl_bonus_not_paid %>% select(participant_id, bonus_total) %>% write.table(file = file_path_paid, quote = FALSE, sep = ", ", col.names = FALSE, row.names = FALSE, append = TRUE)
tbl_bonus_not_paid %>% select(participant_id, bonus_total) %>% write.table(file = file_path_pending, quote = FALSE, sep = ", ", row.names = FALSE)
