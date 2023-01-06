library(tidyverse)


file_path_paid <- "experiments/2022-07-category-learning-II/data/2022-09-02-treps2-experiment/bonus-paid.csv"
file_path_pending <- "experiments/2022-07-category-learning-II/data/2022-09-02-treps2-experiment/bonus-pending.csv"
file_path_new <- "experiments/2022-07-category-learning-II/data/2022-09-02-treps2-experiment/bonus.json"


tbl_bonus_paid <- read.csv(file = file_path_paid) %>% as_tibble()

if (!exists("tbl_bonus_paid")) {
  tbl_bonus_paid <- tibble(participant_id = "0", bonus_total_paid = 0)
}

js_bonus <- read_file(file_path_new)
js_bonus <-str_c("[", str_replace_all(js_bonus, "\\}", "\\},"), "]")
js_bonus <- str_replace(js_bonus, ",\n]", "]")
tbl_bonus_all <- jsonlite::fromJSON(js_bonus) %>% as_tibble()

returned_timeout <- c(
  "61717173748006894b2b54ff",
  "601129f77e0c21000b0c408a",
  "616e5ae706e970fe0aff99b6",
  "5e5d66f2101bf703d65326bc",
  "5f338ba6ea047119dbd6e49e",
  "5ee7b7c9eef92207297a0ad4",
  "60eabe920b976e0972bfa41d",
  "5e11b252deea2b84136a5d21",
  "5eb6f85c1c54b4067b4ba65d",
  "60fdcd33665754977f930324",
  "5c7341f83a67ad00016ec50b",
  "6130e97d4106299f8c6120fa",
  "61372118b7dde713e24191e0",
  "61645364aa6fda7444570fe0"
)



tbl_bonus_not_paid <- tbl_bonus_all %>% 
  left_join(tbl_bonus_paid, by = "participant_id", suffix = c("_all", "_paid")) %>%
  filter(is.na(bonus_total_paid)) %>%
  select(-bonus_total_paid) 
# %>%  rename(bonus_total = bonus_total_all)

tbl_bonus_not_paid <- tbl_bonus_not_paid %>% filter(!(participant_id %in% returned_timeout))

tbl_bonus_not_paid %>% select(participant_id, bonus_total) %>% write.table(file = file_path_paid, quote = FALSE, sep = ", ", col.names = FALSE, row.names = FALSE, append = TRUE)
tbl_bonus_not_paid %>% select(participant_id, bonus_total) %>% write.table(file = file_path_pending, quote = FALSE, sep = ", ", row.names = FALSE)
