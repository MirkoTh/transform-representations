library(tidyverse)


path_data <- c("experiments/2024-03-psychophysics-stimuli-hcai/data/")
# select the time range for the data to be loaded
time_period <- c(
  make_datetime(2024, 3, 11, 8, tz = "CET"), 
  make_datetime(2024, 3, 11, 11, tz = "CET")
)
add_gender <- FALSE
participants_returned <- c()


