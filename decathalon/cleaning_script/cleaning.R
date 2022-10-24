library(tidyverse)
library(readr)
library(here)
library(janitor)
library(tidyr)

raw_decathalon_data <- read_rds(here::here("raw_data/decathlon.rds"))

decathalon_data <- clean_names(raw_decathalon_data)

decathalon_data <- rename(decathalon_data,
                          "javelin" = "javeline",
                          "100m" = "x100m",
                          "400m" = "x400m",
                          "1500m" = "x1500m",
                          "110m_hurdle" = "x110m_hurdle")

decathalon_data <-  tibble::rownames_to_column(decathalon_data, "competitor")

decathalon_data <- pivot_longer(
    decathalon_data,
    cols = -c("competitor", "rank", "points", "competition"),
    names_to = "event",
    values_to = "score")

decathalon_data <-  decathalon_data %>% 
  mutate(competitor = tolower(competitor))


write_csv(decathalon_data, file = "clean_data/decathalon_clean.csv")
