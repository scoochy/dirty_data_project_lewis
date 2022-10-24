library(tidyverse)
library(tidyr)
library(readr)

raw_candy2015 <- readxl::read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
raw_candy2016 <- readxl::read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
raw_candy2017 <- readxl::read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))

all_raw_candy <- bind_rows(raw_candy2015, raw_candy2016, raw_candy2017, .id = "id")


candy_new_names <- all_raw_candy %>% 
  unite(age, 
        c(`How old are you?`, `Q3: AGE`), na.rm = TRUE) %>% 
  unite(trick_or_treating, 
        c(`Are you going actually going trick or treating yourself?`, `Q1: GOING OUT?`),
        na.rm = TRUE) %>% 
  unite(country, 
        c(`Which country do you live in?`, `Q4: COUNTRY`), na.rm = TRUE) %>% 
  unite(gender, 
        c(`Your gender:`, `Q2: GENDER`), na.rm = TRUE) %>% 
  mutate(year = case_when(id == 1 ~ 2015,
                          id == 2 ~ 2016,
                          id == 3 ~ 2017)) 
        

  
                    