library(tidyverse)
library(tidyr)
library(readr)

raw_candy2015 <- readxl::read_xlsx(here::here("raw_data/boing-boing-candy-2015.xlsx"))
raw_candy2016 <- readxl::read_xlsx(here::here("raw_data/boing-boing-candy-2016.xlsx"))
raw_candy2017 <- readxl::read_xlsx(here::here("raw_data/boing-boing-candy-2017.xlsx"))



all_raw_candy <- bind_rows(raw_candy2015, raw_candy2016, raw_candy2017, 
                           .id = "id")

clean_2015 <- janitor::clean_names(raw_candy2015)
clean_2016 <- janitor::clean_names(raw_candy2016)
clean_2017 <- janitor::clean_names(raw_candy2017)

#remove q6_ from 2017 data candy Q's

clean_2017 <- clean_2017 %>% 
  rename_with(~str_replace(.x, "q6_", ""),  .cols = 7:109)

#clean names again to have same format (no x infront of numbers)

clean_2017 <- janitor::clean_names(clean_2017)


#join 3 datasets together

all_clean_candy <- bind_rows(clean_2015, clean_2016, clean_2017, 
                           .id = "id")


#unite columns with same data but different names
#add a year column based on dataset year

candy_new_names <- all_clean_candy %>%
  unite(sweetums,
        c(sweetums, sweetums_a_friend_to_diabetes), na.rm = TRUE) %>% 
  unite(licorice_black,
        c(licorice, licorice_yes_black), na.rm = TRUE) %>% 
  unite(bonkers,
        c(bonkers_the_candy, bonkers), na.rm = TRUE) %>% 
  unite(dark_hershey,
        c(dark_chocolate_hershey, hersheys_dark_chocolate), na.rm = TRUE) %>% 
  unite(hersheys_kisses, 
        c(hersheys_kisses, hershey_s_kissables), na.rm = TRUE) %>% 
  unite(box_o_raisins,
        c(box_o_raisins, boxo_raisins), na.rm = TRUE) %>% 
  unite(mary_janes,
        c(mary_janes,
          anonymous_brown_globs_that_come_in_black_and_orange_wrappers_a_k_a_mary_janes),
        na.rm = TRUE) %>% 
  unite(age, 
        c(how_old_are_you, `q3_age`), na.rm = TRUE) %>% 
  unite(trick_or_treating, 
        c(are_you_going_actually_going_trick_or_treating_yourself, `q1_going_out`),
        na.rm = TRUE) %>% 
  unite(country, 
        c(which_country_do_you_live_in, `q4_country`), na.rm = TRUE) %>% 
  unite(gender, 
        c(your_gender, `q2_gender`), na.rm = TRUE) %>% 
  mutate(year = case_when(id == 1 ~ 2015,
                          id == 2 ~ 2016,
                          id == 3 ~ 2017)) %>% 
  #making blank cells NA (didn't like timestamp)
  mutate(across(.cols = -timestamp, ~ na_if(.x, ""))) %>% 
  mutate(country = tolower(country),
         country = if_else(str_detect(country, "[0-9]"), NA_character_, country),
         country = if_else(str_detect(country, "usa"), "us", country),
         country = if_else(str_detect(country, "state"), "us", country),
         country = if_else(str_detect(country, "england"), "uk", country),
         #it pains me to write this...
         country = if_else(str_detect(country, "scotland"), "uk", country),
         country = if_else(str_detect(country, "merica"), "us", country),
         country = if_else(str_detect(country, "amerca"), "us", country),
         country = if_else(str_detect(country, "alaska"), "us", country),
         country = if_else(str_detect(country, "equator"), NA_character_, country),
         country = if_else(str_detect(country, "atlantis"), NA_character_, country),
         country = if_else(str_detect(country, "denial"), NA_character_, country),
         country = if_else(str_detect(country, "earth"), NA_character_, country),
         country = if_else(str_detect(country, "fear"), NA_character_, country),
         country = if_else(str_detect(country, "lately"), NA_character_, country),
         country = if_else(str_detect(country, "anymore"), NA_character_, country),
         country = if_else(str_detect(country, "soviet"), NA_character_, country),
         country = if_else(str_detect(country, "mur"), "us", country),
         country = if_else(str_detect(country, "cali"), "us", country),
         country = if_else(str_detect(country, "can"), "canada", country),
         country = if_else(str_detect(country, "god"), NA_character_, country),
         country = if_else(str_detect(country, "casc"), NA_character_, country),
         country = if_else(str_detect(country, "end"), "uk", country),
         country = if_else(str_detect(country, "espa"), "spain", country),
         country = if_else(str_detect(country, "eua"), NA_character_, country),
         country = if_else(str_detect(country, "narn"), NA_character_, country),
         country = if_else(str_detect(country, "neve"), NA_character_, country),
         country = if_else(str_detect(country, "jers"), "us", country),
         country = if_else(str_detect(country, "york"), "us", country),
         country = if_else(str_detect(country, "best"), NA_character_, country),
         country = if_else(str_detect(country, "carolina"), "us", country),
         country = if_else(str_detect(country, "pitt"), "us", country),
         country = if_else(str_detect(country, "above"), NA_character_, country),
         country = if_else(str_detect(country, "somewhere"), NA_character_, country),
         country = if_else(str_detect(country, "netherlands"), "netherlands", country),
         country = if_else(str_detect(country, "yoo"), "us", country),
         country = if_else(str_detect(country, "there"), NA_character_, country),
         country = if_else(str_detect(country, "this"), NA_character_, country),
         country = if_else(str_detect(country, "trump"), "us", country),
         country = if_else(str_detect(country, "u[:punct:]s"), "us", country),
         country = if_else(str_detect(country, "u[:space:]s"), "us", country),
         country = if_else(str_detect(country, "u[:punct:]k"), "uk", country),
         country = if_else(str_detect(country, "ud"), "us", country),
         country = if_else(str_detect(country, "united[:space:]k"), "uk", country),
         country = if_else(str_detect(country, "united"), "us", country),
         country = if_else(str_detect(country, "us"), "us", country),
         country = na_if(country, "a")) %>% 
  mutate(age = if_else(str_detect(age, "[:alpha:]"), NA_character_, age),
         age = as.numeric(age)) %>% 
  #apply sensible range to ages 
  filter(age >= 4 & age <= 90)
  
#performed a group by (country) and summarise 
#periodically to find occurrences and no. of each and check remove was accurate
#candy_new_namez <-  candy_new_names %>% 
#    group_by(country) %>% 
#    summarise(n())

#candy_new_namez <- candy_new_names %>% 
#  group_by(age) %>% 
#  summarise(n())

#candy_new_namez <- candy_new_names %>% 
#  group_by(trick_or_treating) %>% 
#  summarise(n())

#candy_new_namez <- candy_new_names %>% 
#  group_by(age) %>% 
#  summarise(n())

#candy_new_namez <- candy_new_names %>% 
#  group_by(gender) %>% 
#  summarise(n())


#ordered alphabetically to make things easier
candy_new_namez <-candy_new_names[,order(colnames(candy_new_names))]
#removed anything under the assumption it would be unlikely to receive while trick
#or treating still kept things not neccessarily "candy"
candy_new_names <- candy_new_names %>% 
  select(-c(abstained_from_m_ming,
            anonymous_brown_globs_that_come_in_black_and_orange_wrappers,
            betty_or_veronica, bonkers_the_board_game,
            broken_glow_stick, cash_or_other_forms_of_legal_tender,
            chardonnay, check_all_that_apply_i_cried_tears_of_sadness_at_the_end_of,
            click_coordinates_x_y, creepy_religious_comics_chick_tracts,
            dental_paraphenalia,
            do_you_eat_apples_the_correct_way_east_to_west_side_to_side_or_do_you_eat_them_like_a_freak_of_nature_south_to_north_bottom_to_top,
            fill_in_the_blank_taylor_swift_is_a_force_for,
            fill_in_the_blank_imitation_is_a_form_of,
            generic_brand_acetaminophen, guess_the_number_of_mints_in_my_hand,
            hugs_actual_physical_hugs, if_you_squint_really_hard_the_words_intelligent_design_would_look_like,
            joy_joy_mit_iodine, kale_smoothie, lapel_pins, peanut_butter_jars,
            person_of_interest_season_3_dvd_box_set_not_including_disc_4_with_hilarious_outtakes,
            peterson_brand_sidewalk_chalk, 
            please_leave_any_remarks_or_comments_regarding_your_choices,
            please_list_any_items_not_included_above_that_give_you_joy,
            please_list_any_items_not_included_above_that_give_you_despair,
            please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jk_rowling,
            please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_jj_abrams,
            please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_beyonce,
            please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_francis_bacon_1561_1626,
            please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_bieber,
            please_estimate_the_degree_s_of_separation_you_have_from_the_following_celebrities_kevin_bacon,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_jk_rowling,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_bruce_lee,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_malala_yousafzai,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_thom_yorke,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_beyonce_knowles,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_jj_abrams,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_hillary_clinton,
            please_estimate_the_degrees_of_separation_you_have_from_the_following_folks_donald_trump,
            please_leave_any_witty_snarky_or_thoughtful_remarks_or_comments_regarding_your_choices,
            q10_dress, q11_day, q12_media_daily_dish, q12_media_science, q12_media_espn, q12_media_yahoo,
            q5_state_province_county_etc, q7_joy_other, q8_despair_other, q9_other_comments,
            real_housewives_of_orange_county_season_9_blue_ray,
            that_dress_that_went_viral_early_this_year_when_i_first_saw_it_it_was,
            that_dress_that_went_viral_a_few_years_back_when_i_first_saw_it_it_was,
            timestamp, vicodin, what_is_your_favourite_font, when_you_see_the_above_image_of_the_4_different_websites_which_one_would_you_most_likely_check_out_please_be_honest,
            which_day_do_you_prefer_friday_or_sunday, which_state_province_county_do_you_live_in,
            white_bread, whole_wheat_anything, x114, york_peppermint_patties_ignore))


#candy_check <-candy_new_names %>% 
#  summarise(across
#            (.cols = 
#                     -c(id, age, trick_or_treating, country, gender, internal_id, year), ~ !.x %in% c(NA, "MEH", "JOY", "DESPAIR")))


#pivoted longer so each row was 1 observation
candy_tidy <- candy_new_names %>% 
  pivot_longer(cols = 
                 -c(id, age, trick_or_treating, country, gender, year),
               names_to = "candy",
               values_to = "feeling")
  
write_csv(candy_tidy, "clean_data/clean_data.csv")


