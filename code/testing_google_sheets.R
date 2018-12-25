#########################################
##                                     ##
##  Testing Google Sheets Integration  ##
##                                     ##
#########################################

# Trying to pull the ping pong data and doing the basic processing that will
# need to be done before entering the dashboard

# https://cran.r-project.org/web/packages/googlesheets/readme/README.html

# https://stackoverflow.com/questions/34967837/rank-variable-by-group-dplyr

library(googlesheets)
library(tidyverse)
library(lubridate) # oddly, not loading as part of tidyverse
(my_sheets <- gs_ls())


library(shiny)
library(googlesheets)
library(DT)

# gap_ss <- gs_gap()

# test <- gs_url("https://docs.google.com/spreadsheets/d/1ozaXrwEWN04xiN_c9VO_Zfr_xhMKRtRuYNVxJg_KL6A/edit?usp=sharing")
# scores <- test %>% gs_copy(to = "scores")


# gap <- gs_title("Gapminder")
# oceania <- gap %>%
#   gs_read(ws = "Oceania")
# oceania
# str(oceania)
# glimpse(oceania)


test <- gs_url("https://docs.google.com/spreadsheets/d/1ozaXrwEWN04xiN_c9VO_Zfr_xhMKRtRuYNVxJg_KL6A/edit?usp=sharing") %>% 
  gs_read()

test2 <- test %>%
  # conforming and merging dates
  mutate(Timestamp = lubridate::mdy_hms(Timestamp)) %>% 
  # Making timestamp mdy and making simple_time mdy into a single column
  mutate(date = case_when(!is.na(Timestamp) ~ as_date(Timestamp), 
                          TRUE ~ mdy(simple_time))) %>% 
  # Adding game number based on the timestamp.
  group_by(date) %>% 
  mutate(game_number = case_when(is.na(game_number) ~ rank(Timestamp, ties.method = "first"),
                                 TRUE ~ as.integer(game_number))) %>% 
  # adding winner
  mutate(diff = Andrew - Yuri) %>% 
  # Showing the winner
  mutate(winner = case_when(
    diff > 0 ~ "Andrew",
    TRUE ~ "Yuri"))
  















# test2 %>% 
#   filter(!is.na(Timestamp)) %>% 
#   group_by(date) %>% 
#   mutate(game_number = rank(Timestamp, ties.method = "first"))
# 
# iris %>% arrange(Species, Sepal.Length) %>%
#   group_by(Species) %>% 
#   mutate(rank = rank(Sepal.Length, ties.method = "first"))
#   
# 
# 
# 
# 
# 
# mutate(date = case_when(!is.na(Timestamp) ~ as_date(Timestamp, "%m/%d/%Y"), TRUE ~ simple_time))
# 
# 
# lubridate::parse_date_time(test$Timestamp, 'mdy HMS')
# lubridate::mdy_hms(test$Timestamp) %>% lubridate::as_date()
# 
#   mutate(Timestamp = as.Date(Timestamp, "%m/%d/%Y")) %>% 
#   mutate(simple_time = as.Date(simple_time, "%m/%d/%Y")) %>% 
#   mutate(date = case_when(!is.na(Timestamp) ~ Timestamp, TRUE ~ simple_time))
# 
# 
# as_datetime(test$Timestamp)
