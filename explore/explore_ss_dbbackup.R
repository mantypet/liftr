# read and clean Starting Strenth App dbBackup.csv
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)
library(purrr)
library(slider)

dbbackup <- read.csv(here::here("data/dbBackup_20230116.csv"))

#' Lift analysis data
#' 
#' One row represents one attempted set on a specific lift with available auxiliary information
#' 
#' NOTE: bug in chinup reps
#' 
#' @param date
#' @param bodyweight
#' @param lift
#' @param weight
#' @param set
#' @param reps
#' 
#' @examples
#' dbbackup <- read.csv(here::here("data/dbBackup_20230116.csv"))
#' lifts_prefix <- c("Squat", "Light.Squat", "Press", "Bench", "Deadlift", "Power.Clean", "Chinup", "Back.Ext")
#' harmonize_lift(dbbackup, lift_prefix = lifts_prefix[1])

harmonize_lift <- function(dbbackup, lift_prefix) {
  reps_col <- paste(lift_prefix, "Reps", sep = ".")
  sets_col <- paste(lift_prefix, "Sets.Completed", sep = ".")
  weight_col <- paste(lift_prefix, "Weight", sep = ".")
  
  lift_reporting <- tolower(str_remove(lift_prefix, "\\."))
  
  dbbackup %>%
    mutate(date = as.Date(Date),
           bodyweight = Bodyweight,
           lift = lift_reporting) %>%
    select(date, bodyweight, lift, starts_with(lift_prefix)) %>%
    mutate(reps = .data[[reps_col]],
           sets = .data[[sets_col]],
           weight = .data[[weight_col]]) %>%
    filter(reps != 0) %>%
    separate(reps, into = c("x", "set1", "set2","set3","set4","set5"), sep = "", remove = TRUE, fill = "right") %>%
    select(date, bodyweight, lift, starts_with("set"), weight) %>%
    select(-sets)
}

lifts_prefix <- c("Squat", "Light.Squat", "Press", "Bench", "Deadlift", "Power.Clean", "Chinup", "Back.Ext")

lift_list <- map(lifts_prefix, ~harmonize_lift(dbbackup, .x))

lifts <- lift_list %>%
  bind_rows()

# Explore squats

lifts.squat <- lifts %>%
  filter(lift == "squat")

ggplot(lifts.squat) +
  geom_point(aes(x = date, y = weight))

# NLPs

lifts.squat.ana <- lifts.squat %>%
  mutate(nlp = case_when(date %in% seq.Date(as.Date("2018-11-22"), as.Date("2019-01-25"), "day") ~ "s1",
                         date %in% seq.Date(as.Date("2019-01-31"), as.Date("2019-03-11"), "day") ~ "s2",
                         date %in% seq.Date(as.Date("2019-05-13"), as.Date("2019-10-11"), "day") ~ "s3",
                         date %in% seq.Date(as.Date("2019-12-15"), as.Date("2020-02-19"), "day") ~ "s4",
                         date %in% seq.Date(as.Date("2020-06-12"), as.Date("2020-08-04"), "day") ~ "s5",
                         date %in% seq.Date(as.Date("2020-08-31"), as.Date("2020-10-04"), "day") ~ "s6",
                         date %in% seq.Date(as.Date("2022-06-01"), as.Date("2022-08-12"), "day") ~ "s7",
                         TRUE ~ NA_character_)) %>%
  filter(!is.na(nlp)) %>%
  mutate(workout = 1) %>%
  group_by(nlp) %>%
  mutate(workout = cumsum(workout)) %>%
  ungroup()

  
ggplot(lifts.squat.ana) +
  geom_smooth(aes(x = workout, y = weight, color = nlp), se = TRUE, method = "lm") + 
  geom_point(aes(x = workout, y = weight, color = nlp))
  

lm(weight ~ workout, data = lifts.squat.ana)


