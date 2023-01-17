# read and clean Starting Strenth App dbBackup.csv
library(dplyr)
library(tidyr)
library(lubridate)
library(stringr)
library(ggplot2)

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

# Date diff vs weight diff

lifts.squat.ana <- lifts.squat %>%
  mutate(date_lag = lag(date),
         weight_lag = lag(weight)) %>%
  mutate(date_diff = as.numeric(date-date_lag),
         weight_diff = weight-weight_lag)

lifts.squat.ana %>%
  mutate(date_diff = ifelse(date_diff > 30, 30, date_diff)) %>%
  ggplot() +
  geom_point(aes(x = date_diff, y = weight_diff))

