# Explore strength trend
library(tidyverse)
library(tsibble)
library(roxygen2)

# Starting values
mindate <- as.Date("2022-05-30")
maxdate <- as.Date("2022-08-20")

start_values <- list("squat" = 110L,
                     "press" = 50L,
                     "bench" = 60L,
                     "deadlift" = 135L,
                     "powerclean" = 50L)

# Date window & training days
date <- seq.Date(mindate, maxdate, "day")
wday <- weekdays(date)
t_day <- 1:length(date)
t_week <- floor(t_day/7)+1

nlp_tbl <- tibble(date, wday, t_week) %>%
  filter(wday %in% c("keskiviikko", "perjantai", "sunnuntai")) %>%
  mutate(set = rep_len(c("A","B"), 35))

# Movements

#'
#' @examples
#' movement <- "squat"
#' df <- nlp_tbl
#' calculate_increments(df, "squat")


calculate_increments <- function(df, movement = c("squat", "press", "bench", "deadlift", "powerclean"), start_conf = start_values) {
  movement <- match.arg(movement)
  start_value <- start_conf[[movement]]
  
  increment <- function(df, movement) {
    switch(movement,
           squat = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5),
           press = case_when(df,
                             t_week == 1 ~ 0,
                             TRUE ~ 2.5),
           bench = case_when(df,
                             t_week == 1 ~ 0,
                             t_week == 2 ~ 5,
                             TRUE ~ 2.5),
           deadlift = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5),
           powerclean = case_when(df,
                             t_week == 1 ~ 0,
                             TRUE ~ 2.5))
  }
  
  selected_set <- case_when(movement == "squat" ~ c("A","B"),
                            movement == "press" ~ "A",
                            movement == "bench" ~ "B",
                            movement == "deadlift" ~ "A",
                            movement == "powerclean" ~ "B")
  
  df %>%
    filter(set %in% selected_set) %>%
    mutate(incr = switch(!!movement,
                         squat = case_when(t_week == 1 ~ 0,
                                           t_week %in% 2 ~ 5,
                                           TRUE ~ 2.5),
                         press = case_when(t_week == 1 ~ 0,
                                           TRUE ~ 2.5),
                         bench = case_when(t_week == 1 ~ 0,
                                           t_week == 2 ~ 5,
                                           TRUE ~ 2.5),
                         deadlift = case_when(t_week == 1 ~ 0,
                                              t_week %in% 2:3 ~ 5,
                                              TRUE ~ 2.5),
                         powerclean = case_when(t_week == 1 ~ 0,
                                                TRUE ~ 2.5))) %>%
    mutate(cumincr = cumsum(incr),
           {{movement}} := start_value+cumincr) %>%
    select(date, !!movement)
}

# Increments
nlp_sq <- nlp_tbl %>%
  calculate_increments("squat")
nlp_pr <- nlp_tbl %>%
  calculate_increments("press")
nlp_bp <- nlp_tbl %>%
  calculate_increments("bench")
nlp_dl <- nlp_tbl %>%
  calculate_increments("deadlift")
nlp_pc <- nlp_tbl %>%
  calculate_increments("powerclean")
# Join movements

nlp <- nlp_tbl %>%
  left_join(nlp_sq, by = "date") %>%
  left_join(nlp_pr, by = "date") %>%
  left_join(nlp_bp, by = "date") %>%
  left_join(nlp_dl, by = "date") %>%
  left_join(nlp_pc, by = "date")
