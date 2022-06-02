# Explore strength trend
library(tidyverse)
library(tsibble)
library(roxygen2)

# Starting values
mindate <- as.Date("2022-05-30")
maxdate <- as.Date("2022-08-20")
sq_start <- 110L
pr_start <- 50L
bp_start <- 60L
dl_start <- 135L

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


calculate_increments <- function(df, movement = c("squat", "press", "bench", "deadlift", "powerclean")) {
  movement <- match.arg(movement)
  increment <- function(df, movement) {
    switch(movement,
           squat = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5),
           press = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5),
           bench = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5),
           deadlift = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5),
           powerclean = case_when(df,
                             t_week == 1 ~ 0,
                             t_week %in% 2:3 ~ 5,
                             TRUE ~ 2.5))
  }
  
  selected_set <- case_when(movement == "squat" ~ c("A","B"),
                            movement == "press" ~ "A",
                            movement == "bench" ~ "B",
                            movement == "deadlift" ~ "A",
                            movement == "powerclean" ~ "B")
  
  df_incr <- df %>%
    filter(set %in% selected_set) %>%
    mutate(dl = dl_start,
           incr = case_when(t_week == 1 ~ 0,
                            t_week %in% 2:3 ~ 5,
                            TRUE ~ 2.5)) %>%
    mutate(cumincr = cumsum(incr),
           dl = dl+cumincr) %>%
    select(date, dl)
}

# Deadlift
nlp_dl <- nlp_tbl %>%
  filter(set == "A") %>%
  mutate(dl = dl_start,
         incr = case_when(t_week == 1 ~ 0,
                          t_week %in% 2:3 ~ 5,
                          TRUE ~ 2.5)) %>%
  mutate(cumincr = cumsum(incr),
         dl = dl+cumincr) %>%
  select(date, dl)

# Join movements
nlp_tbl <- nlp_tbl %>%
  left_join(nlp_dl, by = "date")
