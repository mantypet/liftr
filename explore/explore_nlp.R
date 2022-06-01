# Explore strength trend
library(tidyverse)
library(tsibble)

mindate <- as.Date("2022-05-30")
maxdate <- as.Date("2022-08-20")
date <- seq.Date(mindate, maxdate, "day")
wday <- weekdays(date)
t_day <- 1:length(date)
t_week <- floor(t_day/7)+1

nlp_tbl <- tibble(date, wday, t_week) %>%
  filter(wday %in% c("keskiviikko", "perjantai", "sunnuntai")) %>%
  mutate(set = rep_len(c("A","B"), 35))

# Deadlift

nlp_dl <- nlp_tbl %>%
  filter(set == "A") %>%
  mutate(dl = 135,
         incr = case_when(t_week == 1 ~ 0,
                          t_week %in% 2:3 ~ 5,
                          TRUE ~ 2.5)) %>%
  mutate(cumincr = cumsum(incr),
         dl = dl+cumincr) %>%
  select(date, dl)

# Join moves

nlp_tbl <- nlp_tbl %>%
  left_join(nlp_dl, by = "date")
