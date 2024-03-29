# Explore strength trend
library(tidyverse)
library(tsibble)
library(roxygen2)

# Starting values
mindate <- as.Date("2023-01-01")
maxdate <- as.Date("2023-04-01")

start_values <- list("squat" = 90,
                     "press" = 50,
                     "bench" = 72.5,
                     "deadlift" = 135,
                     "powerclean" = 50)

# Date window & training days
date <- seq.Date(mindate, maxdate, "day")
wday <- weekdays(date)
t_day <- 1:length(date)
t_week <- floor(t_day/7)+1

nlp_tbl <- tibble(date, wday, t_week) %>%
  filter(wday %in% c("keskiviikko", "perjantai", "sunnuntai")) %>%
  mutate(set = rep_len(c("A","B"), 39))

# Movements

#' Calculate NLP-increments. In development -- tweak increments if necessary
#'
#' @examples
#' movement <- "squat"
#' df <- nlp_tbl
#' calculate_increments(df, "squat")


calculate_increments <- function(df, movement = c("squat", "press", "bench", "deadlift", "powerclean"), start_conf = start_values) {
  movement <- match.arg(movement)
  start_value <- start_conf[[movement]]
  
  selected_set <- case_when(movement == "squat" ~ c("A","B"),
                            movement == "press" ~ "A",
                            movement == "bench" ~ "B",
                            movement == "deadlift" ~ "A",
                            movement == "powerclean" ~ "B")
  
  df %>%
    filter(set %in% selected_set) %>%
    mutate(incr = switch(!!movement,
                         squat = case_when(t_week == 1 & set == "B" ~ 2.5,
                                           t_week %in% 2:4 ~ 2.5,
                                           t_week %in% 4:8 & set == "A" ~ 2.5,
                                           t_week > 8 & set == "A" ~ 1.25,
                                           TRUE ~ 0),
                         press = case_when(t_week %in% 1:3 ~ 2.5,
                                           t_week > 3 ~ 1.25,
                                           TRUE ~ 0),
                         bench = case_when(t_week %in% 1:4 ~ 2.5,
                                           t_week > 4 ~ 1.25,
                                           TRUE ~ 0),
                         deadlift = case_when(t_week %in% 2:3 ~ 5,
                                              t_week > 3 ~ 2.5,
                                              TRUE ~ 0),
                         powerclean = case_when(t_week %in% 4:5 ~ 2.5,
                                                t_week > 5 ~ 1.25,
                                                TRUE ~ 0))) %>%
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


# Descriptive analysis

ggplot() +
  geom_point(data = nlp, aes(x = date, y = squat)) +
  geom_point(data = nlp, aes(x = date, y = press)) +
  geom_point(data = nlp, aes(x = date, y = bench)) +
  geom_point(data = nlp, aes(x = date, y = deadlift)) +
  geom_point(data = nlp, aes(x = date, y = powerclean)) +
  ylab("Weight (kg)") +
  xlab("Date") +
  theme()


