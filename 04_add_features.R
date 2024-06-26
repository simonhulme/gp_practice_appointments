library(tidyverse)
library(timetk)

wakefield_working_week_daily <- read_rds("00_data/processed/wakefield_working_week_daily.rds")
wakefield_population_monthly <- read_rds("00_data/processed/wakefield_population_monthly.rds")
wakefield_workforce_monthly  <- read_rds("00_data/processed/wakefield_workforce_monthly.rds")

# Calendar Adjustments ----

## Bank Holidays, Training Days, Pandemic ----

start_date <- min(wakefield_working_week_daily$appointment_date)
end_date   <- max(wakefield_working_week_daily$appointment_date)

bank_holiday_dates <-
    tk_make_holiday_sequence(start_date = start_date,
                             end_date = end_date,
                             calendar = "LONDON")

training_dates_19 <- 
    c("2019-03-20", "2019-04-24", "2019-05-15", "2019-06-19",
      "2019-07-17", "2019-09-18", "2019-10-16", "2019-11-13")

training_dates_20 <- 
    c("2020-01-15", "2020-02-26", "2020-07-15", "2020-08-19", "2020-09-16", 
      "2020-10-14", "2020-11-11")

training_dates_21 <- 
    c("2021-01-13", "2021-02-24", "2021-03-17", "2021-04-21", "2021-05-12",
      "2021-06-16", "2021-07-14", "2021-09-15", "2021-10-13", "2021-11-10")

training_dates_22 <- 
    c("2022-02-23", "2022-03-16", "2022-04-06", "2022-05-11","2022-06-15", 
      "2022-07-13", "2022-09-14", "2022-10-12", "2022-11-09")

training_dates_23 <- 
    c("2023-02-08", "2023-04-19", "2023-05-17", "2023-06-21", "2023-07-12", 
      "2023-08-16", "2023-09-13", "2023-10-11", "2023-11-15")

training_dates_24 <- 
    c("2024-01-17", "2024-02-21", "2024-03-13", "2024-04-17", "2024-05-15",
      "2024-06-19", "2024-07-10", "2024-09-18", "2024-10-16", "2024-11-13")

training_dates <-
    as.Date(c(
        training_dates_19,
        training_dates_20,
        training_dates_21,
        training_dates_22,
        training_dates_23,
        training_dates_24
    ))

pandemic_dates <- 
  tk_make_timeseries(start_date = "2020-03-23", end_date = "2021-07-19", by = "day")

wakefield_working_week_with_calendar_adjustments <- 
  wakefield_working_week_daily %>%
  mutate(holiday  = as_factor(if_else(appointment_date %in% bank_holiday_dates, "Yes", "No")),
         training = as_factor(if_else(appointment_date %in% training_dates, "Yes", "No")),
         pandemic = as_factor(if_else(appointment_date %in% pandemic_dates, "Yes", "No")))

# External Regressors ----

## Population ----

wakefield_population_daily <- 
  tk_make_timeseries(start_date = "2019-05-01", end_date = "2024-05-01") %>% 
  as_tibble_col(column_name = "appointment_date") %>% 
  left_join(wakefield_population_monthly, by = c("appointment_date" = "extract_date")) %>% 
  fill(registered_population)

wakefield_working_week_with_calendar_adjustments_and_population <-
  wakefield_population_daily %>%
  inner_join(wakefield_working_week_with_calendar_adjustments)

## Workforce ----

wakefield_workforce_daily <- 
  tk_make_timeseries(start_date = "2020-09-01", end_date = "2024-05-01") %>% 
  as_tibble_col(column_name = "appointment_date") %>% 
  left_join(wakefield_workforce_monthly, by = c("appointment_date" = "extract_date")) %>% 
  fill(contains("total"))

wakefield_working_week_with_calendar_adjustments_population_workforce <- 
  wakefield_workforce_daily %>% 
  inner_join(wakefield_working_week_with_calendar_adjustments_and_population)


# Save data ----

write_rds(
  wakefield_working_week_with_calendar_adjustments,
  "00_data/processed/wakefield_calendar_adj.rds"
)

write_rds(
  wakefield_working_week_with_calendar_adjustments_and_population,
  "00_data/processed/wakefield_calendar_population.rds"
)

write_rds(
  wakefield_working_week_with_calendar_adjustments_population_workforce,
  "00_data/processed/wakefield_calendar_population_workforce.rds"
)
