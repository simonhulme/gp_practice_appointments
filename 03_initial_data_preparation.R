# DATA PREPARATION

# Load Libraries 
library(tidyverse)
library(timetk)

# Read the raw data
wakefield_daily_raw  <- read_rds("00_data/raw/wakefield_daily_raw.rds")
wakefield_events     <- read_rds("00_data/processed/wakefield_events.rds")
wakefield_population <- read_rds("00_data/processed/wakefield_population_monthly.rds")

# Appointments data ----

## Handle implicitly missing values ----

## These are combinations of categorical levels which have zero frequency

## Generate series of time stamps representing weekdays

week_days <-
    tk_make_weekday_sequence(
        start_date = min(wakefield_daily_raw$appointment_date),
        end_date   = max(wakefield_daily_raw$appointment_date)
    )

# Expand dataset to contain all levels for all dates
wakefield_weekdays <- 
  wakefield_daily_raw %>%
  filter(appointment_date %in% week_days) 

wakefield_weekdays_full <-
  wakefield_weekdays %>%
  complete(appointment_date = week_days,
           hcp_type,
           appt_mode,
           appt_status,
           time_between_book_and_appt) %>%
  replace_na(list(count_of_appointments = 0))

## Encode categorical variables as factors ----

## collapse levels to handle duplication & re-order where order relevant
wakefield_weekdays_full <-
  wakefield_weekdays_full  %>%
    mutate(across(where(is_character), as_factor)) %>% 
    mutate(
        time_between_book_and_appt = fct_relevel(
            time_between_book_and_appt,
            c(
                "Same Day",
                "1 Day",
                "2 to 7 Days",
                "8  to 14 Days",
                "15  to 21 Days",
                "22  to 28 Days",
                "More than 28 Days",
                "Unknown / Data Issue",
                "Unknown / Data Quality"
            )
        ),
        time_between_book_and_appt = fct_collapse(
            time_between_book_and_appt,
            "Unknown" = c("Unknown / Data Issue", "Unknown / Data Quality")
        ),
        appt_status = fct_collapse(
            appt_status,
            "Unknown" = c("Unknown", "Appt Status Not Provided")
        )
    )

## Subset Time Series to Exclude Missing Data ----

wakefield_weekdays_full <- 
  wakefield_weekdays_full %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024")

wakefield_weekdays_full <- 
  wakefield_weekdays_full %>% 
  left_join(wakefield_events, by = c("appointment_date" = "event_date")) %>%
  mutate(count_of_appointments = ifelse(bank_holiday, 0, count_of_appointments)) %>%
  select(appointment_date,
         count_of_appointments,
         hcp_type:time_between_book_and_appt)

## Visualise
wakefield_weekdays_full %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, appointments)

## Save Data ----
write_rds(wakefield_weekdays_full, "00_data/processed/wakefield_weekdays_full.rds")
