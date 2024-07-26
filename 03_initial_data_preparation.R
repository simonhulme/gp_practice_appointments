# DATA PREPARATION

# Load Libraries 
library(tidyverse)
library(timetk)

# Read the raw data
wakefield_raw <- read_rds("00_data/raw/wakefield_daily_raw.rds")

# Appointments data ----

## Handle implicitly missing values ----

## These are combinations of categorical levels which have zero frequency

## Generate series of time stamps with no missing days
all_days <-
    tk_make_timeseries(
        start_date = min(wakefield_raw$appointment_date),
        end_date   = max(wakefield_raw$appointment_date)
    )

# Expand dataset to contain all levels for all dates
wakefield_expanded <- 
    wakefield_raw %>%
    complete(appointment_date = all_days,
             hcp_type,
             appt_mode,
             appt_status,
             time_between_book_and_appt) %>%
    replace_na(list(count_of_appointments = 0))

## Encode categorical variables as factors ----

## collapse levels to handle duplication & re-order where order relevant
wakefield_factorised <-
    wakefield_expanded  %>%
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

wakefield_daily <- 
    wakefield_factorised %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024")

## Visualise
wakefield_daily %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, appointments, .smooth_period = "12 months")

## Save Data ----
write_rds(wakefield_daily, "00_data/processed/wakefield_daily_tbl.rds")

# Events data ----

start_date <- min(wakefield_daily$appointment_date)
end_date   <- max(wakefield_daily$appointment_date)

## Calendar features: Weekends & Bank Holidays ----

weekend_dates <- 
    tk_make_weekend_sequence(start_date = start_date, end_date = end_date)

bank_holiday_dates <-
    tk_make_holiday_sequence(start_date = start_date,
                             end_date = end_date,
                             calendar = "LONDON")

## Training Half Days ----
## Practices close for half day training

training_19 <- 
    c("2019-03-20", "2019-04-24", "2019-05-15", "2019-06-19",
      "2019-07-17", "2019-09-18", "2019-10-16", "2019-11-13")

training_20 <- 
    c("2020-01-15", "2020-02-26", "2020-07-15", "2020-08-19", "2020-09-16", 
      "2020-10-14", "2020-11-11")

training_21 <- 
    c("2021-01-13", "2021-02-24", "2021-03-17", "2021-04-21", "2021-05-12",
      "2021-06-16", "2021-07-14", "2021-09-15", "2021-10-13", "2021-11-10")

training_22 <- 
    c("2022-02-23", "2022-03-16", "2022-04-06", "2022-05-11","2022-06-15", 
      "2022-07-13", "2022-09-14", "2022-10-12", "2022-11-09")

training_23 <- 
    c("2023-02-08", "2023-04-19", "2023-05-17", "2023-06-21", "2023-07-12", 
      "2023-08-16", "2023-09-13", "2023-10-11", "2023-11-15")

training_24 <- 
    c("2024-01-17", "2024-02-21", "2024-03-13", "2024-04-17", "2024-05-15",
      "2024-06-19", "2024-07-10", "2024-09-18", "2024-10-16", "2024-11-13")

training_dates <-
    as.Date(c(
        training_19,
        training_20,
        training_21,
        training_22,
        training_23,
        training_24
    ))

## Pandemic ----

pandemic_dates <- 
  tk_make_timeseries(start_date = "2020-03-23", end_date = "2021-07-19", by = "day")

## Merge into single events data set ----

events <-
    wakefield_daily %>% 
    select(appointment_date) %>% 
    distinct(appointment_date) %>% 
    mutate(
        weekend      = appointment_date %in% weekend_dates,
        bank_holiday = appointment_date %in% bank_holiday_dates,
        training     = appointment_date %in% training_dates,
        pandemic     = appointment_date %in% pandemic_dates
    )

## Correct main data for holiday and weekend: all values should be zero for model
wakefield_daily_corrected <-
    wakefield_daily %>%
    left_join(events) %>%
    mutate(count_of_appointments = ifelse(weekend |
                                              bank_holiday, 0, count_of_appointments)) %>% 
select(appointment_date, count_of_appointments, hcp_type:time_between_book_and_appt)

### visualise to check
wakefield_daily_corrected %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, .value = appointments)

## Save Data
write_rds(events, "00_data/processed/events_tbl.rds")
write_rds(wakefield_daily_corrected, "00_data/processed/wakefield_daily_corrected_tbl.rds")

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


# Convert data to 5 day working week ----

## create index for working week ----
five_day_index <-
    tk_make_weekday_sequence(
        start_date = start_date,
        end_date = end_date,
        remove_weekends = TRUE,
        remove_holidays = FALSE
    )

wakefield_5_day_tbl <-
    wakefield_daily_expanded %>% 
    filter(appointment_date %in% five_day_index) 



## Adjust holiday and weekend values to zero ----
wakefield_daily_adj <-
  wakefield_daily_raw %>%
  left_join(wakefield_events) %>%
  mutate(count_of_appointments = ifelse(weekend |
                                          bank_holiday, 0, count_of_appointments)) %>% 
  select(-c(weekend, bank_holiday, training))

write_rds(wakefield_daily_adj, "00_data/processed/wakefield_daily_adj.rds")


