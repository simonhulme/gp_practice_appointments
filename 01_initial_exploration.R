# Initial Data Exploration ----

## Set Up ----

# Load Libraries

library(tidyverse)
library(timetk)

# Import Data 

wakefield_daily_raw <- read_rds("00_data/raw/wakefield_daily_raw.rds")

## Time Series Plots ----

### Total Appointments ----

total_daily_appts <- 
    wakefield_daily_raw %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    )

total_daily_appts %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments)

total_weekly_appts <- 
    wakefield_daily_raw %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    )

total_weekly_appts %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments)


total_monthly_appts <- 
    wakefield_daily_raw %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    )

total_monthly_appts %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments)

### By Feature ----

#### 1. Appointment Mode ----

appt_mode_daily_appts <- 
    wakefield_daily_raw %>%
    group_by(appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup()

appt_mode_daily_appts %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_vars = appt_mode)

appt_mode_weekly_appts <- 
    wakefield_daily_raw %>%
    group_by(appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup()

appt_mode_weekly_appts %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_vars = appt_mode)

#### 2. Appointment Status ----

appt_status_daily_appts <- 
    wakefield_daily_raw %>%
    group_by(appt_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup()

appt_status_daily_appts %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_vars = appt_status)

appt_status_weekly_appts <- 
    wakefield_daily_raw %>%
    group_by(appt_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup()

appt_status_weekly_appts %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_vars = appt_status)


#### 3. Health Care Professional ----

hcp_type_daily_appts <- 
    wakefield_daily_raw %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup()

hcp_type_daily_appts %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_vars = hcp_type)

hcp_type_weekly_appts <- 
    wakefield_daily_raw %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup()

hcp_type_weekly_appts %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_vars = hcp_type)

wakefield_daily_raw %>% names()

#### 4. Time between booking and appointment ----

time_between_book_and_appt_daily_appts <-
    wakefield_daily_raw %>%
    mutate(time_between_book_and_appt =
               factor(
                   time_between_book_and_appt,
                   levels = c(
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
               )) %>%
    group_by(time_between_book_and_appt) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

time_between_book_and_appt_daily_appts %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .facet_vars = time_between_book_and_appt, .facet_ncol = 2)

time_between_book_and_appt_weekly_appts <-
    wakefield_daily_raw %>%
    mutate(time_between_book_and_appt =
               factor(
                   time_between_book_and_appt,
                   levels = c(
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
               )) %>%
    group_by(time_between_book_and_appt) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

time_between_book_and_appt_weekly_appts %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .facet_vars = time_between_book_and_appt, .facet_ncol = 2)

time_between_book_and_appt_monthly_appts <-
    wakefield_daily_raw %>%
    mutate(time_between_book_and_appt =
               factor(
                   time_between_book_and_appt,
                   levels = c(
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
               )) %>%
    group_by(time_between_book_and_appt) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

time_between_book_and_appt_monthly_appts %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .facet_vars = time_between_book_and_appt, .facet_ncol = 2)

# FINDINGS ----

## 1. data assumes 7 day week but GP opening times relate to 5 day business week ----

## 2. historic data quality issues near start of time series ----
### * different time periods depending on feature ----

## 3. levels duplicated 
### * video / online appointment modes ----
### * unknown vs. data not provided ----
### • data quality vs. data issue

# TODO ----

## * convert to 5-day business week ----
## * combine levels for features where duplicated ----
## * slice time series to exclude significant data quality problems ----