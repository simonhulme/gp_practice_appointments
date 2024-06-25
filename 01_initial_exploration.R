# Initial Data Exploration ----

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
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments, 
        .title = "Weekly low values due to weekends") 

total_weekly_appts <- 
    wakefield_daily_raw %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    )

total_weekly_appts %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .title = "Annual low values due to Xmas and New Year Holidays")

total_monthly_appts <- 
    wakefield_daily_raw %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    )

total_monthly_appts %>%
    plot_time_series(
        .date_var = appointment_date,   
        .value = appointments,
        .title = "Low total appointments due to pandemic but approx linear increase since then")

### Appointment Mode ----

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
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = appt_mode,
        .title = "High frequency 'Unknown' at start of time series"
    )

appt_mode_weekly_appts %>%
    filter_by_time(.date_var = appointment_date,
                   .start_date = "2019-03",
                   .end_date = "2024") %>%
    filter(appt_mode == "Unknown") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .title = "Start date of 2019-03 handles initial 'Unknown' values ")

### Appointment Status ----

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
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = appt_status,
        .title = "High frequency 'Not Provided' at start of time series"
    )

appt_status_weekly_appts %>% 
    filter_by_time(.date_var = appointment_date,
                   .start_date = "2019-03",
                   .end_date = "2024") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = appt_status,
        .title = "Start date of 2019-03 handles initial 'Not Provdided' values ")
    
### Health Care Professional ----

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
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = hcp_type,
        .title = "High frequency 'Not Provided' or 'Unknown' at start of time series"
    )

hcp_type_weekly_appts %>% 
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024") %>% 
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = hcp_type,
        .title = "Start date of 2019-03 handles initial 'Not Provided' and 'Unknown' values"
    )


#### 4. Time between booking and appointment ----

book_appt_time_weekly_appts <-
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

book_appt_time_weekly_appts %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = time_between_book_and_appt,
        .facet_ncol = 2,
        .title = "Groups for Unknown and Data Issue / Quality"
    )

# FINDINGS ----

## 1. data assumes 7 day week rather than 5 day working week ----
## 2. bank holidays result in low values ----
## 3. pandemic had significant impact on total appointments ----
## 4. historic data quality issues near start of time series ----
## 5. categorical data encoded as strings ----
## 6. levels duplicated ----
### * video / online appointment modes ----
### * unknown vs. data not provided ----
### • data quality vs. data issue


# TODO ----

## * convert to 5-day business week ----
## * encode categorical variables as factors ----
## * combine levels for features where duplicated ----
## * create subset of time series to exclude significant data quality problems ----