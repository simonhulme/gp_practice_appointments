# Exploratory Analysis ----

## Set Up ----

# Load Libraries

library(tidyverse)
library(timetk)
library(DataExplorer)

# Import Data 

all_appointments_daily_tbl <-
    read_rds("00_data/processed/wakefield_daily_prepared.rds")

population_monthly_tbl <- 
    read_rds("00_data/processed/wakefield_population_monthly.rds")

# Exploratory Analysis ----

## ACF / PACF ----

### Total Appointments ----

total_appointments_daily_tbl <- 
    all_appointments_daily_tbl %>% 
    summarise_by_time(.date_var = appointment_date, .by = "day", appointments = sum(count_of_appointments))
    
total_appointments_daily_tbl %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments, 
        .lags = 260,
        .title = "Lag diagnostics: Total Appointments by Day", .plotly_slider = TRUE)

total_appointments_daily_tbl %>% 
    summarise_by_time(.date_var = appointment_date, .by = "week", appointments = sum(appointments)) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: Total Appointments by Week")

total_appointments_daily_tbl %>% 
    summarise_by_time(.date_var = appointment_date, .by = "month", appointments = sum(appointments)) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: Total Appointments by Month")


### GP appointments ----

gp_appointments_attended_daily_tbl <- 
    all_appointments_daily_tbl %>% 
    filter(hcp_type == "GP", appt_status == "Attended", appt_mode != "Unknown") %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(.date_var = appointment_date, .by = "day", appointments = sum(count_of_appointments))

gp_appointments_attended_daily_tbl %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

gp_appointments_attended_daily_tbl %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments, 
        .lags = 260,
        .title = "Lag diagnostics: GP Appointments Attended by Day")

gp_appointments_attended_weekly_tbl <- 
    all_appointments_daily_tbl %>% 
    filter(hcp_type == "GP", appt_status == "Attended", appt_mode != "Unknown") %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(.date_var = appointment_date, .by = "week", appointments = sum(count_of_appointments))

gp_appointments_attended_weekly_tbl %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

gp_appointments_attended_weekly_tbl %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments, 
        .lags = 53,
        .title = "Lag diagnostics: GP Appointments Attended by Week")

## CCF ----

total_appointments_attended_monthly_tbl <- 
    all_appointments_daily_tbl %>% 
    filter(appt_status == "Attended") %>% 
    summarise_by_time(.date_var = appointment_date, .by = "month", appointments = sum(count_of_appointments))

total_appointments_attended_monthly_tbl %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

total_appts_attended_population_monthly_tbl <- 
    total_appointments_attended_monthly_tbl %>% 
    left_join(population_monthly_tbl, by =c("appointment_date" = "extract_date"))

total_appts_attended_population_monthly_tbl %>%
    drop_na() %>%
    plot_acf_diagnostics(.date_var = appointment_date,
                         .value = appointments,
                         .ccf_vars = registered_population,
                         .show_ccf_vars_only = TRUE)

# Seasonality ----

## Total Appointments ----

total_appointments_daily_tbl %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total Appointments",
        .interactive = FALSE 
    )

## GP Appointments ----

gp_appointments_attended_daily_tbl %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total GP Appointments",
        .interactive = FALSE, 
    )
   

# Anomalies ----

# Errors or events?

## Total Appointments ----

total_appointments_daily_tbl %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.1,
        .interactive = T, 
        .title = "Mutiple Low Daily Values - Explore Bank Holidays and Training Afternoons"
    )

total_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.1,
        .interactive = T,
        .title = "Low Weekly Values - Explore Holidays and Effect of Pandemic "
    )

total_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.2,
        .interactive = T,
        .title = "Single low value - Impact of Pandemic "
    )

## GP Appointments attended ----

gp_appointments_attended_daily_tbl %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = FALSE, 
        .title = "Mutiple Low Daily Values - Explore Bank Holidays and Training Afternoons"
    )

gp_appointments_attended_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.1,
        .interactive = FALSE,
        .title = "Low Weekly Values - Explore Holidays and Effect of Pandemic "
    )

gp_appointments_attended_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.1,
        .interactive = FALSE,
        .title = "Single low value - Impact of Pandemic "
    )

# Seasonal Decomposition ----

# single time series 

total_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "7 days")

gp_appointments_attended_daily_tbl %>%
    ungroup() %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "28 days")


# grouped time series

gp_appointments_attended_daily_tbl %>%
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "12 months")


# Time Series Regression Plot ----

# Single Time Series
total_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            week(appointment_date) +
            month(appointment_date, label = TRUE) +
            year(appointment_date),
        .show_summary = TRUE)

gp_appointments_attended_daily_tbl %>%
    ungroup() %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            week(appointment_date) +
            month(appointment_date, label = TRUE) +
            year(appointment_date),
        .show_summary = TRUE)

# Grouped Time Series
gp_appointments_attended_daily_tbl %>%
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            as.numeric(appointment_date) +
            month(appointment_date, label = TRUE) +
            week(appointment_date) +
            year(appointment_date),
        .show_summary = TRUE)


# FINDINGS ----

## 1.  ----
## 2.  ----

# TODO ----

## * ----
## * ----