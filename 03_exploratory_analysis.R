# Exploratory Analysis ----

## Set Up ----

# Load Libraries

library(tidyverse)
library(timetk)
library(DataExplorer)

# Import Data 

total_appointments   <-
    read_rds("00_data/processed/sliced/total/wakefield_total_5_day_tbl.rds")

# Exploratory Analysis ----

## Set Up ----

# Load Libraries

library(tidyverse)
library(timetk)
library(DataExplorer)

# Import Data 

## Start with Totals 

total_appointments   <-
    read_rds("00_data/processed/sliced/total/wakefield_total_5_day_tbl.rds")

appointments_by_hcp <- 
    read_rds("00_data/processed/sliced/hcp_type/wakefield_hcp_type_5_day_sliced_tbl.rds")

appointments_by_mode <-
    read_rds("00_data/processed/sliced/appt_mode/wakefield_appt_mode_5_day_sliced_tbl.rds")

## Summaries 

total_appointments %>%
    mutate_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments_avg = mean(appointments)
    ) %>% 
    pivot_longer(cols = contains("appointments")) %>% 
    plot_time_series(.date_var = appointment_date, .value = value, .color_var = name)

appointments_by_hcp %>%
    group_by(hcp_type) %>% 
    mutate_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments_avg = mean(appointments)
    ) %>% 
    pivot_longer(cols = contains("appointments")) %>% 
    plot_time_series(.date_var = appointment_date, .value = value, .color_var = name)
               
appointments_by_mode %>%
    group_by(appointment_mode) %>% 
    mutate_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments_avg = mean(appointments)
    ) %>% 
    pivot_longer(cols = contains("appointments")) %>% 
    plot_time_series(.date_var = appointment_date, .value = value, .color_var = name)
       

## ACF / PACF ----

### Total Appointments ----

total_appointments %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: Total Appointments by Day")

total_appointments %>% 
    summarise_by_time(.date_var = appointment_date, .by = "week", appointments = sum(appointments)) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: Total Appointments by Week")

total_appointments %>% 
    summarise_by_time(.date_var = appointment_date, .by = "month", appointments = sum(appointments)) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: Total Appointments by Month")

## Appointment Mode ----

appointments_by_mode %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(.date_var = appointment_date, .by = "month", appointments = sum(appointments)) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: Appointment Mode by Month")

## Health Care Professional ----

appointments_by_hcp %>% 
    group_by(hcp_type) %>% 
    summarise_by_time(.date_var = appointment_date, .by = "week", appointments = sum(appointments)) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date, 
        .value = appointments,
        .title = "Lag diagnostics: HCP type by Week")

## Will need to focus on sub groups within the data e.g. GP appointments total and by appointment mode

# GP appointments by Week 
appointments_by_hcp %>%
    filter(hcp_type == "GP") %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_acf_diagnostics(.date_var = appointment_date, appointments, .lags = "3 years")

## CCF ----
# - Lagged External Regressors

# univariate series only - review after identifying potential data

# Seasonality ----

## Total Appointments ----

total_appointments %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total Appointments",
        .interactive = FALSE, 
    )

## Total GP Appointments ----

appointments_by_hcp %>%
    filter(hcp_type == "GP") %>% 
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total GP Appointments",
        .interactive = FALSE, 
    )
   
## Total Other Staff Appointments ----

appointments_by_hcp %>%
    filter(hcp_type == "Other Practice staff") %>% 
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total Other Practice Staff Appointments",
        .interactive = FALSE, 
    )

## Total Non GP assigned Appointments ----

appointments_by_hcp %>% 
    filter(hcp_type != "GP") %>% 
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total Non GP assigned Appointments",
        .interactive = FALSE, 
    )

# Anomalies ----

# Errors or events?

## Total Appointments ----

total_appointments %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T, 
        .title = "Mutiple Low Daily Values - Explore Bank Holidays and Training Afternoons"
    )

total_appointments %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .title = "Low Weekly Values - Explore Holidays and Effect of Pandemic "
    )

total_appointments %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .title = "Single low value - Impact of Pandemic "
    )

## Total Appointments By HCP ----

appointments_by_hcp %>% 
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>% 
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T
    )

appointments_by_hcp %>% 
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>% 
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T
    )

# Seasonal Decomposition ----

# single time series 

total_appointments %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "7 days")


# grouped time series

appointments_by_hcp %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "12 months")


# Time Series Regression Plot ----

# Single Time Series
total_appointments %>%
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
appointments_by_hcp %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            as.numeric(appointment_date) +
            month(appointment_date, label = TRUE) +
            week(appointment_date) +
            year(appointment_date),
        .show_summary = TRUE)