library(tidyverse)
library(timetk)
library(DataExplorer)


# Basics ----

## Total Appointments ----

wakefield_total_day_tbl <- read_rds("00_data/processed/wakefield_total_day_tbl.rds")

#### * Daily ----
wakefield_total_day_tbl %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .smooth_span = 0.4,
        .title = "Fall in appointments due to pandemic but steady increase since then",
        .y_lab = "Total daily appointments"
    )

#### * Aggregated ----

wakefield_total_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .title = "Recurring trough in weekly appointments at end of each year", 
        .smooth_span = 0.4,
        .smooth_degree = 1
    ) 

wakefield_total_day_tbl %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .smooth_span = 0.75,
                     .title = "Recurring peak in monthly appointments around October"
    ) 

## By Feature ----

### 1. Appointment Mode ----

wakefield_appt_mode_day_tbl   <- read_rds("00_data/processed/wakefield_appt_mode_day_tbl.rds")

#### * Daily ----
wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

#### * Aggregated ----

wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

### 2. Appointment Status ----

wakefield_appt_status_day_tbl <- read_rds("00_data/processed/wakefield_appt_status_day_tbl.rds")

#### * Daily ----

wakefield_appt_status_day_tbl %>% 
    group_by(appointment_status) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

#### * Aggregated ----

wakefield_appt_status_day_tbl %>%
    group_by(appointment_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments)

### 3. Health Care Professional ----

wakefield_hcp_type_day_tbl    <- read_rds("00_data/processed/wakefield_hcp_type_day_tbl.rds")

#### * Daily ----

wakefield_hcp_type_day_tbl %>% 
    group_by(hcp_type) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

#### * Aggregated ----

wakefield_hcp_type_day_tbl %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .smooth = FALSE)

# ACF Diagnostics ----

## ACF / PACF ----

# Total by Day
wakefield_total_day_tbl %>% 
    plot_acf_diagnostics(.date_var = appointment_date, appointments)

# GP appointments by Week 
wakefield_hcp_type_day_tbl %>%
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
# - Detecting Time-Based Features

wakefield_total_day_tbl %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "week", "month.lbl", "year"),
        .title = "Total Appointments",
        .interactive = FALSE, 
    )

wakefield_appt_mode_day_tbl %>%
    filter(appointment_mode %in% c("Face-to-Face", "Home Visit", "Telephone")) %>% 
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = appointment_mode,
        .feature_set = c("wday.lbl", "month.lbl", "year"),
        .title = "Appointments by Mode",
        .interactive = FALSE
    )

wakefield_appt_status_day_tbl %>%
    filter(appointment_status %in% c("Attended", "DNA")) %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = appointment_status,
        .feature_set = c("wday.lbl", "month.lbl", "year"),
        .title = "Appointments by Status",
        .interactive = FALSE
    )

wakefield_hcp_type_day_tbl %>%
    filter(hcp_type %in% c("GP", "Other Practice staff")) %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .facet_vars = hcp_type, 
        .feature_set = c("wday.lbl", "month.lbl", "year"),
        .title = "Appointments by Health Care Professional",
        .interactive = FALSE,
        .geom = "boxplot"
    )

# Anomalies ----

# errors or events?

# - Single
wakefield_total_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.2,
        .interactive = T
    )

# - Grouped
wakefield_appt_mode_day_tbl %>% 
    filter(appointment_mode %in% c("Face-to-Face", "Home Visit", "Telephone")) %>% 
    group_by(appointment_mode) %>% 
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

wakefield_appt_status_day_tbl %>% 
    filter(appointment_status %in% c("Attended", "DNA")) %>%
    group_by(appointment_status) %>% 
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

wakefield_hcp_type_day_tbl %>% 
    filter(hcp_type %in% c("GP", "Other Practice staff")) %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>% 
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.1,
        .interactive = T
    )

# Seasonal Decomposition ----

# single time series 

wakefield_total_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "28 days")

# grouped time series

wakefield_appt_mode_day_tbl %>% 
    filter(appointment_mode %in% c("Face-to-Face", "Home Visit", "Telephone")) %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>% 
    plot_stl_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .frequency = "12 months",
        .trend = "3 months"
    )

wakefield_appt_status_day_tbl %>% 
    filter(appointment_status %in% c("Attended", "DNA")) %>%
    group_by(appointment_status) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>% 
    plot_stl_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .frequency = "12 months"
    )

wakefield_hcp_type_day_tbl %>% 
    filter(hcp_type %in% c("GP", "Other Practice staff")) %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>% 
    plot_stl_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .frequency = "12 months",
        .trend = "12 months"
    )

# Time Series Regression Plot ----

# Single Time Series
wakefield_total_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
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
wakefield_appt_mode_day_tbl %>%
    filter(appointment_mode %in% c("Face-to-Face", "Home Visit", "Telephone")) %>% 
    group_by(appointment_mode) %>% 
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

wakefield_appt_status_day_tbl %>% 
    filter(appointment_status %in% c("Attended", "DNA")) %>%
    group_by(appointment_status) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            as.numeric(appointment_date) +
            month(appointment_date, label = TRUE) +
            week(appointment_date) +
            year(appointment_date),
        .show_summary = TRUE)

wakefield_hcp_type_day_tbl %>% 
    filter(hcp_type %in% c("GP", "Other Practice staff")) %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            as.numeric(appointment_date) +
            month(appointment_date, label = TRUE) +
            week(appointment_date) +
            year(appointment_date),
        .show_summary = TRUE)
