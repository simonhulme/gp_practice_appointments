# Summarise Time Series Data by Time ----

library(tidyverse)
library(timetk)
library(DataExplorer)

## Total Appointments ----

wakefield_total_day_tbl <- read_rds("00_data/processed/wakefield_total_day_tbl.rds")

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

wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)


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

wakefield_appt_status_day_tbl %>%
    group_by(appointment_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments)

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

wakefield_hcp_type_day_tbl %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .smooth = FALSE)

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

# FINDINGS ----


# TODO ----
