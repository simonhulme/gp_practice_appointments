library(tidyverse)
library(timetk)

wakefield_total_day_tbl       <- read_rds("00_data/processed/wakefield_total_day_tbl.rds")
wakefield_appt_status_day_tbl <- read_rds("00_data/processed/wakefield_appt_status_day_tbl.rds")
wakefield_appt_mode_day_tbl   <- read_rds("00_data/processed/wakefield_appt_mode_day_tbl.rds")
wakefield_hcp_type_day_tbl    <- read_rds("00_data/processed/wakefield_hcp_type_day_tbl.rds")

# Basics ----

## Total Appointments ----

wakefield_total_day_tbl %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .smooth_span = 0.5,
                     .title = "Daily Time Series Plot - Total Appointments")

wakefield_total_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .smooth_period = "12 months",
        .smooth_message = TRUE,
        .smooth_degree = 2,
        .title = "Weekly Time Series Plot - Total Appointments", .interactive = FALSE
    ) +
    theme_bw()

wakefield_total_day_tbl %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .smooth_period = "12 months",
                     .smooth_message = TRUE,
                     .smooth_degree = 1,
                     .title = "Monthly Time Series Plot - Total Appointments"
    )

## By Feature ----

### Appointment Mode ----

wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments))


wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

### Appointment Status ----

wakefield_appt_status_day_tbl %>% 
    group_by(appointment_status) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

wakefield_appt_status_day_tbl %>%
    group_by(appointment_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Weekly Time Series Plot - Appointment Status")


wakefield_appt_status_day_tbl %>%
    group_by(appointment_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Monthly Time Series Plot - Appointment Status")

### Health Care Professional ----

wakefield_hcp_type_day_tbl %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

wakefield_hcp_type_day_tbl %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Weekly Time Series Plot - HCP Type")

wakefield_hcp_type_day_tbl %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .smooth = FALSE,
                     .title = "Monthly Time Series Plot - HCP Type")



