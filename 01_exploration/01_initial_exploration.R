# Initial Data Exploration ----

library(tidyverse)
library(timetk)

## Total Appointments ----

wakefield_total_day_tbl <- read_rds("00_data/processed/wakefield_total_day_tbl.rds")

wakefield_total_day_tbl %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .smooth_span = 0.4,
        .title = "Fall in appointments due to pandemic but steady increase since then",
        .y_lab = "Total daily appointments"
    )

## By Feature ----

### 1. Appointment Mode ----

wakefield_appt_mode_day_tbl   <- read_rds("00_data/processed/wakefield_appt_mode_day_tbl.rds")

wakefield_appt_mode_day_tbl %>% 
    group_by(appointment_mode) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

### 2. Appointment Status ----

wakefield_appt_status_day_tbl <- read_rds("00_data/processed/wakefield_appt_status_day_tbl.rds")

wakefield_appt_status_day_tbl %>% 
    group_by(appointment_status) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

### 3. Health Care Professional ----

wakefield_hcp_type_day_tbl    <- read_rds("00_data/processed/wakefield_hcp_type_day_tbl.rds")

wakefield_hcp_type_day_tbl %>% 
    group_by(hcp_type) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)