# Exploratory Data Analysis

# Set Up ----
library(tidyverse)
library(timetk)

wakefield_daily <- read_rds("00_data/processed/wakefield_daily_tbl.rds")

# Anomalies ----

wakefield_working_week_daily %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .title = "Troughs for total apppointments when weeks that contain holiday "
    )

wakefield_working_week_daily %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .title = ""
    )

# BY MODE & HCP

wakefield_working_week_daily %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online"),
           hcp_type != "HCP Type Not Provided") %>%
    group_by(appt_mode, hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .facet_ncol = 3,
        .title = ""
    )

# BY BOOKING & HCP

same_day_vs_booked_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online"),
           hcp_type != "HCP Type Not Provided") %>%
    group_by(booking, hcp_type) %>% 
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
        .facet_ncol = 3,
        .title = ""
    )

# Seasonal Decomposition ----

## Total Appointments Booked - Total Monthly Appointments

total_booked_daily_tbl %>%
    ungroup() %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(
        appointment_date,
        appointments,
        .feature_set = c("observed", "season", "trend", "remainder")
    )



## Total Appointments Booked - Mean daily value by Month Post Pandemic

total_booked_daily_tbl %>%
    ungroup() %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = mean(appointments)
    ) %>%
    plot_stl_diagnostics(
        appointment_date,
        appointments,
        .feature_set = c("observed", "season", "trend", "remainder"), 
        .frequency = 12
    )

## NEED TO COMMENT ON STL DECOMPOSITION



## separate time series by: hcp / mode / booking

### GP: total ----

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>%
    filter(hcp_type == "GP") %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08") %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(
        appointment_date,
        appointments,
        .feature_set = c("observed", "season", "trend", "remainder"), 
        .frequency = 12
    )






wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))
    
### Other Staff: telephone vs face to face vs home visit ----

wakefield_working_week_daily %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))
    
### GP : Same Day vs Booked ----

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

### Other Practice staff: Same Day vs Booked ----

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

# FINDINGS ----

## 1. Total appointments: Other Practice Staff > GP ----
### * Face-to-face appointments responsible for excess ----
## 2. Most appointments face-to-face followed by telephone ----
## 3. 3.3% of total appointments no attended, 5% status not known ----
### * Percentage DNA and Unknown: Other Practice Staff > GP ----
### * Percentage DNA and Unknown - increases with longer waits ----
## 4. Most appointments are Same Day appointments ----
### * Booked appointments tend to be non-GP appointments ----
## 5. Regular dips in series - Bank Holidays & Staff Training ----
### * Weekly total may be less but daily could be higher ----
## 6. Downward trend in GP appts but upward trend for other staff ----
## 7. GP advance bookings stable but same day appointments falling ----
### * Rapid growth in advance and same day appointments for other staff ----
## 8. Increasing Face-to-Face appointments for all health care professionals  ----
### * Possible correction after significant changes in practice during pandemic ----

# TODO ----
## 1. Create time series to handle anomalies ----
### * Bank Holidays, Training Afternoons, Pandemic ----
### * compare weekly totals with mean daily appointments per week ----
## 2. Focus in on a few subsets of data ----
### * Total Appointments vs GP vs Other Practice staff by mode and booking ----
#### ** 12 potential time series ----
