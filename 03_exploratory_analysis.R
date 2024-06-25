# Exploratory Data Analysis

# Set Up
library(tidyverse)
library(timetk)

wakefield_working_week_daily <- read_rds("00_data/processed/wakefield_working_week_daily.rds")

# Summary Statistics ----

skimr::skim(wakefield_working_week_daily)

# 1348 days between 2019-03-91 and 2024-04-30
# 4 categorical variables (nominal)
# numeric response variable with no missing values

# Exploratory Visualisation ----

### Total Appointments by Feature ----

wakefield_working_week_daily %>%
    group_by(hcp_type) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(hcp_type = fct_reorder(hcp_type, -total_appointments)) %>%
    ggplot(aes(hcp_type, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw()

wakefield_working_week_daily %>%
    group_by(appt_mode) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(appt_mode = fct_reorder(appt_mode, -total_appointments)) %>%
    ggplot(aes(appt_mode, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw()

wakefield_working_week_daily %>%
    group_by(appt_status) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(prop = proportions(total_appointments)) 

wakefield_working_week_daily %>%
    group_by(time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    xlab("Time between booking and appointment date")

### GP vs Other Staff by Feature ----

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff",
           appt_mode %in% c("Face-to-Face", "Telephone")) %>% 
    group_by(hcp_type, appt_mode) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(appt_mode = fct_reorder(appt_mode, -total_appointments)) %>%
    ggplot(aes(appt_mode, total_appointments, fill = hcp_type)) +
    geom_col(color = "grey30", position = "dodge") +
    theme_bw()

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>% 
    group_by(hcp_type, appt_status) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(prop = proportions(total_appointments)) %>% 
    ggplot(aes(hcp_type, total_appointments, fill =  appt_status,)) +
    geom_col(color = "grey30", position = "fill") +
    theme_bw()

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>% 
    group_by(hcp_type, time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments, fill = hcp_type)) +
    geom_col(position = "fill") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    xlab("Time between booking and appointment date")

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>%   
    mutate(appt_status = fct_relevel(appt_status, c("Attended", "Unknown", "DNA"))) %>%
    group_by(hcp_type, appt_status, time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments, fill = appt_status)) +
    geom_col(position = "fill") +
    theme_bw() +
    scale_fill_brewer(palette = 9) +
    labs(title = "Very few DNA or Unknown status for same day appointments",
         subtitle = "DNA and Unknown proportion increases as delay increases") +
    facet_wrap(~ hcp_type) 

# Time series Analysis ----

## All Appointments ----

total_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>%  
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    )

total_daily_tbl %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Time Series Plot: Total Daily Appointments")

total_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: Total Weekly Appointments")

total_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: Total Monthly Appointments")



## Appointment booking: advance vs same-day ----
same_day_vs_booked_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type %in% c("GP", "Other Practice staff") ,
           time_between_book_and_appt != "Unknown") %>%
    mutate(booking =
               if_else(time_between_book_and_appt == "Same Day", "Same Day", "Advance")) %>%
    group_by(hcp_type, booking, appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

same_day_vs_booked_daily_tbl %>%
    group_by(hcp_type, booking) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = total_appointments,
        .smooth = T, 
        .facet_ncol = 2
    )

# Autocorrelation Functions ACF/PACF ----

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        total_appointments = sum(appointments)
    ) %>%
    plot_acf_diagnostics(
        .date_var = appointment_date,
        .value = total_appointments,
        # .lags = 120,
        .interactive = TRUE,
        .title = "Demonstrates presence of autocorrelation and 5-day week seasonality"
    )


# Cross Correlation (CCF) ----

# TODO: need new variables to add in ?lagged indicators

# Seasonality ----

gp_same_day_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "month.lbl", "year"),
        .title = "Total GP Appointments",
        .facet_vars = appt_mode,
        .interactive = FALSE
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

# Anomalies ----

gp_same_day_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
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
        .title = "Troughs for total apppointments when weeks that contain holiday "
    )

gp_same_day_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
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

# Seasonal Decomposition ----

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "7 days")

# grouped time series

gp_same_day_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    group_by(appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .frequency = "7 days")


# Time Series Regression Plot ----

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            as.numeric(appointment_date) +
            week(appointment_date) +
            month(appointment_date, label = TRUE),
        .show_summary = TRUE
    )

# Grouped Time Series
gp_same_day_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    group_by(appt_mode) %>%
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
            week(appointment_date),
        .show_summary = TRUE
    )

# Moving Average Forecasting ----

gp_same_day_daily_tbl

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

# TODO ----

## * Explore Same Day appointments ----
## * Create time series to handle bank holidays ----
## * Model mean daily appointments by week ----
