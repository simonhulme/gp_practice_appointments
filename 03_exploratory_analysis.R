# Exploratory Data Analysis ----

# Set Up

library(tidyverse)
library(timetk)
# library(DataExplorer)

wakefield_working_week_daily <-
    read_rds("00_data/processed/wakefield_working_week_daily.rds")

# Basic Analysis ----

## Summary Statistics ----

skimr::skim(wakefield_working_week_daily)

## Exploratory Visualisation ----

### Overall Total Appointments by Feature ----

wakefield_working_week_daily %>%
    group_by(hcp_type) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(hcp_type = fct_reorder(hcp_type, -total_appointments)) %>%
    ggplot(aes(hcp_type, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw() +
    ggtitle("Only a small proportion of appointments have unknown HCP type")

wakefield_working_week_daily %>%
    group_by(appt_mode) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(appt_mode = fct_reorder(appt_mode, -total_appointments)) %>%
    ggplot(aes(appt_mode, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw() +
    ggtitle("'Face-to-Face' and 'Telephone' account for most appointments")

wakefield_working_week_daily %>%
    group_by(appt_status) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(prop = proportions(total_appointments)) %>%
    mutate(appt_status = fct_reorder(appt_status, -total_appointments)) %>%
    ggplot(aes(x = factor(1), y = prop, fill = appt_status)) +
    geom_col() +
    theme_bw() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    scale_fill_brewer(palette = 8) +
    labs(title = "3.3% of appointments recorded as DNA",
         x = "",
         y = "Proportion",
         fill = "Appointment status")

wakefield_working_week_daily %>%
    group_by(time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw() +
    ggtitle("'Same Day' accounts for most appointments") +
    xlab("Time between booking and appointment date") +
    coord_flip()

### Bivariate analysis ----

#### Appointment Mode vs Time Between Booking and Appointment (GP appointments)
wakefield_working_week_daily %>%
    filter(hcp_type == "GP") %>%
    group_by(appt_mode, time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments, fill = appt_mode)) +
    geom_col(position = "fill") +
    theme_bw() +
    labs(title = "Increasing proportion telephone appts for appts booked more recently")

#### Time Between Booking and Appointment vs Appt Status/DNA (GP appointments)
wakefield_working_week_daily %>%
    filter(hcp_type == "GP") %>%
    mutate(appt_status = fct_relevel(appt_status, c("Attended", "Unknown", "DNA"))) %>%
    group_by(appt_status, time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments, fill = appt_status)) +
    geom_col(position = "fill") +
    theme_bw() +
    scale_fill_brewer(palette = 9) +
    labs(title = "Very few DNA for same day appointments",
         subtitle = "DNA and Unknown proportion increases as delay increases")

# Time series Exploratory Analysis ----

## Series 1: All GP Appointments

gp_total_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type == "GP") %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    )

gp_total_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: All GP Appointments")

gp_total_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: All GP Appointments")

gp_total_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: All GP Appointments")


## Create new dataset with focus on all same day GP appointments ----
gp_same_day_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type == "GP", time_between_book_and_appt == "Same Day") %>%
    group_by(appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

## Time Series Plots ----

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, .value = total_appointments)

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, .value = total_appointments)

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, .value = total_appointments)

## Autocorrelation Functions ACF/PACF ----

gp_same_day_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        total_appointments = sum(appointments)
    ) %>%
    plot_acf_diagnostics(
        .date_var = appointment_date,
        .value = total_appointments,
        .lags = 120,
        .interactive = TRUE,
        .title = "Demonstrates presence of autocorrelation and 5-day week seasonality"
    )


## Cross Correlation (CCF) ----

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
        .by = "month",
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


# FINDINGS ----

## 1. Same Day appointments are the frequent type ----
## 2. Bank Holidays result in lower total weekly appointments but daily rates may be high ----

# TODO ----

## * Explore Same Day appointments ----
## * Create time series to handle bank holidays ----
## * Model mean daily appointments by week ----
