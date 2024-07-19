# Initial Modelling and Exploration

library(tidyverse)
library(modeltime)
library(timetk)
library(forecast)

all_appointments <- read_rds("00_data/processed/wakefield_calendar_population_workforce.rds")

gp_f2f_same_day_appointments <-
    all_appointments %>%
    filter(
        hcp_type == "GP",
        appt_mode == "Face-to-Face",
        appt_status == "Attended",
        time_between_book_and_appt == "Same Day"
    ) %>% 
    select(-c(hcp_type, contains("appt")))

# Variance Reduction ----

## No transformation ----

gp_f2f_same_day_appointments %>%
    plot_time_series(.date_var = appointment_date,
                     .value = count_of_appointments,
                     .smooth_period = "12 months")

gp_f2f_same_day_appointments %>% 
    plot_time_series_regression(
        appointment_date,
        count_of_appointments ~ 
            wday(appointment_date, label = TRUE) +
            month(appointment_date, label = TRUE) +
            year(appointment_date) +
            registered_population +
            total_gp +
            total_nurse +
            total_dpc +
            holiday +
            training +
            pandemic,
        .show_summary = TRUE
    )

## heteroskedasticity: variance increases with time

## Log transformation of response ----

gp_f2f_same_day_appointments %>%
    plot_time_series(.date_var = appointment_date,
                     .value = log1p(count_of_appointments))

gp_f2f_same_day_appointments %>% 
    plot_time_series_regression(
        appointment_date,
        log1p(count_of_appointments) ~ 
            wday(appointment_date, label = TRUE) +
            month(appointment_date, label = TRUE) +
            year(appointment_date) +
            registered_population +
            holiday +
            training +
            pandemic,
        .show_summary = TRUE,
        .interactive  = TRUE
    )

## log transformation appears to model past data well

# Rolling and Smoothing ----

## Sliding / Rolling Functions ----

gp_f2f_same_day_appointments %>%
    mutate(
        count_of_appointments_rolling = count_of_appointments %>% slidify_vec(
        .f = mean,
        .period = 130,
        .align = "center", .partial = TRUE
    )) %>%
    pivot_longer(cols = contains("count_of_appointments"), names_to = "type") %>% 
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = type,
        .smooth = FALSE
    )

## LOESS Smoother ----

gp_f2f_same_day_appointments %>%
    mutate(
        count_of_appointments_smooth = count_of_appointments %>% 
            smooth_vec(
                period = 260
            )) %>%
    pivot_longer(cols = contains("count_of_appointments"), names_to = "type") %>% 
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = type,
        .smooth = FALSE
    )

## Moving Average Forecasting ----

gp_f2f_same_day_appointments %>%
    mutate(mavg_8 = slidify_vec(
        count_of_appointments,
        ~ median(.x, na.rm = TRUE),
        .period = 60,
        .align = "right"
    )) %>% 
    bind_rows(
        future_frame(., .length_out = 60)
    ) %>% 
    fill(mavg_8, .direction = "down") %>%
    pivot_longer(cols = c(count_of_appointments, mavg_8)) %>% 
    select(appointment_date, name, value) %>% 
    plot_time_series(appointment_date, .value = value, .color_var = name, .smooth = FALSE)

# Range Reduction ----

# FTE Staff

total_nurse_FTE <-
    gp_f2f_same_day_appointments %>%
    select(appointment_date, total_nurse)

total_nurse_FTE %>%
    plot_time_series(.date_var = appointment_date, .value = total_nurse)

total_nurse_FTE %>%
    plot_acf_diagnostics(.date_var = appointment_date,
                         .value = total_nurse,
                         .lags = 60)

# Imputation and Outlier Cleaning ----

## This data set has no missing data so will focus on anomaly/outlier detection

gp_f2f_same_day_appointments %>%
    select(appointment_date, count_of_appointments) %>%
    plot_anomaly_diagnostics(.date_var = appointment_date,
                             .value = count_of_appointments,
                             .title = "Low anomalies: Holiday/Training. High anomalies: Day after Holiday")

gp_f2f_same_day_appointments %>%
    select(appointment_date, count_of_appointments) %>%
    mutate(appointments_cleaned = ts_clean_vec(count_of_appointments, period = 5)) %>%
    pivot_longer(-appointment_date) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = name,
        .smooth = FALSE,
        .title = "Cleaning not appropriate as outliers are informative"
    )

## difference between values after Bank Holiday - observed vs. cleaned - indicates number of
## extra appointments expected after Bank Holiday closures 

# Lags and Differencing ----

## Explore lags ----
gp_f2f_same_day_appointments %>% 
    plot_acf_diagnostics(.date_var = appointment_date, count_of_appointments, .lags = 30)

## Regression Model with lags ----
gp_f2f_same_day_appointments %>% 
    tk_augment_lags(.value = count_of_appointments, .lags = c(1:5)) %>% 
    drop_na() %>% 
    mutate(across(contains("count"), log1p)) %>% 
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula  = count_of_appointments ~ 
            wday(appointment_date, label = TRUE) +
            month(appointment_date, label = TRUE) +
            year(appointment_date) +
            registered_population +
            holiday +
            training +
            pandemic +
            count_of_appointments_lag1 +
            count_of_appointments_lag2,
        .show_summary = TRUE,
        .interactive  = TRUE,
        .title = "Lag terms address peak after holiday (but not sure why)"
    )

## Differencing ----

## start looking at cumulative sum of appointments

differenced_data <- 
    gp_f2f_same_day_appointments %>%
    filter_by_time(.date_var = appointment_date,
                   .start_date = "start",
                   .end_date = "end") %>%
    select(appointment_date, count_of_appointments) %>%
    mutate(total_appointments = cumsum(count_of_appointments),
           differences        = diff_vec(count_of_appointments, lag = 1, difference = 1),
           differences_2      = diff_vec(differences, lag = 1, difference = 1)
    )
    
differenced_data %>% 
    pivot_longer(-appointment_date) %>% 
    group_by(name) %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = value,
                     .smooth = FALSE)

differenced_data %>%
    plot_acf_diagnostics(.date_var = appointment_date, .value = differences_2, .lags = 40)

## Fourier Series ----

gp_f2f_same_day_appointments %>% 
    plot_time_series(appointment_date, count_of_appointments)

gp_f2f_same_day_appointments %>% 
    plot_acf_diagnostics(appointment_date, count_of_appointments, .lags = 262)

gp_f2f_same_day_appointments %>%
    select(appointment_date, count_of_appointments, holiday, training, pandemic) %>%
    tk_augment_fourier(
        .date_var = appointment_date,
        .periods = c(7, 365),
        .K = 1
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = log1p(count_of_appointments) ~ as.numeric(appointment_date) + . - appointment_date,
        .show_summary = TRUE
    )
