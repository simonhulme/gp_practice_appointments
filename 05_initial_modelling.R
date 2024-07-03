# Initial Modelling and Exploration

library(tidyverse)
library(modeltime)
library(timetk)
library(forecast)

all_appointments <- read_rds("00_data/processed/wakefield_calendar_population_workforce.rds")

# explore gp f2f same day attended workload 

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
    plot_time_series(.date_var = appointment_date, .value = log1p(count_of_appointments))

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

## Power transformation (Box Cox) ----

gp_f2f_same_day_appointments %>%
    plot_time_series(.date_var = appointment_date,
                     .value = box_cox_vec(count_of_appointments + 1, lambda = "auto"))

gp_f2f_same_day_appointments %>% 
    mutate(count_of_appointments = box_cox_vec(count_of_appointments + 1, lambda = "auto")) %>% 
    plot_time_series_regression(
        appointment_date,
        count_of_appointments ~ 
            wday(appointment_date, label = TRUE) +
            month(appointment_date, label = TRUE) +
            registered_population +
            holiday +
            training +
            total_gp +
            pandemic,
        .show_summary = TRUE
    )

## Box Cox transformation makes model worse 

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

# Other Simple Forecasting Methods ----

## Visualisation----

gp_f2f_same_day_appointments %>% 
    select(appointment_date, appointments = count_of_appointments) %>% 
    mutate(lag_1 = lag(appointments),
           diff  = appointments - lag_1) %>% 
    plot_time_series(.date_var = appointment_date, .value = diff)

gp_f2f_same_day_appointments %>% 
    select(appointment_date, appointments = count_of_appointments) %>% 
    mutate(lag_1 = lag(appointments),
           diff  = appointments - lag_1) %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .value = diff, .lags = 260)


## Benchmark Methods ----

### Mean Method ----

gp_f2f_same_day_appointments %>%
    select(appointment_date, appointments = count_of_appointments) %>% 
    mutate(mean_appointments = mean(appointments)) %>% 
    bind_rows(future_frame(., .date_var = appointment_date, .length_out = 65)) %>% 
    fill(mean_appointments, .direction = "down") %>%
    pivot_longer(-appointment_date) %>% 
    plot_time_series(.date_var = appointment_date, value, name, .smooth = FALSE )

### Simple Naive Method ----

gp_f2f_same_day_appointments %>% 
    select(appointment_date, appointments = count_of_appointments) %>% 
    bind_rows(future_frame(., .date_var = appointment_date, .length_out = 65)) %>% 
    mutate(naive_fc = forecast::naive(appointments, 1) %>% .$fitted) %>% 
    pivot_longer(-appointment_date) %>% 
    plot_time_series(.date_var = appointment_date, value, name, .smooth = FALSE)

### Seasonal Naive Method ---

gp_f2f_same_day_appointments %>% 
    select(appointment_date, appointments = count_of_appointments) %>% 
    # bind_rows(future_frame(., .date_var = appointment_date, .length_out = 65)) %>% 
    mutate(seasonal_lag = lag(appointments, 5)) %>% 
    bind_rows(future_frame(., .date_var = appointment_date, .length_out = 65)) %>% tail(70)
    pivot_longer(-appointment_date) %>% 
    plot_time_series(.date_var = appointment_date, value, name, .smooth = FALSE)

### Naive method with drift

## TODO
    
    
# Range Reduction ----
    
    gp_f2f_same_day_appointments %>%
        select(
            appointment_date,
            contains("total")
        ) %>%
        mutate(across(where(is.numeric), standardize_vec)) %>%
        pivot_longer(-appointment_date) %>%
        plot_time_series(
            .date_var = appointment_date,
            .value = value,
            .color_var = name,
            .smooth = FALSE
        )

# FTE Staff ----
    

    total_nurse_FTE <- 
        gp_f2f_same_day_appointments %>% 
        select(appointment_date, total_nurse)
    
    total_nurse_FTE %>% 
        plot_time_series(.date_var = appointment_date, .value = total_nurse)
    
    total_nurse_FTE %>% 
        plot_acf_diagnostics(.date_var = appointment_date, .value = total_nurse, .lags = 60)
    
    total_nurse_FTE %>%
        plot_time_series(.date_var = appointment_date, .value = diff)
    
    total_nurse_FTE %>% 
        plot_acf_diagnostics(.date_var = appointment_date, .value = diff, .lags = 60)
    
   


    ## Is this an AR1    
    
    total_nurse_FTE_ts <- 
        as.ts(total_nurse_FTE$total_nurse)
    
    ar.mle(total_nurse_FTE_ts)
    
    ts_sim <- tibble(appts  = rep(154.22, 956),
                     ar_sim = arima.sim(model = list(ar = 0.9965), n = 956),
                     adjusted_appts = appts + ar_sim) %>% 
        mutate(appointment_date = total_nurse_FTE$appointment_date)
    
    ts_sim %>% 
        plot_time_series(.date_var = appointment_date, adjusted_appts)

    mean(ts_sim$adjusted_appts)

    ts_sim %>% 
        plot_acf_diagnostics(.date_var = appointment_date, adjusted_appts, .lags = 60)
    