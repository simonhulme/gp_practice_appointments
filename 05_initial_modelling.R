# Initial Modelling and Exploration

library(tidyverse)
library(timetk)

all_appointments <- read_rds("00_data/processed/wakefield_calendar_population_workforce.rds")

# Variance Reduction ----

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

gp_f2f_same_day_appointments %>% 
    ggplot(aes(count_of_appointments)) +
    geom_histogram(binwidth = 250, fill = "skyblue", color = "grey30") +
    theme_bw()

## No transformation ----

gp_f2f_same_day_appointments %>% 
    plot_time_series(.date_var = appointment_date, .value = count_of_appointments, .smooth_period = "12 months")

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



