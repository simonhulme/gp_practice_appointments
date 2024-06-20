# DATA PREPARATION ----

## Set Up ----

# Load Libraries 

library(tidyverse)
library(timetk)

# Import Data

wakefield_daily_raw <- read_rds("00_data/raw/wakefield_daily_raw.rds")

## Convert data to 5 day business week ----

new_index <-
    tk_make_weekday_sequence(
        start_date = min(wakefield_daily_raw$appointment_date),
        end_date = max(wakefield_daily_raw$appointment_date),
        remove_weekends = TRUE,
        remove_holidays = FALSE
    )

wakefield_5_day_tbl <- 
    wakefield_daily_raw %>% 
    filter(appointment_date %in% new_index)

wakefield_5_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .interactive = FALSE,
                     .smooth_period = "12 months") +
    scale_x_date(
        breaks = new_index,
        date_breaks = "year",
        expand = c(0, 0),
        labels = year
    )

## Encode categorical variables as factors  ----

## collapse levels to handle duplication
## re-order levels where order important

wakefield_5_day_factorised_tbl <-
    wakefield_5_day_tbl  %>%
    mutate(across(where(is_character), as_factor)) %>% 
    mutate(
        time_between_book_and_appt = fct_relevel(
            time_between_book_and_appt,
            c(
                "Same Day",
                "1 Day",
                "2 to 7 Days",
                "8  to 14 Days",
                "15  to 21 Days",
                "22  to 28 Days",
                "More than 28 Days",
                "Unknown / Data Issue",
                "Unknown / Data Quality"
            )
        ),
        time_between_book_and_appt = fct_collapse(
            time_between_book_and_appt,
            "Unknown" = c("Unknown / Data Issue", "Unknown / Data Quality")
        ),
        appt_status = fct_collapse(
            appt_status,
            "Unknown" = c("Unknown", "Appt Status Not Provided")
        )
    )


## Slice Time Series to Exclude Missing Data ----

## hcp type

wakefield_5_day_factorised_tbl %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

wakefield_5_day_factorised_tbl %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024") %>% 
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)


### Appointment Mode ----

wakefield_5_day_factorised_tbl %>%
    group_by(appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

wakefield_5_day_factorised_tbl %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024") %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)


### Appointment Status ----

wakefield_5_day_factorised_tbl %>%
    group_by(appt_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

wakefield_5_day_factorised_tbl %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024") %>% 
    group_by(appt_status) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

### Time between booking and appointment ----

wakefield_5_day_factorised_tbl %>%
    group_by(time_between_book_and_appt) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .facet_ncol = 2)

wakefield_5_day_factorised_tbl %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024") %>% 
    group_by(time_between_book_and_appt) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments,  .facet_ncol = 2)

### Prepare data for exploratory analysis ----

wakefield_daily_prepared <- 
    wakefield_5_day_factorised_tbl %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024")

wakefield_daily_prepared %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, appointments, .smooth_period = "12 months")

write_rds(wakefield_daily_prepared, "00_data/processed/wakefield_daily_prepared.rds")