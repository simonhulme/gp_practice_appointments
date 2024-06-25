# DATA PREPARATION ----

# Load Libraries 

library(tidyverse)
library(timetk)

# Read the raw data
wakefield_daily_raw <- read_rds("00_data/raw/wakefield_daily_raw.rds")

# Handle implicitly missing values ----

## Generate series with no missing days ----

start_date <- min(wakefield_daily_raw$appointment_date)
end_date   <- max(wakefield_daily_raw$appointment_date)

all_days_wakefield_daily <- tk_make_timeseries(start_date = start_date, end_date = end_date)

## Expand dataset to contain all levels for all dates ----

wakefield_daily_expanded <- 
    wakefield_daily_raw %>%
    complete(appointment_date = all_days_wakefield_daily,
             hcp_type,
             appt_mode,
             appt_status,
             time_between_book_and_appt) %>%
    replace_na(list(count_of_appointments = 0))

## Check the expanded dataset ----
wakefield_daily_expanded %>% 
    mutate(day = day(appointment_date)) %>% 
    count(day) %>% 
    view()

# Convert data to 5 day working week ----

## create index for working week ----
five_day_index <-
    tk_make_weekday_sequence(
        start_date = start_date,
        end_date = end_date,
        remove_weekends = TRUE,
        remove_holidays = FALSE
    )

wakefield_5_day_tbl <-
    wakefield_daily_expanded %>% 
    filter(appointment_date %in% five_day_index) 

# Encode categorical variables as factors  ----

## collapse levels to handle duplication
## re-order where order relevant

wakefield_5_day_factors_tbl <-
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


# Subset Time Series to Exclude Missing Data ----

wakefield_working_week_daily <- 
    wakefield_5_day_factors_tbl %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2019-03", .end_date = "2024")

wakefield_working_week_daily %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date, appointments, .smooth_period = "12 months")

write_rds(wakefield_working_week_daily, "00_data/processed/wakefield_working_week_daily.rds")