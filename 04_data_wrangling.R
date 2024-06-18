library(tidyverse)
library(timetk)

all_appointments_daily_tbl <- read_rds("00_data/processed/wakefield_daily_prepared.rds")

all_appointments_daily_tbl$appointment_date %>% summary()

# Add Holiday Sequence ----

holidays <- 
    tk_make_holiday_sequence(start_date = "2019-03-01", end_date = "2024-04-30", calendar = "LONDON")

daily_appts_with_holidays <- 
    all_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    mutate(holiday = as_factor(if_else(appointment_date %in% holidays, "Yes", "No")))

daily_appts_with_holidays %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ 
            day(appointment_date) +
            week(appointment_date) +
            month(appointment_date, label = TRUE): holiday +
            year(appointment_date),
        .show_summary = TRUE)


## practice forecasting

model_fit_glm <- 
    glm(appointments ~
           wday(appointment_date, label = TRUE) +
           month(appointment_date, label = TRUE) + 
           year(appointment_date) * holiday, 
       data = daily_appts_with_holidays, family = "poisson")

future_tbl <-
    daily_appts_with_holidays %>%
    future_frame(.date_var = appointment_date, .length_out = "12 months") %>%
    filter(appointment_date %in%
               tk_make_weekday_sequence(
                   start_date = min(appointment_date),
                   end_date   = max(appointment_date)
               )) %>%
    mutate(
        holiday = if_else(
            appointment_date %in%
                tk_make_holiday_sequence(
                    start_date = min(appointment_date),
                    end_date = max(appointment_date),
                    calendar = "LONDON"
                ),
            "Yes",
            "No"
        ),
        holiday = as_factor(holiday)
    ) 

predictions_vec <- 
    predict(model_fit_glm, future_tbl) %>% 
    exp() %>% 
    unname()

daily_appts_with_holidays %>%
    select(appointment_date, appointments) %>%
    add_column(type = "actual") %>%
    bind_rows(future_tbl %>%
                  mutate(appointments = predictions_vec,
                         type = "predicted")) %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2023-04-01") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .color_var = type,
        .smooth = FALSE
    )

