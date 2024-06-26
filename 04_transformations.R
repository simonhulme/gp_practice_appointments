# Set Up ----

library(tidyverse)
library(timetk)

wakefield_working_week_daily <- read_rds("00_data/processed/wakefield_working_week_daily.rds")

# Calendar Adjustments ----

## Create Flags and adjust weekly values by using mean per day / session

## Apply to whole time series prior to creating more focused analysis sets 

## Bank Holidays, Training Days, Pandemic  ----

start_date <- min(wakefield_working_week_daily$appointment_date)
end_date   <- max(wakefield_working_week_daily$appointment_date)

holiday_dates <-
    tk_make_holiday_sequence(start_date = start_date,
                             end_date = end_date,
                             calendar = "LONDON")

training_dates_19 <- 
    c("2019-03-20", "2019-04-24", "2019-05-15", "2019-06-19",
      "2019-07-17", "2019-09-18", "2019-10-16", "2019-11-13")

training_dates_20 <- 
    c("2020-01-15", "2020-02-26", "2020-07-15", "2020-08-19", "2020-09-16", 
      "2020-10-14", "2020-11-11")

training_dates_21 <- 
    c("2021-01-13", "2021-02-24", "2021-03-17", "2021-04-21", "2021-05-12",
      "2021-06-16", "2021-07-14", "2021-09-15", "2021-10-13", "2021-11-10")

training_dates_22 <- 
    c("2022-02-23", "2022-03-16", "2022-04-06", "2022-05-11","2022-06-15", 
      "2022-07-13", "2022-09-14", "2022-10-12", "2022-11-09")

training_dates_23 <- 
    c("2023-02-08", "2023-04-19", "2023-05-17", "2023-06-21", "2023-07-12", 
      "2023-08-16", "2023-09-13", "2023-10-11", "2023-11-15")

training_dates_24 <- 
    c("2024-01-17", "2024-02-21", "2024-03-13", "2024-04-17", "2024-05-15",
      "2024-06-19", "2024-07-10", "2024-09-18", "2024-10-16", "2024-11-13")

training_dates <-
    as.Date(c(
        training_dates_19,
        training_dates_20,
        training_dates_21,
        training_dates_22,
        training_dates_23,
        training_dates_24
    ))

pandemic_dates <- 
  tk_make_timeseries(start_date = "2020-03-23", end_date = "2021-07-19", by = "day")

wakefield_working_week_with_calendar_adjustments <- 
  wakefield_working_week_daily %>%
  mutate(holiday = as_factor(if_else(appointment_date %in% holiday_dates, "Yes", "No")),
         training = if_else(appointment_date %in% training_dates, "Yes", "No"),
         pandemic = as_factor(if_else(appointment_date %in% pandemic_dates, "Yes", "No")))

## Check if new feature map observed data appropriately using a linear regression model

## Focus on GP face to face same day appointments attended

gp_f2f_same_day_attended <- 
  wakefield_working_week_with_calendar_adjustments %>%
    filter(hcp_type == "GP",
           time_between_book_and_appt == "Same Day",
           appt_mode == "Face-to-Face",
           appt_status == "Attended") 

gp_f2f_same_day_attended %>%
  plot_time_series_regression(
    .date_var = appointment_date,
    .formula = count_of_appointments ~
      as.numeric(appointment_date) +
      wday(appointment_date, label = TRUE) +
      week(appointment_date) +
      month(appointment_date, label = TRUE) +
      year(appointment_date) +
      holiday +
      training +
      pandemic,
    .show_summary = TRUE
  )
 
## Adjust weekly figure to mean appointments per session ----

gp_f2f_same_day_attended_mean_weekly <-
  wakefield_working_week_with_calendar_adjustments %>%
  filter(
    hcp_type == "GP",
    time_between_book_and_appt == "Same Day",
    appt_mode == "Face-to-Face",
    appt_status == "Attended"
  ) %>%
  mutate(sessions = case_when(holiday == "Yes" ~ 0,
                              training == "Yes" ~ 1,
                              TRUE ~ 2)) %>%
  mutate_by_time(
    .date_var = appointment_date,
    .by = "week",
    sessions_per_week = sum(sessions)
  ) %>%
  summarise_by_time(
    .date_var = appointment_date,
    .by = "week",
    mean_per_session = mean(sum(count_of_appointments) / sessions_per_week)
  )

gp_f2f_same_day_attended_mean_weekly %>%
  plot_time_series(.date_var = appointment_date,
                   .value = mean_per_session,
                   .smooth = TRUE,
                   .title = "GP f2f same day attended appts - adj for Bank Holidays and TARGET")
  
# PROGRESS UP TO HERE ----

## Variance Reduction ----

gp_f2f_same_day_attended_weekly <- 
    gp_f2f_same_day_attended %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        count_of_appointments = sum(count_of_appointments)
    ) 

gp_f2f_same_day_attended_weekly %>%
    plot_time_series(.date_var = appointment_date,
                     .value = count_of_appointments,
                     .title = "GP f2f same day appointments attended weekly")

### untransformed ----

glm_fitted <- 
    lm(count_of_appointments ~
           as.numeric(appointment_date) +
           month(appointment_date, label = TRUE) +
           year(appointment_date),
        data = gp_f2f_same_day_attended_weekly)

summary(glm_fitted)

lmtest::bptest(
    glm_fitted,
    data = gp_f2f_same_day_attended_weekly
)

broom::augment(glm_fitted, gp_f2f_same_day_attended_weekly) %>% 
    select(appointment_date, count_of_appointments, .fitted) %>% 
    mutate(.resid  = count_of_appointments - .fitted) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

### log transformed ----

glm_log_fitted <- 
    lm(log1p(count_of_appointments) ~
           as.numeric(appointment_date) +
           week(appointment_date) +
           month(appointment_date, label = TRUE) +
           year(appointment_date),
       data = gp_f2f_same_day_attended_weekly)

summary(glm_log_fitted)

lmtest::bptest(
    glm_log_fitted,
    data = gp_f2f_same_day_attended_weekly
)

broom::augment(glm_log_fitted, gp_f2f_same_day_attended_weekly) %>% 
    select(appointment_date, count_of_appointments, .fitted) %>% 
    mutate(.fitted = exp(.fitted),
           .resid  = count_of_appointments - .fitted) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

###  box cox transformed ---- 

glm_boxcox_fitted <- 
    lm(box_cox_vec(count_of_appointments, lambda = "auto") ~
           as.numeric(appointment_date) +
           month(appointment_date, label = TRUE) +
           factor(year(appointment_date)),
       data = gp_f2f_same_day_attended_weekly)

summary(glm_boxcox_fitted)

lmtest::bptest(
    glm_boxcox_fitted,
    data = gp_f2f_same_day_attended_weekly
)

broom::augment(glm_boxcox_fitted, gp_f2f_same_day_attended_weekly) %>% 
    select(appointment_date, count_of_appointments, .fitted) %>% 
    mutate(.fitted = box_cox_inv_vec(.fitted, lambda = 0.358388489030188),
           .resid  = count_of_appointments - .fitted) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

broom::augment(glm_boxcox_fitted, gp_f2f_same_day_attended_weekly) %>% 
    select(appointment_date, count_of_appointments, .fitted) %>% 
    mutate(.fitted = box_cox_inv_vec(.fitted, lambda = 0.358388489030188),
           .resid  = count_of_appointments - .fitted) %>% 
    ggplot(aes(.resid)) +
    geom_histogram(fill = "steelblue", color = "grey30") +
    theme_bw()


# Population ----

# TODO:
# Adjust for changes in population

# Workforce ----

# TODO
# Adjust for staffing levels



# Moving averages and smoothing transformation ----

## Moving / Rolling Averages ----

### Total Appointments Monthly
all_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>% 
    mutate(appointments_roll = slidify_vec(appointments, mean, .period = 12, .align = "center")) %>%
    pivot_longer(contains("appointments"), names_to = "type") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = type,
        .smooth = FALSE
    )

### Mean Same Day GP Appt per day by mode - Weekly
same_day_attended_gp_weekly_mean_by_mode_tbl <-
    all_appointments_daily_tbl %>%
    filter(hcp_type == "GP",
           ! appt_mode %in% c("Unknown", "Video Conference/Online"),
           time_between_book_and_appt == "Same Day",
           appt_status == "Attended") %>%
    group_by(appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        mean_daily_appointments = mean(appointments)
    ) 

same_day_attended_gp_weekly_mean_by_mode_tbl %>%
    mutate(
        rolled_mean_appointments = slidify_vec(
            mean_daily_appointments,
            mean,
            .period = 52,
            .align = "center"
        )
    ) %>%
    pivot_longer(contains("mean"), names_to = "type") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = type,
        .smooth = FALSE
    ) 

## Smoothing using LOESS ----

### Total Appointments Monthly
all_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>% 
    mutate(appointments_smooth = smooth_vec(appointments, period = 12)) %>%
    pivot_longer(contains("appointments"), names_to = "type") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = type,
        .smooth = FALSE
    )

### Mean Same Day GP Appt per day by mode - Weekly

same_day_attended_gp_weekly_mean_by_mode_tbl %>%
    mutate(
        smooth_mean_appointments = smooth_vec(
            mean_daily_appointments,
            period = 52
        )
    ) %>%
    pivot_longer(contains("mean"), names_to = "type") %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = type,
        .smooth = FALSE, 
        .title = "Mean Daily Same Day Face-to-Face GP Appointments Attended per Week"
    ) 

## Revisit Decomposition using STL ----

all_appointments_daily_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(.date_var = appointment_date,
                         .value = appointments, 
                         .trend = "1 year",
                         .frequency = "1 week", 
                         .feature_set = c("observed", "season", "trend", "remainder"))


## Explore other features from data ----

# TODO


