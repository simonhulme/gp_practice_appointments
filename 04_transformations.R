# Transformations for improved models ----




library(tidyverse)
library(timetk)

all_appointments_daily_tbl <- read_rds("00_data/processed/wakefield_working_week_daily.rds")

## Transformations and adjustments ----

### Calendar Adjustments ----

# use mean daily appointments weekly / monthly
# to reflect workload around Bank Holidays

all_appointments_daily_tbl %>%
    # filter(hcp_type == "GP", appt_mode == "Face-to-Face", appt_status == "Attended") %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        mean_daily_appointments = sum(appointments)
    ) %>% 
    plot_time_series(.date_var = appointment_date, .value = mean_daily_appointments)

### Population Adjustments ----

# TODO:
# Adjust for changes in population
# With Workforce data could also adjust for staffing levels


### Variance Reduction ----

# Methods to handle outliers and heteroskedasticity 

gp_appointments_weekly <- 
    all_appointments_daily_tbl %>%
    filter(hcp_type == "GP") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) 

gp_appointments_weekly %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

# fit linear model to untransformed data

glm_fitted <- 
    lm(appointments ~
            as.numeric(appointment_date) +
            month(appointment_date, label = TRUE) +
            year(appointment_date),
        data = gp_appointments_weekly)

summary(glm_fitted)

lmtest::bptest(
    glm_fitted,
    data = gp_appointments_weekly
)

broom::augment(glm_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.resid  = appointments - .fitted) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

broom::augment(glm_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.resid  = appointments - .fitted) %>% 
    ggplot(aes(.resid)) +
    geom_histogram(fill = "steelblue", color = "grey30") +
    theme_bw()


# repeat using log transformed response 

glm_log_fitted <- 
    lm(log1p(appointments) ~
            as.numeric(appointment_date) +
            month(appointment_date, label = TRUE) +
            year(appointment_date),
        data = gp_appointments_weekly)

summary(glm_log_fitted)

lmtest::bptest(
    glm_log_fitted,
    data = gp_appointments_weekly
)

broom::augment(glm_log_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.fitted = exp(.fitted),
           .resid  = appointments - .fitted) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

broom::augment(glm_log_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.fitted = exp(.fitted),
           .resid  = appointments - .fitted) %>% 
    ggplot(aes(.resid)) +
    geom_histogram(fill = "steelblue", color = "grey30") +
    theme_bw()

# repeat using box cox transformed response 

glm_boxcox_fitted <- 
    lm(box_cox_vec(appointments, lambda = "auto") ~
           as.numeric(appointment_date) +
           month(appointment_date, label = TRUE) +
           factor(year(appointment_date)),
       data = gp_appointments_weekly)

summary(glm_boxcox_fitted)

lmtest::bptest(
    glm_boxcox_fitted,
    data = gp_appointments_weekly
)

broom::augment(glm_boxcox_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.fitted = box_cox_inv_vec(.fitted, lambda = 1.99992424816297),
           .resid  = appointments - .fitted) %>% 
    ggplot(aes(.fitted, .resid)) +
    geom_point() +
    geom_smooth() +
    theme_bw()

broom::augment(glm_boxcox_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.fitted = box_cox_inv_vec(.fitted, lambda = 1.99992424816297),
           .resid  = appointments - .fitted) %>% 
    ggplot(aes(.resid)) +
    geom_histogram(fill = "steelblue", color = "grey30") +
    theme_bw()

broom::augment(glm_boxcox_fitted, gp_appointments_weekly) %>% 
    select(appointment_date, appointments, .fitted) %>% 
    mutate(.fitted = box_cox_inv_vec(.fitted, lambda = 1.99992424816297)) %>% 
    pivot_longer(cols = -appointment_date) %>% 
    ggplot(aes(appointment_date, value, color = name)) +
    geom_line() +
    theme_bw()


# Add Holiday Sequence ----

holidays <-
    tk_make_holiday_sequence(start_date = "2019-03-01",
                             end_date = "2024-04-30",
                             calendar = "LONDON")

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

## may be better manually modelling this using glm and poisson regression

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

# Add Custom Training Half-Day Sequence (TARGET) ----




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


