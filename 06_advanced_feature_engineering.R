# Advanced Feature Engineering - Building and Testing Basic Linear Models

# Load Libraries
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)

# Load Data
appointments <- read_rds("00_data/processed/wakefield_daily_adjusted.rds")
events       <- read_rds("00_data/processed/wakefield_events.rds")

# Check the formula of the best model
best_model   <- read_rds("00_models/best_lm.rds")
best_model$terms %>% formula()

# Filter and Aggregate Data
gp_appts_tbl <- appointments %>%
    filter(hcp_type == "GP", appt_status == "Attended") %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08-01") %>% 
    summarise_by_time(.date_var = appointment_date, .by = "day", appointments = sum(count_of_appointments))

# Prepare Data
data_prepared_full_tbl <- gp_appts_tbl %>%
    bind_rows(future_frame(
        .data = .,
        .date_var = appointment_date,
        .length_out = 40
    )) %>%
    left_join(events, by = c("appointment_date" = "event_date")) %>%
    select(-weekend) %>%
    
    # add event lags
    tk_augment_lags(.value = c(bank_holiday, training),
                    .lags = 1:3) %>%
    
    # add lagged values
    tk_augment_lags(.value = appointments, .lags = 40) %>%
    
    # add lagged rolling features
    tk_augment_slidify(
        .value = appointments_lag40,
        .f = mean,
        .period = c(20, 40, 60),
        .align = "center",
        .partial = TRUE
    ) %>% 
    
    # remove missing values
    drop_na()

# Plot Time Series Data
data_prepared_full_tbl %>% plot_time_series(.date_var = appointment_date, .value = appointments)

# Split Data into Training and Testing Sets
train_test_tbl <- data_prepared_full_tbl %>% filter(!is.na(appointments))
forecast_tbl   <- data_prepared_full_tbl %>% filter(is.na(appointments))

splits <- train_test_tbl %>% time_series_split(date_var = appointment_date, assess = 40, cumulative = TRUE)
splits %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(.date_var = appointment_date, .value = appointments)

# Build Linear Models

model_spec_lm <- linear_reg() %>% set_engine("lm")

## Increase model complexity

## linear trend plus events

recipe_linear_trend_events <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num)  %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_linear_trend_events <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_linear_trend_events) %>% 
    fit(training(splits))

## spline model plus events

recipe_spline_events <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 4) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_spline_events <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spline_events) %>% 
    fit(training(splits))

calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_trend_events,
        workflow_fit_spline_events
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))

## add in time signature features

### linear trend

recipe_linear_events_time <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_linear_events_time <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_linear_events_time) %>% 
    fit(training(splits))

### splines

recipe_spline_events_time <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 4) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_spline_events_time <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spline_events_time) %>% 
    fit(training(splits))

calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_trend_events,
        workflow_fit_spline_events,
        workflow_fit_linear_events_time,
        workflow_fit_spline_events_time
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))

# event lags

### linear trend with event lag

recipe_linear_trend_events_time_lag <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_rm(appointment_date, contains("lag40")) %>%  
    step_naomit(starts_with("lag_"))

workflow_fit_linear_events_time_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_linear_trend_events_time_lag) %>% 
    fit(training(splits))

### splines with event lag

recipe_spline_events_time_lag <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 4) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_rm(appointment_date, contains("lag40")) %>% 
    step_naomit(contains("lag_"))

workflow_fit_spline_events_time_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spline_events_time_lag) %>% 
    fit(training(splits))

calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_events_time,
        workflow_fit_spline_events_time, 
        workflow_fit_linear_events_time_lag,
        workflow_fit_spline_events_time_lag
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl, conf_interval = 0.8)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))

## USE LAGGED ROLLING AVERAGE TO MODEL TREND RATHER THAN DATE

# use best model so far and remove any trend components and use lagged rolling averages instead

recipe_lagged_rolling_mean <-
    recipe(appointments ~. , training(splits)) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_rm(appointment_date, appointments_lag40) %>% 
    step_naomit(contains("lag_"))

recipe_lagged_rolling_mean %>% prep() %>% bake(new_data = NULL) %>% glimpse()

workflow_fit_lagged_rolling_mean <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_lagged_rolling_mean) %>% 
    fit(training(splits))

calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_events_time, 
        workflow_fit_linear_events_time_lag,
        workflow_fit_lagged_rolling_mean
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl, conf_interval = 0.9)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))



