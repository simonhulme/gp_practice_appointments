# Advanced Feature Engineering - Building and Testing Linear Models

# Load Libraries ----
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)
library(poissonreg)

# Load Data ----
appointments <- read_rds("00_data/processed/wakefield_daily_adjusted.rds")
events       <- read_rds("00_data/processed/wakefield_events.rds")

# Check formula of the best model ----
best_model   <- read_rds("00_models/best_lm.rds")
best_model$terms %>% formula()

# Filter and Aggregate Data
gp_appts_tbl <-
    appointments %>%
    filter(hcp_type == "GP", appt_status == "Attended") %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08-01") %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    )

# Prepare Data ----
data_prepared_full_tbl <- gp_appts_tbl %>%
    
    # extend into future (forecast horizon)
    bind_rows(future_frame(
        .data = .,
        .date_var = appointment_date,
        .length_out = 56
    )) %>%
    
    # add events (training and bank holidays)
    left_join(events, by = c("appointment_date" = "event_date")) %>%
    select(-weekend) %>%
    
    # add event lags
    tk_augment_lags(.value = c(bank_holiday, training),
                    .lags = 1:3) %>%
    
    # add lagged rolling mean values
    tk_augment_lags(.value = appointments, .lags = 56) %>%
    tk_augment_slidify(
        .value = appointments_lag56,
        .f = mean,
        .period = c(20, 40, 60),
        .align = "center",
        .partial = TRUE
    ) %>% 
    
    # remove rows with missing lagged values
    drop_na(-appointments)

# Split Data into Training and Testing Sets ----
train_test_tbl <- data_prepared_full_tbl %>% filter(!is.na(appointments))
forecast_tbl   <- data_prepared_full_tbl %>% filter(is.na(appointments))

splits <- train_test_tbl %>% time_series_split(date_var = appointment_date, assess = 40, cumulative = TRUE)
splits %>% tk_time_series_cv_plan() %>% plot_time_series_cv_plan(.date_var = appointment_date, .value = appointments)

# Build Linear Models ----

model_spec_lm <- 
    linear_reg(penalty = 0.01) %>% 
    set_engine("glmnet")

## Baseline Models: trend + events ----

### with linear trend ----

recipe_linear_trend_events <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num)  %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_linear_trend_events <- 
    workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_linear_trend_events) %>% 
    fit(training(splits))

### with spline ----

recipe_spline_events <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 3) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_spline_events <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spline_events) %>% 
    fit(training(splits))

### with polynomial ----

recipe_poly_events <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 2) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_poly_events <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_poly_events) %>% 
    fit(training(splits))

### with lagged rolling mean ----

recipe_roll_mean_events <-
    recipe(appointments ~. , training(splits)) %>% 
    step_rm(contains("bank_holiday_lag"), contains("training_lag"), appointments_lag40, appointment_date)
            
workflow_fit_roll_mean_events <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_roll_mean_events) %>% 
    fit(training(splits))

### Assess Accuracy ----
calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_trend_events,
        workflow_fit_spline_events,
        workflow_fit_poly_events,
        workflow_fit_roll_mean_events
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))

## Add in Time Features ----

### with linear trend ----

recipe_linear_events_time <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

recipe_linear_events_time %>% prep() %>% bake(new_data = NULL) %>% glimpse()

workflow_fit_linear_events_time <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_linear_events_time) %>% 
    fit(training(splits))

### with spline ----

recipe_spline_events_time <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_spline_events_time <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spline_events_time) %>% 
    fit(training(splits))

### with polynomial ----

recipe_poly_events_time <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

workflow_fit_poly_events_time <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_poly_events_time) %>% 
    fit(training(splits))

### with lagged rolling mean ----

recipe_roll_mean_events_time <-
    recipe(appointments ~. , training(splits)) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("bank_holiday_lag"), contains("training_lag"), appointments_lag40, appointment_date)

workflow_fit_roll_mean_events_time <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_roll_mean_events_time) %>% 
    fit(training(splits))

### Assess Accuracy ----
calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_events_time,
        workflow_fit_spline_events_time,
        workflow_fit_poly_events_time,
        workflow_fit_roll_mean_events_time
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))

## Add in Event Lags ----

### with linear trend ----

recipe_linear_trend_events_time_lag <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date, contains("lag56")) %>%  
    step_naomit(starts_with("lag_"))

workflow_fit_linear_events_time_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_linear_trend_events_time_lag) %>% 
    fit(training(splits))

### with spline ----

recipe_spline_events_time_lag <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date, contains("lag56")) %>% 
    step_naomit(contains("lag_"))

workflow_fit_spline_events_time_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spline_events_time_lag) %>% 
    fit(training(splits))

### with polynomial ----

recipe_poly_events_time_lag <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date, contains("lag56")) %>% 
    step_naomit(contains("lag_"))

workflow_fit_poly_events_time_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_poly_events_time_lag) %>% 
    fit(training(splits))

### with lagged rolling mean ----

recipe_roll_mean_events_time_lag <-
    recipe(appointments ~. , training(splits)) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointments_lag56, appointment_date)

workflow_fit_roll_mean_events_time_lag <- workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_roll_mean_events_time_lag) %>% 
    fit(training(splits))

### Assess Accuracy ----
calibration_tbl <-
    modeltime_table(
        workflow_fit_linear_events_time_lag,
        workflow_fit_spline_events_time_lag,
        workflow_fit_poly_events_time_lag,
        workflow_fit_roll_mean_events_time_lag
    ) %>%
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
    modeltime_forecast(new_data = testing(splits), actual_data = train_test_tbl) %>% 
    plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = testing(splits))

# Forecast Values ----

refit_tbl <-
    calibration_tbl %>%
    modeltime_refit(data = train_test_tbl)

refit_tbl %>%
    modeltime_forecast(new_data = forecast_tbl,
                       actual_data = train_test_tbl,
                       conf_interval = 0.95) %>% 
    mutate(wday = wday(.index, label = TRUE)) %>% 
    mutate(across(where(is.numeric), round)) %>% 
    filter(!wday %in% c("Sat", "Sun")) %>% 
    plot_modeltime_forecast()

forecast_values_tbl <- 
    refit_tbl %>%
    modeltime_forecast(new_data = forecast_tbl,
                       actual_data = train_test_tbl,
                       conf_interval = 0.95) %>% 
    mutate(wday = wday(.index, label = TRUE)) %>% 
    mutate(across(where(is.numeric), round)) %>% 
    filter(.model_id == 2, !wday %in% c("Sat", "Sun")) 

forecast_values_tbl %>% 
    plot_modeltime_forecast()

forecast_values_tbl %>% 
    mutate(date = as.Date(.index)) %>% 
    select(date, wday, .value, .conf_lo, .conf_hi)

           