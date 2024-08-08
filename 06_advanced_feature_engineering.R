# Advanced Feature Engineering - Building and Testing Linear Models

# Load Libraries ----
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)

# Load Data ----

objects <- read_rds("00_data/processed/gp_appts_attended_objects.rds")

# Extract Data ----

data_prepared_full_tbl <- objects$data$data_prepared_full_tbl
train_test_tbl         <- objects$data$train_test_tbl
train_tbl              <- objects$data$train_tbl
test_tbl               <- objects$data$test_tbl
forecast_tbl           <- objects$data$forecast_tbl

# Build Linear Models ----

##  Check formula of the best model ----
best_model   <- read_rds("00_models/best_lm.rds")
best_model$terms %>% formula()

## Define model spec ----
model_spec_glmnet <- 
    linear_reg(penalty = 0.01) %>% 
    set_engine("glmnet")

## Trend plus Events ----

### with linear trend ----

events_linear_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num)  %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_linear <- 
    workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_linear_recipe) %>% 
    fit(train_tbl)

### with non-linear trend ----
#### with 3 splines ----

events_splines3_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 3) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_splines3 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_splines3_recipe) %>% 
    fit(train_tbl)

#### with 4 splines ----

events_splines4_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 4) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_splines4 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_splines4_recipe) %>% 
    fit(train_tbl)

#### with 2nd degree polynomial ----

events_polynomial2_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 2) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_polynomial2 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_polynomial2_recipe) %>% 
    fit(train_tbl)

#### with 3rd degree polynomial ----

events_polynomial3_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 3) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_polynomial3 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_polynomial3_recipe) %>% 
    fit(train_tbl)

### Assess Accuracy ----
calibration_tbl <-
    modeltime_table(
        fitted_events_linear, 
        fitted_events_splines3,
        fitted_events_splines4,
        fitted_events_polynomial2,
        fitted_events_polynomial3
        ) %>%
    modeltime_calibrate(new_data = test_tbl)

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = test_tbl, actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = test_tbl)

## Add in Time Features ----

### with linear trend ----

events_time_linear_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_time_linear <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_time_linear_recipe) %>% 
    fit(train_tbl)

### with non-linear trend ----

#### with 3 splines ----

events_time_splines3_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_time_splines3 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_time_splines3_recipe) %>% 
    fit(train_tbl)

#### with 4 splines ----

events_time_splines4_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 4) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_time_splines4 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_time_splines4_recipe) %>% 
    fit(train_tbl)

#### with 2nd degree polynomial ----

events_time_polynomial2_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 2) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_time_polynomial2 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_time_polynomial2_recipe) %>% 
    fit(train_tbl)

#### with 3rd degree polynomial ----

events_time_polynomial3_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(contains("lag"), appointment_date)

fitted_events_time_polynomial3 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_time_polynomial3_recipe) %>% 
    fit(train_tbl)

### Assess Accuracy ----
calibration_tbl <-
    modeltime_table(
        fitted_events_linear,
        fitted_events_time_linear, 
        fitted_events_time_splines3,
        fitted_events_time_splines4,
        fitted_events_time_polynomial2,
        fitted_events_time_polynomial3
    ) %>%
    modeltime_calibrate(new_data = test_tbl)

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = test_tbl, actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = test_tbl)

## Add in Event Lags ----

### with linear trend ----

events_lag_time_linear_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date)

fitted_events_lag_time_linear <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_lag_time_linear_recipe) %>% 
    fit(train_tbl)

### with non-linear trend ----

#### with 3 splines ----

events_lag_time_splines3_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date)

fitted_events_lag_time_splines3 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_lag_time_splines3_recipe) %>% 
    fit(train_tbl)

#### with 4 splines ----

events_lag_time_splines4_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_ns(index_num, deg_free = 4) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date)

fitted_events_lag_time_splines4 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_lag_time_splines4_recipe) %>% 
    fit(train_tbl)

#### with 2nd degree polynomial ----

events_lag_time_polynomial2_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 2) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date)

fitted_events_lag_time_polynomial2 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_lag_time_polynomial2_recipe) %>% 
    fit(train_tbl)

#### with 3rd degree polynomial ----

events_lag_time_polynomial3_recipe <- 
    recipe(appointments_per_1k_trans ~. , train_tbl) %>% 
    step_mutate(index_num = as.numeric(appointment_date)) %>% 
    step_normalize(index_num) %>% 
    step_poly(index_num, degree = 3) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_rm(appointment_date)

fitted_events_lag_time_polynomial3 <- workflow() %>% 
    add_model(model_spec_glmnet) %>% 
    add_recipe(events_lag_time_polynomial3_recipe) %>% 
    fit(train_tbl)

### Assess Accuracy ----
calibration_tbl <-
    modeltime_table(
        fitted_events_time_linear,
        fitted_events_lag_time_linear,
        fitted_events_lag_time_splines3,
        fitted_events_lag_time_splines4,
        fitted_events_lag_time_polynomial2,
        fitted_events_lag_time_polynomial3
    ) %>%
    modeltime_calibrate(new_data = test_tbl)

forecast_tbl <- calibration_tbl %>%
    modeltime_forecast(new_data = test_tbl, actual_data = train_test_tbl)

forecast_tbl %>% plot_modeltime_forecast()

calibration_tbl %>% modeltime_accuracy(new_data = test_tbl)

## Add in Fourier Terms ----





# Forecasting ----
refit_tbl <-
    calibration_tbl %>%
    modeltime_refit(data = train_test_tbl)

forecast_values_tbl <- 
    refit_tbl %>%
    modeltime_forecast(new_data = forecast_tbl,
                       actual_data = train_test_tbl,
                       conf_interval = 0.68) %>% 
    
    # correct for weekdays only
    mutate(wday = wday(.index, label = TRUE)) %>% 
    filter(!wday %in% c("Sat", "Sun")) %>% 
    select(-wday) %>% 
    
    # invert transformation
    mutate(across(.value:.conf_hi, 
           ~ standardize_inv_vec(.x, 
                                 mean = objects$params$standardise$mean,
                                 sd   = objects$params$standardise$sd))) %>% 
    mutate(across(.value:.conf_hi, 
                 ~ box_cox_inv_vec(.x,
                                   lambda = objects$params$box_cox$lambda)))

forecast_values_tbl %>% 
    plot_modeltime_forecast()

forecast_values_tbl %>% 
    mutate(date = as.Date(.index),
           wday = wday(date, label = TRUE)) %>% 
    filter(.key == "prediction") %>% 
    select(.model_id, .model_desc, date, wday, .value, .conf_lo, .conf_hi)

           