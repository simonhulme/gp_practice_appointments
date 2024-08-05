# Advanced Feature Engineering - Building and Testing Basic Linear Models ----


## Set up: packages and data ----
library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)

appointments <- read_rds("00_data/processed/wakefield_daily_adjusted.rds")
events       <- read_rds("00_data/processed/wakefield_events.rds")

## Data: training, testing & forecast ----

## Focus on single subset: Total GP appointments attended - post-pandemic data
## Start using untransformed response 

gp_appts_tbl <-
    appointments %>%
    filter(hcp_type == "GP",
           appt_status == "Attended") %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08-01") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) 

data_prepared_full_tbl <-
    gp_appts_tbl %>%
    
    # add future window
    bind_rows(future_frame(
        .data = .,
        .date_var = appointment_date,
        .length_out = 40
    )) %>%
    
    # add events
    left_join(events, by = c("appointment_date" = "event_date")) %>% 
    select(-weekend) 

data_prepared_full_tbl %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

# Split data into training/testing plus forecast

train_test_tbl <- 
    data_prepared_full_tbl %>% 
    filter(!is.na(appointments))

forecast_tbl <- 
    data_prepared_full_tbl %>% 
    filter(is.na(appointments))

splits <- train_test_tbl %>% 
    time_series_split(date_var = appointment_date,
                      assess = 40,
                      cumulative = TRUE)

splits %>%
    tk_time_series_cv_plan() %>%
    plot_time_series_cv_plan(.date_var = appointment_date, .value = appointments)


## Build linear model ----

recipe_spec_base <- 
    recipe(appointments ~. , training(splits)) %>% 
    step_date(appointment_date, features = c("dow", "month")) %>% 
    step_dummy(all_nominal_predictors(), one_hot = FALSE) %>% 
    step_mutate(index.num = as.numeric(appointment_date)) %>% 
    step_normalize(index.num)

model_spec_lm <- 
    linear_reg() %>% 
    set_engine("lm")

workflow_fit_lm  <- 
    workflow() %>% 
    add_model(model_spec_lm) %>% 
    add_recipe(recipe_spec) %>% 
    fit(training(splits))



calibration_tbl <- 
    modeltime_table(workflow_fit_lm) %>% 
    modeltime_calibrate(new_data = testing(splits))

calibration_tbl %>%
    modeltime_forecast(
        new_data = testing(splits),
        actual_data = data_prepared_tbl,
        conf_interval = 0.9
    ) %>%
    plot_modeltime_forecast()

calibration_tbl %>% 
    modeltime_accuracy()

workflow_fit_lm %>% 
    extract_fit_parsnip() %>% 
    pluck("fit") %>% 
    summary() %>% 
    pluck("adj.r.squared")




