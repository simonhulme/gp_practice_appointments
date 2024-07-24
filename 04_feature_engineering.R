# Initial Modelling and Exploration

## Use a linear regression model along with a variety of feature engineering techniques:

## * trends - linear and non-linear
## * seasonality 
## * other time based features 
## * fourier terms
## * lagged dependent variable
## * event data
## * external regressors

## Focus on most relevant subset of the data: GP appointments
## - look at total, same day and advanced booking separately
## - combine face-to-face, telephone, and video/online to reflect in surgery workload

# Set Up ----

# Libraries
library(tidyverse)
library(modeltime)
library(timetk)

# Raw Data
all_appointments <- read_rds("00_data/processed/wakefield_daily_corrected_tbl.rds")

## Explore autocorrelation
gp_appts_time_signature_boxcox_tbl %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .value = appointments_trans, .lags = 262)

## evidence of order 1 autoregression - ideally model using ARIMA but in exploratory analysis
## model using lagged term.
## run model so far and explore residuals

## create linear model

lm_fit_1 <-
    lm(
        formula = appointments_trans ~ splines::ns(index.num, df = 4) + wday.lbl + month.lbl,
        data = gp_appts_time_signature_boxcox_tbl
    )

summary(lm_fit_1)
anova(lm_fit_1)

residuals(lm_fit_1) %>% hist()

fitted_model <- 
    broom::augment(lm_fit_1, gp_appts_time_signature_boxcox_tbl %>% select(appointment_date, appointments_trans))

fitted_model %>% 
    ggplot(aes(.fitted, appointments_trans)) +
    geom_point() +
    geom_abline() +
    theme_bw()

fitted_model %>% 
    plot_time_series(.date_var = appointment_date, .resid)

fitted_model %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .resid, .lags = 20)


## add in lag1 terms
gp_appts_lagged_dependent_tbl <- 
    gp_appts_time_signature_boxcox_tbl %>% 
    tk_augment_lags(.value = appointments_trans, .lags = 1) %>% 
    drop_na()
    
gp_appts_lagged_dependent_tbl %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = 
            appointments_trans ~ 
            splines::ns(index.num, df = 4) + wday.lbl + month.lbl + appointments_trans_lag1,
        .show_summary = TRUE
    )

# revise model to include lagged terms

lm_fit_2 <-
    lm(appointments_trans ~ 
            splines::ns(index.num, df = 4) + wday.lbl + month.lbl + appointments_trans_lag1,
        data = gp_appts_lagged_dependent_tbl
    )

summary(lm_fit_2)
anova(lm_fit_2)

residuals(lm_fit_2) %>% hist()

fitted_model <- 
    broom::augment(lm_fit_2, gp_appts_lagged_dependent_tbl %>% select(appointment_date, appointments_trans))

fitted_model %>% 
    ggplot(aes(.fitted, appointments_trans)) +
    geom_point() +
    geom_abline() +
    theme_bw()

fitted_model %>% 
    plot_time_series(.date_var = appointment_date, .resid)

fitted_model %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .resid, .lags = 260)

## adding in fourier terms to handle annual pattern

gp_appts_lagged_dependent_tbl %>% 
    plot_acf_diagnostics(.date_var = appointment_date, appointments_trans, .lags = 261)

gp_appts_fourier_tbl <- 
    gp_appts_lagged_dependent_tbl %>% 
    tk_augment_fourier(.date_var = appointment_date, .periods = c(260))

gp_appts_fourier_tbl %>% glimpse()

gp_appts_fourier_tbl %>% 
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = 
            appointments_trans ~ 
            splines::ns(index.num, df = 4) + wday.lbl + month.lbl + appointments_trans_lag1 + 
            appointment_date_sin260_K1 + appointment_date_cos260_K1,
        .show_summary = TRUE
    )

## revise linear model again

lm_fit_3 <-
    lm(formula = 
           appointments_trans ~ 
           splines::ns(index.num, df = 4) + wday.lbl + month.lbl + appointments_trans_lag1 + 
           appointment_date_sin260_K1 + appointment_date_cos260_K1,
       data = gp_appts_fourier_tbl
    )

summary(lm_fit_3)
anova(lm_fit_3)

residuals(lm_fit_3) %>% hist()

fitted_model <- 
    broom::augment(lm_fit_3, gp_appts_fourier_tbl %>% select(appointment_date, appointments_trans))

fitted_model %>% 
    ggplot(aes(.fitted, appointments_trans)) +
    geom_point() +
    geom_abline() +
    theme_bw()

fitted_model %>% 
    plot_time_series(.date_var = appointment_date, .resid)

fitted_model %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .resid, .lags = 260)


## modelling events

events <- 
    all_appointments %>% 
    filter_by_time(.date_var = appointment_date, .start_date = "2021-07-20") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        holiday = first(holiday),
        training = first(training)
    )

gp_appts_events_tbl <- 
    gp_appts_fourier_tbl %>% 
    left_join(events) 

gp_appts_events_tbl %>% glimpse()

## visualising events

g <- 
    gp_appts_events_tbl %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .interactive = FALSE,
        .smooth = FALSE
    ) +
    geom_point(colour = "red",
               data = . %>% filter(holiday == "Yes")) +
    geom_point(colour = "green",
               data = . %>% filter(training == "Yes")) 
    
plotly::ggplotly(g)

## model events and re-visualise
gp_appts_events_tbl %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments_trans ~ 
            splines::ns(index.num, df = 4) + wday.lbl + month.lbl + appointments_trans_lag1 + 
            appointment_date_sin260_K1 + appointment_date_cos260_K1 + holiday + training,
        .show_summary = TRUE
    )

## revise linear model again

lm_fit_4 <-
    lm(formula = 
           appointments_trans ~ 
           splines::ns(index.num, df = 4) + wday.lbl + month.lbl + appointments_trans_lag1 + 
           appointment_date_sin260_K1 + appointment_date_cos260_K1 + holiday + training,
       data = gp_appts_events_tbl
    )

summary(lm_fit_4)
anova(lm_fit_4)

residuals(lm_fit_4) %>% hist()

fitted_model <- 
    broom::augment(lm_fit_4, gp_appts_events_tbl %>% select(appointment_date, appointments_trans))

fitted_model %>% 
    ggplot(aes(.fitted, appointments_trans)) +
    geom_point(alpha = 0.5) +
    geom_abline() +
    theme_bw()

fitted_model %>% 
    plot_time_series(.date_var = appointment_date, .resid)

fitted_model %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .resid, .lags = 260)








## add in terms to account for lagged events
gp_f2f_same_day_events_with_lags_tbl <- 
    gp_f2f_same_day_events_tbl %>% 
    mutate(training_lag = lag(training),
           holiday_lag = lag(holiday)) %>% 
    drop_na()

gp_f2f_same_day_events_with_lags_tbl %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ splines::ns(index.num, df = 5) + .,
        .show_summary = TRUE
    )

## explore xregs - lagged

gp_f2f_same_day_xregs <- 
    gp_f2f_same_day_events_with_lags_tbl %>%
    left_join(gp_f2f_same_day_appointments %>% select(appointment_date, total_gp:registered_population))

## cross correlation
gp_f2f_same_day_xregs %>% 
    select(appointment_date, appointments, total_gp:registered_population) %>% 
    plot_acf_diagnostics(.date_var = appointment_date, .value = appointments, .ccf_vars = c(total_gp:registered_population))

# model
gp_f2f_same_day_xregs %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ splines::ns(index.num, df = 1) + .,
        .show_summary = TRUE
    )


## simplify model
gp_f2f_same_day_xregs %>%
    select(
        appointment_date,
        appointments,
        index.num,
        year,
        month.lbl,
        wday.lbl,
        week,
        appointment_date_cos260_K1,
        appointment_date_sin260_K1,
        holiday:registered_population
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = appointments ~ splines::ns(index.num, df = 6) + . - appointment_date - index.num,
        .show_summary = TRUE
    )


# Cross Correlation (CCF) ----

# with log transformation, standardisation and outlier cleaning
# results in series that can be compared with each other without being dominated by holiday data

hcp_booking_type_wide_tbl <- 
    same_day_vs_advance_daily_tbl %>% 
    group_by(hcp_type, booking) %>% 
    summarise_by_time(.date_var = appointment_date, .by = "day", appointments = sum(appointments)) %>% 
    pivot_wider(names_from = c(hcp_type, booking), values_from = appointments) %>% 
    janitor::clean_names() %>% 
    select(appointment_date, gp_same_day, everything()) %>% 
    mutate(across(where(is.numeric), ~ log1p(.x) %>% standardize_vec)) %>% 
    mutate(across(where(is.numeric), ~ ts_clean_vec(.x, period = 5)))

hcp_booking_type_wide_tbl %>% 
    pivot_longer(cols = - appointment_date) %>% 
    plot_time_series(.date_var = appointment_date, .value = value, .facet_vars = name)

hcp_booking_type_wide_tbl %>%
    plot_acf_diagnostics(
        .date_var = appointment_date,
        .value = gp_same_day,
        .ccf_vars = c(
            gp_advance,
            other_practice_staff_advance,
            other_practice_staff_same_day
        ),
        .show_ccf_vars_only = TRUE,
        .show_white_noise_bars = TRUE, 
        .lags = 60
    )















# Range Reduction ----

# FTE Staff

total_nurse_FTE <-
    gp_f2f_same_day_appointments %>%
    select(appointment_date, total_nurse)

total_nurse_FTE %>%
    plot_time_series(.date_var = appointment_date, .value = total_nurse)

total_nurse_FTE %>%
    plot_acf_diagnostics(.date_var = appointment_date,
                         .value = total_nurse,
                         .lags = 60)

# Imputation and Outlier Cleaning ----

## This data set has no missing data so will focus on anomaly/outlier detection

gp_f2f_same_day_appointments %>%
    select(appointment_date, count_of_appointments) %>%
    plot_anomaly_diagnostics(.date_var = appointment_date,
                             .value = count_of_appointments,
                             .title = "Low anomalies: Holiday/Training. High anomalies: Day after Holiday")

gp_f2f_same_day_appointments %>%
    select(appointment_date, count_of_appointments) %>%
    mutate(appointments_cleaned = ts_clean_vec(count_of_appointments, period = 5)) %>%
    pivot_longer(-appointment_date) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = value,
        .color_var = name,
        .smooth = FALSE,
        .title = "Cleaning not appropriate as outliers are informative"
    )

## difference between values after Bank Holiday - observed vs. cleaned - indicates number of
## extra appointments expected after Bank Holiday closures 

# Lags and Differencing ----

## Explore lags ----
gp_f2f_same_day_appointments %>% 
    plot_acf_diagnostics(.date_var = appointment_date, count_of_appointments, .lags = 30)

## Regression Model with lags ----
gp_f2f_same_day_appointments %>% 
    tk_augment_lags(.value = count_of_appointments, .lags = c(1:5)) %>% 
    drop_na() %>% 
    mutate(across(contains("count"), log1p)) %>% 
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula  = count_of_appointments ~ 
            wday(appointment_date, label = TRUE) +
            month(appointment_date, label = TRUE) +
            year(appointment_date) +
            registered_population +
            holiday +
            training +
            pandemic +
            count_of_appointments_lag1 +
            count_of_appointments_lag2,
        .show_summary = TRUE,
        .interactive  = TRUE,
        .title = "Lag terms address peak after holiday (but not sure why)"
    )

## Differencing ----

## start looking at cumulative sum of appointments

differenced_data <- 
    gp_f2f_same_day_appointments %>%
    filter_by_time(.date_var = appointment_date,
                   .start_date = "start",
                   .end_date = "end") %>%
    select(appointment_date, count_of_appointments) %>%
    mutate(total_appointments = cumsum(count_of_appointments),
           differences        = diff_vec(count_of_appointments, lag = 1, difference = 1),
           differences_2      = diff_vec(differences, lag = 1, difference = 1)
    )
    
differenced_data %>% 
    pivot_longer(-appointment_date) %>% 
    group_by(name) %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = value,
                     .smooth = FALSE)

differenced_data %>%
    plot_acf_diagnostics(.date_var = appointment_date, .value = differences_2, .lags = 40)

## Fourier Series ----

gp_f2f_same_day_appointments %>% 
    plot_time_series(appointment_date, count_of_appointments)

gp_f2f_same_day_appointments %>% 
    plot_acf_diagnostics(appointment_date, count_of_appointments, .lags = 262)

gp_f2f_same_day_appointments %>%
    select(appointment_date, count_of_appointments, holiday, training, pandemic) %>%
    tk_augment_fourier(
        .date_var = appointment_date,
        .periods = c(7, 365),
        .K = 1
    ) %>%
    plot_time_series_regression(
        .date_var = appointment_date,
        .formula = log1p(count_of_appointments) ~ as.numeric(appointment_date) + . - appointment_date,
        .show_summary = TRUE
    )
