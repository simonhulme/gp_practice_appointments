# DATA PREPARATION ----

## Set Up ----
# Load Libraries 

library(tidyverse)
library(timetk)

# Import Data

wakefield_total_day_tbl       <- read_rds("00_data/processed/wakefield_total_day_tbl.rds")
wakefield_appt_mode_day_tbl   <- read_rds("00_data/processed/wakefield_appt_mode_day_tbl.rds")
wakefield_appt_status_day_tbl <- read_rds("00_data/processed/wakefield_appt_status_day_tbl.rds")
wakefield_hcp_type_day_tbl    <- read_rds("00_data/processed/wakefield_hcp_type_day_tbl.rds")

## Convert data to 5 day business week ----

### Daily Total ----

new_index <-
    tk_make_weekday_sequence(
        start_date = min(wakefield_total_day_tbl$appointment_date),
        end_date = max(wakefield_total_day_tbl$appointment_date),
        remove_weekends = TRUE,
        remove_holidays = FALSE
    )

wakefield_total_5_day_tbl <- 
    wakefield_total_day_tbl %>% 
    filter(appointment_date %in% new_index) 

wakefield_total_5_day_tbl %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .interactive = FALSE) + 
    scale_x_date(
        breaks = new_index,
        date_breaks = "year", 
        expand = c(0, 0),
        labels = year
    ) 

### Appointment Mode ----

new_index <-
    tk_make_weekday_sequence(
        start_date = min(wakefield_appt_mode_day_tbl$appointment_date),
        end_date = max(wakefield_appt_mode_day_tbl$appointment_date)
    )

wakefield_appt_mode_5_day_tbl <- 
    wakefield_appt_mode_day_tbl %>% 
    filter(appointment_date %in% new_index) 

wakefield_appt_mode_5_day_tbl %>% 
    group_by(appointment_mode) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .interactive = FALSE) + 
    scale_x_date(
        breaks = new_index,
        date_breaks = "year", 
        expand = c(0, 0),
        labels = year
    ) 

### Appointment Status ----

new_index <-
    tk_make_weekday_sequence(
        start_date = min(wakefield_appt_status_day_tbl$appointment_date),
        end_date = max(wakefield_appt_status_day_tbl$appointment_date)
    )

wakefield_appt_status_5_day_tbl <- 
    wakefield_appt_status_day_tbl %>% 
    filter(appointment_date %in% new_index) 

wakefield_appt_status_5_day_tbl %>% 
    group_by(appointment_status) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .interactive = FALSE) + 
    scale_x_date(
        breaks = new_index,
        date_breaks = "year", 
        expand = c(0, 0),
        labels = year
    ) 

### Health Care Professional ----

new_index <-
    tk_make_weekday_sequence(
        start_date = min(wakefield_hcp_type_day_tbl$appointment_date),
        end_date = max(wakefield_hcp_type_day_tbl$appointment_date)
    )

wakefield_hcp_type_5_day_tbl <- 
    wakefield_hcp_type_day_tbl %>% 
    filter(appointment_date %in% new_index) 

wakefield_hcp_type_5_day_tbl %>% 
    group_by(hcp_type) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments, .interactive = FALSE) +
    scale_x_date(
        breaks = new_index,
        date_breaks = "year", 
        expand = c(0, 0),
        labels = year
    ) 

## Combine levels to remove duplicate data ----

### Appointment Mode ----

wakefield_appt_mode_5_day_tbl <-
    wakefield_appt_mode_5_day_tbl %>%
    mutate(
        appointment_mode = as_factor(appointment_mode),
        appointment_mode = fct_collapse(
            appointment_mode,
            "Video/Online" = c("Video/Online", "Video Conference/Online")
        )
    )

wakefield_appt_mode_5_day_tbl %>% 
    group_by(appointment_mode) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments) 

### Appointment Status ----

wakefield_appt_status_5_day_tbl <-
    wakefield_appt_status_5_day_tbl %>% 
    mutate(
        appointment_status = as_factor(appointment_status),
        appointment_status = fct_collapse(
            appointment_status,
            "Unknown" = c("Unknown", "Appt Status Not Provided")
        )
    ) 

wakefield_appt_status_5_day_tbl %>% 
    group_by(appointment_status) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

### Health Care Professional ----

wakefield_hcp_type_5_day_tbl <-
    wakefield_hcp_type_5_day_tbl %>% 
    mutate(
        hcp_type = as_factor(hcp_type),
        hcp_type = fct_collapse(
            hcp_type,
            "Unknown" = c("Unknown", "HCP Type Not Provided")
        )
    ) 

wakefield_hcp_type_5_day_tbl %>% 
    group_by(hcp_type) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments)

## Slice Time Series to Exclude Missing Data ----

### Appointment Mode ----

appt_mode_5_day_sliced_tbl <- 
    wakefield_appt_mode_5_day_tbl %>% 
    filter_by_time(.date_var = appointment_date, .start_date = "2018-", .end_date = "2023") 

appt_mode_5_day_sliced_tbl%>% 
    group_by(appointment_mode) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments) 

### Appointment Status ----

appt_status_5_day_sliced_tbl <- 
    wakefield_appt_status_5_day_tbl %>% 
    filter_by_time(.date_var = appointment_date, .start_date = "2019-02", .end_date = "2023") 

appt_status_5_day_sliced_tbl %>% 
    group_by(appointment_status) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments) 

### Health Care Professional ----

hcp_type_5_day_sliced_tbl <- 
    wakefield_hcp_type_5_day_tbl %>% 
    filter_by_time(.date_var = appointment_date, .start_date = "2018-04", .end_date = "2023")

hcp_type_5_day_sliced_tbl %>% 
    group_by(hcp_type) %>% 
    plot_time_series(.date_var = appointment_date, .value = appointments) 


## Aggregate Data ----

### Total Appointments ----

wakefield_total_5_day_weekly_tbl <- 
    wakefield_total_5_day_tbl %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) 

wakefield_total_5_day_weekly_tbl %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Weekly Time Series: Holiday troughs and pandemic shock event")

wakefield_total_5_day_monthly_tbl <- 
    wakefield_total_day_tbl %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    )

wakefield_total_5_day_monthly_tbl %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Monthly Time Series: Regular peak around October")

### By Feature ----

#### 1. Appointment Mode ----

appt_mode_5_day_weekly_tbl <- 
    appt_mode_5_day_sliced_tbl %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week", .type = "floor",
        appointments = sum(appointments)
    ) 

appt_mode_5_day_weekly_tbl %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Weekly time series: Unexplained spike in 'Unknown' mode in Feb 2019")

appt_mode_5_day_monthly_tbl <- 
    appt_mode_5_day_sliced_tbl %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    )

appt_mode_5_day_monthly_tbl %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Monthly time series: Phone and f2f appear inversely related")

#### 2. Appointment Status ----

appt_status_5_day_weekly_tbl <- 
    appt_status_5_day_weekly_tbl%>%
    group_by(appointment_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) 

appt_status_5_day_weekly_tbl %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Weekly time series: Spike in 'Unknown' status Mar 2020 - ? pandemic")

appt_status_5_day_monthly_tbl <- 
    appt_status_5_day_weekly_tbl %>%
    group_by(appointment_status) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) 

appt_status_5_day_monthly_tbl %>%
    plot_time_series(.date_var = appointment_date,
                     .value = appointments)

#### 3. Health Care Professional ----

hcp_type_5_day_weekly_tbl <- 
    hcp_type_5_day_sliced_tbl %>%
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) 

hcp_type_5_day_weekly_tbl %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .title = "Weekly time series: Spike in 'Unknown' Feb 2019 but small relative to total appts",
        .smooth = TRUE,
        .facet_scales = "fixed",
        .interactive = F
    )

hcp_type_5_day_monthly_tbl <- 
    hcp_type_5_day_sliced_tbl %>% 
    group_by(hcp_type) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    )

hcp_type_5_day_monthly_tbl %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = appointments,
        .title = "Monthly time series",
        .smooth = TRUE
    )

# Save Data ----

## Total Appointments  ----

write_rds(
    wakefield_total_5_day_tbl,
    "00_data/processed/sliced/total/wakefield_total_5_day_tbl.rds"
)

write_rds(
    wakefield_total_5_day_weekly_tbl,
    "00_data/processed/sliced/total/wakefield_total_5_day_weekly_tbl.rds"
)

write_rds(
    wakefield_total_5_day_monthly_tbl,
    "00_data/processed/sliced/total/wakefield_total_5_day_monthly_tbl.rds"
)

## Appointment Mode ----

write_rds(
    appt_mode_5_day_sliced_tbl,
    "00_data/processed/sliced/appt_mode/wakefield_appt_mode_5_day_sliced_tbl.rds"
)

write_rds(
    appt_mode_5_day_weekly_tbl,
    "00_data/processed/sliced/appt_mode/wakefield_appt_mode_5_day_weekly_tbl.rds"
)

write_rds(
    appt_mode_5_day_monthly_tbl,
    "00_data/processed/sliced/appt_mode/wakefield_appt_mode_5_day_monthly_tbl.rds"
)

## Appointment Status ----

write_rds(
    appt_status_5_day_sliced_tbl,
    "00_data/processed/sliced/appt_status/wakefield_appt_status_5_day_sliced_tbl.rds"
)

write_rds(
    appt_status_5_day_weekly_tbl,
    "00_data/processed/sliced/appt_status/wakefield_appt_status_5_day_weekly_tbl.rds"
)

write_rds(
    appt_status_5_day_monthly_tbl,
    "00_data/processed/sliced/appt_status/wakefield_appt_status_5_day_monthly_tbl.rds"
)

## Health Care Professional ----

write_rds(
    hcp_type_5_day_sliced_tbl,
    "00_data/processed/sliced/hcp_type/wakefield_hcp_type_5_day_sliced_tbl.rds"
)

write_rds(
    hcp_type_5_day_weekly_tbl,
    "00_data/processed/sliced/hcp_type/wakefield_hcp_type_5_day_weekly_tbl.rds"
)

write_rds(
    hcp_type_5_day_monthly_tbl,
    "00_data/processed/sliced/hcp_type/wakefield_hcp_type_5_day_monthly_tbl.rds"
)

# FINDINGS ----

## 1. Plots reveal trends, seasonality and the shock effects of the pandemic ----
## 2. Some sporadic data quality issues remain after slicing data ----

# TODO ----

# * explore including pandemic or restricting to post pandemic.
# * explore options for handling sporadic missing data e.g. using imputation.