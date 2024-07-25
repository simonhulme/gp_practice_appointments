## Advanced Feature Engineering ----

library(tidyverse)
library(timetk)
library(tidymodels)
library(modeltime)

all_appointments <- read_rds("00_data/processed/wakefield_daily_corrected_tbl.rds")
events <- read_rds("00_data/processed/events_tbl.rds")

gp_appts_tbl <-
    all_appointments %>%
    filter(hcp_type == "GP",
           appt_status == "Attended") %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-07-20") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    left_join(events) %>% 
    filter(!weekend) %>% 
    select(-weekend, - pandemic)

forecast_horizon <- 40

data_prepared_full_tbl <- 
    gp_appts_tbl %>% 
    bind_rows(
        future_frame(.data = ., .date_var = appointment_date, .length_out = forecast_horizon)
    ) 


