library(tidyverse)
library(timetk)

wakefield_raw <- read_rds("00_data/raw/wakefield_daily.rds")

# total appointments

wakefield_total_day_tbl <-
    wakefield_raw %>%
    summarise_by_time(.date_var = appointment_date,
                      .by = "day",
                      appointments = sum(count_of_appointments)) %>%
    pad_by_time(.date_var = appointment_date,
                .by = "day",
                .pad_value = 0)

write_rds(wakefield_total_day_tbl, "00_data/processed/wakefield_total_day_tbl.rds")

# grouped by appointment status

wakefield_appt_status_day_tbl <- 
    wakefield_raw %>% 
    select(appointment_date, appointment_status, count_of_appointments) %>% 
    group_by(appointment_status) %>% 
    summarise_by_time(.date_var = appointment_date,
                      .by = "day",
                      appointments = sum(count_of_appointments)) %>% 
    ungroup()

write_rds(wakefield_appt_status_day_tbl, "00_data/processed/wakefield_appt_status_day_tbl.rds")

# grouped by health care professional

wakefield_hcp_type_day_tbl <- 
    wakefield_raw %>% 
    select(appointment_date, hcp_type, count_of_appointments) %>% 
    group_by(hcp_type) %>% 
    summarise_by_time(.date_var = appointment_date,
                      .by = "day",
                      appointments = sum(count_of_appointments)) %>% 
    ungroup()

write_rds(wakefield_hcp_type_day_tbl, "00_data/processed/wakefield_hcp_type_day_tbl.rds")

# grouped by appointment mode 

wakefield_appointment_mode_day_tbl <- 
    wakefield_raw %>% 
    select(appointment_date, appointment_mode, count_of_appointments) %>% 
    group_by(appointment_mode) %>% 
    summarise_by_time(.date_var = appointment_date,
                      .by = "day",
                      appointments = sum(count_of_appointments)) %>% 
    ungroup()

write_rds(wakefield_appointment_mode_day_tbl, "00_data/processed/wakefield_appt_mode_day_tbl.rds")
