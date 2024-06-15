# Extract daily data for Wakefield ----

library(tidyverse)

## SUB-ICB LOCATION CODE: 03R

get_gpad_daily <- function(url, sub_icb_location_code) {
    
    temporary_file <- tempfile()
    
    download.file(url, temporary_file)
    
    contents <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset("CSV")
    
    data <- 
        map(contents, ~ read_csv(unz(temporary_file, .x))) 
    
    return(data)
}

urls <-
    c(
        "https://files.digital.nhs.uk/D2/F7DA4B/Appointments_GP_Daily_CSV_Apr_24.zip",
        "https://files.digital.nhs.uk/20/40049A/Appointments_GP_Daily_Oct21.zip",
        "https://files.digital.nhs.uk/30/71883D/Appointments_GP_Daily_Apr19.zip"
    )

sub_icb_location_code <- "03R"

data_1 <- get_gpad_daily(url = urls[[1]], sub_icb_location_code = sub_icb_location_code)

data_1_filtered <- 
    data_1 %>% map(~ filter(.x, SUB_ICB_LOCATION_CODE == sub_icb_location_code)) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )
    
data_2 <- get_gpad_daily(url = urls[[2]], sub_icb_location_code = sub_icb_location_code)
data_2 <- map(data_2, ~ rename(.x, SUB_ICB_LOCATION_CODE = CCG_CODE))

data_2_filtered <- 
    data_2 %>% map(~ filter(.x, SUB_ICB_LOCATION_CODE == sub_icb_location_code)) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

data_3 <- get_gpad_daily(url = urls[[3]], sub_icb_location_code = sub_icb_location_code)
data_3 <- map(data_3, ~ rename(.x, SUB_ICB_LOCATION_CODE = CCG_CODE))

data_3_filtered <- 
    data_3 %>% map(~ filter(.x, SUB_ICB_LOCATION_CODE == sub_icb_location_code)) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

data <- 
    list(data_1_filtered, data_2_filtered, data_3_filtered) %>% 
    reduce(bind_rows) %>% 
    mutate(Appointment_Date = dmy(Appointment_Date)) %>%
    arrange(Appointment_Date) %>% 
    janitor::clean_names()
    
data









# wakefield_total_day_tbl <-
#     wakefield_raw %>%
#     summarise_by_time(.date_var = appointment_date,
#                       .by = "day",
#                       appointments = sum(count_of_appointments))
# 
# write_rds(wakefield_total_day_tbl, "00_data/processed/wakefield_total_day_tbl.rds")
# 
# # grouped by appointment status
# 
# wakefield_appt_status_day_tbl <- 
#     wakefield_raw %>% 
#     select(appointment_date, appointment_status, count_of_appointments) %>% 
#     group_by(appointment_status) %>% 
#     summarise_by_time(.date_var = appointment_date,
#                       .by = "day",
#                       appointments = sum(count_of_appointments)) %>% 
#     ungroup()
# 
# write_rds(wakefield_appt_status_day_tbl, "00_data/processed/wakefield_appt_status_day_tbl.rds")
# 
# # grouped by health care professional
# 
# wakefield_hcp_type_day_tbl <- 
#     wakefield_raw %>% 
#     select(appointment_date, hcp_type, count_of_appointments) %>% 
#     group_by(hcp_type) %>% 
#     summarise_by_time(.date_var = appointment_date,
#                       .by = "day",
#                       appointments = sum(count_of_appointments)) %>% 
#     ungroup()
# 
# write_rds(wakefield_hcp_type_day_tbl, "00_data/processed/wakefield_hcp_type_day_tbl.rds")
# 
# # grouped by appointment mode 
# 
# wakefield_appointment_mode_day_tbl <- 
#     wakefield_raw %>% 
#     select(appointment_date, appointment_mode, count_of_appointments) %>% 
#     group_by(appointment_mode) %>% 
#     summarise_by_time(.date_var = appointment_date,
#                       .by = "day",
#                       appointments = sum(count_of_appointments)) %>% 
#     ungroup()
# 
# write_rds(wakefield_appointment_mode_day_tbl, "00_data/processed/wakefield_appt_mode_day_tbl.rds")