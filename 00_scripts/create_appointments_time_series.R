# Extract daily data for Wakefield ----

library(tidyverse)
library(timetk)

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

data_1 <- 
    get_gpad_daily(url = urls[[1]], sub_icb_location_code = sub_icb_location_code) %>% 
    map(~ filter(.x, SUB_ICB_LOCATION_CODE == sub_icb_location_code)) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )
    
data_2 <- 
    get_gpad_daily(url = urls[[2]], sub_icb_location_code = sub_icb_location_code) %>% 
    map(~ filter(.x, CCG_CODE == sub_icb_location_code)) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

data_3 <- 
    get_gpad_daily(url = urls[[3]], sub_icb_location_code = sub_icb_location_code) %>% 
    map(~ filter(.x, CCG_CODE == sub_icb_location_code)) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

wakefield_daily_raw <- 
    list(data_1, data_2, data_3) %>% 
    reduce(bind_rows) %>% 
    mutate(Appointment_Date = dmy(Appointment_Date)) %>%
    arrange(Appointment_Date) %>% 
    janitor::clean_names()
    
write_rds(wakefield_daily_raw, "00_data/raw/wakefield_daily_raw.rds")