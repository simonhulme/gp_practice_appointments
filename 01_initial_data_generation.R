# Generating Time Series Data for selected area - Wakefield (code: 03R)

library(tidyverse)
library(timetk)

# Appointments Data ----
source("00_scripts/get_daily_gpad.R")

##  Extract raw data from website ----

### Nov 2017 to Apr 2019  ----
subset_1 <- 
    get_daily_gpad(url = "https://files.digital.nhs.uk/30/71883D/Appointments_GP_Daily_Apr19.zip")

data_1 <- 
    subset_1 %>% 
    map(~ filter(.x, CCG_CODE == "03R")) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

### May 2019 to Oct 2021 ----

subset_2 <- 
    get_daily_gpad("https://files.digital.nhs.uk/20/40049A/Appointments_GP_Daily_Oct21.zip")

data_2 <- 
    subset_2 %>% 
    map(~ filter(.x, CCG_CODE == "03R")) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

### Nov 2021 to Apr 2024 ----

subset_3 <- 
    get_daily_gpad("https://files.digital.nhs.uk/D2/F7DA4B/Appointments_GP_Daily_CSV_Apr_24.zip") 

data_3 <- 
    subset_3 %>% 
    map(~ filter(.x, SUB_ICB_LOCATION_CODE == "03R")) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

### Jun 2024 -----

subset_4 <- 
    get_daily_gpad("https://files.digital.nhs.uk/A9/D843D9/Appointments_GP_Daily_CSV_Jun_24.zip")

data_4 <- 
    subset_4 %>% 
    map(~ filter(.x, SUB_ICB_LOCATION_CODE == "03R")) %>% 
    reduce(bind_rows) %>% 
    select(
        Appointment_Date,
        HCP_TYPE,
        APPT_MODE,
        TIME_BETWEEN_BOOK_AND_APPT,
        APPT_STATUS,
        COUNT_OF_APPOINTMENTS
    )

## Combine datasets ----

wakefield_daily_raw <- 
    list(data_1, data_2, data_3, data_4) %>% 
    reduce(bind_rows) %>% 
    unique() %>% # Jun 24 data contains duplicate data
    mutate(Appointment_Date = dmy(Appointment_Date)) %>%
    arrange(Appointment_Date) %>% 
    janitor::clean_names()

## Save data ----
write_rds(wakefield_daily_raw, "00_data/raw/wakefield_daily_raw.rds")

# Events data ----

# events include weekends, bank holidays and training days

start_date <- min(wakefield_daily_raw$appointment_date)
end_date   <- max(wakefield_daily_raw$appointment_date)

## Calendar features: Weekends & Bank Holidays ----

weekend_dates <- 
    tk_make_weekend_sequence(start_date = start_date, end_date = end_date)

bank_holiday_dates <-
    tk_make_holiday_sequence(start_date = start_date,
                             end_date = end_date,
                             calendar = "LONDON")

## Training Half Days ----
## Practices close for half day training

training_dates <- list(
    training_19 =
        c(
            "2019-03-20",
            "2019-04-24",
            "2019-05-15",
            "2019-06-19",
            "2019-07-17",
            "2019-09-18",
            "2019-10-16",
            "2019-11-13"
        ),
    training_20 =
        c(
            "2020-01-15",
            "2020-02-26",
            "2020-07-15",
            "2020-08-19",
            "2020-09-16",
            "2020-10-14",
            "2020-11-11"
        ),
    training_21 =
        c(
            "2021-01-13",
            "2021-02-24",
            "2021-03-17",
            "2021-04-21",
            "2021-05-12",
            "2021-06-16",
            "2021-07-14",
            "2021-09-15",
            "2021-10-13",
            "2021-11-10"
        ),
    
    training_22 =
        c(
            "2022-02-23",
            "2022-03-16",
            "2022-04-06",
            "2022-05-11",
            "2022-06-15",
            "2022-07-13",
            "2022-09-14",
            "2022-10-12",
            "2022-11-09"
        ),
    
    training_23 =
        c(
            "2023-02-08",
            "2023-04-19",
            "2023-05-17",
            "2023-06-21",
            "2023-07-12",
            "2023-08-16",
            "2023-09-13",
            "2023-10-11",
            "2023-11-15"
        ),
    
    training_24 =
        c(
            "2024-01-17",
            "2024-02-21",
            "2024-03-13",
            "2024-04-17",
            "2024-05-15",
            "2024-06-19",
            "2024-07-10",
            "2024-09-18",
            "2024-10-16",
            "2024-11-13"
        )
)

training_dates <-
    as.Date(flatten_chr(training_dates))

## Merge into single events data set ----

wakefield_events <-
    wakefield_daily_raw %>% 
    select(appointment_date) %>% 
    distinct(appointment_date) %>% 
    mutate(
        weekend      = appointment_date %in% weekend_dates,
        bank_holiday = appointment_date %in% bank_holiday_dates,
        training     = appointment_date %in% training_dates
    )

## Adjust holiday and weekend values to zero ----
wakefield_daily_adj <-
    wakefield_daily_raw %>%
    left_join(wakefield_events) %>%
    mutate(count_of_appointments = ifelse(weekend |
                                              bank_holiday, 0, count_of_appointments)) %>% 
    select(appointment_date, count_of_appointments, hcp_type:time_between_book_and_appt)

## Save Data ----
write_rds(wakefield_events, "00_data/processed/wakefield_events.rds") 
write_rds(wakefield_daily_adj, "00_data/processed/wakefield_daily_adj.rds")

# External features ----



