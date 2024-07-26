# Generating Time Series Data for selected area - Wakefield (code: 03R)

library(tidyverse)
library(timetk)
library(rvest)

# Appointments Data ----
source("00_scripts/collect_gp_appointment_data.R")
options(timeout = max(300, getOption("timeout")))

urls <- c(
    "https://files.digital.nhs.uk/C3/C7568F/Appointments_GP_Daily_Jun19.zip",     # 01 Jan 2018 to 30 Jun 2019
    "https://files.digital.nhs.uk/25/445FF6/Appointments_GP_Daily_Dec21.zip",     # 01 Jul 2019 to 31 Dec 2021
    "https://files.digital.nhs.uk/A9/D843D9/Appointments_GP_Daily_CSV_Jun_24.zip" # 01 Jan 2022 to 30 Jun 2024
)

raw_data <- map(urls, extract_zipped_csv_files)

processed_data <- 
    raw_data %>% 
    flatten() %>% 
    map(tidy_csv_files, area = "03R")

wakefield_daily_raw <-
    reduce(processed_data, bind_rows) %>% 
    arrange(appointment_date)

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

## Save Data ----
write_rds(wakefield_events, "00_data/processed/wakefield_events.rds") 

# External features ----

