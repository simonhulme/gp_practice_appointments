# Collect GP Appointments Data ----

library(tidyverse)

# Appointments Data ----

##  Extract raw data from website ----

## https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice

extract_zipped_csv_files <- function(url) {
    temporary_file <- tempfile()
    download.file(url, temporary_file)
    
    contents <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset("CSV|csv")
    
    data <- 
        map(contents, ~ read_csv(unz(temporary_file, .x))) 
    
    return(data)
}

# tidy data so consistent format for joining

tidy_csv_files <- function(df, area) {
    
    rename_columns <- function(df) {
        if ("CCG_CODE" %in% names(df)) {
            df <- df %>% rename(AREA_CODE = CCG_CODE)
        }
        if ("SUB_ICB_LOCATION_CODE" %in% names(df)) {
            df <- df %>% rename(AREA_CODE = SUB_ICB_LOCATION_CODE)
        }
        if ("SUB_ICB_LOC_CODE" %in% names(df)) {
            df <- df %>% rename(AREA_CODE = SUB_ICB_LOC_CODE)
        }
        
        df <- df %>% janitor::clean_names()
        return(df)
    }
    
    reformat_dates <- function(df) {
        df %>% mutate(appointment_date = parse_date_time(
            appointment_date,
            orders = c("dmy", "ymd")
        ))
    }
    
    filter_select_data <- function(df, area) {
        df %>% 
            filter(area_code == area) %>% 
            select(area_code, appointment_date:count_of_appointments)
    }
    
    output <- 
        df %>% 
        rename_columns() %>% 
        reformat_dates() %>% 
        filter_select_data(area = area)
    
    output
}


## Extract 1st data set ----

url <- "https://files.digital.nhs.uk/30/71883D/Appointments_GP_Daily_Apr19.zip"

test_1 <- extract_zipped_csv_files(url = url)

test_1 # list of 18 elements

# tidy 1st element

tidied_test_1 <- 
    test_1[[1]] %>% 
    tidy_csv_files(area = "03R")

tidied_test_1

## apply tidying to multiple files

tidied_data <- 
    test_1 %>% 
    map(tidy_csv_files)

tidied_data

reduce(tidied_data, bind_rows) %>% arrange(appointment_date)






tidy_csv_files <- function(list_of_csv_files) { #add in area code later
    
    
    
    
    
    
    
    output <-
        extracted_csv_files %>%
        map(rename_columns) %>%
        map(~ filter(.x, AREA_CODE == area_code)) %>%
        reduce(bind_rows) %>%
        select(
            Appointment_Date,
            HCP_TYPE,
            APPT_MODE,
            TIME_BETWEEN_BOOK_AND_APPT,
            APPT_STATUS,
            COUNT_OF_APPOINTMENTS
        ) %>%
        janitor::clean_names()
    
    return(output)
}









collect_gp_appointment_data <- function(urls, area_code) {
    
    ## HELPER FUNCTIONS
    
    extract_zipped_csv_files <- function(url) {
        temporary_file <- tempfile()
        download.file(url, temporary_file)
        
        contents <- 
            unzip(temporary_file, list = TRUE) %>% 
            pull(Name) %>% 
            str_subset("CSV|csv")
        
        data <- 
            map(contents, ~ read_csv(unz(temporary_file, .x))) 
        
        return(data)
    }
    
    rename_columns <- function(df) {
        if ("CCG_CODE" %in% names(df)) {
            df <- df %>% rename(AREA_CODE = CCG_CODE)
        }
        if ("SUB_ICB_LOCATION_CODE" %in% names(df)) {
            df <- df %>% rename(AREA_CODE = SUB_ICB_LOCATION_CODE)
        }
        if ("SUB_ICB_LOC_CODE" %in% names(df)) {
            df <- df %>% rename(AREA_CODE = SUB_ICB_LOC_CODE)
        }
        
        return(df)
    }
    
    reformat_dates <- function(df) {
        df %>% mutate(Appointment_Date = parse_date_time(
            Appointment_Date,
            orders = c("dmy", "ymd")
        ))
    }
    
    extracted_csv_files <- 
        map(urls, extract_zipped_csv_files) %>% 
        flatten()
    
    tidy_csv_files <- function(extracted_csv_files, area_code) {

        output <-
            extracted_csv_files %>%
            map(rename_columns) %>%
            map(~ filter(.x, AREA_CODE == area_code)) %>%
            reduce(bind_rows) %>%
            select(
                Appointment_Date,
                HCP_TYPE,
                APPT_MODE,
                TIME_BETWEEN_BOOK_AND_APPT,
                APPT_STATUS,
                COUNT_OF_APPOINTMENTS
            ) %>%
            janitor::clean_names()

        return(output)
    }
    
    output <- 
        map(extracted_csv_files, tidy_csv_files)
    
}


## WITH 2 URLS ----

urls <- c(
    "https://files.digital.nhs.uk/30/71883D/Appointments_GP_Daily_Apr19.zip",
    "https://files.digital.nhs.uk/20/40049A/Appointments_GP_Daily_Oct21.zip"
)

area_code <- "03R"

test <- collect_gp_appointment_data(urls = urls[[1]], area_code = area_code)




data <-  test %>% flatten()


tidy_csv_files <- function(extracted_csv_files) {
    
    output <-
        extracted_csv_files %>%
        map(rename_columns) 
        
        # map(~ filter(.x, AREA_CODE == "03R")) 
        # reduce(bind_rows) %>%
        # select(
        #     Appointment_Date,
        #     HCP_TYPE,
        #     APPT_MODE,
        #     TIME_BETWEEN_BOOK_AND_APPT,
        #     APPT_STATUS,
        #     COUNT_OF_APPOINTMENTS
        # ) %>%
       # janitor::clean_names()
    
    return(output)
}

output <- 
    map(test, tidy_csv_files) 

output %>% map(names)

### Nov 2017 to Apr 2019  ----

url <-  "https://files.digital.nhs.uk/30/71883D/Appointments_GP_Daily_Apr19.zip"
area_code <- "03R"

earliest_data_raw <- 
    collect_gp_appointment_data(url = url, area_code = area_code)

earliest_data_raw





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