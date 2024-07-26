# Collect GP Appointments Data ----

library(tidyverse)

## Extract Raw Data from Web Pages ----

# Extract raw data from webpages

## https://digital.nhs.uk/data-and-information/publications/statistical/appointments-in-general-practice

# Extract raw data from webpages

extract_zipped_csv_files <- function(url) {
    temporary_file <- tempfile()
    download.file(url, temporary_file)
    
    contents <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset("CCG_CSV|SUB_ICB_LOCATION_CSV")
    
    data <- 
        map(contents, ~ read_csv(unz(temporary_file, .x))) 
    
    return(data)
}

# ensure consistent format for joining and analysis

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