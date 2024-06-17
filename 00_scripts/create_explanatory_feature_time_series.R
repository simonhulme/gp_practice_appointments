# create time series for potential explanatory features ----

# Extract Monthly GP Online Consultation Data for chosen area (Wakefield) ----

library(tidyverse)
library(timetk)

## SUB-ICB LOCATION CODE: 03R

get_gp_data_daily <- function(url) {
    
    temporary_file <- tempfile()
    
    download.file(url, temporary_file)
    
    contents <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name)
    
    data <- 
        map(contents, ~ read_csv(unz(temporary_file, .x)))
    
    return(data)
}

url <-
    c(
        paste0(
            "https://files.digital.nhs.uk/91/1B80AF/Submissions%20via%20Online%20Consultation%20",
            "Systems%20in%20General%20Practice%20-%20April%202024.zip"
        )
    )

df <- 
    get_oc_data_daily(url = url) %>% 
    pluck(1) %>% 
    filter(SUB_ICB_LOCATION_CODE == "03R") %>% 
    group_by(METRIC) %>% 
    summarise_by_time(.date_var = MONTH, .by = "day", TOTAL = sum(VALUE)) %>%
    ungroup() %>% 
    filter(str_detect(METRIC, "SUBMISSION")) %>% 
    pivot_wider(id_cols = MONTH, names_from = METRIC, values_from = TOTAL) %>% 
    janitor::clean_names() %>% 
    rename_with(~ str_remove(.x, "oc_"), .cols = contains("submission")) %>% 
    rename_with(~ str_remove(.x, "submission_type_"), .cols = contains("submission")) %>% 
    rename_with(~ str_remove(.x, "_submissions"), .cols = contains("submission"))

df

## Patients Registered ----

df <- get_gp_data_daily(url = "https://files.digital.nhs.uk/C1/9B13E8/gp-reg-pat-prac-all.zip") %>% pluck(1)

df %>% 
    filter(SUB_ICB_LOCATION_CODE == "03R") %>% 
    summarise_by_time(.date_var = EXTRACT_DATE, .by = "month", total_registered_patients = sum(NUMBER_OF_PATIENTS))

url_zip_date <- 
    "https://files.digital.nhs.uk/C1/9B13E8/gp-reg-pat-prac-all.zip"


urls_zip_no_date <- 
    c(
      "https://files.digital.nhs.uk/7C/902806/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/E7/BD21B4/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/E1/956EEE/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/6D/F296C9/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/3B/B0AB30/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/A5/9FA179/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/91/DBC57F/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/5C/0C3806/gp-reg-pat-prac-all.zip",
      "https://files.digital.nhs.uk/CA/B026C9/gp-reg-pat-prac-all.zip")

urls_csv <- c(
      "https://files.digital.nhs.uk/19/AADF93/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/AF/9B0FC8/gp-reg-pat-prac-all.csv")

data_1 <- 
    map(url_zip_date, ~ get_gp_data_daily(.x) %>% pluck(1))
  
data_2 <- 
    map(urls_zip_no_date, ~ get_gp_data_daily(.x) %>% pluck(1)) %>% 
    map(~ .x %>% mutate(EXTRACT_DATE = dmy(EXTRACT_DATE)))

data_3 <- 
    map(urls_csv, read_csv) %>% 
    map(~ .x %>% mutate(EXTRACT_DATE = dmy(EXTRACT_DATE)))

df <- reduce(list(data_1, data_2, data_3), bind_rows)

df %>% arrange(EXTRACT_DATE)
