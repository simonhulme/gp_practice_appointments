library(tidyverse)
library(timetk)
library(rvest)

# obtain zipped files ----

## September 2020 to July 2022 ----

url_zipped_files <- list("https://files.digital.nhs.uk/32/6E1670/GPWFPracticeCSVpt2.072022.zip")

## August 2022 to latest ----

## scrape web pages for urls for zipped practice CSV file 

base_url <- 
    "https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/"

## create sequence of dates representing last day of each month 

dates <- tk_make_timeseries(start_date = "2022-08", end_date = "2024-05", by = "month")

dates_urls <- format.Date(dates, "%d-%B-%Y") %>% map(str_to_lower)

full_urls <- paste0(base_url, dates_urls)

get_urls_for_all_data <- function(url) {
   
     html_obj <- read_html(url)
    
    data_url <-
        html_elements(html_obj, "body") %>%
        html_elements("#resources > div > div")
    
    return(data_url)
    
}

data_urls <- map(full_urls, get_urls_for_all_data)

get_urls_for_practice_data <- function(data_urls) {
    
    total_urls = length(data_urls)
    
    url_list = list()
    
    for (url in 1:total_urls) {
        
        url <-  data_urls %>% 
            pluck(url) %>% 
            html_element("div > a") %>% 
            html_attr("href")
        
        url_list = c(url_list, url)
        
    }
    
    url_list
}

urls <- 
    map(data_urls, get_urls_for_practice_data) %>% 
    map(~ str_subset(.x, "GPWPracticeCSV"))

urls <- 
    url_zipped_files %>% 
    c(urls)

urls

# ----

extract_zipped_data <- function(url) {
    
    temporary_file <- tempfile()
    download.file(url, temporary_file)
    
    list_of_csv_files <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset(".csv") %>% 
        str_subset("High", negate = TRUE)

    extract_dates <- 
        list_of_csv_files %>% 
        str_remove("^[:digit:]+.") %>% 
        str_remove("( General Practice )") %>% 
        str_remove("[:punct:] ") %>% 
        str_remove(" Practice [L|l]evel.csv") 
        
    zipped_data <- 
        tibble(
           extract_date = extract_dates,
           csv_file     = list_of_csv_files
        )
    
    zipped_data <- 
        zipped_data %>% 
        mutate(
            data = map(csv_file, ~ read_csv(unz(temporary_file, .x))),
            data = map(data, ~ filter(.x, SUB_ICB_CODE == "03R")))
    
    process_workforce_data <- function(data) {
        
        data %>% 
            select(PRAC_CODE, TOTAL_GP_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE) %>% 
            mutate(across(where(is.character), ~ na_if(.x, "ND"))) %>% 
            mutate(across(contains("TOTAL"), as.numeric))
    }
    
    workforce_data <- 
        zipped_data %>% 
        mutate(data = map(data, process_workforce_data)) %>% 
        unnest(data) %>% 
        select(-csv_file) %>% 
        mutate(extract_date = dmy(paste0("01 ", extract_date))) %>% 
        replace_na(list(TOTAL_GP_FTE = 0, TOTAL_NURSES_FTE = 0, TOTAL_DPC_FTE = 0)) %>% 
        group_by(extract_date) %>% 
        summarise(
            total_gp = sum(TOTAL_GP_FTE) %>% round(2),
            total_nurse = sum(TOTAL_NURSES_FTE) %>% round(2),
            total_dpc = sum(TOTAL_DPC_FTE) %>% round(2)
        )
    
    return(workforce_data)
}

workforce_data_list <- 
    map(urls, extract_zipped_data)

workforce_df <- 
    reduce(workforce_data_list, bind_rows)

write_rds(workforce_df, "00_data/processed/wakefield_workforce_monthly.rds")
