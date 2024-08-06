library(tidyverse)
library(rvest)
library(timetk)

# WRITE AS A FUNCTION

get_monthly_population_by_area <- function(area_code, start_date, end_date) {
    
    # Helper Functions
    
    find_urls_containing_links_to_data <- function(start_date, end_date) {
        
        base_url <- 
            "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/"
        
        dates <- 
            tk_make_timeseries(start_date = start_date, end_date = end_date, by = "month")
        
        convert_date <- function(date) {
            
            year   <- year(date)
            month  <- str_to_lower(month(date, label = TRUE, abbr = FALSE))
            
            output <- paste(month, year, sep = "-")
            
            return(output)
        }
        
        dates_url <- map_chr(dates, convert_date)
        
        full_url <- paste0(base_url, dates_url)
        
        return(full_url)
    }
    
    extract_urls_for_datasets <- function(url) {
        
        html_obj <- read_html(url)
        
        data_url <- 
            html_elements(html_obj, "body") %>% 
            html_element("#resources > div:nth-child(2) > div:nth-child(1) > div > a") %>% 
            html_attr("href")
        
        return(data_url)
    }
    
   download_data_from_csv_files <- function(dataset_urls) {
       
       csv_urls <- dataset_urls[str_detect(dataset_urls, "csv")]
       
       downloaded_csv_files <- map(csv_urls, read_csv)
       
       return(downloaded_csv_files)
       
   }

   download_data_from_zip_files <- function(dataset_urls) {
       
       zip_urls <- dataset_urls[str_detect(dataset_urls, "zip")]
       
       get_csv_from_url_zip <- function(url) {
           temporary_file <- tempfile()
           
           download.file(url, temporary_file)
           
           contents <- 
               unzip(temporary_file, list = TRUE) %>% 
               pull(Name) %>% 
               str_subset("csv")
           
           data <- 
               map(contents, ~ read_csv(unz(temporary_file, .x))) 
           
           return(data) 
       }
       
       zip_files_downloaded <- 
           map(zip_urls, get_csv_from_url_zip) %>% 
           flatten()
       
       return(zip_files_downloaded)
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
       df %>% mutate(EXTRACT_DATE = parse_date_time(
           EXTRACT_DATE,
           orders = c("dmy", "ymd")
       ))
   }
   
   full_urls <-
       find_urls_containing_links_to_data(start_date = start_date, end_date = end_date)

   dataset_urls <- map(full_urls, extract_urls_for_datasets)

   downloaded_csv_files <-
       download_data_from_csv_files(dataset_urls)
    
   downloaded_zip_files <- 
       download_data_from_zip_files(dataset_urls)

   all_data <- 
       append(downloaded_csv_files, downloaded_zip_files) 
   
   population_by_area <- 
       map(all_data, rename_columns) %>% 
       map(~ filter(.x, AREA_CODE == area_code)) %>% 
       map(reformat_dates) %>% 
       reduce(bind_rows) %>%
       janitor::clean_names() %>%
       summarise_by_time(
           .date_var = extract_date,
           .by = "month",
           population = sum(number_of_patients)) %>% 
       mutate(extract_date = as.Date(extract_date))
       
   return(population_by_area)

}