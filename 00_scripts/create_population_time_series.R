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
       
       csv_files_downloaded <- map(csv_urls, read_csv)
       
       return(csv_files_downloaded)
       
   }

   ## download data from zip files
   
   ### identify zip files
   zip_urls <- dataset_urls[str_detect(dataset_urls, "zip")]
   
   ### download csv from zip files
   
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
   
   
   
   
   
   
    full_url <- find_urls_containing_links_to_data(start_date = start_date, end_date = end_date)
    dataset_urls <- map(full_url, extract_urls_for_datasets)
    csv_files <- download_data_from_csv_files(dataset_urls)
    
    return(csv_files)
    
}


csv_files <- get_monthly_population_by_area(start_date = "2019-05-01", end_date = "2024-07-01")

csv_files


# Population ----

## Available monthly with files accessed from different web pages 
## Web pages have same base url but each ends with different month and year

## Get URLs for webpages that link to datasets

base_url <- 
    "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/"

dates <- 
    tk_make_timeseries(start_date = "2019-05-01", end_date = "2024-07-01", by = "month")

convert_date <- function(date) {
    
    year   <- year(date)
    month  <- str_to_lower(month(date, label = TRUE, abbr = FALSE))
    
    output <- paste(month, year, sep = "-")
    
    return(output)
}

dates_url <- map_chr(dates, convert_date)

full_url <- paste0(base_url, dates_url)

## Extract urls for data from each webpage 

get_dataset_urls <- function(url) {
    
    html_obj <- read_html(url)
    
    data_url <- 
        html_elements(html_obj, "body") %>% 
        html_element("#resources > div:nth-child(2) > div:nth-child(1) > div > a") %>% 
        html_attr("href")
    
    return(data_url)
}

dataset_urls <- map(full_url, get_dataset_urls)

## download data from csv files

### identify csv files
csv_urls <- dataset_urls[str_detect(dataset_urls, "csv")]

### download csv files
csv_files_downloaded <-
    map(csv_urls, read_csv)

## download data from zip files

### identify zip files
zip_urls <- dataset_urls[str_detect(dataset_urls, "zip")]

### download csv from zip files

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

## Join files together

all_csvs <- 
    append(csv_files_downloaded, zip_files_downloaded) 

## Standardise variable names and class
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

wakefield_population <- 
    map(all_csvs, rename_columns) %>% 
    map(~ filter(.x, AREA_CODE == "03R")) %>% 
    map(reformat_dates)


wakefield_population_monthly <- 
    wakefield_population %>%
    reduce(bind_rows) %>%
    janitor::clean_names() %>%
    summarise_by_time(
        .date_var = extract_date,
        .by = "month",
        population = sum(number_of_patients))

wakefield_population_monthly %>% 
    plot_time_series(.date_var = extract_date, .value = population)

## save data
write_rds(wakefield_population_monthly, "00_data/processed/wakefield_population_monthly.rds")