# Create time series for potential explanatory features 

library(tidyverse)
library(rvest)
library(timetk)

# WRITE AS A FUNCTION

# Population ----

## Available monthly with files accessed from different web pages 
## Web pages have same base url but each ends with different month and year

## Get URLs for webpages that link to datasets

base_url <- 
    "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/"

dates <- seq.Date(from = as.Date("2019-05-01"), to = as.Date("2024-07-01"), by = "month")

convert_date <- function(date) {
    
    year   <- year(date)
    month  <- str_to_lower(month(date, label = TRUE, abbr = FALSE))
    
    output <- paste(month, year, sep = "-")
    
    return(output)
}

date_url <- map_chr(dates, convert_date)

full_url <- paste0(base_url, date_url)

## Extract urls for data from each webpage 

get_urls_for_data <- function(url) {
    
    html_obj <- read_html(url)
    
    data_url <- 
        html_elements(html_obj, "body") %>% 
        html_element("#resources > div:nth-child(2) > div:nth-child(1) > div > a") %>% 
        html_attr("href")
    
    return(data_url)
}

data_urls <- map(full_url, get_urls_for_data)

## download data from csv files

csv_urls <- data_urls[str_detect(data_urls, "csv")]

csv_files_downloaded <-
    map(csv_urls, read_csv)

## download data from zip files

zip_urls <- data_urls[str_detect(data_urls, "zip")]

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

all_downloads <- 
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
    map(all_downloads, rename_columns) %>% 
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
