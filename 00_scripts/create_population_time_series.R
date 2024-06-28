# Create time series for potential explanatory features 

library(tidyverse)
library(rvest)
library(timetk)

# Population ----

## Available monthly with files accessed from different web pages 
## Web pages have same base url but each ends with different month and year

population_url <- 
    "https://digital.nhs.uk/data-and-information/publications/statistical/patients-registered-at-a-gp-practice/"

dates <- seq.Date(from = as.Date("2019-05-01"), to = as.Date("2024-05-01"), by = "month")

convert_date <- function(date) {
    
    year   <- year(date)
    month  <- str_to_lower(month(date, label = TRUE, abbr = FALSE))
    
    output <- paste(month, year, sep = "-")
    
    return(output)
}

dates_url <- map_chr(dates, convert_date)

full_url <- paste0(population_url, dates_url)

## Extract address for data from each webpage 

get_urls_for_data <- function(url) {
    
    html_obj <- read_html(url)
    
    data_url <- 
        html_elements(html_obj, "body") %>% 
        html_element("#resources > div:nth-child(2) > div:nth-child(1) > div > a") %>% 
        html_attr("href")
    
    return(data_url)
}

data_urls <- map(full_url, get_urls_for_registered_populations)

csv_urls <- urls[str_detect(data_urls, "csv")]

## download data using urls 

registered_population <-
    map(csv_urls, read_csv) %>%
    map( ~ .x %>% mutate(EXTRACT_DATE = dmy(EXTRACT_DATE))) %>%
    reduce(bind_rows) %>%
    janitor::clean_names() %>%
    filter(sub_icb_location_code == "03R" | ccg_code == "03R") %>%
    summarise_by_time(
        .date_var = extract_date,
        .by = "month",
        registered_population = sum(number_of_patients)
    )

## save data
write_rds(registered_population, "00_data/processed/wakefield_population_monthly.rds")
