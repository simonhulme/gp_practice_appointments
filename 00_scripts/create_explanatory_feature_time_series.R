# Create time series for potential explanatory features 

library(tidyverse)
library(rvest)
library(timetk)

# Population
# Workforce
# On-line Consultations
# Telephone Consultation

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

# Workforce ----

## data from September 2020 to July 2022 ----

temporary_file <- tempfile()

download.file("https://files.digital.nhs.uk/32/6E1670/GPWFPracticeCSVpt2.072022.zip", temporary_file)

contents <- 
  unzip(temporary_file, list = TRUE) %>% 
  pull(Name)

### September 2020 ----

september_2020 <- 
  read_csv(unz(temporary_file, contents[[3]]))

df_september_2020 <- 
  september_2020 %>% 
  filter(SUB_ICB_CODE == "03R") %>% 
  select(PRAC_CODE, PRAC_NAME, TOTAL_GP_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE) %>% 
  mutate(extract_date = as.Date("2020-09-30"), .before = PRAC_CODE) %>% 
  mutate(across(where(is.character), ~ na_if(.x, "ND"))) %>% 
  mutate(across(contains("TOTAL"), as.numeric))
  
### December 2020 ----

december_2020 <- 
  read_csv(unz(temporary_file, contents[[4]]))

df_december_2020 <- 
  december_2020 %>% 
  filter(SUB_ICB_CODE == "03R") %>% 
  select(PRAC_CODE, PRAC_NAME, TOTAL_GP_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE) %>% 
  mutate(extract_date = as.Date("2020-12-31"), .before = PRAC_CODE) %>% 
  mutate(across(where(is.character), ~ na_if(.x, "ND"))) %>% 
  mutate(across(contains("TOTAL"), as.numeric))

### March 2021 ----

march_2021 <- 
  read_csv(unz(temporary_file, contents[[5]]))

df_march_2021 <- 
  march_2021 %>% 
  filter(SUB_ICB_CODE == "03R") %>% 
  select(PRAC_CODE, PRAC_NAME, TOTAL_GP_FTE, TOTAL_NURSES_FTE, TOTAL_DPC_FTE, TOTAL_ADMIN_FTE) %>% 
  mutate(extract_date = as.Date("2020-12-31"), .before = PRAC_CODE) %>% 
  mutate(across(where(is.character), ~ na_if(.x, "ND"))) %>% 
  mutate(across(contains("TOTAL"), as.numeric))


data <- 
  map(contents, ~ read_csv(unz(temporary_file, .x))) 

data <- 
  data %>% 
  map(~ filter(.x, SUB_ICB_CODE == "03R"))

workforce <- 
  data[[1]] %>%
  select("TOTAL_GP_FTE", "TOTAL_NURSES_FTE", "TOTAL_DPC_FTE", "TOTAL_ADMIN_FTE") %>% 
  mutate(across(everything(), as.numeric)) %>% 
  colSums() %>% 
  round(2)





workforce_url <- 
  "https://digital.nhs.uk/data-and-information/publications/statistical/general-and-personal-medical-services/"

## create sequence of dates representing last day of each month 

dates <- 
  tk_make_timeseries(start_date = "2022-01", end_date = "2024-05", by = "month")

dates_urls <- format.Date(dates, "%d-%B-%Y") %>% map(str_to_lower)

full_urls <- paste0(workforce_url, dates_urls)

# scrape urls for data sets

## get all urls as a list

get_urls_for_data <- function(url) {
  html_obj <- read_html(url)
  
  data_url <-
    html_elements(html_obj, "body") %>%
    html_elements("#resources > div > div")
  
  return(data_url)
  
}

data_urls <- map(full_urls, get_urls_for_data)

extract_individual_urls <- function(data_urls) {
  
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

urls <- map(map(data_urls, extract_individual_urls), ~ str_subset(.x, "GPWPracticeCSV"))

urls

dates_urls[[7]]





## TODO

# get_gp_data_daily <- function(url) {
#     
#     temporary_file <- tempfile()
#     
#     download.file(url, temporary_file)
#     
#     contents <- 
#         unzip(temporary_file, list = TRUE) %>% 
#         pull(Name)
#     
#     data <- 
#         map(contents, ~ read_csv(unz(temporary_file, .x)))
#     
#     return(data)
# }

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

# Patients Registered by Area

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
      "https://files.digital.nhs.uk/AF/9B0FC8/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/F1/3E4D66/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/87/E03B72/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/64/D830C6/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/7C/D10294/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/ED/C675EF/gp-reg-pat-prac-all.csv",
      "https://files.digital.nhs.uk/D7/73169E/gp-reg-pat-prac-all.csv"
      )

data_1 <- 
    map(url_zip_date, ~ get_gp_data_daily(.x) %>% pluck(1))
  
data_2 <- 
    map(urls_zip_no_date, ~ get_gp_data_daily(.x) %>% pluck(1)) %>% 
    map(~ .x %>% mutate(EXTRACT_DATE = dmy(EXTRACT_DATE)))

data_3 <- 
    map(urls_csv, read_csv) %>% 
    map(~ .x %>% mutate(EXTRACT_DATE = dmy(EXTRACT_DATE)))

df <- 
    reduce(list(data_1, data_2, data_3), bind_rows) %>%
    janitor::clean_names() %>% 
    filter(sub_icb_location_code == "03R") %>%
    summarise_by_time(.date_var = extract_date, .by = "day", registered_population = sum(number_of_patients))

df %>%
    plot_time_series(.date_var = extract_date, registered_population, .smooth= FALSE)
ﬂ
