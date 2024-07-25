# Extract Daily GP Appointments Data for selected SUB ICB Location ---

library(tidyverse)
library(timetk)

get_daily_gpad <- function(url) {
    # Extracts daily GP appointments data from zipped csv files off NHS website
    
    # Inputs
    # - url             : web address for dataset
    
    # Output
    # - list of monthly data frames that contain data about GP appointments for all NHSE areas
    
    temporary_file <- tempfile()
    
    download.file(url, temporary_file)
    
    contents <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset("CSV")
    
    data <- 
        map(contents, ~ read_csv(unz(temporary_file, .x))) 
    
    return(data)
}