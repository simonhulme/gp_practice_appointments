# Extract Daily GP Appointments Data for selected SUB ICB Location ---

library(tidyverse)
library(timetk)

get_daily_gpad <- function(url) {
    # Extracts daily GP appointments data from NHS website
    
    # Inputs
    # - url             : web address for data
    # - location_code   : 3 digit sub icb location code
    
    # Output
    # - list containing daily gp appointments data by month for all NHS areas in England
    
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