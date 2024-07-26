# Extract zipped csv files ----

library(tidyverse)
library(timetk)

extract_zipped_csv_files <- function(url) {
    
    temporary_file <- tempfile()
    
    download.file(url, temporary_file)
    
    contents <- 
        unzip(temporary_file, list = TRUE) %>% 
        pull(Name) %>% 
        str_subset("CSV|csv")
    
    data <- 
        map(contents, ~ read_csv(unz(temporary_file, .x))) 
    
    return(data)
}