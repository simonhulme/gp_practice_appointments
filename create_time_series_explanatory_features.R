# create other time series for potential explanatory features ----

library(tidyverse)
library(httr2)

find_phe_area_codes <- function(area = NULL) {
    # Function to find PHE area codes for CCGS / ICB sub locations in England
    
    # Function to fetch table of sub ICB names and codes
    create_sub_icb_location_table <- function() {
        response <- 
            request("https://fingertips.phe.org.uk/api") %>%
            req_url_path_append("areas/by_area_type") %>%
            req_url_query(area_type_id = "167") %>%
            req_perform() %>%
            resp_body_json()
        
        output_tbl <- 
            map_df(response, as_tibble) %>%
            select(area_code = Code, name = Short)
        
        return(output_tbl)
    }
    
    # Function to handle input and filtering
    process_input <- function(area, sub_icb_location_tbl) {
        input <- str_to_lower(area)
        
        filtered_tbl <- 
            sub_icb_location_tbl %>%
            filter(str_detect(str_to_lower(name), input))
        
        return(filtered_tbl)
    }
    
    # Fetch sub ICB location table
    sub_icb_location_tbl <- create_sub_icb_location_table()
    
    # Handle case when no area provided
    if (is.null(area)) {
        return(sub_icb_location_tbl)
    }
    
    # Process input and filter locations
    filtered_tbl <- process_input(area, sub_icb_location_tbl)
    
    # Action if more than one area found
    if (nrow(filtered_tbl) > 1) {
        areas <- str_c(filtered_tbl$Area, collapse = ", ")
        stop(str_glue("More than one location found. Choose between {areas}"))
    }
    
    # Action if no match for selection
    if (nrow(filtered_tbl) == 0) {
        stop("No match for selection - check spelling or view full table")
    }
    
    return(filtered_tbl$area_code)
}



extract_indicator_data <- function(area_code, indicator_id, indicator_name) {
    
    # fetch response from API and extract data
    indicator_data <- 
        fetch_fingertips_data(area_code, indicator_id) %>% 
        pluck(1, "Data")
    
    # use tidy eval to create tbl containing practice codes and values of chosen indicator
    indicator_tbl <- 
        tibble(practice_code  = map_chr(indicator_data, pluck("AreaCode")),
               {{ indicator_name }} := map_dbl(indicator_data, pluck("Val")))
    
    return(indicator_tbl)
}




## population ----

