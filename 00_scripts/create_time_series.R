library(tidyverse)

wakefield_raw <- read_rds("00_data/raw/wakefield_daily.rds")

glimpse(wakefield_raw)

# aggregate

wakefield_total <- 
    wakefield_raw %>% 
    select(appointment_date) %>% 
    group_by(appointment_date) %>% 
    summarise(total_appointments = n())

write_rds(wakefield_total, "00_data/processed/wakefield_total.rds")