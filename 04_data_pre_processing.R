# Data Pre Processing ----

## Set Up ----

# Load Libraries

library(tidyverse)
library(timetk)
library(DataExplorer)

# Import Data 

## Start with Totals 

total_appointments   <-
    read_rds("00_data/processed/sliced/total/wakefield_total_5_day_tbl.rds")

appointments_by_mode <-
    read_rds("00_data/processed/sliced/appt_mode/wakefield_appt_mode_5_day_sliced_tbl.rds")

appointments_by_hcp <- 
    read_rds("00_data/processed/sliced/hcp_type/wakefield_hcp_type_5_day_sliced_tbl.rds")


