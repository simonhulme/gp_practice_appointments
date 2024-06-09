library(tidyverse)
library(timetk)

wakefield_raw <- read_rds("00_data/raw/wakefield_daily.rds")

wakefield_total_day_tbl <-
    wakefield_raw %>%
    summarise_by_time(.date_var = appointment_date,
                      .by = "day",
                      appointments = n()) %>%
    pad_by_time(.date_var = appointment_date,
                .by = "day",
                .pad_value = 0)

write_rds(wakefield_total_day_tbl, "00_data/processed/wakefield_total_day_tbl.rds")