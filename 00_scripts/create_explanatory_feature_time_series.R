# create time series for potential explanatory features ----

## Extract data for a single GP practice

## Trinity Medical Centre ----

### Codes

## WFD ODS CODE : 03R
## WFD PHE CODE : E38000190
## PRACTICE CODE: B87017

library(tidyverse)
library(fingertipsR)

### QOF Population Data ----

qof_annual_population <- 
    fingertips_data(IndicatorID = 114, AreaTypeID = 7) %>% 
    as_tibble() %>% 
    filter(AreaCode == "B87017") %>% 
    select(Timeperiod, Value) %>% 
    mutate(date = str_split_i(Timeperiod, "/", 1)) %>% 
    mutate(date = as.Date(paste0(date, "-04-01"))) %>% 
    mutate(date = date + years(1)) %>% 
    select(date, population = Value)

write_rds(qof_annual_population, "00_data/raw/qof_annual_population.rds")

###  TODO ----

