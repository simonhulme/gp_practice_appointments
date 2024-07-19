# Exploratory Data Analysis

# Set Up ----
library(tidyverse)
library(timetk)

wakefield_working_week_daily <- read_rds("00_data/processed/wakefield_working_week_daily.rds")

# Summary Statistics ----

skimr::skim(wakefield_working_week_daily)

# 1348 days between 2019-03-91 and 2024-04-30
# 4 categorical variables (nominal)
# numeric response variable with no missing values

# Exploratory Visualisation ----

### Total Appointments by Feature ----

wakefield_working_week_daily %>%
    group_by(hcp_type) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(hcp_type = fct_reorder(hcp_type, -total_appointments)) %>%
    ggplot(aes(hcp_type, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw()

wakefield_working_week_daily %>%
    group_by(appt_mode) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(appt_mode = fct_reorder(appt_mode, -total_appointments)) %>%
    ggplot(aes(appt_mode, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw()

wakefield_working_week_daily %>%
    group_by(appt_status) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(prop = proportions(total_appointments)) 

wakefield_working_week_daily %>%
    group_by(time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments)) +
    geom_col(fill = "skyblue", color = "grey30") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    xlab("Time between booking and appointment date")


### GP vs Other Staff by Feature ----
wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff",
           appt_mode %in% c("Face-to-Face", "Telephone")) %>% 
    group_by(hcp_type, appt_mode) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(appt_mode = fct_reorder(appt_mode, -total_appointments)) %>%
    ggplot(aes(appt_mode, total_appointments, fill = hcp_type)) +
    geom_col(color = "grey30", position = "dodge") +
    theme_bw()

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>% 
    group_by(hcp_type, appt_status) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    mutate(prop = proportions(total_appointments)) %>% 
    ggplot(aes(hcp_type, total_appointments, fill =  appt_status,)) +
    geom_col(color = "grey30", position = "fill") +
    theme_bw()

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>% 
    group_by(hcp_type, time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments, fill = hcp_type)) +
    geom_col(position = "fill") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
    xlab("Time between booking and appointment date")

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>%   
    mutate(appt_status = fct_relevel(appt_status, c("Attended", "Unknown", "DNA"))) %>%
    group_by(hcp_type, appt_status, time_between_book_and_appt) %>%
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ggplot(aes(time_between_book_and_appt, total_appointments, fill = appt_status)) +
    geom_col(position = "fill") +
    theme_bw() +
    scale_fill_brewer(palette = 9) +
    labs(title = "Very few DNA or Unknown status for same day appointments",
         subtitle = "DNA and Unknown proportion increases as delay increases") +
    facet_wrap(~ hcp_type) 

# Time series Analysis ----

## Total Appointments Booked ----

total_booked_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff") %>%  
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>% 
    ungroup() %>% 
    pivot_wider(names_from = hcp_type, values_from = appointments) %>% 
    mutate(Total = GP + `Other Practice staff`) %>% 
    pivot_longer(cols = -appointment_date, names_to = "hcp_type", values_to = "appointments")

# using loess smoother 
total_booked_daily_tbl %>% 
    group_by(hcp_type) %>% 
    plot_time_series(.date_var = appointment_date,
                     .value = appointments,
                     .title = "Time Series Plot: Total Daily Appointments")

total_booked_daily_tbl %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: Total Weekly Appointments")

total_booked_daily_tbl %>%
    group_by(hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(.date_var = appointment_date,
                     .value = total_appointments,
                     .title = "Time Series Plot: Total Monthly Appointments")

# using moving/rolling average
total_booked_daily_tbl %>% 
    filter(hcp_type == "Total") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    mutate(appointments_roll = slidify_vec(appointments, .f = mean, .period = 12)) %>% 
    pivot_longer(cols = contains("appointments")) %>% 
    plot_time_series(.date_var = appointment_date, .value = value, .color_var = name, .smooth = FALSE)

## Advance vs. Same-Day Appointment Booking by HCP type ----

same_day_vs_advance_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type %in% c("GP", "Other Practice staff") ,
           time_between_book_and_appt != "Unknown") %>%
    mutate(booking =
               if_else(time_between_book_and_appt == "Same Day", "Same Day", "Advance")) %>%
    group_by(hcp_type, booking, appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

same_day_vs_advance_daily_tbl %>%
    group_by(hcp_type, booking) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = total_appointments,
        .smooth = T, 
        .facet_ncol = 2
    )

same_day_vs_advance_daily_tbl %>%
    group_by(hcp_type, booking) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = total_appointments,
        .smooth = T, 
        .facet_ncol = 2
    )

## Face-to-Face vs Telephone appointments by HCP Type ----

f2f_vs_phone_daily_tbl <-
    wakefield_working_week_daily %>%
    filter(hcp_type %in% c("GP", "Other Practice staff") ,
           appt_mode %in% c("Face-to-Face", "Telephone")) %>% 
    group_by(hcp_type, appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    ungroup()

f2f_vs_phone_daily_tbl %>% 
    group_by(hcp_type, appt_mode) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        total_appointments = sum(appointments)
    ) %>%
    plot_time_series(
        .date_var = appointment_date,
        .value = total_appointments,
        .smooth = T, 
        .facet_ncol = 2
    )

## TODO: comment on below 

# Autocorrelation Functions (ACF/PACF) ----

total_booked_daily_tbl %>%
    group_by(hcp_type) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .interactive = TRUE,
        .title = "ACF / PACF Plots: GP/Other Staff Total Appointments",
        .lags = 260
    )

same_day_vs_advance_daily_tbl %>% 
    filter(hcp_type == "GP") %>% 
    group_by(booking) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .interactive = TRUE,
        .title = "ACF / PACF Plots: GP Appts Advance Booked / Same Day",
        .lags = 260
    )

same_day_vs_advance_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff") %>% 
    group_by(booking) %>% 
    plot_acf_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .interactive = TRUE,
        .title = "ACF / PACF Plots: Other Practice staff Advance Booked / Same Day",
        .lags = 260
    )

# Seasonality ----

## Summary ----
same_day_vs_booked_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "month.lbl", "year"),
        .title = "",
        .facet_vars = appt_mode,
        .interactive = FALSE
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff",
           appt_mode %in% c("Face-to-Face", "Telephone")) %>% 
    mutate(day = wday(appointment_date, label = TRUE),
           appt_mode = fct_rev(appt_mode)) %>% 
    group_by(day, hcp_type, appt_mode) %>% 
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ungroup() %>% 
    ggplot(aes(day, total_appointments, fill = appt_mode)) +
    geom_col(position = "fill") +
    theme_bw() +
    facet_wrap(~ hcp_type) +
    scale_fill_brewer(type = "qual", palette = 3)

wakefield_working_week_daily %>%
    filter(hcp_type == "GP" | hcp_type == "Other Practice staff",
           appt_mode %in% c("Face-to-Face", "Telephone")) %>% 
    mutate(month = month(appointment_date, label = TRUE),
           appt_mode = fct_rev(appt_mode)) %>% 
    group_by(month, hcp_type, appt_mode) %>% 
    summarise(total_appointments = sum(count_of_appointments)) %>%
    ungroup() %>% 
    ggplot(aes(month, total_appointments, fill = appt_mode)) +
    geom_col(position = "fill") +
    theme_bw() +
    facet_wrap(~ hcp_type) +
    scale_fill_brewer(type = "qual", palette = 3)

same_day_vs_booked_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    plot_seasonal_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .feature_set = c("wday.lbl", "month.lbl", "year"),
        .title = "",
        .facet_vars = c(booking, hcp_type),
        .interactive = FALSE,
    ) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5))

same_day_vs_booked_daily_tbl %>% 
    mutate(day = wday(appointment_date, label = TRUE)) %>% 
    group_by(day, hcp_type, booking) %>% 
    summarise(appointments = sum(appointments)) %>%
    ungroup() %>% 
    ggplot(aes(day, appointments, fill = booking)) +
    geom_col(position = "fill") +
    theme_bw() +
    facet_wrap(~ hcp_type) +
    scale_fill_brewer(palette = 3)

same_day_vs_booked_daily_tbl %>% 
    mutate(month = month(appointment_date, label = TRUE),
           appt_mode = fct_rev(appt_mode)) %>% 
    group_by(month, hcp_type, booking) %>%
    summarise(appointments = sum(appointments)) %>%
    ungroup() %>% 
    ggplot(aes(month, appointments, fill = booking)) +
    geom_col(position = "fill") +
    theme_bw() +
    facet_wrap(~ hcp_type) +
    scale_fill_brewer(type = "qual", palette = 3)



# Anomalies ----

wakefield_working_week_daily %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .title = "Troughs for total apppointments when weeks that contain holiday "
    )

wakefield_working_week_daily %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online")) %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .title = ""
    )

# BY MODE & HCP

wakefield_working_week_daily %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online"),
           hcp_type != "HCP Type Not Provided") %>%
    group_by(appt_mode, hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .facet_ncol = 3,
        .title = ""
    )

# BY BOOKING & HCP

same_day_vs_booked_daily_tbl %>%
    filter(!appt_mode %in% c("Unknown", "Video Conference/Online"),
           hcp_type != "HCP Type Not Provided") %>%
    group_by(booking, hcp_type) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_anomaly_diagnostics(
        .date_var = appointment_date,
        .value = appointments,
        .alpha = 0.05,
        .interactive = T,
        .facet_ncol = 3,
        .title = ""
    )

# Seasonal Decomposition ----

## Total Appointments Booked - Total Monthly Appointments

total_booked_daily_tbl %>%
    ungroup() %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(
        appointment_date,
        appointments,
        .feature_set = c("observed", "season", "trend", "remainder")
    )



## Total Appointments Booked - Mean daily value by Month Post Pandemic

total_booked_daily_tbl %>%
    ungroup() %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = mean(appointments)
    ) %>%
    plot_stl_diagnostics(
        appointment_date,
        appointments,
        .feature_set = c("observed", "season", "trend", "remainder"), 
        .frequency = 12
    )

## NEED TO COMMENT ON STL DECOMPOSITION



## separate time series by: hcp / mode / booking

### GP: total ----

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP") %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>%
    filter(hcp_type == "GP") %>%
    filter_by_time(.date_var = appointment_date, .start_date = "2021-08") %>%
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(
        appointment_date,
        appointments,
        .feature_set = c("observed", "season", "trend", "remainder"), 
        .frequency = 12
    )






wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))










wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))
    
### Other Staff: telephone vs face to face vs home visit ----

wakefield_working_week_daily %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

wakefield_working_week_daily %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(appt_mode) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(count_of_appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))
    
### GP : Same Day vs Booked ----

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "GP", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

### Other Practice staff: Same Day vs Booked ----

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "day",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "week",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

same_day_vs_booked_daily_tbl %>% 
    filter(hcp_type == "Other Practice staff", appt_mode %in% c("Face-to-Face", "Telephone", "Home Visit")) %>% 
    group_by(booking) %>% 
    summarise_by_time(
        .date_var = appointment_date,
        .by = "month",
        appointments = sum(appointments)
    ) %>%
    plot_stl_diagnostics(appointment_date, appointments, .feature_set = c("observed", "season", "trend", "remainder"))

# FINDINGS ----

## 1. Total appointments: Other Practice Staff > GP ----
### * Face-to-face appointments responsible for excess ----
## 2. Most appointments face-to-face followed by telephone ----
## 3. 3.3% of total appointments no attended, 5% status not known ----
### * Percentage DNA and Unknown: Other Practice Staff > GP ----
### * Percentage DNA and Unknown - increases with longer waits ----
## 4. Most appointments are Same Day appointments ----
### * Booked appointments tend to be non-GP appointments ----
## 5. Regular dips in series - Bank Holidays & Staff Training ----
### * Weekly total may be less but daily could be higher ----
## 6. Downward trend in GP appts but upward trend for other staff ----
## 7. GP advance bookings stable but same day appointments falling ----
### * Rapid growth in advance and same day appointments for other staff ----
## 8. Increasing Face-to-Face appointments for all health care professionals  ----
### * Possible correction after significant changes in practice during pandemic ----

# TODO ----
## 1. Create time series to handle anomalies ----
### * Bank Holidays, Training Afternoons, Pandemic ----
### * compare weekly totals with mean daily appointments per week ----
## 2. Focus in on a few subsets of data ----
### * Total Appointments vs GP vs Other Practice staff by mode and booking ----
#### ** 12 potential time series ----
