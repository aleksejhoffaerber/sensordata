# packages

library(dplyr)
library(tidyr)
library(purrr)
library(tsibble)
library(ggplot2)
library(lubridate)

# preliminary information -----

# components: shows that component os exchanged every month (differenr supplier, and component id, same sub machine)
# sensors: 53 sensors that measure every five minutes, show different (unknown) values
# shutdowns: regular factory shutdowns 

# LOADING ----

components <- readr::read_delim("Data/component_changes.csv", delim = ";") 
sensors <- readr::read_delim("Data/sensor_measurements.csv", delim = ";")
shutdowns <- readr::read_delim("Data/shutdowns.csv", delim = ";")

sensors <- sensors %>% 
  mutate(sensor_name = sensor_name %>% 
           stringr::str_replace("Sensor", "") %>%
           as.numeric() %>% 
           as.factor())

sensors_avg <- sensors %>% 
  group_by(meas_dttm = floor_date(meas_dttm, "hours"), sensor_name) %>% 
  summarise(value = mean(value, na.rm = T))

shutdowns <- shutdowns %>% 
  mutate(diff = shutdown_end_dttm - shutdown_start_dttm)

# DATA WRANGLING ----

start <- sensors_avg %>% ungroup() %>% slice(1) %>% select(meas_dttm) %>% as.matrix() %>% as_date()
end <- sensors_avg %>% ungroup() %>% tail(1) %>% select(meas_dttm) %>% as.matrix() %>% as_date()

# only 3 maintenance period that cross with the sensor data
components <- components %>% 
  filter(installation_dttm >= start &
           installation_dttm <= end) %>% 
  mutate(change = row_number())

# join data frames  
lifetime <- components %>% 
  mutate(replaced = "Y") %>% 
  right_join(sensors_avg, by = c("installation_dttm" = "meas_dttm")) %>% 
  arrange(installation_dttm) %>% 
  # fill empty values
  fill(supplier, sub_machine, component_id, change) %>% 
  # indicate datetime of replacement
  mutate(replaced = replace_na(replaced, "N"))

# add lifetime 
lifetime <- lifetime %>% 
  left_join(components %>% select(installation_dttm), keep = T) %>% 
  fill(installation_dttm.y) %>% 
  mutate(lifetime = ((installation_dttm.x - installation_dttm.y) / 3600) %>% as.numeric()) %>% 
  select(-installation_dttm.y) %>% 
  rename(installation_dttm = installation_dttm.x)

# remove shutdowns ------
# aggregation on hourly level
shutdown <- shutdowns %>% 
  mutate(shutdown_start_dttm = floor_date(shutdown_start_dttm, "hours"), shutdown_start_dttm,
         shutdown_end_dttm = floor_date(shutdown_end_dttm, "hours"), shutdown_end_dttm) %>% 
  arrange(shutdown_start_dttm) %>%
  # filter on defined area
  filter(shutdown_start_dttm >= start & 
           shutdown_end_dttm <= end)

# create to be deleted data
to_del <- map(1:nrow(shutdown), ~lifetime %>% 
      distinct(installation_dttm) %>% 
      filter(installation_dttm >= shutdown[.x,1] & installation_dttm <= shutdown[.x,2]) %>% 
      mutate(to_delete = case_when(installation_dttm >= shutdown[.x,1] & installation_dttm <= shutdown[.x,2] ~ "delete",
                                   TRUE ~ "keep"))) %>% 
  reduce(bind_rows)

lifetime_clean <- lifetime %>% 
  anti_join(to_del)


# EDA -----

change_at <- lifetime_clean %>% 
  group_by(change) %>% 
  summarise(max_lifetime = max(lifetime),
            date = max(installation_dttm)) %>% 
  filter(change < 3) %>% 
  as.data.frame()

# lifetime distribution
lifetime_clean %>% 
  ggplot(aes(installation_dttm, lifetime)) +
  geom_vline(xintercept = change_at$date, colour = "#0533ff", linetype = "dashed") +
  geom_line() +
  labs(title = "Component lifetime across complete data history",
       x = "Date",
       y = "Component Lifetime") +
  theme_bw()

# ggsave("01_EDA_Data Path.png", path = "Plots", 
#        width = 9, height = 5, dpi = 300)

# sensor behavior ----


lifetime_clean %>% 
  ggplot(aes(installation_dttm, value)) +
  geom_line(alpha = .3) +
  geom_vline(xintercept = change_at$date, colour = "red") +
  facet_wrap(~sensor_name, scales = "free_y") +
  labs(title = "Sensor data across data sample and replacement timestamps",
       x = "Date",
       y = "Sensor Values") +
  theme_bw()
  
# ggsave("04_EDA_Sensors.png", path = "Plots", 
#        width = 22, height = 10, dpi = 300)

sensor_id_combinations <- crossing(lifetime_clean %>% distinct(sensor_name),
                                   lifetime_clean %>% distinct(component_id)) %>% 
  filter(component_id < 1407)

# correlation analysis, by sensor ----- 
sensor_lifetime_corr <- map(1:nlevels(lifetime_clean$sensor_name), ~ lifetime_clean %>%
      filter(sensor_name == .x) %>%
      select(value, lifetime) %>%
      corrr::correlate(quiet = T) %>%
      slice(1) %>%
      select(lifetime) %>%
      as.numeric() %>%
      tibble(sensor = .x,
             corr = .)) %>%
  reduce(bind_rows)

sensor_lifetime_corr %>% 
  ggplot(aes(reorder(sensor, -corr), corr)) +
  geom_col(position = position_stack(reverse = TRUE), fill = "#e6aa32") +
  # geom_text(aes(label = scales::number(corr, accuracy = .01)), angle = 90, hjust = -.5) +
  scale_y_continuous(limits = c(-1,1)) +
  labs(title = "Sensor-Lifetime correlation across whole sample",
       x = "Sensor",
       y = "Correlation") +
  theme_bw()

# ggsave("02_EDA_Correlation Whole Sample.png", path = "Plots", 
#        width = 10, height = 3, dpi = 300)

# correlation analysis, by sensor and component id 
sensor_lifetime_corr_id <- map(1:nrow(sensor_id_combinations), ~ lifetime_clean %>% 
                              filter(sensor_name == paste(sensor_id_combinations[.x,1]),
                                     component_id == as.numeric(sensor_id_combinations[.x,2])) %>% 
                              select(value, lifetime) %>% 
                              corrr::correlate(quiet = T) %>% 
                              slice(1) %>% 
                              select(lifetime) %>% 
                              as.numeric() %>% 
                              tibble(component_id = as.numeric(sensor_id_combinations[.x,2]),
                                     sensor = paste(sensor_id_combinations[.x,1]),
                                     corr = .)) %>% 
  reduce(bind_rows)

sensor_lifetime_corr_id %>% 
  ggplot(aes(reorder(sensor, -corr), corr)) +
  geom_col(position = position_stack(reverse = TRUE), fill = "#e6aa32") +
  # geom_text(aes(label = scales::number(corr, accuracy = .01)), angle = 90, hjust = -.5) +
  scale_y_continuous(limits = c(-1,1)) +
  facet_wrap(~component_id, nrow = 2) +
  labs(title = "Sensor-Lifetime correlation by component ID (same manufacturer)",
       x = "Sensor",
       y = "Correlation") +
  theme_bw()

# ggsave("03_EDA_Correlation by ID.png", path = "Plots", 
#        width = 10, height = 3, dpi = 300)

# graph of best correlated component ----

lifetime_clean %>%
  filter(sensor_name == 35) %>% 
  ggplot(aes(lifetime, value)) +
  geom_rect(aes(xmin = 815, xmax = 887, ymin = 0.02, ymax = 0.125), fill = "#e6aa32", alpha = .1) +
  geom_line(alpha = .3) +
  geom_smooth(se = F, colour = "#0533ff") +
  theme_bw() +
  labs(title = "Sensor 35 across lifetime with marked replacement intervals",
       subtitle = "Highest correlation with lifetime but only until the 800 hour mark",
       x = "Component Lifetime",
       y = "Sensor Value") +
  scale_x_continuous(breaks = seq(0,900,50)) +
  scale_y_continuous(limits = c(0.02,0.125))

# ggsave("05_EDA_Sensor Correaltion.png", path = "Plots", 
#        width = 10, height = 6, dpi = 300)
  

