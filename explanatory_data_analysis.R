# packages

library(dplyr)
library(recipes)
library(tsibble)
library(ggplot2)
library(lubridate)
library(patchwork)
library(tidymodels)

# preliminary information -----

# components: shows that component os exchanged every month (differenr supplier, and component id, same sub machine)
# sensors: 53 sensors that measure every five minutes, show different (unknown) values
# shutdowns: regular factory shutdowns 

# load data ----

components <- readr::read_delim("Data/component_changes.csv", delim = ";") 
sensors <- readr::read_delim("Data/sensor_measurements.csv", delim = ";")
shutdowns <- readr::read_delim("Data/shutdowns.csv", delim = ";")

# TODO: how to combine the data sets? 1) meas_dttm & 

# TODO: change all date value to tsibble date format

sensors <- sensors %>% 
  mutate(sensor_name = sensor_name %>% 
           stringr::str_replace("Sensor", "") %>%
           as.numeric() %>% 
           as.factor())

sensors_avg <- sensors %>% 
  group_by(meas_dttm = floor_date(meas_dttm, "hours"), sensor_name) %>% 
  summarise(value = mean(value, na.rm = T))

# TODO: shutdowns: diff between start - end
shutdowns <- shutdowns %>% 
  mutate(diff = shutdown_end_dttm - shutdown_start_dttm)


# EDA ----

sensors_avg %>% 
  ggplot(aes(sensor_name, value)) + 
  geom_boxplot() +
  theme_bw()
  

# combine sensors and component data

# TODO: waht is meant with "lifetime" given sensors?
# especially, because it is changed monthly...


start <- sensors_avg %>% ungroup() %>% slice(1) %>% select(meas_dttm) %>% as.matrix() %>% as_date()
end <- sensors_avg %>% ungroup() %>% tail(1) %>% select(meas_dttm) %>% as.matrix() %>% as_date()

# only 3 maintenance period that cross with the sensor data
components <- components %>% 
  filter(installation_dttm >= start &
           installation_dttm <= end) %>% 
  mutate(change = row_number())

# join data frames  ------
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

# check for cleanness
lifetime_clean %>% 
  ggplot(aes(installation_dttm, value)) +
  geom_line() +
  facet_wrap(~sensor_name, scales = "free") +
  theme_bw()

# correlation analysis

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
  geom_col(position = position_stack(reverse = TRUE)) +
  # geom_text(aes(label = scales::number(corr, accuracy = .01)), angle = 90, hjust = -.5) +
  scale_y_continuous(limits = c(-1,1)) +
  labs(x = "Sensor",
       y = "Correlation") +
  theme_bw()

# forecasting model (prediction for sensor 35)




