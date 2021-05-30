# packages

library(dplyr)
library(recipes)
library(tsibble)
library(ggplot2)
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
# FIXME: sensor values - comparable across different sensors

# TODO: change sensor_name to factor
# TODO: change all date value to tsibble date format

sensors <- sensors %>% 
  mutate(sensor_name = sensor_name %>% 
           stringr::str_replace("Sensor", "") %>%
           as.numeric() %>% 
           as.factor())

# EDA ----

sensors %>% 
  ggplot(aes(sensor_name, value)) + 
  geom_boxplot() +
  theme_bw()
  

# feature engineering ------

# TODO: shutdowns: diff between start - end