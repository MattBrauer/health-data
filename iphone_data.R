library(tidyverse)
library(lubridate)
library(insol)
library(zoo)
library(forecast)
library(tsibble)

iphone_data_dir <- "data/apple_health_export"
iphone_data <- enframe(fs::dir_ls(iphone_data_dir, glob = "*.csv")) %>%
  mutate(name = gsub(paste0(iphone_data_dir, "/"), "", name), name = gsub(".csv", "", name)) %>%
  filter(name != "ActivitySummary")

files <- as.list(iphone_data %>% pull(value))

extracted_data <- lapply(files, read_csv)
names(extracted_data) <- iphone_data %>% pull(name)
extracted_data <- lapply(extracted_data, function(dataset)
  dataset %>%
    mutate_at(vars(matches("Date")), as_datetime)) #as.Date))


data_summary <- lapply(extracted_data, function(ds) ds %>% select(sourceName, device, creationDate, startDate, endDate))

ActivitySummary <- read_csv("data/apple_health_export/ActivitySummary.csv") %>%
  filter(dateComponents > '1980-01-01')

ActiveEnergy <- read_csv(files[1])

extracted_data$BodyMass %>%
  mutate_at(vars(matches("Date")), as.Date) %>%
  filter(startDate > '2019-06-01') %>%
  ggplot(aes(x=startDate, y=value)) +
  geom_point()

ActivitySummary %>%
  mutate(date = dateComponents) %>%
  full_join(
extracted_data$RestingHeartRate %>%
  mutate_at(vars(matches("Date")), as.Date) %>%
  group_by(startDate) %>%
  summarize(value = mean(value)) %>%
  rename(date = startDate, RestingHeartRate = value)
) %>%
  select(date, RestingHeartRate, activeEnergyBurned, appleExerciseTime, appleStandHours) %>%
  ggplot(aes(x=activeEnergyBurned, y=RestingHeartRate)) + geom_point()
  

extracted_data$RestingHeartRate %>%
  mutate(rolling_avg = rollapply(value, 60, median, fill=NA, partial=TRUE, align="right")) %>%
  select(startDate, value, rolling_avg) %>%
  rename(date = startDate) %>%
  full_join(workouts %>% filter(name=="Run")) %>%
  {ggplot(.) + geom_point(aes(x=date, y=value)) +
  geom_rug(data = . %>% filter(!is.na(name)),
           aes(x = date), inherit.aes = F, size=0.1)}

extracted_data$BodyMass %>%
  mutate(rolling_avg = rollapply(value, 60, median, fill=NA, partial=TRUE, align="right")) %>%
  select(startDate, value, rolling_avg) %>%
  rename(date = startDate) %>%
  full_join(workouts %>% filter(name=="Run")) %>%
  filter(date > '2019-01-01') %>%
  {ggplot(.) + geom_point(aes(x=date, y=value)) +
      geom_rug(data = . %>% filter(!is.na(name)),
               aes(x = date), inherit.aes = F, size=0.5)}

extracted_data$AppleExerciseTime %>%
  mutate(date = as.Date(startDate), hour = hour(startDate)) %>%
  group_by(date, hour) %>%
  summarize(hourly = sum(value)) %>%
  mutate(datehour = as_datetime(date) + hours(hour)) %>%
  filter(date > '2019-01-01') %>%
  ggplot() +
  geom_point(aes(datehour, hourly), alpha = 0.1)

extracted_data$AppleExerciseTime %>%
  mutate(date = as.Date(startDate), hour = hour(startDate)) %>%
  group_by(date) %>%
  summarize(daily = sum(value)) %>%
  mutate(weekly = rollapply(daily, 30, mean, fill=NA, partial=TRUE, align="right")) %>%
  ggplot() +
  geom_point(aes(date, daily), alpha = 0.1) +
  geom_line(aes(date, weekly))

