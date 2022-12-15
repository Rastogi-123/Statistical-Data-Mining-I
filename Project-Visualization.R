## Libraries
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(rpart)
library(randomForestSRC)
library(tidyverse)
library(tree)
library(Metrics)
library(caret)
library(car)
library(pls)
library(kernlab)
library(MASS)
library(performance)
library(ROCR)
library(lmtest)
library(randomForest)
library(corrplot)
library(lubridate)
library(ggcorrplot)
library(reshape2)
library(GGally)
library(kableExtra)
library(e1071)
library(ranger)
library(broom)

## Data

# Data Limitations
flights <- read.csv(file = "Flights.csv", header = TRUE) %>%
  filter(MONTH == 2)


# Data Preprocessing
str(flights)

# Cleaning the Data
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "1", "Mon")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "2", "Tue")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "3", "Wed")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "4", "Thu")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "5", "Fri")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "6", "Sat")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "7", "Sun")

flights$DAY_OF_WEEK <- factor(flights$DAY_OF_WEEK) %>%
  ordered(levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))
flights$AIRLINE <- factor(flights$AIRLINE)
flights$FLIGHT_NUMBER <- factor(flights$FLIGHT_NUMBER)
flights$TAIL_NUMBER <- factor(flights$TAIL_NUMBER)
flights$ORIGIN_AIRPORT <- factor(flights$ORIGIN_AIRPORT)
flights$DESTINATION_AIRPORT <- factor(flights$DESTINATION_AIRPORT)
flights$CANCELLATION_REASON <- factor(flights$CANCELLATION_REASON) %>%
  na.omit()

# Missing Data
summary(flights)

flights = flights %>% 
  mutate( TAIL_NUMBER = ifelse(is.na(TAIL_NUMBER), 0, TAIL_NUMBER), 
          AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY), 
          SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY),
          AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY), 
          LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY),
          WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY),
          CANCELLATION_REASON = ifelse(is.na(CANCELLATION_REASON), 0, CANCELLATION_REASON),
          TAXI_IN = ifelse(is.na(TAXI_IN), 0, TAXI_IN),
          AIR_TIME = ifelse(is.na(AIR_TIME), 0, AIR_TIME),
          ELAPSED_TIME = ifelse(is.na(ELAPSED_TIME), 0, ELAPSED_TIME),
          SCHEDULED_TIME = ifelse(is.na(SCHEDULED_TIME), 0, SCHEDULED_TIME),
          WHEELS_OFF = ifelse(is.na(WHEELS_OFF), 0, WHEELS_OFF),
          TAXI_OUT = ifelse(is.na(TAXI_OUT), 0, TAXI_OUT)
  )


## Exploratory Analysis

## Delayed Arrival Time

# Delayed Arrival Time per Airline
ggplot(flights, aes(x=AIRLINE, y=ARRIVAL_DELAY, fill=AIRLINE))+
  geom_boxplot()+
  labs(title = "Delayed Arrival Time in Minutes by Airline", x = "Airlines", y = "Delay in Minutes") +
  guides(fill=guide_legend(title="Airline"))

airlines <- read.csv(file = "Airlines.csv", header = TRUE)

airlines %>% 
  arrange(IATA_CODE) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

flights %>% 
  group_by(AIRLINE) %>% 
  summarize(Minimum = min(na.omit(ARRIVAL_DELAY)),
            Median = median(na.omit(ARRIVAL_DELAY)),
            Mean = round(mean(na.omit(ARRIVAL_DELAY)),2),
            Maximum = max(na.omit(ARRIVAL_DELAY))) %>% 
  kable() %>% 
  kable_styling(bootstrap_options = c("striped", "hover"))

# Delayed Arrival Time per Day of the Week
ggplot(flights, aes(x = DAY_OF_WEEK, y = ARRIVAL_DELAY, fill = DAY_OF_WEEK))+
  geom_boxplot()+
  labs(title="Delayed Arrival Time in Minutes by Day of the Week", x = "Day of the Week", y ="Delay in Minutes") +
  guides(fill=guide_legend(title="Day of the Week"))

flights %>% 
  group_by(DAY_OF_WEEK) %>% 
  summarize(min = min(na.omit(ARRIVAL_DELAY)),
            median = median(na.omit(ARRIVAL_DELAY)),
            mean = round(mean(na.omit(ARRIVAL_DELAY)),2),
            max = max(na.omit(ARRIVAL_DELAY))) %>% 
  kable(col.names = c("Day of the Week", "Minimum", "Median", "Mean", "Maximum")) %>% 
  kable_styling(bootstrap_options = c("striped","hover")) 

# Delayed Arrival Time Depending on the Times of the Day
flights = flights %>% 
  mutate(ARRIVAL_PART_DAY = case_when(
    ARRIVAL_TIME >= 600 & ARRIVAL_TIME < 1200 ~ "Morning",
    ARRIVAL_TIME >= 1200 & ARRIVAL_TIME < 1800 ~ "Afternoon",
    ARRIVAL_TIME >= 1800 & ARRIVAL_TIME < 2100 ~ "Evening",
    ARRIVAL_TIME <= 2100 & ARRIVAL_TIME < 600 ~ "Night" ))

flights$ARRIVAL_PART_DAY = factor(flights$ARRIVAL_PART_DAY)

ARRIVAL_MEANS = flights %>% 
  drop_na(ARRIVAL_DELAY) %>% 
  group_by(ARRIVAL_PART_DAY) %>% 
  summarize(mean_arrival_delay = mean(ARRIVAL_DELAY)) %>% 
  arrange(factor(ARRIVAL_PART_DAY, levels = c('Morning','Afternoon','Evening','Night')))

ARRIVAL_MEANS %>% 
  drop_na(ARRIVAL_PART_DAY) %>% 
  mutate(ARRIVAL_PART_DAY = fct_relevel(ARRIVAL_PART_DAY, 'Morning','Afternoon','Evening','Night')) %>% 
  ggplot() +
  geom_col(aes(x=ARRIVAL_PART_DAY, y=mean_arrival_delay, fill =ARRIVAL_PART_DAY)) + 
  labs(title="Average Delay in Minutes Related by Part of the Day", x ="Scheduled Arrival", y="Avearage Delay in Minutes") +
  guides(fill=guide_legend(title="Part of the Day"))
