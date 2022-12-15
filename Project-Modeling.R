# Libraries
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
library(glmnet)
library(glmnetUtils)
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
library(varImp)


# Reading Dataset
flights <- read.csv(file = "Flights.csv", header = TRUE)

# Subsetting Dataset
flights <- flights %>% 
  filter(MONTH == 2)

flights <- flights %>% 
  filter(AIRLINE == "AA"| AIRLINE == "DL")

flights <- flights %>% 
  filter(ORIGIN_AIRPORT == "ATL" | ORIGIN_AIRPORT == "DFW")

flights <- flights %>% 
  filter(DAY < 15)

# Removing Unnecessary Columns
flights <- subset(flights, select = -c(YEAR, MONTH, CANCELLED, CANCELLATION_REASON, 
                                       DAY, FLIGHT_NUMBER, TAIL_NUMBER, DESTINATION_AIRPORT))

# Handling Missing Data
flights <- flights %>% 
  mutate( 
          AIR_SYSTEM_DELAY = ifelse(is.na(AIR_SYSTEM_DELAY), 0, AIR_SYSTEM_DELAY), 
          SECURITY_DELAY = ifelse(is.na(SECURITY_DELAY), 0, SECURITY_DELAY),
          AIRLINE_DELAY = ifelse(is.na(AIRLINE_DELAY), 0, AIRLINE_DELAY), 
          LATE_AIRCRAFT_DELAY = ifelse(is.na(LATE_AIRCRAFT_DELAY), 0, LATE_AIRCRAFT_DELAY),
          WEATHER_DELAY = ifelse(is.na(WEATHER_DELAY), 0, WEATHER_DELAY),
          TAXI_IN = ifelse(is.na(TAXI_IN), 0, TAXI_IN),
          AIR_TIME = ifelse(is.na(AIR_TIME), 0, AIR_TIME),
          ELAPSED_TIME = ifelse(is.na(ELAPSED_TIME), 0, ELAPSED_TIME),
          SCHEDULED_TIME = ifelse(is.na(SCHEDULED_TIME), 0, SCHEDULED_TIME),
          WHEELS_OFF = ifelse(is.na(WHEELS_OFF), 0, WHEELS_OFF),
          TAXI_OUT = ifelse(is.na(TAXI_OUT), 0, TAXI_OUT),
          
  )

# Plot Correlation Matrix with Continuous Variables and Remove Collinearity
continous_data <- subset(flights, select = -c(ARRIVAL_DELAY,DAY_OF_WEEK,AIRLINE, 
                                             ORIGIN_AIRPORT,DIVERTED,SECURITY_DELAY))

cor_matrix <- cor(continous_data, use = "pairwise.complete.obs")
corrplot(cor_matrix, method = "number")
highlyCorrelated <- findCorrelation(cor_matrix, cutoff = 0.7, exact = TRUE)

removed_continous_data <- continous_data[,highlyCorrelated]
removed_columns_names <- as.array(colnames(removed_continous_data))
flights <- flights[,!(names(flights) %in% removed_columns_names)]


# One Hot Encoding Categorical Variables
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "1", "Mon")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "2", "Tue")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "3", "Wed")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "4", "Thu")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "5", "Fri")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "6", "Sat")
flights$DAY_OF_WEEK <- replace(flights$DAY_OF_WEEK, flights$DAY_OF_WEEK == "7", "Sun")

catogerical_cols <- subset(flights, select = c(DAY_OF_WEEK, AIRLINE, ORIGIN_AIRPORT, DIVERTED, SECURITY_DELAY))

dmy <- dummyVars("~ .", data = catogerical_cols)
dummy_coded_columns <- data.frame(predict(dmy, newdata = catogerical_cols))

flights <- subset(flights, select = -c(DAY_OF_WEEK, AIRLINE, ORIGIN_AIRPORT, DIVERTED, SECURITY_DELAY))
flights <- cbind(flights, dummy_coded_columns) %>%
  na.omit()

# Splitting Dataset into Train and Test Datasets
set.seed(123)
train_idx <- sample(1:nrow(flights), size = 0.8 * nrow(flights))

train <- flights[train_idx,]
test <- flights[-train_idx,]


# Fitting Models

# Linear Regression
lm.fit <- lm(ARRIVAL_DELAY ~ ., data = train)
summary(delay.lm)

pred.lm <- predict(lm.fit, newdata = test)
RMSE(pred.lm, test$ARRIVAL_DELAY)
R2(pred.lm, test$ARRIVAL_DELAY)

# Principal Component Regression
pcr.fit <- pcr(ARRIVAL_DELAY ~ ., data = train)
pred.pcr <- predict(pcr.fit, newdata = test)

dim(pred.pcr)

pcr.error <- sapply(
  1:22,
  function(i) mean((pred.pcr[, , i] - test$ARRIVAL_DELAY)^2)
)

pcr.opt <- which.min(pcr.error)

pcr.fit$coefficients[, , pcr.opt]
pcr.error[pcr.opt]

# Ridge Regression
ridge.fit <- cv.glmnet(ARRIVAL_DELAY ~ ., data = train, alpha = 0, nfolds = 10)
ridge.fit$lambda.min
coef(ridge.fit, s = ridge.fit$lambda.min)
plot(ridge.fit)

pred.ridge <- predict(ridge.fit, newdata = test)
RMSE(pred.ridge, test$ARRIVAL_DELAY)
R2(pred.ridge, test$ARRIVAL_DELAY)


# Lasso Regression
lasso.fit <- cv.glmnet(ARRIVAL_DELAY ~ ., data = train, alpha = 1, nfolds = 10)
lasso.fit$lambda.min
coef(lasso.fit, s = lasso.fit$lambda.min)
plot(lasso.fit)

pred.lasso <- predict(lasso.fit, newdata = test)
RMSE(pred.lasso, test$ARRIVAL_DELAY)
R2(pred.lasso, test$ARRIVAL_DELAY)


# Random Forest Regression
tree.forest <- randomForest(
  ARRIVAL_DELAY ~ ., 
  data = train, 
  mtry = sqrt(ncol(train) - 1), 
  importance = TRUE, 
  ntree = 1000)

pred.forest <- predict(tree.forest, newdata = test)
RMSE(pred.forest, test$ARRIVAL_DELAY)
R2(pred.forest, test$ARRIVAL_DELAY)


# Support Vector Machine Regression
svm.tune <- tune(svm, ARRIVAL_DELAY ~ ., data = train, kernel = "linear", 
                    ranges = list(cost = seq(0.01, 10, length = 20)))
svm.cost <- svm.tune$best.model$cost
svm.best <- svm(ARRIVAL_DELAY ~ ., data = train, kernel = "linear", cost = svm.cost)

pred.svm <- predict(svm.best, newdata = test)
RMSE(pred.svm, test$ARRIVAL_DELAY)
R2(pred.svm, test$ARRIVAL_DELAY)
