library(stringr)
library(tidyverse)
library(dplyr)
library(lubridate)
library(MASS)
library(zipcodeR)
library(parallel)
library(rpart)
library(caret)
library(reshape)
library(pbapply)
library(Metrics)
library(superml)
library(zoom)
library(zoo)
require(tseries)
library(ggfortify)
library(ggplot2)
library(forecast)
library(lava)
library(sf)
library(sgo)
library(caret)
library(Kendall)
set.seed(27164)

goldcrest_data <- read.csv("ebd_GB_goldcr1_200201_202212_relApr-2023_uk_jan_dec_2002_2022_processed.txt")
goldcrest_data
checklist_lc <- read.csv("checklist_landcover.csv")
all_lc <- read.csv("all_locations_landcover.csv")

# Merge checklist with goldcrest
goldcrest_data <- merge(goldcrest_data, checklist_lc, by = "checklist_id")

summary(goldcrest_data)
par(mfrow = c(1,1))
# Find columns with NA value
len <- length(goldcrest_data)
na <- c()
for (i in 1:len){
  na <- c(na, sum(is.na(goldcrest_data[,i])))
}
na


clen <- length(checklist_lc)
cna <- c()
for (i in 1:clen){
  cna <- c(cna, sum(is.na(checklist_lc[,i])))
} 

# Create df with row of NA values of each column
data1 <- goldcrest_data %>%
  filter(is.na(goldcrest_data$time_observations_started) == TRUE)
data3 <- goldcrest_data %>%
    filter(is.na(goldcrest_data$duration_minutes) == TRUE)
data4 <- goldcrest_data %>%
  filter(is.na(goldcrest_data$effort_distance_km) == TRUE)
data5 <- goldcrest_data %>%
  filter(is.na(goldcrest_data$number_observers) == TRUE)
data6 <- goldcrest_data %>%
  filter(is.na(goldcrest_data$observation_count) == TRUE)

cdata1 <- checklist_lc %>%
  filter(is.na(checklist_lc$broadleaf_woodland) == TRUE)
cdata2 <- checklist_lc %>%
  filter(is.na(checklist_lc$conferous_woodland) == TRUE)
cdata3 <- checklist_lc %>%
  filter(is.na(checklist_lc$arable) == TRUE)
cdata4 <- checklist_lc %>%
  filter(is.na(checklist_lc$improved_grassland) == TRUE)
cdata5 <- checklist_lc %>%
  filter(is.na(checklist_lc$semi_natural_grassland) == TRUE)
cdata6 <- checklist_lc %>%
  filter(is.na(checklist_lc$mountain_heath_bog) == TRUE)
cdata7 <- checklist_lc %>%
  filter(is.na(checklist_lc$saltwater) == TRUE)
cdata8 <- checklist_lc %>%
  filter(is.na(checklist_lc$freshwater) == TRUE)
cdata9 <- checklist_lc %>%
  filter(is.na(checklist_lc$coastal) == TRUE)
cdata10 <- checklist_lc %>%
  filter(is.na(checklist_lc$built_up_urban) == TRUE)
all.equal(cdata1,cdata2)

# Find row number with NA
na1 <- which(is.na(goldcrest_data$time_observations_started)==TRUE)
na2 <- which(is.na(goldcrest_data$time_observations_started_decimal)==TRUE)
na3 <- which(is.na(goldcrest_data$duration_minutes)==TRUE)
na4 <- which(is.na(goldcrest_data$effort_distance_km)==TRUE)
na5 <- which(is.na(goldcrest_data$number_observers)==TRUE)
na6 <- which(is.na(goldcrest_data$observation_count)==TRUE)

cna2 <- which(is.na(goldcrest_data$broadleaf_woodland)==TRUE)

goldcrest_data[6324:6328,]

# Remove row with NA location data
goldcrest_data <- goldcrest_data[-cna2,]

len <- length(goldcrest_data)
na <- c()
for (i in 1:len){
  na <- c(na, sum(is.na(goldcrest_data[,i])))
}
na

# Create function for mode
Mode <- function(x) {
  ux <- na.omit(unique(x))
  tab <- tabulate(match(x, ux)); ux[tab == max(tab) ]
}

# Replace NA in number of observer with mode
mean(goldcrest_data$number_observers, na.rm = TRUE)
median(goldcrest_data$number_observers, na.rm = TRUE)
Mode(na.omit(goldcrest_data$number_observers))

goldcrest_data$number_observers <- replace_na(goldcrest_data$number_observers,
                                              Mode(na.omit(goldcrest_data$number_observers)))

length(which(goldcrest_data$number_observers == 0))

for (i in 1:length(goldcrest_data$number_observers)){
  if (goldcrest_data$observation_count[i] >= 0 & goldcrest_data$number_observers[i] == 0){
    goldcrest_data$number_observers[i] <- goldcrest_data$number_observers[i] + 1
  }
}
sum(is.na(goldcrest_data$number_observers) == TRUE)
sum(goldcrest_data$number_observers == 0)

# Replace NA in number of observation
mean(goldcrest_data$observation_count, na.rm = TRUE)
median(goldcrest_data$observation_count, na.rm = TRUE)
Mode(na.omit(goldcrest_data$observation_count))

goldcrest_data$observation_count <- replace_na(goldcrest_data$observation_count,
                                              Mode(na.omit(goldcrest_data$observation_count)))
sum(is.na(goldcrest_data$observation_count) == TRUE)

summary(goldcrest_data)
str(goldcrest_data)

# Change observation date to date format
goldcrest_data$observation_date <- date(goldcrest_data$observation_date)

# Plot date against count
plot(goldcrest_data$observation_date, goldcrest_data$observation_count, 
     ylab = "Observation Count", xlab = "Date", lwd=2,
     main = "Goldcrest", cex.lab=1.6, cex.main=1.6, cex.axis=1.4)

# Find columns with NA value
len <- length(goldcrest_data)
na <- c()
for (i in 1:len){
  na <- c(na, sum(is.na(goldcrest_data[,i])))
}
na


# Extract date and observation from data
mini_gc <- data.frame(Date = goldcrest_data$observation_date,
                      Observation = goldcrest_data$observation_count/goldcrest_data$number_observers)

# Reorder using date
mini_gc <- mini_gc %>%
  arrange(ymd(mini_gc$Date))
mini_gc_2 <- mini_gc
mini_gc_2$Date <- format(mini_gc_2$Date, "%Y-%m")
mini_gc_2 <- mini_gc_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_2 <- as.data.frame(mini_gc_2)
hist(mini_gc_2$Observation, main = "Histogram average observation per month",
     xlab = "Average Observation")
gc2 <- ts(mini_gc_2, frequency = 12, start = c(2002,1), end = c(2022,12))
plot(gc2[,2], main = "Time series of average observation per month", xlab = "Time",
     ylab = "Average observation")
Months <- c("J","F","M","A","M","J","J","A","S","O","N","D")
points(gc2[,2], pch=Months, cex=0.75, font=4, col=1:4)

mini_gc_3 <- mini_gc
mini_gc_3$Date <- format(mini_gc_3$Date, "%Y")
mini_gc_3 <- mini_gc_3 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_3_1 <- as.data.frame(mini_gc_3)
hist(mini_gc_3$Observation)
gc3 <- ts(mini_gc_3, frequency = 1, start = 2002, end = 2022)
plot(gc3[,2], main = "Time series of average observation per year", xlab = "Time",
     ylab = "Average observation")

autoplot(gc2) +
  ggtitle("Goldcrest Count") +
  ylab("GC count") +
  xlab("Year")

ggseasonplot(gc2[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count")

ggseasonplot(gc2[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count")

ggsubseriesplot(gc2[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count")



gc2Comp <- decompose(gc2[,2])
plot(gc2Comp)
gc2SeasonAdj <- gc2[,2] - gc2Comp$seasonal
plot.ts(gc2SeasonAdj, main = "Seasonally adjusted time series of average observation",
        xlab = "Time", ylab = "Average observation")
Box.test((gc2SeasonAdj), lag = 6, type = "Ljung-Box")
Box.test(gc2Comp$x, lag = 6, type = "Ljung-Box")
Box.test(gc2[,2], lag = min(21, 12/5), type = "Ljung-Box")
Box.test(gc2[,2], lag = 12, type = "Ljung-Box")
checkresiduals(gc2[,2])
# p-value < 0.05, reject null hypothesis, values are dependent

MannKendall(gc2[,2])
# p-value < 0.05, reject null hypothesis, time series shows significant trend

adf.test(gc2[,2])
# adf.test(gc2SeasonAdj, k = 12*((length(gc2SeasonAdj_saltwater)/100)^(1/4)))
# Data is already stationary at original series, statistical properties of 
# process does not change over time, properties does not depends on time

# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process

################################################################################
# Identify minimum order of differencing needed for stationary
fdiff <- diff(gc2[,2])
sdiff <- diff(gc2[,2], differences = 2)
tdiff <- diff(gc2[,2], differences = 3)
par(mfrow = c(2,2))
plot(gc2[,2], main = "Original Time series", cex.main = 1)
plot(fdiff, main = "First order differentiation of time series"
     , ylab = "Average observation", cex.main = 1)
plot(sdiff, main = "Second order differentiation of time series"
     , ylab = "Average observation", cex.main = 1)
plot(tdiff, main = "Third order differentiation of time series"
     , ylab = "Average observation", cex.main = 1)

par(mfrow = c(1,2))
Acf(gc2[,2], main = "Original ACF")
Pacf(gc2[,2], main = "Original PACF")

Acf(fdiff, main = "First Order ACF")
Pacf(fdiff, main = "First Order PACF")

Acf(sdiff, main = "Second Order ACF")
Pacf(sdiff, main = "Second Order PACF")

Acf(tdiff, main = "Third Order ACF")
Pacf(tdiff, main = "Third Order PACF")
# Original time series chosen
################################################################################
gc2arima1 <- arima(gc2[,2], order = c(1,0,0), 
                              seasonal = list(order = c(0,0,1), period = 12))
gc2arima2 <-  arima(gc2[,2], order = c(1,0,0),
                    seasonal = list(order = c(1,0,0), period = 12))
gc2arima3 <-  arima(gc2[,2], order = c(1,0,0),
                    seasonal = list(order = c(2,0,0), period = 12))
gc2arima4 <-  arima(gc2[,2], order = c(1,0,0),
                    seasonal = list(order = c(1,0,1), period = 12))
gc2arima5 <-  arima(gc2[,2], order = c(1,0,0),
                    seasonal = list(order = c(0,0,1), period = 12))
gc2arima6 <-  arima(gc2[,2], order = c(1,0,0),
                    seasonal = list(order = c(0,0,2), period = 12))
gc2arima7 <-  arima(gc2[,2], order = c(1,0,0),
                    seasonal = list(order = c(0,0,0), period = 12))
AIC(gc2arima1)
AIC(gc2arima2)
AIC(gc2arima3)
AIC(gc2arima4)
AIC(gc2arima5)
AIC(gc2arima6)
AIC(gc2arima7)
# ARIMA model predict future values
## Seasonal
gc2Seasonarima <- auto.arima(gc2[,2], stationary = TRUE, 
                             seasonal = TRUE, ic = "aic", stepwise = TRUE,
                             approximation = FALSE)

# Seasonal ARIMA (1,0,0)(1,0,0)_12

# Check residual autocorrelation
acf(residuals(gc2Seasonarima), main = "ACF plot of residual",
    cex.main = 0.25)
pacf(residuals(gc2Seasonarima), main = "PACF plot of residual",
     cex.main = 0.25)

checkresiduals(gc2Seasonarima)

# p-value > 0.05, does not reject null hypothesis, There is no autocorrelation in the data.
# Residual shows white noise

# -------------------------------------------------------------------------


par(mfrow = c(1,1))
adf.test(gc2Seasonarima$residuals)

# Forecasting 1
length(gc2)
gc2Seasonarima.train <- arima(gc2[1:228,2], order = c(1,0,0), 
                              seasonal = list(order = c(1,0,0), period = 12))
gc2Season.test <- gc2[229:252,2]
gc2Seasonarima.predict.2 <- predict(gc2Seasonarima.train, n.ahead = 24)
rmse(gc2Season.test, gc2Seasonarima.predict.2$pred)
gc2Seasonarima.pred <- ts(gc2Seasonarima.predict.2$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(gc2[,2], main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2Seasonarima.pred, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)
# RMSE < 0.2, predicted value is very close to actual value

# Forecasting 2
fore <- forecast(gc2Seasonarima)
plot(fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(fore)
# RMSE = 0.255, between 0.2 and 0.5, good forecast

# Split goldcrest_data by location
goldcrest2 <- goldcrest_data[,c(19:28)]
head(goldcrest2)
goldcrest_data$Landcover <- colnames(goldcrest2)[apply(goldcrest2, 1, which.max)]

gc_broadleaf <- goldcrest_data %>%
  filter(Landcover == "broadleaf_woodland")
gc_conferous <- goldcrest_data %>%
  filter(Landcover == "conferous_woodland")
gc_arable <- goldcrest_data %>%
  filter(Landcover == "arable")
gc_improved <- goldcrest_data %>%
  filter(Landcover == "improved_grassland")
gc_natural <- goldcrest_data %>%
  filter(Landcover == "semi_natural_grassland")
gc_mountain <- goldcrest_data %>%
  filter(Landcover == "mountain_health_bag")
gc_saltwater <- goldcrest_data %>%
  filter(Landcover == "saltwater")
gc_freshwater <- goldcrest_data %>%
  filter(Landcover == "freshwater")
gc_coastal <- goldcrest_data %>%
  filter(Landcover == "coastal")
gc_urban <- goldcrest_data %>%
  filter(Landcover == "built_up_urban")

nrow(goldcrest_data)
nrow(gc_arable)
nrow(gc_broadleaf)
nrow(gc_coastal)
nrow(gc_conferous)
nrow(gc_freshwater)
nrow(gc_improved)
nrow(gc_mountain)
nrow(gc_natural)
nrow(gc_saltwater)
nrow(gc_urban)

length(which(month(goldcrest_data$observation_date) == 1))
length(which(month(goldcrest_data$observation_date) == 2))
length(which(month(goldcrest_data$observation_date) == 3))
length(which(month(goldcrest_data$observation_date) == 4))
length(which(month(goldcrest_data$observation_date) == 5))
length(which(month(goldcrest_data$observation_date) == 6))
length(which(month(goldcrest_data$observation_date) == 7))
length(which(month(goldcrest_data$observation_date) == 8))
length(which(month(goldcrest_data$observation_date) == 9))
length(which(month(goldcrest_data$observation_date) == 10))
length(which(month(goldcrest_data$observation_date) == 11))
length(which(month(goldcrest_data$observation_date) == 12))
################################################################################
# Urban
# Extract date and observation from data
mini_gc_urban <- data.frame(Date = gc_urban$observation_date,
                            Observation = gc_urban$observation_count/gc_urban$number_observers)

# Reorder using date
mini_gc_urban <- mini_gc_urban %>%
  arrange(ymd(mini_gc_urban$Date))

mini_gc_urban_2 <- mini_gc_urban
mini_gc_urban_2$Date <- format(mini_gc_urban_2$Date, "%Y-%m")
mini_gc_urban_2 <- mini_gc_urban_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_urban_2 <- as.data.frame(mini_gc_urban_2)
hist(mini_gc_urban_2$Observation, main = "Histogram of average observation at urban area",
     xlab = "Average Observation")
gc2_urban <- ts(mini_gc_urban_2, frequency = 12, start = c(2002,1), end = c(2022,12))
plot(gc2_urban[,2], main = "Time series of average observation at Urban area", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_urban[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Urban)")

ggseasonplot(gc2_urban[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Urban)")

ggsubseriesplot(gc2_urban[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Urban)")

gc2Comp_urban <- decompose(gc2_urban[,2])
Box.test(gc2Comp_urban$random, lag = 13, type = "Ljung-Box")
Box.test(gc2_urban[,2], lag = 12, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_urban)
gc2SeasonAdj_urban <- gc2_urban[,2] - gc2Comp_urban$seasonal

adf.test(gc2_urban[,2])
# Data is already stationary at original series, statistical properties of 
# process does not change over time, properties does not depends on time
par(mfrow = c(1,2))
acf(gc2_urban[,2])
pacf(gc2_urban[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process

gc_urban_arima <- auto.arima(gc2_urban[,2], max.p = 13, max.P = 13, stationary = TRUE, ic = "aic",
                             stepwise = TRUE, approximation = FALSE)
# ARIMA(1,0,1)(1,0,1)_12
checkresiduals(gc_urban_arima)
# p-value > 0.05, does not reject null hypothesis, there is no auto-correlation

par(mfrow = c(1,1))

# Forecast 1
gc2.arima.train_urban <- arima(gc2_urban[1:228,2], order = c(1,0,1), 
                               seasonal = list(order = c(1,0,1), period = 12))
gc2.test_urban <- gc2_urban[229:252,2]
gc2.arima.predict.2_urban <- predict(gc2.arima.train_urban, n.ahead = 24)
rmse(gc2.test_urban, gc2.arima.predict.2_urban$pred)
gc2.arima.pred_urban <- ts(gc2.arima.predict.2_urban$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(gc2_urban[,2], xlim = c(2019,2023), main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_urban, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
urban_fore <- forecast(gc_urban_arima)
plot(urban_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(urban_fore)


################################################################################
# Coastal
# Extract date and observation from data
mini_gc_coastal <- data.frame(Date = gc_coastal$observation_date,
                              Observation = gc_coastal$observation_count/gc_coastal$number_observers)

# Reorder using date
mini_gc_coastal <- mini_gc_coastal %>%
  arrange(ymd(mini_gc_coastal$Date))

mini_gc_coastal_2 <- mini_gc_coastal
mini_gc_coastal_2$Date <- format(mini_gc_coastal_2$Date, "%Y-%m")
mini_gc_coastal_2 <- mini_gc_coastal_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_coastal_2 <- as.data.frame(mini_gc_coastal_2[205:252,])
hist(mini_gc_coastal_2$Observation, main = "Histogram of average observation at coastal area",
     xlab = "Average Observation")
gc2_coastal <- ts(mini_gc_coastal_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_coastal[,2], main = "Time series of average observation at coastal area", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_coastal[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Coastal)")

ggseasonplot(gc2_coastal[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Coastal)")

ggsubseriesplot(gc2_coastal[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Coastal)")

gc2Comp_coastal <- decompose(gc2_coastal[,2])
Box.test(gc2Comp_coastal$random, lag = 13, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_coastal)
gc2SeasonAdj_coastal <- gc2_coastal[,2] - gc2Comp_coastal$seasonal
Box.test(gc2_coastal[,2], lag = 12, type = "Ljung-Box")
acf(gc2_coastal[,2])
adf.test(gc2_coastal[,2])
# time series is not stationary

# Differencing for stationarity
fdiff_coastal <- diff(gc2_coastal[,2])
is.ts(gc2_coastal)
# First order of differencing achive stationary
adf.test(fdiff_coastal)

# Identify minimum order of differencing needed for stationary
fdiff_coastal <- diff(gc2_coastal[,2])
sdiff_coastal <- diff(gc2_coastal[,2], differences = 2)
tdiff_coastal <- diff(gc2_coastal[,2], differences = 3)
par(mfrow = c(2,2))
plot(gc2_coastal[,2], main = "Original Time series", cex.main = 1)
plot(fdiff_coastal, main = "First order differentiation of time series"
     , ylab = "Average observation", cex.main = 1)
plot(sdiff_coastal, main = "Second order differentiation of time series"
     , ylab = "Average observation", cex.main = 1)
plot(tdiff_coastal, main = "Third order differentiation of time series"
     , ylab = "Average observation", cex.main = 1)

par(mfrow = c(1,2))
Acf(gc2_coastal[,2], main = "Original ACF")
Pacf(gc2_coastal[,2], main = "Original PACF")

Acf(fdiff_coastal, main = "First Order ACF")
Pacf(fdiff_coastal, main = "First Order PACF")

Acf(sdiff_coastal, main = "Second Order ACF")
Pacf(sdiff_coastal, main = "Second Order PACF")

Acf(tdiff_coastal, main = "Third Order ACF")
Pacf(tdiff_coastal, main = "Third Order PACF")


par(mfrow = c(1,2))
acf(gc2_coastal[,2])
pacf(gc2_coastal[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process
gc_coastal_arima <- auto.arima(fdiff_coastal, max.p = 13, max.P = 13,
                               stationary = TRUE, ic = "aic", stepwise = TRUE,
                               approximation = FALSE)
# ARIMA(3,0,0)(1,0,0)_12
checkresiduals(gc_coastal_arima)
autoplot(gc_coastal_arima)
adf.test(gc_coastal_arima$residuals)
# p-value > 0.05, does not reject null hypothesis, values are independent
acf(residuals(gc_coastal_arima))
pacf(residuals(gc_coastal_arima))

par(mfrow = c(1,1))

# Forecast 1
gc2.arima.train_coastal <- arima(fdiff_coastal, order = c(3,0,0), 
                                 seasonal = list(order = c(1,0,0), period = 12))
gc2.test_coastal <- fdiff_coastal[24:47]
gc2.arima.predict.2_coastal <- predict(gc2.arima.train_coastal, n.ahead = 24)
rmse(gc2.test_coastal, gc2.arima.predict.2_coastal$pred)
gc2.arima.pred_coastal <- ts(gc2.arima.predict.2_coastal$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(fdiff_coastal, main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_coastal, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
coastal_fore <- forecast(gc_coastal_arima)
plot(coastal_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(coastal_fore)


################################################################################
# Saltwater
# Extract date and observation from data
mini_gc_saltwater <- data.frame(Date = gc_saltwater$observation_date,
                                Observation = gc_saltwater$observation_count/gc_saltwater$number_observers)

# Reorder using date
mini_gc_saltwater <- mini_gc_saltwater %>%
  arrange(ymd(mini_gc_saltwater$Date))

saltwater_gc <- mini_gc_saltwater
tabY <- table(cut(saltwater_gc$Date, 'year'))
surveysum_Y <- data.frame(Date = format(as.Date(names(tabY)), "%Y"), Frequency = as.vector(tabY))
survey_saltwater_Y <- ts(surveysum_Y, frequency = 1, start = 2002, end = 2022)
plot(survey_saltwater_Y[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")


mini_gc_saltwater_2 <- mini_gc_saltwater
mini_gc_saltwater_2$Date <- format(mini_gc_saltwater_2$Date, "%Y-%m")
mini_gc_saltwater_2 <- mini_gc_saltwater_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_saltwater_2 <- as.data.frame(mini_gc_saltwater_2[197:244,])
hist(mini_gc_saltwater_2$Observation)
gc2_saltwater <- ts(mini_gc_saltwater_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_saltwater[,2], main = "Time series of average observation at saltwater area", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_saltwater[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Saltwater)")

ggseasonplot(gc2_saltwater[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Saltwater)")

ggsubseriesplot(gc2_saltwater[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Saltwater)")

gc2Comp_saltwater <- decompose(gc2_saltwater[,2])
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_saltwater)
gc2SeasonAdj_saltwater <- gc2_saltwater[,2] - gc2Comp_saltwater$seasonal

Box.test(gc2_saltwater[,2], lag = 12, type = "Ljung-Box")
adf.test(gc2_saltwater[,2])
# Data is not stationary at original series, statistical properties of 
# process does not change over time, properties does not depends on time
par(mfrow = c(1,2))
acf(gc2_saltwater[,2])
pacf(gc2_saltwater[,2])


fdiff_saltwater <- diff(gc2_saltwater[,2])
adf.test(fdiff_saltwater)
# first-order differentiation on the non-stationary time series, it is transformed
# into a stationary time series, which may exhibit temporal dependencies and 
# patterns that can be captured by an ARIMA model.

gc_saltwater_arima <- auto.arima(fdiff_saltwater, max.p = 13, max.P = 13, 
                                 stationary = TRUE, ic = "aic", stepwise = TRUE,
                                 approximation = FALSE)
# ARIMA(3,0,0)
checkresiduals(gc_saltwater_arima)
# p-value > 0.05, does not reject null hypothesis, values are independent
adf.test(gc_saltwater_arima$residuals)

par(mfrow = c(1,1))


# Forecast 1
gc2.arima.train_saltwater <- arima(fdiff_saltwater, order = c(3,0,0))
gc2.test_saltwater <- fdiff_saltwater[24:47]
gc2.arima.predict.2_saltwater <- predict(gc2.arima.train_saltwater, n.ahead = 24)
rmse(gc2.test_saltwater, gc2.arima.predict.2_saltwater$pred)
gc2.arima.pred_saltwater <- ts(gc2.arima.predict.2_saltwater$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(fdiff_saltwater, main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_saltwater, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
saltwater_fore <- forecast(gc_saltwater_arima)
plot(saltwater_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(saltwater_fore)

################################################################################
# Freshwater
# Extract date and observation from data
mini_gc_freshwater <- data.frame(Date = gc_freshwater$observation_date,
                                 Observation = gc_freshwater$observation_count/gc_freshwater$number_observers)

# Reorder using date
mini_gc_freshwater <- mini_gc_freshwater %>%
  arrange(ymd(mini_gc_freshwater$Date))

mini_gc_freshwater_2 <- mini_gc_freshwater
mini_gc_freshwater_2$Date <- format(mini_gc_freshwater_2$Date, "%Y-%m")
mini_gc_freshwater_2 <- mini_gc_freshwater_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_freshwater_2 <- as.data.frame(mini_gc_freshwater_2)
hist(mini_gc_freshwater_2$Observation)
gc2_freshwater <- ts(mini_gc_freshwater_2, frequency = 12, start = c(2002,1), end = c(2022,12))
plot(gc2_freshwater[,2], main = "Time series of average observation at freshwater area", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_freshwater[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Freshwater)")

ggseasonplot(gc2_freshwater[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Freshwater)")

ggsubseriesplot(gc2_freshwater[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Freshwater)")

gc2Comp_freshwater <- decompose(gc2_freshwater[,2])
Box.test(gc2Comp_freshwater$random, lag = 13, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_freshwater)
gc2SeasonAdj_freshwater <- gc2_freshwater[,2] - gc2Comp_freshwater$seasonal


Box.test(gc2_freshwater[,2], lag = 12, type = "Ljung-Box")

adf.test(gc2_freshwater[,2])
# Data is already stationary at original series, statistical properties of 
# process does not change over time, properties does not depends on time
par(mfrow = c(1,2))
acf(gc2_freshwater[,2])
pacf(gc2_freshwater[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process
gc_freshwater_arima <- auto.arima(gc2_freshwater[,2], max.p = 13, max.P = 13, 
                                  stationary = TRUE, ic = "aic", stepwise = TRUE,
                                  approximation = FALSE)
# SARIMA(1,0,1)(4,0,0)[12]
checkresiduals(gc_freshwater_arima)
# p-value < 0.05, reject null hypothesis, there exist auto-correlation


par(mfrow = c(1,1))

# Forecast 1
gc2.arima.train_freshwater <- arima(gc2_freshwater[1:228,2], order = c(1,0,1), 
                                    seasonal = list(order = c(4,0,0), period = 12))
gc2.test_freshwater <- gc2_freshwater[229:252,2]
gc2.arima.predict.2_freshwater <- predict(gc2.arima.train_freshwater, n.ahead = 24)
rmse(gc2.test_freshwater, gc2.arima.predict.2_freshwater$pred)
gc2.arima.pred_freshwater <- ts(gc2.arima.predict.2_freshwater$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(gc2_freshwater[,2], main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_freshwater, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
freshwater_fore <- forecast(gc_freshwater_arima)
plot(freshwater_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(freshwater_fore)

################################################################################
# Natural
# Extract date and observation from data
mini_gc_natural <- data.frame(Date = gc_natural$observation_date,
                              Observation = gc_natural$observation_count/gc_natural$number_observers)

# Reorder using date
mini_gc_natural <- mini_gc_natural %>%
  arrange(ymd(mini_gc_natural$Date))

natural_gc <- mini_gc_natural
tabY <- table(cut(natural_gc$Date, 'year'))
surveysum_Y <- data.frame(Date = format(as.Date(names(tabY)), "%Y"), Frequency = as.vector(tabY))
survey_natural_Y <- ts(surveysum_Y, frequency = 1, start = 2002, end = 2022)
plot(survey_natural_Y[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")

mini_gc_natural_2 <- mini_gc_natural
mini_gc_natural_2$Date <- format(mini_gc_natural_2$Date, "%Y-%m")
mini_gc_natural_2 <- mini_gc_natural_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_natural_2 <- as.data.frame(mini_gc_natural_2[205:252,])
hist(mini_gc_natural_2$Observation)
gc2_natural <- ts(mini_gc_natural_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_natural[,2], main = "Time series of average observation at Semi-natural grassland ", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_natural[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Semi Natural Grassland)")

ggseasonplot(gc2_natural[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Semi Natural Grassland)") +
  theme(plot.title = element_text(size = 12))

ggsubseriesplot(gc2_natural[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Semi Natural Grassland)")

gc2Comp_natural <- decompose(gc2_natural[,2])
Box.test(gc2Comp_natural$random, lag = 13, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_natural)
gc2SeasonAdj_natural <- gc2_natural[,2] - gc2Comp_natural$seasonal

Box.test(gc2_natural[,2], lag = 13, type = "Ljung-Box")
adf.test(gc2_natural[,2])
# Data is not stationary at original series
par(mfrow = c(1,2))
acf(gc2_natural[,2])
pacf(gc2_natural[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process
fdiff_natural <- diff(gc2_natural[,2])
adf.test(fdiff_natural)

gc_natural_arima <- auto.arima(fdiff_natural, max.p = 13, max.P = 13, 
                               stationary = TRUE, ic = "aic", stepwise = TRUE,
                               approximation = FALSE)
# SARIMA(0,0,2)(1,0,0)[12]
checkresiduals(gc_natural_arima)
adf.test(gc_natural_arima$residuals)
# p-value < 0.05, reject null hypothesis, there exist auto-correlation
par(mfrow = c(1,1))

# Forecast 1
gc2.arima.train_natural <- arima(fdiff_natural, order = c(0,0,2), 
                                 seasonal = list(order = c(1,0,0), period = 12))
gc2.test_natural <- fdiff_natural[24:47]
gc2.arima.predict.2_natural <- predict(gc2.arima.train_natural, n.ahead = 24)
rmse(gc2.test_natural, gc2.arima.predict.2_natural$pred)
gc2.arima.pred_natural <- ts(gc2.arima.predict.2_natural$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(fdiff_natural, main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_natural, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
natural_fore <- forecast(gc_natural_arima)
plot(natural_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(natural_fore)

################################################################################
# Broadleaf
# Extract date and observation from data
mini_gc_broadleaf <- data.frame(Date = gc_broadleaf$observation_date,
                                Observation = gc_broadleaf$observation_count/gc_broadleaf$number_observers)

# Reorder using date
mini_gc_broadleaf <- mini_gc_broadleaf %>%
  arrange(ymd(mini_gc_broadleaf$Date))

mini_gc_broadleaf_2 <- mini_gc_broadleaf
mini_gc_broadleaf_2$Date <- format(mini_gc_broadleaf_2$Date, "%Y-%m")
mini_gc_broadleaf_2 <- mini_gc_broadleaf_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_broadleaf_2 <- as.data.frame(mini_gc_broadleaf_2[205:252,])
hist(mini_gc_broadleaf_2$Observation)
gc2_broadleaf <- ts(mini_gc_broadleaf_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_broadleaf[,2], main = "Time series of average observation at Broadleaf woodland ", xlab = "Time",
     ylab = "Average observation")
mini_gc_broadleaf_3 <- as.data.frame((mini_gc_broadleaf_2))
gc3_broadleaf <- ts(mini_gc_broadleaf_3, frequency = 12, start = c(2002,1), end = c(2022, 12))
adf.test(gc3_broadleaf[,2])
Box.test(gc3_broadleaf[,2], lag = 12, type = "Ljung-Box")

ggseasonplot(gc2_broadleaf[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Broadleaf Woodland)")

ggseasonplot(gc2_broadleaf[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Broadleaf Woodland)")

ggsubseriesplot(gc2_broadleaf[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Broadleaf Woodland)")

gc2Comp_broadleaf <- decompose(gc2_broadleaf[,2])
Box.test(gc2Comp_broadleaf$random, lag = 12, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_broadleaf)
gc2SeasonAdj_broadleaf <- gc2_broadleaf[,2] - gc2Comp_broadleaf$seasonal
Box.test(gc2_broadleaf[,2], lag = 12, type = "Ljung-Box")
adf.test(gc2_broadleaf[,2])
# Data is not stationary at original series
par(mfrow = c(1,2))
acf(gc2_broadleaf[,2])
pacf(gc2_broadleaf[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process
fdiff_broadleaf <- diff(gc2_broadleaf[,2])
adf.test(fdiff_broadleaf)

gc_broadleaf_arima <- auto.arima(fdiff_broadleaf, max.p = 13, max.P = 13, 
                                 stationary = TRUE, ic = "aic",
                                 stepwise = TRUE, approximation = FALSE)
# ARIMA(0,0,0)(1,0,0)_12
checkresiduals(gc_broadleaf_arima)
adf.test(gc_broadleaf_arima$residuals)

par(mfrow = c(1,1))

# Forecast 1
gc2.arima.train_broadleaf <- arima(fdiff_broadleaf, order = c(0,0,0), 
                                   seasonal = list(order = c(1,0,0), period = 12))
gc2.test_broadleaf <- fdiff_broadleaf[24:47]
gc2.arima.predict.2_broadleaf <- predict(gc2.arima.train_broadleaf, n.ahead = 24)
rmse(gc2.test_broadleaf, gc2.arima.predict.2_broadleaf$pred)
gc2.arima.pred_broadleaf <- ts(gc2.arima.predict.2_broadleaf$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(fdiff_broadleaf, main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_broadleaf, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
broadleaf_fore <- forecast(gc_broadleaf_arima)
plot(broadleaf_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(broadleaf_fore)

################################################################################
# Coniferous
# Extract date and observation from data
mini_gc_conferous <- data.frame(Date = gc_conferous$observation_date,
                                Observation = gc_conferous$observation_count/gc_conferous$number_observers)

# Reorder using date
mini_gc_conferous <- mini_gc_conferous %>%
  arrange(ymd(mini_gc_conferous$Date))

conferous_gc <- mini_gc_conferous
tab <- table(cut(conferous_gc$Date, 'month'))
surveysum_m <- data.frame(Date = format(as.Date(names(tab)), "%Y-%m"), Frequency = as.vector(tab))
survey_conferous_m <- ts(surveysum_m, frequency = 12, start = c(2002,1), end = c(2022,12))
plot(survey_conferous_m[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")

tabY <- table(cut(conferous_gc$Date, 'year'))
surveysum_Y <- data.frame(Date = format(as.Date(names(tabY)), "%Y"), Frequency = as.vector(tabY))
survey_conferous_Y <- ts(surveysum_Y, frequency = 1, start = 2002, end = 2022)
plot(survey_conferous_Y[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")

mini_gc_conferous_2 <- mini_gc_conferous
mini_gc_conferous_2$Date <- format(mini_gc_conferous_2$Date, "%Y-%m")
mini_gc_conferous_2 <- mini_gc_conferous_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_conferous_2 <- as.data.frame(mini_gc_conferous_2[191:238,])
nrow(mini_gc_conferous_2)
hist(mini_gc_conferous_2$Observation)
gc2_conferous <- ts(mini_gc_conferous_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_conferous[,2], main = "Time series of average observation at Coniferous woodland ", xlab = "Time",
     ylab = "Average observation")

ggseasonplot(gc2_conferous[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (coniferous Woodland)")

ggseasonplot(gc2_conferous[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Coniferous Woodland)") +
  theme(plot.title = element_text(size = 12))

ggsubseriesplot(gc2_conferous[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (conferous Woodland)")

gc2Comp_conferous <- decompose(gc2_conferous[,2])
Box.test(gc2Comp_conferous$random, lag = 13, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_conferous)
gc2SeasonAdj_conferous <- gc2_conferous[,2] - gc2Comp_conferous$seasonal

Box.test(gc2_conferous[,2], lag = 12, type = "Ljung-Box")
adf.test(gc2_conferous[,2])
# Data is already stationary at original series, statistical properties of 
# process does not change over time, properties does not depends on time
par(mfrow = c(1,2))
acf(gc2_conferous[,2])
pacf(gc2_conferous[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process

gc_conferous_arima <- auto.arima(gc2_conferous[,2], max.p = 13, max.P = 13, 
                                 stationary = TRUE, ic = "aic", stepwise = TRUE,
                                 approximation = FALSE, seasonal = TRUE)
# ARIMA(1,0,0)
checkresiduals(gc_conferous_arima)
# p-value > 0.05, does not reject null hypothesis, values are independent
# ACF plot show ts not a WN
adf.test(gc_conferous_arima$residuals)

par(mfrow = c(1,1))
# Forecast 1
gc2.arima.train_conferous <- arima(gc2_conferous[1:24,2], order = c(1,0,0))
gc2.test_conferous <- gc2_conferous[25:48,2]
gc2.arima.predict.2_conferous <- predict(gc2.arima.train_conferous, n.ahead = 24)
rmse(gc2.test_conferous, gc2.arima.predict.2_conferous$pred)
gc2.arima.pred_conferous <- ts(gc2.arima.predict.2_conferous$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(gc2_conferous[,2], main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_conferous, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
conferous_fore <- forecast(gc_conferous_arima)
plot(conferous_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(conferous_fore)

################################################################################
# Arable
# Extract date and observation from data
mini_gc_arable <- data.frame(Date = gc_arable$observation_date,
                             Observation = gc_arable$observation_count/gc_arable$number_observers)

# Reorder using date
mini_gc_arable <- mini_gc_arable %>%
  arrange(ymd(mini_gc_arable$Date))

arable_gc <- mini_gc_arable
tab <- table(cut(arable_gc$Date, 'month'))
surveysum_m <- data.frame(Date = format(as.Date(names(tab)), "%Y-%m"), Frequency = as.vector(tab))
survey_arable_m <- ts(surveysum_m, frequency = 12, start = c(2002,1), end = c(2022,12))
plot(survey_arable_m[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")

tabY <- table(cut(arable_gc$Date, 'year'))
surveysum_Y <- data.frame(Date = format(as.Date(names(tabY)), "%Y"), Frequency = as.vector(tabY))
survey_arable_Y <- ts(surveysum_Y, frequency = 1, start = 2002, end = 2022)
plot(survey_arable_Y[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")


mini_gc_arable_2 <- mini_gc_arable
mini_gc_arable_2$Date <- format(mini_gc_arable_2$Date, "%Y-%m")
mini_gc_arable_2 <- mini_gc_arable_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_arable_2 <- as.data.frame(mini_gc_arable_2[205:252,])
hist(mini_gc_arable_2$Observation)
gc2_arable <- ts(mini_gc_arable_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_arable[,2], main = "Time series of average observation at Arable area", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_arable[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Arable)")

ggseasonplot(gc2_arable[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Arable)")

ggsubseriesplot(gc2_arable[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Arable)")

gc2Comp_arable <- decompose(gc2_arable[,2])
Box.test(gc2Comp_arable$random, lag = 13, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_arable)
gc2SeasonAdj_arable <- gc2_arable[,2] - gc2Comp_arable$seasonal

Box.test(gc2_arable[,2], lag = 12, type = "Ljung-Box")
adf.test(gc2_arable[,2])
# Data is not stationary at original series
par(mfrow = c(1,2))
acf(gc2_arable[,2])
pacf(gc2_arable[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process
fdiff_arable <- diff(gc2_arable[,2])
adf.test(fdiff_arable)

gc_arable_arima <- auto.arima(fdiff_arable, max.p = 13, max.P = 13, 
                              stationary = TRUE, ic = "aic", stepwise = TRUE,
                              approximation = FALSE)
# SARIMA(2,0,0)(1,0,0)[12]
checkresiduals(gc_arable_arima)
adf.test(gc_arable_arima$residuals)
# p-value > 0.05, does not reject null hypothesis, values are independent
# Not a WN

par(mfrow = c(1,1))

# Forecast 1
gc2.arima.train_arable <- arima(fdiff_arable, order = c(2,0,0), 
                                seasonal = list(order = c(1,0,0), period = 12))
gc2.test_arable <- fdiff_arable[24:47]
gc2.arima.predict.2_arable <- predict(gc2.arima.train_arable, n.ahead = 24)
rmse(gc2.test_arable, gc2.arima.predict.2_arable$pred)
gc2.arima.pred_arable <- ts(gc2.arima.predict.2_arable$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(fdiff_arable, main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_arable, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
arable_fore <- forecast(gc_arable_arima)
plot(arable_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(arable_fore)


################################################################################
# Improved
# Extract date and observation from data
mini_gc_improved <- data.frame(Date = gc_improved$observation_date,
                               Observation = gc_improved$observation_count/gc_improved$number_observers)

# Reorder using date
mini_gc_improved <- mini_gc_improved %>%
  arrange(ymd(mini_gc_improved$Date))

improved_gc <- mini_gc_improved
tab <- table(cut(improved_gc$Date, 'month'))
surveysum_m <- data.frame(Date = format(as.Date(names(tab)), "%Y-%m"), Frequency = as.vector(tab))
survey_improved_m <- ts(surveysum_m, frequency = 12, start = c(2002,1), end = c(2022,12))
plot(survey_improved_m[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")

tabY <- table(cut(improved_gc$Date, 'year'))
surveysum_Y <- data.frame(Date = format(as.Date(names(tabY)), "%Y"), Frequency = as.vector(tabY))
survey_improved_Y <- ts(surveysum_Y, frequency = 1, start = 2002, end = 2022)
plot(survey_improved_Y[,2], main = "Frequency of survey in each month", ylab = "Number of survey", xlab = "Date")

mini_gc_improved_2 <- mini_gc_improved
mini_gc_improved_2$Date <- format(mini_gc_improved_2$Date, "%Y-%m")
mini_gc_improved_2 <- mini_gc_improved_2 %>%
  group_by(Date) %>%
  summarise(across(Observation, mean))
mini_gc_improved_2 <- as.data.frame(mini_gc_improved_2[205:252,])
hist(mini_gc_improved_2$Observation)
gc2_improved <- ts(mini_gc_improved_2, frequency = 12, start = c(2019,1), end = c(2022,12))
plot(gc2_improved[,2], main = "Time series of average observation at Improved grassland", xlab = "Time",
     ylab = "Average observation")
ggseasonplot(gc2_improved[,2], year.labels=TRUE, year.labels.left=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Seasonal plot: Goldcrest observation count (Improved grassland)")

ggseasonplot(gc2_improved[,2], polar=TRUE) +
  ylab("Goldcrest") +
  ggtitle("Polar seasonal plot: Goldcrest observation count (Improved grassland)")

ggsubseriesplot(gc2_improved[,2]) +
  ylab("Goldcrest") +
  ggtitle("Seasonal subseries plot: Goldcrest observation count (Improved grassland)")

gc2Comp_improved <- decompose(gc2_improved[,2])
Box.test(gc2Comp_improved$random, lag = 13, type = "Ljung-Box")
# p-value < 0.05, reject null hypothesis, values are dependent

plot(gc2Comp_improved)
gc2SeasonAdj_improved <- gc2_improved[,2] - gc2Comp_improved$seasonal

Box.test(gc2_improved[,2], lag = 12, type = "Ljung-Box")
adf.test(gc2_improved[,2])
# Data is not stationary at original series, differencing required
fdiff_improved <- diff(gc2_improved[,2])
adf.test(fdiff_improved)

par(mfrow = c(1,2))
acf(gc2_improved[,2])
pacf(gc2_improved[,2])
# ACF shows data not a white noise, it is not independently and identically distributed
# and mean != 0
# Not white noise = Not GARCH process
gc_improved_arima <- auto.arima(fdiff_improved, max.p = 13, max.P = 13, 
                                stationary = TRUE, ic = "aic", approximation = FALSE,
                                stepwise = TRUE)
# SARIMA(0,0,1)(1,0,0)[12]
checkresiduals(gc_improved_arima)
Box.test(gc_improved_arima$residuals, lag = 12, type = "Ljung-Box")
adf.test(gc_improved_arima$residuals)
# p-value > 0.05, does not reject null hypothesis, values are independent
# is a WN

par(mfrow = c(1,1))


# Forecast 1
gc2.arima.train_improved <- arima(fdiff_improved, order = c(0,0,1), 
                                  seasonal = list(order = c(1,0,0), period = 12))
gc2.test_improved <- fdiff_improved[24:47]
gc2.arima.predict.2_improved <- predict(gc2.arima.train_improved, n.ahead = 24)
rmse(gc2.test_improved, gc2.arima.predict.2_improved$pred)
gc2.arima.pred_improved <- ts(gc2.arima.predict.2_improved$pred, frequency = 12, start = c(2020,1), end = c(2022,12))
plot(fdiff_improved, main = "Forecast of time series of 2020-2022", ylab = "Average observation")
lines(gc2.arima.pred_improved, col = "red")
legend(x = "topright", legend=c("Original time series", "Predicted Time series"),
       col=c("black", "red"), lty=1, cex=0.8, box.lty = 0)

# Forecast 2
improved_fore <- forecast(gc_improved_arima)
plot(improved_fore, xlim = c(2020,2025), main = "Forecast of time series of 2023-2025", ylab = "Average observation")
accuracy(improved_fore)

################################################################################
plot(gc2[,2], main = "Time series of average observation per month",
     xlab = "Time",
     ylab = "Average observation")
lines(gc2_urban[,2], col = "red")
lines(gc2_coastal[,2], col = "green")
lines(gc2_saltwater[,2], col = "blue")
lines(gc2_freshwater[,2], col = "pink")
lines(gc2_natural[,2], col = "yellow")
lines(gc2_broadleaf[,2], col = "orange")
lines(gc2_conferous[,2], col = "purple")
lines(gc2_arable[,2], col = "grey")
lines(gc2_improved[,2], col = "brown")
points(gc2[,2], pch=Months, cex=0.75, font=4, col=1:4)
points(gc2_urban[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_coastal[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_saltwater[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_freshwater[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_natural[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_broadleaf[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_conferous[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_arable[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
points(gc2_improved[,2], pch = Months, cex = 0.75, font = 4, col = 1:4)
legend(x = "topright", legend=c("Goldcrest time series", 
                                "Urban Time series",
                                "Coastal Time series",
                                "Saltwater Time series",
                                "Freshwater Time series",
                                "Natural Time series",
                                "Broadleaf Time series",
                                "Coniferous Time series",
                                "Arable Time series",
                                "Imporved Time series"),
       col=c("black", "red","green", "blue", "pink", "yellow", "orange", 
                    "purple", "gray", "brown"), lty=1, cex=0.8, box.lty = 0)


plot(gc2[,2], main = "Time series of average observation per month",
     xlab = "Time",
     ylab = "Average observation")
lines(gc2.arima.pred_urban, col = "red")
lines(gc2.arima.pred_coastal, col = "green")
lines(gc2.arima.pred_saltwater, col = "blue")
lines(gc2.arima.pred_freshwater, col = "pink")
lines(gc2.arima.pred_natural, col = "yellow")
lines(gc2.arima.pred_broadleaf, col = "orange")
lines(gc2.arima.pred_conferous, col = "purple")
lines(gc2.arima.pred_arable, col = "grey")
lines(gc2.arima.pred_improved, col = "brown")
